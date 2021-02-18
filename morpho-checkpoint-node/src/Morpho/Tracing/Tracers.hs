{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Morpho.Tracing.Tracers
  ( ChainInformation (..),
    Tracers (..),
    chainInformation,
    mkTracers,
    withTip,
  )
where

import Cardano.BM.Data.LogItem
import Cardano.BM.Data.Tracer (WithSeverity (..), annotateSeverity, mkObject, showTracing, trStructured)
import Cardano.BM.Trace
import Cardano.BM.Tracing
import Cardano.Prelude hiding (show)
import Codec.CBOR.Read (DeserialiseFailure)
import Control.Monad.Class.MonadSTM hiding (atomically)
import Control.Monad.Class.MonadTime
import Data.Aeson
import qualified Data.Text as Text
import Morpho.Config.Types
import Morpho.Ledger.Block
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.Update
import Morpho.Node.RunNode ()
import Morpho.Tracing.TracingOrphanInstances
import Morpho.Tracing.Types
import Network.Mux.Trace (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Types as ChainDB
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import qualified Ouroboros.Network.NodeToClient as NtC
import Ouroboros.Network.NodeToNode
import qualified Ouroboros.Network.NodeToNode as NtN
import Ouroboros.Network.Point
import Ouroboros.Network.Snocket (LocalAddress)
import Prelude (show)

data Tracers peer localPeer h c = Tracers
  { -- | Trace the ChainDB (flag '--trace-chain-db' will turn on textual output)
    chainDBTracer :: Tracer IO (ChainDB.TraceEvent (MorphoBlock h c)),
    -- | Consensus-specific tracers.
    consensusTracers :: Consensus.Tracers IO peer localPeer (MorphoBlock h c),
    -- | Trace the IP subscription manager (flag '--trace-ip-subscription' will turn on textual output)
    ipSubscriptionTracer :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr)),
    -- | Trace the DNS subscription manager (flag '--trace-dns-subscription' will turn on textual output)
    dnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr)),
    -- | Trace the DNS resolver (flag '--trace-dns-resolver' will turn on textual output)
    dnsResolverTracer :: Tracer IO (WithDomainName DnsTrace),
    -- | Trace Mux
    muxTracer :: Tracer IO (WithMuxBearer peer MuxTrace),
    -- | Trace error policy resolution (flag '--trace-error-policy' will turn on textual output)
    muxLocalTracer :: Tracer IO (WithMuxBearer (ConnectionId LocalAddress) MuxTrace),
    errorPolicyTracer :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace),
    powNodeRpcTracer :: Tracer IO PoWNodeRpcTrace,
    extractStateTracer :: Tracer IO (ExtractStateTrace h c),
    timeTravelErrorTracer :: Tracer IO (TimeTravelError (MorphoBlock h c)),
    -- | Chain Tip tracer.
    --
    --   Note: we're currently not using structured logging approach
    --   for the chain Tip tracer. Implementing all the required
    --   typeclasses is a hassle and we're not mining these
    --   structured logs anyways, we are just plain-text quering
    --   them. Modeling the chain tip hash as Text should be good
    --   enough for now.
    chainTipTracer :: Tracer IO Text,
    nodeToNodeTracers :: NodeToNode.Tracers IO peer (MorphoBlock h c) DeserialiseFailure,
    nodeToClientTracers :: NodeToClient.Tracers IO localPeer (MorphoBlock h c) DeserialiseFailure,
    handshakeTracer :: Tracer IO NtN.HandshakeTr,
    handshakeLocalTracer :: Tracer IO NtC.HandshakeTr,
    localErrorPolicyTracer :: Tracer IO (NtN.WithAddr NtC.LocalAddress NtN.ErrorPolicyTrace),
    acceptPolicyTracer :: Tracer IO AcceptConnectionsPolicyTrace
  }

-- | get information about a chain fragment
data ChainInformation = ChainInformation
  { slots :: Word64,
    blocks :: Word64,
    -- | the actual number of blocks created over the maximum
    -- expected number of blocks that could be created
    density :: Rational
  }

-- | Definition of the measurement datatype for the transactions.
data MeasureTxs blk
  = MeasureTxsTimeStart [GenTx blk] Word Word Time -- num txs, total size in bytes
  | MeasureTxsTimeStop SlotNo blk [GenTx blk]

deriving instance (Eq blk, Eq (GenTx blk)) => Eq (MeasureTxs blk)

deriving instance (Show blk, Show (GenTx blk)) => Show (MeasureTxs blk)

instance HasPrivacyAnnotation (MeasureTxs blk)

instance HasSeverityAnnotation (MeasureTxs blk)

instance Transformable Text IO (MeasureTxs blk) where
  trTransformer = trStructured

-- TODO(KS): Clarify the structure of the type.
instance ToObject (MeasureTxs blk) where
  toObject _verb _ =
    mkObject
      [ "kind" .= String "MeasureTxsTimeStart"
      ]

-- | Generates all the tracers necessary for the checkpointing node.
--
-- Note: the constraint on the morpho block is necessary for the
-- Condense implementations.
mkTracers ::
  forall peer localPeer blk h c.
  ( Show peer,
    Show localPeer,
    MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c,
    LedgerSupportsProtocol blk
  ) =>
  TraceOptions ->
  Trace IO Text ->
  Tracers peer localPeer h c
mkTracers traceOptions tracer =
  Tracers
    { chainDBTracer =
        showOnOff
          (traceChainDB traceOptions)
          "ChainDB"
          tracer,
      consensusTracers =
        mkConsensusTracers traceOptions,
      ipSubscriptionTracer =
        showOnOff
          (traceIpSubscription traceOptions)
          "IpSubscription"
          tracer,
      dnsSubscriptionTracer =
        showOnOff
          (traceDnsSubscription traceOptions)
          "DnsSubscription"
          tracer,
      dnsResolverTracer =
        showOnOff
          (traceDnsResolver traceOptions)
          "DnsResolver"
          tracer,
      muxTracer =
        showOnOff
          (traceMux traceOptions)
          "Mux"
          tracer,
      muxLocalTracer =
        showOnOff
          (traceMux traceOptions)
          "LocalMux"
          tracer,
      errorPolicyTracer =
        showOnOff
          (traceErrorPolicy traceOptions)
          "ErrorPolicy"
          tracer,
      extractStateTracer =
        showOnOff
          (traceLedgerState traceOptions)
          "ExtractState"
          tracer,
      powNodeRpcTracer =
        showOnOff
          (tracePoWNodeRpc traceOptions)
          "PoWNodeRpc"
          tracer,
      timeTravelErrorTracer =
        showOnOff
          (traceTimeTravelError traceOptions)
          "TimeTravelError"
          tracer,
      chainTipTracer =
        showOnOff
          (traceTimeTravelError traceOptions)
          "chainTipTracer"
          tracer,
      nodeToNodeTracers =
        nodeToNodeTracers' traceOptions tracer,
      nodeToClientTracers =
        nodeToClientTracers' traceOptions tracer,
      handshakeTracer =
        showOnOff
          (traceHandshake traceOptions)
          "Handshake"
          tracer,
      handshakeLocalTracer =
        showOnOff
          (traceHandshake traceOptions)
          "HandshakeLocal"
          tracer,
      localErrorPolicyTracer =
        showOnOff
          (traceErrorPolicy traceOptions)
          "Handshake"
          tracer,
      acceptPolicyTracer =
        showOnOff
          (traceErrorPolicy traceOptions)
          "Handshake"
          tracer
    }
  where
    mkConsensusTracers :: TraceOptions -> Consensus.Tracers IO peer localPeer blk
    mkConsensusTracers traceOpts =
      Consensus.Tracers
        { Consensus.chainSyncClientTracer =
            showOnOff
              (traceChainSyncClient traceOpts)
              "ChainSyncClient"
              tracer,
          Consensus.chainSyncServerHeaderTracer =
            showOnOff
              (traceChainSyncHeaderServer traceOpts)
              "ChainSyncHeaderServer"
              tracer,
          Consensus.chainSyncServerBlockTracer =
            showOnOff
              (traceChainSyncBlockServer traceOpts)
              "ChainSyncBlockServer"
              tracer,
          Consensus.blockFetchDecisionTracer =
            showOnOff
              (traceBlockFetchDecisions traceOpts)
              "BlockFetchDecision"
              tracer,
          Consensus.blockFetchClientTracer =
            showOnOff
              (traceBlockFetchClient traceOpts)
              "BlockFetchClient"
              tracer,
          Consensus.blockFetchServerTracer =
            showOnOff
              (traceBlockFetchServer traceOpts)
              "BlockFetchServer"
              tracer,
          Consensus.txInboundTracer =
            showOnOff
              (traceTxInbound traceOpts)
              "TxInbound"
              tracer,
          Consensus.txOutboundTracer =
            showOnOff
              (traceTxOutbound traceOpts)
              "TxOutbound"
              tracer,
          Consensus.localTxSubmissionServerTracer =
            showOnOff
              (traceLocalTxSubmissionServer traceOpts)
              "LocalTxSubmissionServer"
              tracer,
          Consensus.mempoolTracer =
            showOnOff (traceMempool traceOpts) "Mempool" tracer,
          Consensus.forgeTracer =
            showOnOff
              (traceForge traceOpts)
              "Forge"
              tracer,
          Consensus.blockchainTimeTracer =
            showOnOff
              True
              "BlockchainTime"
              tracer,
          Consensus.forgeStateTracer =
            showOnOff
              True
              "ForgeState"
              tracer
        }

chainInformation ::
  forall block.
  AF.HasHeader block =>
  AF.AnchoredFragment block ->
  ChainInformation
chainInformation frag =
  ChainInformation
    { slots = slotN,
      blocks = blockN,
      density = calcDensity blockD slotD
    }
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD =
      slotN
        - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo . fromMaybe 1 $ withOriginToMaybe (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _ -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b

-- | A way to satisfy tracer which requires current tip.  The tip is read from
-- a mutable cell.
withTip ::
  TVar IO (Point blk) ->
  Tracer IO (WithTip blk a) ->
  Tracer IO a
withTip varTip tr = Tracer $ \msg -> do
  tip <- atomically $ readTVar varTip
  traceWith (contramap (WithTip tip) tr) msg

--------------------------------------------------------------------------------
-- NodeToNode Tracers
--------------------------------------------------------------------------------

nodeToNodeTracers' ::
  ( Show peer,
    MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c
  ) =>
  TraceOptions ->
  Trace IO Text ->
  NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToNodeTracers' traceOptions tracer =
  NodeToNode.Tracers
    { NodeToNode.tChainSyncTracer =
        showOnOff
          (traceChainSyncProtocol traceOptions)
          "ChainSyncProtocol"
          tracer,
      NodeToNode.tChainSyncSerialisedTracer =
        showOnOff
          (traceChainSyncProtocol traceOptions)
          "ChainSyncProtocolSerialised"
          tracer,
      NodeToNode.tBlockFetchTracer =
        showOnOff
          (traceBlockFetchProtocol traceOptions)
          "BlockFetchProtocol"
          tracer,
      NodeToNode.tBlockFetchSerialisedTracer =
        showOnOff
          (traceBlockFetchProtocolSerialised traceOptions)
          "BlockFetchProtocolSerialised"
          tracer,
      NodeToNode.tTxSubmissionTracer =
        showOnOff
          (traceTxSubmissionProtocol traceOptions)
          "TxSubmissionProtocol"
          tracer
    }

nodeToClientTracers' ::
  ( Show peer,
    blk ~ MorphoBlock h c,
    HashAlgorithm h,
    BftCrypto c
  ) =>
  TraceOptions ->
  Trace IO Text ->
  NodeToClient.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToClientTracers' traceOptions tracer =
  NodeToClient.Tracers
    { NodeToClient.tChainSyncTracer =
        showOnOff
          (traceLocalChainSyncProtocol traceOptions)
          "LocalChainSyncProtocol"
          tracer,
      NodeToClient.tTxSubmissionTracer =
        showOnOff
          (traceLocalTxSubmissionProtocol traceOptions)
          "LocalTxSubmissionProtocol"
          tracer,
      NodeToClient.tStateQueryTracer =
        showOnOff
          (traceLocalStateQueryProtocol traceOptions)
          "LocalStateQueryProtocol"
          tracer
    }

instance Show a => Show (WithSeverity a) where
  show (WithSeverity _sev a) = show a

showOnOff ::
  (Show a, HasSeverityAnnotation a) =>
  Bool ->
  LoggerName ->
  Trace IO Text ->
  Tracer IO a
showOnOff False _ _ = nullTracer
showOnOff True name trcer =
  annotateSeverity $
    showTracing $
      contramap Text.pack $
        toLogObject $
          appendName name trcer
