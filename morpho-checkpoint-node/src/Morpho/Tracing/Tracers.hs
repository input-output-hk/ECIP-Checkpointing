{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Morpho.Tracing.Tracers
  ( Tracers (..),
    mkTracers,
  )
where

import Cardano.BM.Data.Tracer (WithSeverity (..), annotateSeverity, showTracing)
import Cardano.BM.Trace
import Cardano.BM.Tracing
import Cardano.Crypto.DSIGN.Class
import Cardano.Prelude hiding (show)
import Codec.CBOR.Read (DeserialiseFailure)
import Data.Aeson
import qualified Data.Text as Text
import Morpho.Ledger.Block
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.Update
import Morpho.Node.RunNode ()
import Morpho.RPC.Abstract
import Morpho.Tracing.TracingOrphanInstances ()
import Morpho.Tracing.Types
import Network.Mux.Trace (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import Ouroboros.Consensus.Protocol.BFT
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Types as ChainDB
import Ouroboros.Network.Diffusion
import qualified Ouroboros.Network.NodeToClient as NtC
import Ouroboros.Network.NodeToNode
import qualified Ouroboros.Network.NodeToNode as NtN
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.Snocket (LocalAddress)
import Prelude (String, show)

data Tracers peer localPeer h c = Tracers
  { -- | Used for top-level morpho traces during initialization
    morphoInitTracer :: Tracer IO MorphoInitTrace,
    -- | Trace the ChainDB (flag '--trace-chain-db' will turn on textual output)
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
    powNodeRpcTracer :: forall i o ev. (Show ev, ToJSON ev, HasSeverityAnnotation ev) => Tracer IO (RpcTrace ev i o),
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
    acceptPolicyTracer :: Tracer IO AcceptConnectionsPolicyTrace,
    diffusionInitializationTracer :: Tracer IO DiffusionInitializationTracer,
    ledgerPeersTracer :: Tracer IO TraceLedgerPeers
  }

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
    Signable (BftDSIGN c) (MorphoStdHeader h c),
    LedgerSupportsProtocol blk
  ) =>
  Trace IO Text ->
  IO (Tracers peer localPeer h c)
mkTracers tracer = do
  pure
    Tracers
      { morphoInitTracer =
          annotateSeverity $
            toLogObject $
              appendName "MorphoInit" tracer,
        chainDBTracer =
          annotateSeverity $
            toLogObject $
              appendName "ChainDB" tracer,
        consensusTracers =
          mkConsensusTracers,
        ipSubscriptionTracer =
          annotateSeverity $
            toLogObject $
              appendName "IpSubscription" tracer,
        dnsSubscriptionTracer =
          annotateSeverity $
            toLogObject $
              appendName "DnsSubscription" tracer,
        dnsResolverTracer =
          annotateSeverity $
            toLogObject $
              appendName "DnsResolver" tracer,
        muxTracer =
          annotateSeverity $
            toLogObject $
              appendName "Mux" tracer,
        muxLocalTracer =
          annotateSeverity $
            toLogObject $
              appendName "LocalMux" tracer,
        errorPolicyTracer =
          annotateSeverity $
            toLogObject $
              appendName "ErrorPolicy" tracer,
        extractStateTracer =
          annotateSeverity $
            toLogObject $
              appendName "ExtractState" tracer,
        powNodeRpcTracer =
          annotateSeverity $
            toLogObject $
              appendName "PoWNodeRpc" tracer,
        timeTravelErrorTracer =
          annotateSeverity $
            toLogObject $
              appendName "TimeTravelError" tracer,
        chainTipTracer =
          annotateSeverity $
            toLogObject $
              appendName "chainTipTracer" tracer,
        nodeToNodeTracers =
          nodeToNodeTracers' tracer,
        nodeToClientTracers =
          nodeToClientTracers' tracer,
        handshakeTracer =
          annotateSeverity $
            toLogObject $
              appendName "Handshake" tracer,
        handshakeLocalTracer =
          annotateSeverity $
            toLogObject $
              appendName "HandshakeLocal" tracer,
        localErrorPolicyTracer =
          annotateSeverity $
            toLogObject $
              appendName "Handshake" tracer,
        acceptPolicyTracer =
          annotateSeverity $
            toLogObject $
              appendName "Handshake" tracer,
        diffusionInitializationTracer =
          annotateSeverity $
            toLogObject $
              appendName "DiffusionInitialization" tracer,
        ledgerPeersTracer =
          annotateSeverity $
            toLogObject $
              appendName "LedgerPeers" tracer
      }
  where
    mkConsensusTracers :: Consensus.Tracers IO peer localPeer blk
    mkConsensusTracers =
      Consensus.Tracers
        { Consensus.chainSyncClientTracer =
            toLogObject $
              appendName "ChainSyncClient" tracer,
          Consensus.chainSyncServerHeaderTracer =
            toLogObject $
              appendName "ChainSyncHeaderServer" tracer,
          Consensus.chainSyncServerBlockTracer =
            toLogObject $
              appendName "ChainSyncBlockServer" tracer,
          Consensus.blockFetchDecisionTracer =
            toLogObject $
              appendName "BlockFetchDecision" tracer,
          Consensus.blockFetchClientTracer =
            toLogObject $
              appendName "BlockFetchClient" tracer,
          Consensus.blockFetchServerTracer =
            toLogObject $
              appendName "BlockFetchServer" tracer,
          Consensus.txInboundTracer =
            toLogObject $
              appendName "TxInbound" tracer,
          Consensus.txOutboundTracer =
            toLogObject $
              appendName "TxOutbound" tracer,
          Consensus.localTxSubmissionServerTracer =
            toLogObject $
              appendName "LocalTxSubmissionServer" tracer,
          Consensus.mempoolTracer =
            annotateSeverity $
              toLogObject $
                appendName "Mempool" tracer,
          Consensus.forgeTracer =
            annotateSeverity $
              toLogObject $
                appendName "Forge" tracer,
          Consensus.blockchainTimeTracer =
            annotateSeverity $
              showTracing $
                withName "BlockFetchProtocolSerialised" tracer,
          -- TODO: trace the forge state if we add any.
          Consensus.forgeStateInfoTracer = Tracer $ const mempty,
          -- TODO: Trace this
          Consensus.keepAliveClientTracer = Tracer $ const mempty
        }

--------------------------------------------------------------------------------
-- NodeToNode Tracers
--------------------------------------------------------------------------------

nodeToNodeTracers' ::
  ( Show peer,
    MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c
  ) =>
  Trace IO Text ->
  NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToNodeTracers' tracer =
  NodeToNode.Tracers
    { NodeToNode.tChainSyncTracer =
        annotateSeverity $
          toLogObject $
            appendName "ChainSyncProtocol" tracer,
      NodeToNode.tChainSyncSerialisedTracer =
        annotateSeverity $
          toLogObject $
            appendName "ChainSyncProtocolSerialised" tracer,
      NodeToNode.tBlockFetchTracer =
        annotateSeverity $
          toLogObject $
            appendName "BlockFetchProtocol" tracer,
      NodeToNode.tBlockFetchSerialisedTracer =
        annotateSeverity $
          showTracing $
            withName "BlockFetchProtocolSerialised" tracer,
      NodeToNode.tTxSubmissionTracer =
        annotateSeverity $
          toLogObject $
            appendName "TxSubmissionProtocol" tracer,
      NodeToNode.tTxSubmission2Tracer =
        annotateSeverity $
          toLogObject $
            appendName "TxSubmission2Protocol" tracer
    }

nodeToClientTracers' ::
  ( Show peer,
    blk ~ MorphoBlock h c
  ) =>
  Trace IO Text ->
  NodeToClient.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToClientTracers' tracer =
  NodeToClient.Tracers
    { NodeToClient.tChainSyncTracer =
        annotateSeverity $
          toLogObject $
            appendName "LocalChainSyncProtocol" tracer,
      NodeToClient.tTxSubmissionTracer =
        annotateSeverity $
          toLogObject $
            appendName "LocalTxSubmissionProtocol" tracer,
      NodeToClient.tStateQueryTracer =
        annotateSeverity $
          toLogObject $
            appendName "LocalStateQueryProtocol" tracer
    }

instance Show a => Show (WithSeverity a) where
  show (WithSeverity _sev a) = show a

withName :: Text -> Trace IO Text -> Tracer IO String
withName name tr = contramap Text.pack $ toLogObject $ appendName name tr
