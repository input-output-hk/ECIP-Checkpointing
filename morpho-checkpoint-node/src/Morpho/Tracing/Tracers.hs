{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Morpho.Tracing.Tracers
  ( Tracers (..),
    mkTracers,
  )
where

import Cardano.BM.Data.LogItem
import Cardano.BM.Trace
import Cardano.BM.Tracing
import Cardano.Crypto.DSIGN.Class
import Cardano.Prelude hiding (show)
import Codec.CBOR.Read (DeserialiseFailure)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Morpho.Config.Types
import Morpho.Ledger.Block
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.Ledger.Update
import Morpho.RPC.Abstract
import Morpho.Tracing.Orphan.TracingInstances ()
import Morpho.Tracing.Pretty (MPretty (..))
import Morpho.Tracing.Types
import Morpho.Tracing.Verbosity
import Network.Mux.Trace (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket
import Ouroboros.Consensus.Ledger.SupportsProtocol
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
import Prettyprinter (layoutCompact)
import Prettyprinter.Render.Text (renderStrict)

data Tracers peer h c = Tracers
  { -- | Used for top-level morpho traces during initialization
    morphoInitTracer :: Tracer IO MorphoInitTrace,
    -- | Trace the ChainDB (flag '--trace-chain-db' will turn on textual output)
    chainDBTracer :: Tracer IO (ChainDB.TraceEvent (MorphoBlock h c)),
    -- | Consensus-specific tracers.
    consensusTracers :: forall localPeer. Consensus.Tracers IO peer localPeer (MorphoBlock h c),
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
    nodeToNodeTracers :: NodeToNode.Tracers IO peer (MorphoBlock h c) DeserialiseFailure,
    handshakeTracer :: Tracer IO NtN.HandshakeTr,
    handshakeLocalTracer :: Tracer IO NtC.HandshakeTr,
    localErrorPolicyTracer :: Tracer IO (NtN.WithAddr NtC.LocalAddress NtN.ErrorPolicyTrace),
    acceptPolicyTracer :: Tracer IO AcceptConnectionsPolicyTrace,
    diffusionInitializationTracer :: Tracer IO DiffusionInitializationTracer,
    ledgerPeersTracer :: Tracer IO TraceLedgerPeers
  }

-- Logs a traversable by logging each item separately
-- Copied from co-log-core:
-- https://hackage.haskell.org/package/co-log-core-0.2.1.1/docs/Colog-Core-Action.html#v:separate
separate :: (Traversable f, Applicative m) => Tracer m msg -> Tracer m (f msg)
separate (Tracer action) = Tracer (traverse_ action)

toGenericObject ::
  ( MonadIO m,
    MPretty b,
    HasSeverityAnnotation b,
    HasPrivacyAnnotation b,
    MinLogRecursion b,
    ToJSON b
  ) =>
  NodeConfiguration ->
  Trace m Text ->
  Tracer m b
toGenericObject nc tr = Tracer $ \arg ->
  let logRec = ncVerbosity nc + minLogRecursion arg
      value = snd $ limitRecursion logRec (toJSON arg)
      text = renderStrict $ layoutCompact $ mpretty arg
      -- Insert the formatted text into the structured value, so that we can
      -- easily get a nice-looking value for display
      finalObj =
        HM.fromList
          [ "value" .= value,
            "text" .= String text
          ]
   in traceWith tr =<< do
        meta <- mkLOMeta (getSeverityAnnotation arg) (getPrivacyAnnotation arg)
        return (mempty, LogObject mempty meta (LogStructuredText finalObj text))

-- | Generates all the tracers necessary for the checkpointing node.
--
-- Note: the constraint on the morpho block is necessary for the
-- Condense implementations.
mkTracers ::
  forall peer blk h c.
  ( Show peer,
    ToJSON peer,
    MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c,
    Signable (BftDSIGN c) (MorphoStdHeader h c),
    LedgerSupportsProtocol blk
  ) =>
  NodeConfiguration ->
  Trace IO Text ->
  IO (Tracers peer h c)
mkTracers nc tracer = do
  pure
    Tracers
      { morphoInitTracer =
          toGenericObject nc $
            appendName "morpho-init" tracer,
        chainDBTracer =
          toGenericObject nc $
            appendName "chain-db" tracer,
        consensusTracers =
          mkConsensusTracers (appendName "consensus" tracer),
        ipSubscriptionTracer =
          toGenericObject nc $
            appendName "ip-subs" tracer,
        dnsSubscriptionTracer =
          toGenericObject nc $
            appendName "dns-subs" tracer,
        dnsResolverTracer =
          toGenericObject nc $
            appendName "dns-resolve" tracer,
        muxTracer =
          toGenericObject nc $
            appendName "mux" tracer,
        muxLocalTracer =
          toGenericObject nc $
            appendName "local-mux" tracer,
        errorPolicyTracer =
          toGenericObject nc $
            appendName "error-policy" tracer,
        extractStateTracer =
          toGenericObject nc $
            appendName "extract-state" tracer,
        powNodeRpcTracer =
          toGenericObject nc $
            appendName "rpc" tracer,
        timeTravelErrorTracer =
          toGenericObject nc $
            appendName "time-travel" tracer,
        nodeToNodeTracers =
          nodeToNodeTracers' nc tracer,
        handshakeTracer =
          toGenericObject nc $
            appendName "handshake" tracer,
        handshakeLocalTracer =
          toGenericObject nc $
            appendName "local-handshake" tracer,
        localErrorPolicyTracer =
          toGenericObject nc $
            appendName "local-error-policy" tracer,
        acceptPolicyTracer =
          toGenericObject nc $
            appendName "accept-policy" tracer,
        diffusionInitializationTracer =
          toGenericObject nc $
            appendName "diffusion-init" tracer,
        ledgerPeersTracer =
          toGenericObject nc $
            appendName "ledger-peers" tracer
      }
  where
    mkConsensusTracers :: Trace IO Text -> Consensus.Tracers IO peer localPeer blk
    mkConsensusTracers ctracer =
      Consensus.Tracers
        { Consensus.chainSyncClientTracer =
            toGenericObject nc $
              appendName "chain-sync-client" ctracer,
          Consensus.chainSyncServerHeaderTracer =
            toGenericObject nc $
              appendName "chain-sync-server-header" ctracer,
          Consensus.chainSyncServerBlockTracer =
            toGenericObject nc $
              appendName "chain-sync-server-block" ctracer,
          Consensus.blockFetchDecisionTracer =
            separate $
              toGenericObject nc $
                appendName "block-fetch-decision" ctracer,
          Consensus.blockFetchClientTracer =
            toGenericObject nc $
              appendName "block-fetch-client" ctracer,
          Consensus.blockFetchServerTracer =
            toGenericObject nc $
              appendName "block-fetch-server" ctracer,
          Consensus.txInboundTracer =
            toGenericObject nc $
              appendName "tx-in" ctracer,
          Consensus.txOutboundTracer =
            toGenericObject nc $
              appendName "tx-out" ctracer,
          Consensus.localTxSubmissionServerTracer =
            toGenericObject nc $
              appendName "local-tx-submission" ctracer,
          Consensus.mempoolTracer =
            toGenericObject nc $
              appendName "mempool" ctracer,
          Consensus.forgeTracer =
            toGenericObject nc $
              appendName "forge" ctracer,
          Consensus.blockchainTimeTracer =
            toGenericObject nc $
              appendName "blockchain-time" ctracer,
          -- TODO: trace the forge state if we add any.
          Consensus.forgeStateInfoTracer = Tracer $ const mempty,
          -- TODO: Trace this
          Consensus.keepAliveClientTracer = Tracer $ const mempty
        }

--------------------------------------------------------------------------------
-- NodeToNode Tracers
--------------------------------------------------------------------------------

nodeToNodeTracers' ::
  ( ToJSON peer,
    Show peer,
    MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c
  ) =>
  NodeConfiguration ->
  Trace IO Text ->
  NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToNodeTracers' nc tracer =
  NodeToNode.Tracers
    { NodeToNode.tChainSyncTracer =
        toGenericObject nc $
          appendName "chain-sync-protocol" tracer,
      NodeToNode.tChainSyncSerialisedTracer =
        toGenericObject nc $
          appendName "chain-sync-protocol-serialized" tracer,
      NodeToNode.tBlockFetchTracer =
        toGenericObject nc $
          appendName "block-fetch-protocol" tracer,
      NodeToNode.tBlockFetchSerialisedTracer =
        toGenericObject nc $
          appendName "block-fetch-protocol-serialized" tracer,
      NodeToNode.tTxSubmissionTracer =
        toGenericObject nc $
          appendName "tx-submission-protocol" tracer,
      NodeToNode.tTxSubmission2Tracer =
        toGenericObject nc $
          appendName "tx-submission-protocol-2" tracer
    }
