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
  Int ->
  Trace m Text ->
  Tracer m b
toGenericObject verbosity tr = Tracer $ \arg ->
  let logRec = verbosity + minLogRecursion arg
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
  ( MPretty peer,
    ToJSON peer,
    MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c,
    Signable (BftDSIGN c) (MorphoStdHeader h c),
    LedgerSupportsProtocol blk
  ) =>
  Int ->
  Trace IO Text ->
  IO (Tracers peer h c)
mkTracers verbosity tracer = do
  pure
    Tracers
      { morphoInitTracer =
          toGenericObject verbosity $
            appendName "morpho-init" tracer,
        chainDBTracer =
          toGenericObject verbosity $
            appendName "chain-db" tracer,
        consensusTracers =
          mkConsensusTracers (appendName "consensus" tracer),
        ipSubscriptionTracer =
          toGenericObject verbosity $
            appendName "ip-subs" tracer,
        dnsSubscriptionTracer =
          toGenericObject verbosity $
            appendName "dns-subs" tracer,
        dnsResolverTracer =
          toGenericObject verbosity $
            appendName "dns-resolve" tracer,
        muxTracer =
          toGenericObject verbosity $
            appendName "mux" tracer,
        muxLocalTracer =
          toGenericObject verbosity $
            appendName "local-mux" tracer,
        errorPolicyTracer =
          toGenericObject verbosity $
            appendName "error-policy" tracer,
        extractStateTracer =
          toGenericObject verbosity $
            appendName "extract-state" tracer,
        powNodeRpcTracer =
          toGenericObject verbosity $
            appendName "rpc" tracer,
        timeTravelErrorTracer =
          toGenericObject verbosity $
            appendName "time-travel" tracer,
        nodeToNodeTracers =
          nodeToNodeTracers' verbosity tracer,
        handshakeTracer =
          toGenericObject verbosity $
            appendName "handshake" tracer,
        handshakeLocalTracer =
          toGenericObject verbosity $
            appendName "local-handshake" tracer,
        localErrorPolicyTracer =
          toGenericObject verbosity $
            appendName "local-error-policy" tracer,
        acceptPolicyTracer =
          toGenericObject verbosity $
            appendName "accept-policy" tracer,
        diffusionInitializationTracer =
          toGenericObject verbosity $
            appendName "diffusion-init" tracer,
        ledgerPeersTracer =
          toGenericObject verbosity $
            appendName "ledger-peers" tracer
      }
  where
    mkConsensusTracers :: Trace IO Text -> Consensus.Tracers IO peer localPeer blk
    mkConsensusTracers ctracer =
      Consensus.Tracers
        { Consensus.chainSyncClientTracer =
            toGenericObject verbosity $
              appendName "chain-sync-client" ctracer,
          Consensus.chainSyncServerHeaderTracer =
            toGenericObject verbosity $
              appendName "chain-sync-server-header" ctracer,
          Consensus.chainSyncServerBlockTracer =
            toGenericObject verbosity $
              appendName "chain-sync-server-block" ctracer,
          Consensus.blockFetchDecisionTracer =
            separate $
              toGenericObject verbosity $
                appendName "block-fetch-decision" ctracer,
          Consensus.blockFetchClientTracer =
            toGenericObject verbosity $
              appendName "block-fetch-client" ctracer,
          Consensus.blockFetchServerTracer =
            toGenericObject verbosity $
              appendName "block-fetch-server" ctracer,
          Consensus.txInboundTracer =
            toGenericObject verbosity $
              appendName "tx-in" ctracer,
          Consensus.txOutboundTracer =
            toGenericObject verbosity $
              appendName "tx-out" ctracer,
          Consensus.localTxSubmissionServerTracer =
            toGenericObject verbosity $
              appendName "local-tx-submission" ctracer,
          Consensus.mempoolTracer =
            toGenericObject verbosity $
              appendName "mempool" ctracer,
          Consensus.forgeTracer =
            toGenericObject verbosity $
              appendName "forge" ctracer,
          Consensus.blockchainTimeTracer =
            toGenericObject verbosity $
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
    MPretty peer,
    MorphoStateDefaultConstraints h c,
    blk ~ MorphoBlock h c
  ) =>
  Int ->
  Trace IO Text ->
  NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO)
nodeToNodeTracers' verbosity tracer =
  NodeToNode.Tracers
    { NodeToNode.tChainSyncTracer =
        toGenericObject verbosity $
          appendName "chain-sync-protocol" tracer,
      NodeToNode.tChainSyncSerialisedTracer =
        toGenericObject verbosity $
          appendName "chain-sync-protocol-serialized" tracer,
      NodeToNode.tBlockFetchTracer =
        toGenericObject verbosity $
          appendName "block-fetch-protocol" tracer,
      NodeToNode.tBlockFetchSerialisedTracer =
        toGenericObject verbosity $
          appendName "block-fetch-protocol-serialized" tracer,
      NodeToNode.tTxSubmissionTracer =
        toGenericObject verbosity $
          appendName "tx-submission-protocol" tracer,
      NodeToNode.tTxSubmission2Tracer =
        toGenericObject verbosity $
          appendName "tx-submission-protocol-2" tracer
    }
