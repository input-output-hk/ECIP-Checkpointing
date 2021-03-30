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

import Cardano.BM.Data.Tracer (WithSeverity (..), showTracing)
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
          toLogObject $
            appendName "morpho-init" tracer,
        chainDBTracer =
          toLogObject $
            appendName "chain-db" tracer,
        consensusTracers =
          mkConsensusTracers (appendName "consensus" tracer),
        ipSubscriptionTracer =
          toLogObject $
            appendName "ip-subs" tracer,
        dnsSubscriptionTracer =
          toLogObject $
            appendName "dns-subs" tracer,
        dnsResolverTracer =
          toLogObject $
            appendName "dns-resolve" tracer,
        muxTracer =
          toLogObject $
            appendName "mux" tracer,
        muxLocalTracer =
          toLogObject $
            appendName "local-mux" tracer,
        errorPolicyTracer =
          toLogObject $
            appendName "error-policy" tracer,
        extractStateTracer =
          toLogObject $
            appendName "extract-state" tracer,
        powNodeRpcTracer =
          toLogObject $
            appendName "rpc" tracer,
        timeTravelErrorTracer =
          toLogObject $
            appendName "time-travel" tracer,
        chainTipTracer =
          toLogObject $
            appendName "chain-tip" tracer,
        nodeToNodeTracers =
          nodeToNodeTracers' tracer,
        nodeToClientTracers =
          nodeToClientTracers' tracer,
        handshakeTracer =
          toLogObject $
            appendName "handshake" tracer,
        handshakeLocalTracer =
          toLogObject $
            appendName "local-handshake" tracer,
        localErrorPolicyTracer =
          toLogObject $
            appendName "local-error-policy" tracer,
        acceptPolicyTracer =
          toLogObject $
            appendName "accept-policy" tracer,
        diffusionInitializationTracer =
          toLogObject $
            appendName "diffusion-init" tracer,
        ledgerPeersTracer =
          toLogObject $
            appendName "ledger-peers" tracer
      }
  where
    mkConsensusTracers :: Trace IO Text -> Consensus.Tracers IO peer localPeer blk
    mkConsensusTracers ctracer =
      Consensus.Tracers
        { Consensus.chainSyncClientTracer =
            toLogObject $
              appendName "chain-sync-client" ctracer,
          Consensus.chainSyncServerHeaderTracer =
            toLogObject $
              appendName "chain-sync-server-header" ctracer,
          Consensus.chainSyncServerBlockTracer =
            toLogObject $
              appendName "chain-sync-server-block" ctracer,
          Consensus.blockFetchDecisionTracer =
            toLogObject $
              appendName "block-fetch-decision" ctracer,
          Consensus.blockFetchClientTracer =
            toLogObject $
              appendName "block-fetch-client" ctracer,
          Consensus.blockFetchServerTracer =
            toLogObject $
              appendName "block-fetch-server" ctracer,
          Consensus.txInboundTracer =
            toLogObject $
              appendName "tx-in" ctracer,
          Consensus.txOutboundTracer =
            toLogObject $
              appendName "tx-out" ctracer,
          Consensus.localTxSubmissionServerTracer =
            toLogObject $
              appendName "local-tx-submission" ctracer,
          Consensus.mempoolTracer =
            toLogObject $
              appendName "mempool" ctracer,
          Consensus.forgeTracer =
            toLogObject $
              appendName "forge" ctracer,
          Consensus.blockchainTimeTracer =
            showTracing $
              withName "blockchain-time" ctracer,
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
        toLogObject $
          appendName "chain-sync-protocol" tracer,
      NodeToNode.tChainSyncSerialisedTracer =
        toLogObject $
          appendName "chain-sync-protocol-serialized" tracer,
      NodeToNode.tBlockFetchTracer =
        toLogObject $
          appendName "block-fetch-protocol" tracer,
      NodeToNode.tBlockFetchSerialisedTracer =
        showTracing $
          withName "block-fetch-protocol-serialized" tracer,
      NodeToNode.tTxSubmissionTracer =
        toLogObject $
          appendName "tx-submission-protocol" tracer,
      NodeToNode.tTxSubmission2Tracer =
        toLogObject $
          appendName "tx-submission-protocol-2" tracer
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
        toLogObject $
          appendName "local-chain-sync-protocol" tracer,
      NodeToClient.tTxSubmissionTracer =
        toLogObject $
          appendName "local-tx-submission-protocol" tracer,
      NodeToClient.tStateQueryTracer =
        toLogObject $
          appendName "local-state-query-protocol" tracer
    }

instance Show a => Show (WithSeverity a) where
  show (WithSeverity _sev a) = show a

withName :: Text -> Trace IO Text -> Tracer IO String
withName name tr = contramap Text.pack $ toLogObject $ appendName name tr
