{-# LANGUAGE FlexibleInstances #-}

module Morpho.Tracing.Verbosity where

import Cardano.Prelude
import Data.Aeson
import Morpho.Ledger.Block
import Morpho.Ledger.SnapshotTimeTravel
import Morpho.RPC.Abstract
import Morpho.Tracing.Types
import Network.Mux
import qualified Network.Socket as Socket
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Mempool
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( TraceChainSyncClientEvent,
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( TraceChainSyncServerEvent (..),
  )
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
import Ouroboros.Consensus.Node.Tracers (TraceForgeEvent, TraceLabelCreds (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Types as ChainDB
import Ouroboros.Network.BlockFetch
import Ouroboros.Network.Diffusion
import Ouroboros.Network.NodeToNode
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.TxSubmission.Inbound
import Ouroboros.Network.TxSubmission.Outbound
  ( TraceTxSubmissionOutbound (..),
  )

-- | Limits the recursion depth of the given 'Value' to the specified 'Int'
-- When the recursion depth is reached, a JSON object is replaced with the
-- string @{ ... }@, while a JSON list is replaced with the string @[ ... ]@
-- Also returned is an 'Any' value representing whether any such recursion
-- limiting was done
limitRecursion :: Int -> Value -> (Any, Value)
limitRecursion n (Object xs)
  | n <= 0 = (Any True, String "{ ... }")
  | otherwise =
    -- This uses the Applicative instance
    --   Monoid a => Applicative ((,) a)
    -- with a ~ Any for the traverse
    second Object $ traverse (limitRecursion (n - 1)) xs
limitRecursion n (Array xs)
  | n <= 0 = (Any True, String "[ ... ]")
  | otherwise =
    -- Also uses above-described Applicative instance
    -- We don't decrease n here because JSON lists are pretty slim. We're
    -- mainly just concerned about objects for the recursion
    second Array $ traverse (limitRecursion n) xs
limitRecursion _ v = (Any False, v)

class MinLogRecursion a where
  minLogRecursion :: a -> Int

instance MinLogRecursion a => MinLogRecursion (WithIPList a) where
  minLogRecursion (WithIPList _ _ a) = 1 + minLogRecursion a

instance MinLogRecursion a => MinLogRecursion (WithDomainName a) where
  minLogRecursion (WithDomainName _ a) = 1 + minLogRecursion a

instance MinLogRecursion a => MinLogRecursion (WithMuxBearer peer a) where
  minLogRecursion (WithMuxBearer _ a) = 1 + minLogRecursion a

instance MinLogRecursion a => MinLogRecursion (WithAddr peer a) where
  minLogRecursion (WithAddr _ a) = 1 + minLogRecursion a

instance MinLogRecursion a => MinLogRecursion (TraceLabelPeer peer a) where
  minLogRecursion (TraceLabelPeer _ a) = 1 + minLogRecursion a

instance MinLogRecursion a => MinLogRecursion (TraceLabelCreds a) where
  minLogRecursion (TraceLabelCreds _ a) = minLogRecursion a

instance MinLogRecursion MorphoInitTrace where
  minLogRecursion _ = 10

instance MinLogRecursion (ExtractStateTrace h c) where
  minLogRecursion _ = 10

instance MinLogRecursion (ChainDB.TraceAddBlockEvent (MorphoBlock h c)) where
  minLogRecursion ChainDB.AddedToCurrentChain {} = 4
  minLogRecursion ChainDB.SwitchedToAFork {} = 4
  minLogRecursion _ = 10

instance MinLogRecursion (ChainDB.TraceEvent (MorphoBlock h c)) where
  minLogRecursion (ChainDB.TraceAddBlockEvent x) = 1 + minLogRecursion x
  minLogRecursion _ = 10

instance MinLogRecursion (SubscriptionTrace Socket.SockAddr) where
  minLogRecursion _ = 10

instance MinLogRecursion DnsTrace where
  minLogRecursion _ = 10

instance MinLogRecursion MuxTrace where
  minLogRecursion _ = 10

instance MinLogRecursion ErrorPolicyTrace where
  minLogRecursion _ = 10

instance MinLogRecursion (RpcTrace e i o) where
  minLogRecursion _ = 10

instance MinLogRecursion (TimeTravelError blk) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceSendRecv a) where
  minLogRecursion _ = 10

instance MinLogRecursion AcceptConnectionsPolicyTrace where
  minLogRecursion _ = 10

instance MinLogRecursion DiffusionInitializationTracer where
  minLogRecursion _ = 10

instance MinLogRecursion TraceLedgerPeers where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceChainSyncClientEvent blk) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceChainSyncServerEvent blk) where
  minLogRecursion _ = 10

instance MinLogRecursion (FetchDecision a) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceFetchClientState blk) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceBlockFetchServerEvent blk) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceTxSubmissionInbound txid tx) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceTxSubmissionOutbound txid tx) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceLocalTxSubmissionServerEvent blk) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceEventMempool blk) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceForgeEvent blk) where
  minLogRecursion _ = 10

instance MinLogRecursion (TraceBlockchainTimeEvent t) where
  minLogRecursion _ = 10
