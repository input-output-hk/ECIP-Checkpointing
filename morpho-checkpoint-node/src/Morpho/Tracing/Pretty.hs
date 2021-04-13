{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Morpho.Tracing.Pretty where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash, HashAlgorithm, hashToTextAsHex)
import Cardano.Prelude hiding (list)
import qualified Data.Text as Text
import Morpho.Ledger.Block (Header (..), MorphoBlock (..), MorphoBlockTx (morphoBlockGenTx), MorphoBody (..), MorphoStdHeader (..))
import Morpho.Ledger.PowTypes (Vote)
import Morpho.Ledger.Tx (Tx (Tx))
import Morpho.Ledger.Update (GenTx, MorphoStateDefaultConstraints)
import Morpho.Node.RunNode ()
import Network.Mux (MuxTrace, WithMuxBearer)
import qualified Network.Socket as Socket (SockAddr)
import Ouroboros.Consensus.Block (ChainHash, HeaderHash, Point (..), RealPoint, StandardHash, blockHash, headerPoint, pointSlot, realPointSlot)
import Ouroboros.Consensus.Block.Abstract (BlockNo (..), SlotNo (..))
import Ouroboros.Consensus.BlockchainTime (SystemStart (..), TraceBlockchainTimeEvent (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTxId, txId)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import Ouroboros.Consensus.Mempool (TraceEventMempool)
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server (TraceBlockFetchServerEvent)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server (TraceChainSyncServerEvent)
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server (TraceLocalTxSubmissionServerEvent)
import Ouroboros.Consensus.Node.Tracers (TraceLabelCreds (TraceLabelCreds))
import Ouroboros.Consensus.NodeKernel (TraceForgeEvent (..))
import Ouroboros.Consensus.Protocol.BFT (BftCrypto (BftDSIGN))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import Ouroboros.Consensus.Util.Condense (Condense, condense)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.BlockFetch.ClientState
  ( TraceLabelPeer (..),
  )
import Ouroboros.Network.Diffusion (DiffusionInitializationTracer)
import qualified Ouroboros.Network.NodeToClient as NtC
import Ouroboros.Network.NodeToNode
  ( DnsTrace (..),
    ErrorPolicyTrace (..),
    SubscriptionTrace (..),
    WithAddr (..),
    WithDomainName (..),
    WithIPList (..),
  )
import qualified Ouroboros.Network.NodeToNode as NtN
import Ouroboros.Network.PeerSelection.LedgerPeers (TraceLedgerPeers)
import Ouroboros.Network.TxSubmission.Inbound (TraceTxSubmissionInbound)
import Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound)
import Prettyprinter (Doc, Pretty (..), list, viaShow, (<+>))

class MPretty a where
  mpretty :: a -> Doc ann

instance (HashAlgorithm h, BftCrypto c) => MPretty (MorphoBlock h c) where
  mpretty MorphoBlock {..} =
    "("
      <> mpretty morphoPrev
      <> "->"
      <> mpretty morphoHeaderHash
      <> ","
      <> mpretty morphoSlotNo
      <> ","
      <> mpretty morphoTxs
      <> ")"
    where
      MorphoHeader {..} = morphoHeader
      MorphoStdHeader {..} = morphoHeaderStd
      MorphoBody {..} = morphoBody

instance
  (HashAlgorithm h, BftCrypto c) =>
  MPretty (Header (MorphoBlock h c))
  where
  mpretty MorphoHeader {..} =
    "("
      <> mpretty morphoPrev
      <> "->"
      <> mpretty morphoHeaderHash
      <> ","
      <> mpretty morphoSlotNo
      <> ")"
    where
      MorphoStdHeader {..} = morphoHeaderStd

instance MPretty MorphoBlockTx where
  mpretty = mpretty . morphoBlockGenTx

instance MPretty Tx where
  mpretty (Tx vote) = mpretty vote

instance MPretty Vote where
  mpretty = viaShow

instance MPretty (Hash h a) where
  mpretty = pretty . Text.take 8 . hashToTextAsHex

instance MPretty SlotNo where
  mpretty (SlotNo n) = pretty n

instance MPretty BlockNo where
  mpretty (BlockNo n) = pretty n

instance MPretty (TraceBlockchainTimeEvent t) where
  mpretty (TraceStartTimeInTheFuture (SystemStart start) toWait) =
    "Waiting"
      <+> viaShow toWait
      <+> "until genesis start time at"
      <+> viaShow start
  mpretty TraceSystemClockMovedBack {} =
    "System clock moved back an acceptable time span"
  mpretty TraceCurrentSlotUnknown {} = "Current slot is not yet known"

instance
  (HashAlgorithm h, BftCrypto c, Signable (BftDSIGN c) (MorphoStdHeader h c), MorphoStateDefaultConstraints h c) =>
  MPretty (TraceChainSyncClientEvent (MorphoBlock h c))
  where
  mpretty = viaShow

instance MPretty (TraceLocalTxSubmissionServerEvent (MorphoBlock h s)) where
  mpretty = viaShow

instance
  ( Signable (BftDSIGN s) (MorphoStdHeader h s),
    MorphoStateDefaultConstraints h s
  ) =>
  MPretty (ChainDB.TraceAddBlockEvent (MorphoBlock h s))
  where
  mpretty = viaShow

instance (StandardHash blk) => MPretty (TraceChainSyncServerEvent blk) where
  mpretty = viaShow

instance MPretty (TraceBlockFetchServerEvent blk) where
  mpretty = viaShow

instance MPretty a => MPretty (TraceLabelCreds a) where
  mpretty (TraceLabelCreds _ a) = mpretty a

instance
  ( Signable (BftDSIGN c) (MorphoStdHeader h c),
    MorphoStateDefaultConstraints h c
  ) =>
  MPretty (TraceForgeEvent (MorphoBlock h c))
  where
  mpretty (TraceAdoptedBlock slotNo blk txs) =
    "Adopted forged block for slot"
      <+> mpretty slotNo
      <> ":"
      <+> mpretty (blockHash blk)
      <> "; TxIds:"
      <+> viaShow (map txId txs)
  mpretty (TraceBlockFromFuture currentSlot tip) =
    "Forged block from future: current slot"
      <+> mpretty currentSlot
      <> ", tip being"
      <+> mpretty tip
  mpretty (TraceSlotIsImmutable slotNo tipPoint tipBlkNo) =
    "Forged for immutable slot" <> mpretty slotNo
      <> ", tip:"
      <+> mpretty tipPoint
      <> ", block no:"
      <+> pretty (unBlockNo tipBlkNo)
  mpretty (TraceDidntAdoptBlock slotNo _) =
    "Didn't adopt forged block at slot" <+> mpretty slotNo
  mpretty (TraceForgedBlock slotNo _ blk _) =
    "Forged block for slot" <+> mpretty slotNo <+> mpretty blk
  mpretty (TraceForgedInvalidBlock slotNo _ reason) =
    "Forged invalid block for slot"
      <+> mpretty slotNo
      <> ", reason:"
      <+> mpretty reason
  mpretty (TraceNodeIsLeader slotNo) =
    "Leading slot" <+> mpretty slotNo
  mpretty (TraceNodeNotLeader slotNo) =
    "Not leading slot" <+> mpretty slotNo
  mpretty (TraceNodeCannotForge slotNo reason) =
    "We are the leader for slot"
      <+> mpretty slotNo
      <> ", but we cannot lead because:"
      <+> viaShow reason
  mpretty (TraceNoLedgerState slotNo _blk) =
    "No ledger state at slot" <+> mpretty slotNo
  mpretty (TraceNoLedgerView slotNo _) =
    "No ledger view at slot" <+> mpretty slotNo
  mpretty (TraceStartLeadershipCheck slotNo) =
    "Testing for leadership at slot" <+> mpretty slotNo
  mpretty (TraceBlockContext slotNo _blk point) =
    "The block to connect to is at slot"
      <+> mpretty slotNo
      <> ", with point"
      <+> viaShow point
  mpretty (TraceLedgerState slotNo point) =
    "The ledger state at the block to connect to is at slot"
      <+> mpretty slotNo
      <> ", with point"
      <+> viaShow point
  mpretty (TraceLedgerView slotNo) =
    "Ledger view for the current slot number"
      <+> mpretty slotNo
      <+> "obtained"
  mpretty (TraceForgeStateUpdateError slotNo err) =
    "Updating the forge state at slot"
      <+> mpretty slotNo
      <+> "failed with"
      <+> viaShow err

instance
  (Signable (BftDSIGN c) (MorphoStdHeader h c), MorphoStateDefaultConstraints h c) =>
  MPretty (ChainDB.TraceEvent (MorphoBlock h c))
  where
  mpretty (ChainDB.TraceAddBlockEvent ev) = case ev of
    ChainDB.IgnoreBlockOlderThanK pt ->
      "Ignoring block older than K:" <+> mpretty pt
    ChainDB.IgnoreBlockAlreadyInVolatileDB pt ->
      "Ignoring block already in DB:" <+> mpretty pt
    ChainDB.IgnoreInvalidBlock pt _reason ->
      "Ignoring previously seen invalid block:" <+> mpretty pt
    ChainDB.AddedBlockToQueue pt sz ->
      "Block added to queue:" <+> mpretty pt <+> "queue size" <+> pretty sz
    ChainDB.BlockInTheFuture pt slot ->
      "Ignoring block from future:" <+> mpretty pt <+> ", slot" <+> mpretty slot
    ChainDB.StoreButDontChange pt ->
      "Ignoring block:" <+> mpretty pt
    ChainDB.TryAddToCurrentChain pt ->
      "Block fits onto the current chain:" <+> mpretty pt
    ChainDB.TrySwitchToAFork pt _ ->
      "Block fits onto some fork:" <+> mpretty pt
    ChainDB.AddedToCurrentChain _ _ _ c ->
      "Chain extended, new tip:" <+> mpretty (AF.headPoint c)
    ChainDB.SwitchedToAFork _ _ _ c ->
      "Switched to a fork, new tip:" <+> mpretty (AF.headPoint c)
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock err pt ->
        "Invalid block" <+> mpretty pt <> ":" <+> viaShow err
      ChainDB.InvalidCandidate c ->
        "Invalid candidate" <+> mpretty (AF.headPoint c)
      ChainDB.ValidCandidate c ->
        "Valid candidate" <+> mpretty (AF.headPoint c)
      ChainDB.CandidateContainsFutureBlocks c hdrs ->
        "Candidate contains blocks from near future:"
          <+> mpretty (AF.headPoint c)
          <> ", slots"
          <+> pretty (Text.intercalate ", " (map (Text.pack . condense . headerPoint) hdrs))
      ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs ->
        "Candidate contains blocks from future exceeding clock skew limit:"
          <+> mpretty (AF.headPoint c)
          <> ", slots"
          <+> pretty (Text.intercalate ", " (map (Text.pack . condense . headerPoint) hdrs))
    ChainDB.AddedBlockToVolatileDB pt _ _ ->
      "Chain added block" <+> mpretty pt
    ChainDB.ChainSelectionForFutureBlock pt ->
      "Chain selection run for block previously from future:" <+> mpretty pt
  mpretty (ChainDB.TraceLedgerReplayEvent ev) = case ev of
    LedgerDB.ReplayFromGenesis _replayTo -> "Replaying ledger from genesis"
    LedgerDB.ReplayFromSnapshot snap tip' _replayTo ->
      "Replaying ledger from snapshot"
        <+> viaShow snap
        <+> "at"
        <+> mpretty tip'
    LedgerDB.ReplayedBlock pt _ replayTo ->
      "Replayed block: slot" <+> viaShow (realPointSlot pt) <+> "of" <+> viaShow (pointSlot replayTo)
  mpretty (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot snap pt ->
      "Took ledger snapshot" <+> viaShow snap <+> "at" <+> mpretty pt
    LedgerDB.DeletedSnapshot snap ->
      "Deleted old snapshot" <+> viaShow snap
    LedgerDB.InvalidSnapshot snap failure ->
      "Invalid snapshot" <+> viaShow snap <+> viaShow failure
  mpretty (ChainDB.TraceCopyToImmutableDBEvent ev) = case ev of
    ChainDB.CopiedBlockToImmutableDB pt ->
      "Copied block" <+> mpretty pt <+> "to the ImmutableDB"
    ChainDB.NoBlocksToCopyToImmutableDB ->
      "There are no blocks to copy to the ImmutableDB"
  mpretty (ChainDB.TraceGCEvent ev) = case ev of
    ChainDB.PerformedGC slot ->
      "Performed a garbage collection for" <+> mpretty slot
    ChainDB.ScheduledGC slot _difft ->
      "Scheduled a garbage collection for" <+> mpretty slot
  mpretty (ChainDB.TraceOpenEvent ev) = case ev of
    ChainDB.OpenedDB immTip tip' ->
      "Opened db with immutable tip at"
        <+> mpretty immTip
        <+> "and tip"
        <+> mpretty tip'
    ChainDB.ClosedDB immTip tip' ->
      "Closed db with immutable tip at"
        <+> mpretty immTip
        <+> "and tip"
        <+> mpretty tip'
    ChainDB.OpenedImmutableDB immTip epoch ->
      "Opened imm db with immutable tip at"
        <+> mpretty immTip
        <+> "and epoch"
        <+> viaShow epoch
    ChainDB.OpenedVolatileDB -> "Opened vol db"
    ChainDB.OpenedLgrDB -> "Opened lgr db"
  mpretty (ChainDB.TraceFollowerEvent ev) = case ev of
    ChainDB.NewFollower -> "New follower was created"
    ChainDB.FollowerNoLongerInMem _ -> "FollowerNoLongerInMem"
    ChainDB.FollowerSwitchToMem _ _ -> "FollowerSwitchToMem"
    ChainDB.FollowerNewImmIterator _ _ -> "FollowerNewImmIterator"
  mpretty (ChainDB.TraceInitChainSelEvent ev) = case ev of
    ChainDB.InitChainSelValidation _ -> "InitChainSelValidation"
  mpretty (ChainDB.TraceIteratorEvent ev) = case ev of
    ChainDB.UnknownRangeRequested _ -> "UnknownRangeRequested"
    ChainDB.BlockMissingFromVolatileDB _ -> "BlockMissingFromVolDB"
    ChainDB.StreamFromImmutableDB _ _ -> "StreamFromImmDB"
    ChainDB.StreamFromBoth {} -> "StreamFromBoth"
    ChainDB.StreamFromVolatileDB {} -> "StreamFromVolDB"
    ChainDB.BlockWasCopiedToImmutableDB _ -> "BlockWasCopiedToImmDB"
    ChainDB.BlockGCedFromVolatileDB _ -> "BlockGCedFromVolDB"
    ChainDB.SwitchBackToVolatileDB -> "SwitchBackToVolDB"
  mpretty (ChainDB.TraceImmutableDBEvent ev) = "TraceImmutableDBEvent" <+> viaShow ev
  mpretty (ChainDB.TraceVolatileDBEvent ev) = "TraceVolDBEvent" <+> viaShow ev

instance
  (Show (GenTxId blk), Show (ApplyTxErr blk), Show (GenTx blk)) =>
  MPretty (TraceEventMempool blk)
  where
  mpretty = viaShow

instance MPretty NtN.HandshakeTr where
  mpretty = viaShow

instance MPretty NtC.HandshakeTr where
  mpretty = viaShow

instance MPretty NtN.AcceptConnectionsPolicyTrace where
  mpretty = viaShow

instance MPretty DiffusionInitializationTracer where
  mpretty = viaShow

instance MPretty TraceLedgerPeers where
  mpretty = viaShow

instance (Show peer, Show a) => MPretty (TraceLabelPeer peer a) where
  mpretty = viaShow

instance MPretty (TraceTxSubmissionInbound txid tx) where
  mpretty = viaShow

instance (Show tx, Show txid) => MPretty (TraceTxSubmissionOutbound txid tx) where
  mpretty = viaShow

instance (Show addr) => MPretty (WithAddr addr ErrorPolicyTrace) where
  mpretty = viaShow

instance MPretty (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  mpretty = viaShow

instance MPretty (WithDomainName DnsTrace) where
  mpretty = viaShow

instance MPretty (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  mpretty = viaShow

instance (Show peer) => MPretty (WithMuxBearer peer MuxTrace) where
  mpretty = viaShow

instance MPretty a => MPretty [a] where
  mpretty xs = list $ map mpretty xs

instance (StandardHash blk) => MPretty (ChainHash blk) where
  mpretty = viaShow

instance
  (LedgerSupportsProtocol blk) =>
  MPretty (ChainDB.InvalidBlockReason blk)
  where
  mpretty = viaShow

instance Condense (HeaderHash blk) => MPretty (Point blk) where
  mpretty pt =
    case pt of
      GenesisPoint -> "genesis (origin)"
      BlockPoint slot h -> pretty $ condense h ++ "@" ++ condense slot

instance Condense (HeaderHash blk) => MPretty (RealPoint blk) where
  mpretty = pretty . Text.pack . condense
