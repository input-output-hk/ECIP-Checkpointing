{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Morpho.Tracing.Pretty where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash, hashToTextAsHex)
import Cardano.Prelude hiding (list)
import Codec.CBOR.Read (DeserialiseFailure (DeserialiseFailure))
import Codec.CBOR.Term (Term)
import qualified Data.ByteString.Base16.Lazy as BL16
import qualified Data.IP as IP
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Clock (DiffTime, NominalDiffTime, UTCTime)
import Morpho.Common.Bytes
import Morpho.Common.Conversions (bytesToHex)
import Morpho.Crypto.ECDSASignature (PublicKey (PublicKey), Signature, recoverPublicKey)
import Morpho.Ledger.Block (Header (..), MorphoBlock (..), MorphoBlockTx (..), MorphoBody (..), MorphoStdHeader (..))
import Morpho.Ledger.PowTypes (Checkpoint (Checkpoint), PowBlockHash (..), PowBlockNo (..), PowBlockRef (..), Vote (..), powBlockRefToBytes)
import Morpho.Ledger.Serialise (NestedCtxt_ (CtxtMorpho))
import Morpho.Ledger.State (MorphoError (MorphoInvalidHash, MorphoTransactionError), MorphoState (MorphoState), MorphoTransactionError (MorphoAlreadyCheckpointed, MorphoCandidateBeforeCheckpoint, MorphoDuplicateVote, MorphoInvalidSignature, MorphoUnknownPublicKey, MorphoWrongDistance))
import Morpho.Ledger.Tx (Tx (Tx))
import Morpho.Ledger.Update (GenTx (MorphoGenTx), MorphoStateDefaultConstraints, TxId (MorphoGenTxId), VoteError (FailedToSignBlockRef), WontPushCheckpoint (WontPushCheckpointNoGeneratedCheckpoint, WontPushCheckpointNotMorphoTip))
import Morpho.Node.RunNode ()
import Network.Mux (MuxTrace, WithMuxBearer)
import Network.Mux.Compat (WithMuxBearer (WithMuxBearer))
import qualified Network.Socket as Socket
import Ouroboros.Consensus.Block (ChainHash (BlockHash, GenesisHash), HeaderHash, IsEBB (IsNotEBB), Point (..), RealPoint, blockHash, headerPoint, pointSlot, realPointSlot)
import Ouroboros.Consensus.Block.Abstract (BlockNo (..), SlotNo (..))
import Ouroboros.Consensus.Block.EBB (IsEBB (IsEBB))
import Ouroboros.Consensus.Block.NestedContent (NestedCtxt (NestedCtxt))
import Ouroboros.Consensus.Block.RealPoint (RealPoint (RealPoint))
import Ouroboros.Consensus.BlockchainTime (SystemStart (..), TraceBlockchainTimeEvent (..))
import Ouroboros.Consensus.HeaderValidation (HeaderEnvelopeError (UnexpectedBlockNo, UnexpectedPrevHash, UnexpectedSlotNo), HeaderError (HeaderEnvelopeError, HeaderProtocolError))
import Ouroboros.Consensus.Ledger.Extended (ExtValidationError (ExtValidationErrorHeader, ExtValidationErrorLedger))
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId, txId)
import Ouroboros.Consensus.Mempool (TraceEventMempool (..))
import Ouroboros.Consensus.Mempool.API (MempoolSize (MempoolSize))
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server (TraceBlockFetchServerEvent)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client (Our (..), Their (..), TraceChainSyncClientEvent (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server (TraceChainSyncServerEvent (TraceChainSyncRollBackward, TraceChainSyncRollForward, TraceChainSyncServerRead, TraceChainSyncServerReadBlocked))
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server (TraceLocalTxSubmissionServerEvent (TraceReceivedTx))
import Ouroboros.Consensus.Node.NetworkProtocolVersion (NodeToClientVersion, NodeToNodeVersion)
import Ouroboros.Consensus.Node.Tracers (TraceLabelCreds (TraceLabelCreds))
import Ouroboros.Consensus.NodeKernel (TraceForgeEvent (..))
import Ouroboros.Consensus.Protocol.BFT (BftCrypto (BftDSIGN), BftValidationErr (BftInvalidSignature))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.FS.API.Types (FsPath)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal as ImmutableDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB.OnDisk (DiskSnapshot, InitFailure (InitFailureGenesis, InitFailureRead, InitFailureTooRecent))
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader (SerialisedHeaderFromDepPair))
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Types as VolatileDB
import Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr (ReadFailed, TrailingBytes))
import Ouroboros.Consensus.Util.Condense (condense)
import Ouroboros.Consensus.Util.DepPair (GenDepPair (GenDepPair))
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (ChainUpdate (AddBlock, RollBack), Serialised (Serialised), Tip (Tip, TipGenesis))
import Ouroboros.Network.BlockFetch.Client (FetchRequest (FetchRequest))
import Ouroboros.Network.BlockFetch.ClientState
  ( ChainRange (ChainRange),
    TraceFetchClientState (AcknowledgedFetchRequest, AddedFetchRequest, ClientTerminating, CompletedBlockFetch, CompletedFetchBatch, RejectedFetchBatch, StartedFetchBatch),
    TraceLabelPeer (..),
  )
import Ouroboros.Network.BlockFetch.Decision (FetchDecline (..))
import Ouroboros.Network.Codec (AnyMessageAndAgency (AnyMessageAndAgency), PeerHasAgency (ClientAgency, ServerAgency))
import Ouroboros.Network.Diffusion (DiffusionInitializationTracer (ConfiguringLocalSocket, ConfiguringServerSocket, CreateSystemdSocketForSnocketPath, CreatedLocalSocket, CreatingServerSocket, DiffusionErrored, ListeningLocalSocket, ListeningServerSocket, LocalSocketUp, RunLocalServer, RunServer, ServerSocketUp, UnsupportedLocalSystemdSocket, UnsupportedReadySocketCase, UsingSystemdSocket))
import Ouroboros.Network.Mux (ControlMessage)
import qualified Ouroboros.Network.NodeToClient as NtC
import Ouroboros.Network.NodeToNode
  ( AcceptConnectionsPolicyTrace (ServerTraceAcceptConnectionHardLimit, ServerTraceAcceptConnectionRateLimiting),
    DnsTrace (..),
    ErrorPolicyTrace (..),
    SubscriptionTrace (..),
    WithAddr (..),
    WithDomainName (..),
    WithIPList (..),
  )
import qualified Ouroboros.Network.NodeToNode as NtN
import Ouroboros.Network.PeerSelection.LedgerPeers (AccPoolStake (AccPoolStake), IP (IPv4, IPv6), PoolStake (PoolStake), RelayAddress (RelayAddressAddr, RelayAddressDomain), TraceLedgerPeers (FetchingNewLedgerState, PickedPeer, PickedPeers))
import Ouroboros.Network.PeerSelection.RootPeersDNS (Domain, DomainAddress (DomainAddress))
import Ouroboros.Network.Point (WithOrigin (At, Origin))
import Ouroboros.Network.Protocol.BlockFetch.Type (ClientHasAgency, Message, ServerHasAgency)
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import qualified Ouroboros.Network.Protocol.Handshake.Type as Handshake
import qualified Ouroboros.Network.Protocol.Trans.Hello.Type as Hello
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TxSubmission
import Ouroboros.Network.Snocket (FileDescriptor, LocalAddress (LocalAddress))
import Ouroboros.Network.Subscription (ConnectResult (ConnectSuccess, ConnectSuccessLast, ConnectValencyExceeded))
import Ouroboros.Network.TxSubmission.Inbound (ProcessedTxCount (ProcessedTxCount), TraceTxSubmissionInbound (TraceTxInboundCanRequestMoreTxs, TraceTxInboundCannotRequestMoreTxs, TraceTxInboundTerminated, TraceTxSubmissionCollected, TraceTxSubmissionProcessed))
import Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (TraceControlMessage, TraceTxSubmissionOutboundRecvMsgRequestTxs, TraceTxSubmissionOutboundSendMsgReplyTxs))
import Prettyprinter (Doc, Pretty (..), comma, concatWith, hsep, punctuate, viaShow, (<+>))

class MPretty a where
  mpretty :: a -> Doc ann

instance MPretty (HeaderHash blk) => MPretty (MorphoState blk) where
  mpretty (MorphoState lastCkpt ckptAt votes tip) =
    "MorphoState(morphoTip:"
      <+> mpretty tip
      <> ", lastCheckpoint:"
      <+> mpretty lastCkpt
      <> ", checkpointAt:"
      <+> mpretty ckptAt
      <> ", currentVotes:"
      <+> mpretty (Map.elems votes)
      <> ")"

instance MPretty PublicKey where
  mpretty (PublicKey bytes) = mpretty bytes

instance MPretty Bytes where
  mpretty = pretty . ("0x" <>) . Text.take 8 . bytesToHex

instance MPretty (MorphoBlock h c) where
  mpretty
    MorphoBlock
      { morphoHeader = MorphoHeader {morphoHeaderHash, morphoHeaderStd},
        morphoBody
      } =
      "MorphoBlock(hash:"
        <+> mpretty morphoHeaderHash
        <> ", prevHash:"
        <+> mpretty (morphoPrev morphoHeaderStd)
        <> ", slot:"
        <+> mpretty (morphoSlotNo morphoHeaderStd)
        <> ", blockNo:"
        <+> mpretty (morphoBlockNo morphoHeaderStd)
        <> ", body:"
        <+> mpretty morphoBody
        <> ")"

instance MPretty MorphoBody where
  mpretty (MorphoBody txs) =
    "MorphoBody("
      <> hsep (punctuate comma (map mpretty txs))
      <> ")"

instance MPretty (Header (MorphoBlock h c)) where
  mpretty MorphoHeader {..} =
    "MorphoHeader(prevHash:"
      <+> mpretty morphoPrev
      <> ", headerHash:"
      <+> mpretty morphoHeaderHash
      <> ", slotNo:"
      <+> mpretty morphoSlotNo
      <> ")"
    where
      MorphoStdHeader {..} = morphoHeaderStd

instance MPretty MorphoBlockTx where
  mpretty (MorphoBlockTx genTx genTxId) =
    "Transaction(id:"
      <+> mpretty genTxId
      <> ", tx:"
      <+> mpretty genTx
      <> ")"

instance MPretty (ChainHash (MorphoBlock h c)) where
  mpretty GenesisHash = "Genesis"
  mpretty (BlockHash h) = mpretty h

instance MPretty (HeaderHash a) => MPretty (Tip a) where
  mpretty TipGenesis = "TipGenesis"
  mpretty (Tip s h b) =
    "Tip(slot:"
      <+> mpretty s
      <> ", hash:"
      <+> mpretty h
      <> ", blockNo:"
      <+> mpretty b
      <> ")"

instance MPretty Tx where
  mpretty (Tx vote) = mpretty vote

instance MPretty Vote where
  mpretty (Vote block signature) =
    let bytes = powBlockRefToBytes block
     in "Vote(block:"
          <+> mpretty block
          <> ", byPubkey:"
          <+> ppPublicKey bytes signature
          <> ")"

instance MPretty PowBlockNo where
  mpretty (PowBlockNo n) = pretty n

instance MPretty PowBlockHash where
  mpretty (PowBlockHash bytes) = mpretty bytes

instance MPretty PowBlockRef where
  mpretty (PowBlockRef number hash) =
    "PowBlock(number:" <+> mpretty number <> ", hash:" <+> mpretty hash <> ")"

instance MPretty Checkpoint where
  mpretty (Checkpoint powBlockRef signatures) =
    let bytes = powBlockRefToBytes powBlockRef
        pubKeys = map (ppPublicKey bytes) signatures
     in "Checkpoint(block:"
          <+> mpretty powBlockRef
          <> ", signedByPubkeys:"
          <+> ppList pubKeys
          <> ")"

instance MPretty (HeaderHash blk) => MPretty (WontPushCheckpoint blk) where
  mpretty WontPushCheckpointNoGeneratedCheckpoint =
    "Won't push checkpoint: no checkpoint generated yet"
  mpretty (WontPushCheckpointNotMorphoTip p1 p2) =
    "Won't push checkpoint: checkpoint block"
      <+> mpretty p1
      <+> "is not the current Morpho tip"
      <+> mpretty p2

instance MPretty VoteError where
  mpretty (FailedToSignBlockRef powBlockRef) =
    "Failed to sign block reference" <+> mpretty powBlockRef

instance MPretty (Hash h a) where
  mpretty = pretty . ("0x" <>) . Text.take 8 . hashToTextAsHex

instance MPretty SlotNo where
  mpretty (SlotNo n) = pretty n

instance MPretty BlockNo where
  mpretty (BlockNo n) = pretty n

instance (MPretty a) => MPretty (WithOrigin a) where
  mpretty Origin = "Origin"
  mpretty (At a) = mpretty a

instance MPretty (TraceBlockchainTimeEvent t) where
  mpretty (TraceStartTimeInTheFuture (SystemStart start) toWait) =
    "Waiting"
      <+> mpretty toWait
      <+> "until genesis start time at"
      <+> mpretty start
  mpretty TraceSystemClockMovedBack {} =
    "System clock moved back an acceptable time span"
  mpretty TraceCurrentSlotUnknown {} = "Current slot is not yet known"

instance MPretty UTCTime where
  mpretty = viaShow

instance MPretty NominalDiffTime where
  mpretty = viaShow

instance
  ( BftCrypto c,
    Signable (BftDSIGN c) (MorphoStdHeader h c),
    MorphoStateDefaultConstraints h c
  ) =>
  MPretty (TraceChainSyncClientEvent (MorphoBlock h c))
  where
  mpretty (TraceDownloadedHeader h) =
    "While following a candidate chain, rolled forward by downloading the header:"
      <+> mpretty h
  mpretty (TraceRolledBack p) =
    "While following a candidate chain, rolled back to"
      <+> mpretty p
  mpretty (TraceFoundIntersection p (Our our) (Their their)) =
    "Found the intersection point"
      <+> mpretty p
      <+> "between our chain fragment"
      <+> mpretty our
      <+> "and the candidate's chain"
      <+> mpretty their
  mpretty (TraceException err) =
    "Exception thrown by the Chain Sync Client:" <+> viaShow err
  mpretty (TraceTermination result) =
    "Client has terminated:" <+> viaShow result

instance MPretty (GenTx (MorphoBlock h c)) where
  mpretty (MorphoGenTx x) = mpretty x

instance MPretty (GenTxId (MorphoBlock h c)) where
  mpretty (MorphoGenTxId mTxId) = mpretty mTxId

instance (MPretty (HeaderHash blk)) => MPretty (TraceChainSyncServerEvent blk) where
  mpretty (TraceChainSyncServerRead tip chainUpdate) =
    "Synchronizing chain: current tip is" <+> mpretty tip <> "." <+> mpretty chainUpdate
  mpretty (TraceChainSyncServerReadBlocked tip chainUpdate) =
    "Synchronizing chain blocked: current tip is" <+> mpretty tip <> "." <+> mpretty chainUpdate
  mpretty (TraceChainSyncRollForward point) =
    "Synchronizing chain: roll forward to" <+> mpretty point
  mpretty (TraceChainSyncRollBackward point) =
    "Synchronizing chain: roll backward to" <+> mpretty point

instance (MPretty (HeaderHash blk), MPretty a) => MPretty (ChainUpdate blk a) where
  mpretty (AddBlock a) = "Chain update by adding block" <+> mpretty a
  mpretty (RollBack p) = "Chain update by rolling back to" <+> mpretty p

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
      <> ", hash:"
      <+> mpretty (blockHash blk)
      <> ", txIds:"
      <+> mpretty (map txId txs)
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
      <+> mpretty tipBlkNo
  mpretty (TraceDidntAdoptBlock slotNo _) =
    "Didn't adopt forged block at slot" <+> mpretty slotNo
  mpretty (TraceForgedBlock slotNo _ blk _) =
    "Forged block for slot" <+> mpretty slotNo <> ":" <+> mpretty blk
  mpretty (TraceForgedInvalidBlock slotNo _ reason) =
    "Forged invalid block for slot"
      <+> mpretty slotNo
      <> ", reason:"
      <+> mpretty reason
  mpretty (TraceNodeIsLeader slotNo) =
    "Leading slot" <+> mpretty slotNo
  mpretty (TraceNodeNotLeader slotNo) =
    "Not leading slot" <+> mpretty slotNo
  mpretty (TraceNodeCannotForge _ reason) = absurd reason
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
      <+> mpretty point
  mpretty (TraceLedgerState slotNo point) =
    "The ledger state at the block to connect to is at slot"
      <+> mpretty slotNo
      <> ", with point"
      <+> mpretty point
  mpretty (TraceLedgerView slotNo) =
    "Ledger view for the current slot number"
      <+> mpretty slotNo
      <+> "obtained"
  mpretty (TraceForgeStateUpdateError _ err) = absurd err

instance
  ( Signable (BftDSIGN c) (MorphoStdHeader h c),
    MorphoStateDefaultConstraints h c
  ) =>
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
      "Block added to queue:" <+> mpretty pt <> ". Queue size" <+> pretty sz
    ChainDB.BlockInTheFuture pt slot ->
      "Ignoring block from future:" <+> mpretty pt <+> ", slot" <+> mpretty slot
    ChainDB.StoreButDontChange pt ->
      "Ignoring block:" <+> mpretty pt
    ChainDB.TryAddToCurrentChain pt ->
      "Block fits onto the current chain:" <+> mpretty pt
    ChainDB.TrySwitchToAFork pt _ ->
      "Block fits onto some fork:" <+> mpretty pt
    ChainDB.AddedToCurrentChain _ _ _ c ->
      "Chain extended, new slot:" <+> mpretty (AF.headSlot c)
    ChainDB.SwitchedToAFork _ _ _ c ->
      "Switched to a fork, new slot:" <+> mpretty (AF.headSlot c)
    ChainDB.AddBlockValidation ev' -> case ev' of
      ChainDB.InvalidBlock err pt ->
        "Invalid block:" <+> mpretty pt <> ":" <+> mpretty err
      ChainDB.InvalidCandidate c ->
        "Invalid candidate:" <+> mpretty (AF.headPoint c)
      ChainDB.ValidCandidate c ->
        "Valid candidate:" <+> mpretty (AF.headPoint c)
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
        <+> mpretty snap
        <+> "at"
        <+> mpretty tip'
    LedgerDB.ReplayedBlock pt _ replayTo ->
      "Replayed block: slot"
        <+> mpretty (realPointSlot pt)
        <+> "of"
        <+> mpretty (pointSlot replayTo)
  mpretty (ChainDB.TraceLedgerEvent ev) = case ev of
    LedgerDB.TookSnapshot snap pt ->
      "Took ledger snapshot" <+> mpretty snap <+> "at" <+> mpretty pt
    LedgerDB.DeletedSnapshot snap ->
      "Deleted old snapshot" <+> mpretty snap
    LedgerDB.InvalidSnapshot snap failure ->
      "Invalid snapshot" <+> mpretty snap <+> mpretty failure
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
        <+> mpretty epoch
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
  mpretty (ChainDB.TraceImmutableDBEvent ev) = case ev of
    ImmutableDB.NoValidLastLocation -> "Immutable DB: No valid last location"
    ImmutableDB.ValidatedLastLocation chunkNo tip ->
      "Immutable DB: Validated last location with chunkNo"
        <+> mpretty chunkNo
        <+> "and tip"
        <+> mpretty tip
    ImmutableDB.ValidatingChunk chunkNo ->
      "Immutable DB: Validating chunk" <+> mpretty chunkNo
    ImmutableDB.MissingChunkFile chunkNo ->
      "Immutable DB: Missing chunk file" <+> mpretty chunkNo
    ImmutableDB.InvalidChunkFile chunkNo chunkFileError ->
      "Immutable DB: Invalid chunk file"
        <+> mpretty chunkNo
        <+> "because:"
        <+> mpretty chunkFileError
    ImmutableDB.ChunkFileDoesntFit h1 h2 ->
      "Immutable DB: The has of the last block ("
        <> mpretty h1
        <> ") doesn't match the previous hash ("
        <> mpretty h2
        <> ") of the first block in the current epoch"
    ImmutableDB.MissingPrimaryIndex chunkNo ->
      "Immutable DB: Missing primary index" <+> mpretty chunkNo
    ImmutableDB.MissingSecondaryIndex chunkNo ->
      "Immutable DB: Missing secondary index" <+> mpretty chunkNo
    ImmutableDB.InvalidPrimaryIndex chunkNo ->
      "Immutable DB: Invalid primary index" <+> mpretty chunkNo
    ImmutableDB.InvalidSecondaryIndex chunkNo ->
      "Immutable DB: Invalid secondary index" <+> mpretty chunkNo
    ImmutableDB.RewritePrimaryIndex chunkNo ->
      "Immutable DB: Rewrite primary index" <+> mpretty chunkNo
    ImmutableDB.RewriteSecondaryIndex chunkNo ->
      "Immutable DB: Rewrite secondary index" <+> mpretty chunkNo
    ImmutableDB.Migrating t ->
      "Immutable DB: Performing a migration of the on-disk files:" <+> pretty t
    ImmutableDB.DeletingAfter o -> "Immutable DB: Deleting after" <+> mpretty o
    ImmutableDB.DBAlreadyClosed -> "Immutable DB: DB already closed"
    ImmutableDB.DBClosed -> "Immutable DB: DB closed"
    ImmutableDB.TraceCacheEvent traceCacheEvent -> case traceCacheEvent of
      ImmutableDB.TraceCurrentChunkHit chunkNo _ ->
        "Immutable DB: The current chunk" <+> mpretty chunkNo <+> "was a hit"
      ImmutableDB.TracePastChunkHit chunkNo _ ->
        "Immutable DB: The least recently used past chunk"
          <+> mpretty chunkNo
          <+> "was a hit"
      ImmutableDB.TracePastChunkMiss chunkNo _ ->
        "Immutable DB: The least recently used past chunk"
          <+> mpretty chunkNo
          <+> "was a miss"
      ImmutableDB.TracePastChunkEvict chunkNo _ ->
        "The least recently used past chunk"
          <+> mpretty chunkNo
          <+> "was evicted because the cache was full."
      ImmutableDB.TracePastChunksExpired chunkNos _ ->
        "Past chunks"
          <+> mpretty chunkNos
          <+> "were expired from the cache because they haven't been used for a while."
  mpretty (ChainDB.TraceVolatileDBEvent ev) = case ev of
    VolatileDB.DBAlreadyClosed -> "Volatile DB: DB already closed"
    VolatileDB.DBAlreadyOpen -> "Volatile DB: DB already open"
    VolatileDB.BlockAlreadyHere h ->
      "Volatile DB: Block" <+> mpretty h <+> "already here"
    VolatileDB.TruncateCurrentFile path ->
      "Volatile DB: Truncate current file path" <+> mpretty path
    VolatileDB.Truncate parseErr path offset ->
      "Volatile DB: Truncate file path"
        <+> mpretty path
        <+> "with block offset"
        <+> mpretty offset <> ":"
        <+> mpretty parseErr
    VolatileDB.InvalidFileNames paths ->
      "Volatile DB: Invalid file names:" <+> mpretty paths

instance MPretty (LedgerDB.InitFailure (MorphoBlock h c)) where
  mpretty (InitFailureRead err) = "We failed to deserialise the snapshot." <+> mpretty err
  mpretty (InitFailureTooRecent realPoint) =
    "This snapshot is too recent (ahead of the tip of the chain):" <+> mpretty realPoint
  mpretty InitFailureGenesis =
    "This snapshot was of the ledger state at genesis, even though we never take snapshots at genesis, so this is unexpected."

instance MPretty ReadIncrementalErr where
  mpretty (ReadFailed (DeserialiseFailure byteOffset c)) =
    "Could not deserialise the following data with byte offset"
      <+> pretty byteOffset <> ":"
      <+> pretty c
  mpretty (TrailingBytes bs) =
    "Deserialisation was successful, but there was additional data" <+> mpretty bs

instance MPretty (ImmutableDB.ChunkFileError (MorphoBlock h c)) where
  mpretty (ImmutableDB.ChunkErrRead err) = "A block could not be decoded." <+> mpretty err
  mpretty (ImmutableDB.ChunkErrHashMismatch headerHash chainHash) =
    "The previous hash of a block"
      <+> mpretty headerHash
      <+> "did not match the hash of the previous block"
      <+> mpretty chainHash
  mpretty (ImmutableDB.ChunkErrCorrupt point) =
    "The integrity verification of the block with the given point"
      <+> mpretty point
      <+> "returned False, indicating that the block got corrupted."

instance MPretty (VolatileDB.ParseError (MorphoBlock h c)) where
  mpretty (VolatileDB.BlockReadErr err) = "A block could not be parsed." <+> mpretty err
  mpretty (VolatileDB.BlockCorruptedErr hash) = "A block was corrupted, e.g., checking its signature and/or hash failed:" <+> mpretty hash
  mpretty (VolatileDB.DuplicatedBlock headerHash fspath1 fspath2) =
    "A block with the same hash"
      <+> mpretty headerHash
      <+> "occurred twice in the VolatileDB files:"
      <+> mpretty fspath1
      <+> "and"
      <+> mpretty fspath2

instance
  (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) =>
  MPretty (ExtValidationError (MorphoBlock h c))
  where
  mpretty (ExtValidationErrorLedger ledgerError) = mpretty ledgerError
  mpretty (ExtValidationErrorHeader headerErr) = mpretty headerErr

instance MPretty (MorphoError (MorphoBlock h c)) where
  mpretty (MorphoTransactionError vote morphoTxErr) =
    "Transaction error for"
      <+> mpretty vote
      <> ":"
      <+> mpretty morphoTxErr
  mpretty (MorphoInvalidHash hash1 hash2) =
    "Error when updating Morpho tip. The previous header hash of the new block"
      <+> mpretty hash1
      <+> "should be equal to the header hash of the current tip"
      <+> mpretty hash2

instance MPretty MorphoTransactionError where
  mpretty MorphoCandidateBeforeCheckpoint = "Vote occurs before latest checkpoint"
  mpretty MorphoAlreadyCheckpointed = "Vote is already a checkpoint"
  mpretty MorphoWrongDistance = "Vote is at wrong distance"
  mpretty MorphoInvalidSignature = "Vote has invalid signature"
  mpretty MorphoDuplicateVote = "Duplicated vote"
  mpretty MorphoUnknownPublicKey = "Vote has an unknown public key"

instance MPretty (HeaderError (MorphoBlock h c)) where
  mpretty (HeaderProtocolError err) = mpretty err
  mpretty (HeaderEnvelopeError err) = mpretty err

instance MPretty BftValidationErr where
  mpretty (BftInvalidSignature signature) =
    "BFT invalid signature:" <+> pretty signature

instance MPretty (HeaderEnvelopeError (MorphoBlock h c)) where
  mpretty (UnexpectedBlockNo expectedBlockNo actualBlockNo) =
    "Invalid block number. Expected"
      <+> mpretty expectedBlockNo
      <+> "but got"
      <+> mpretty actualBlockNo
  mpretty (UnexpectedSlotNo expectedSlotNo actualSlotNo) =
    "Invalid slot number. Expected minimum slot number of"
      <+> mpretty expectedSlotNo
      <+> "but got"
      <+> mpretty actualSlotNo
  mpretty (UnexpectedPrevHash oldTip prevHash) =
    "Invalid hash (in the reference to the previous block). The current tip is"
      <+> mpretty oldTip
      <+> "and the previous hash of the new block is"
      <+> mpretty prevHash

instance MPretty DiskSnapshot where
  mpretty = viaShow

instance MPretty ImmutableDB.ChunkNo where
  mpretty (ImmutableDB.ChunkNo n) = pretty n

instance MPretty VolatileDB.BlockOffset where
  mpretty (VolatileDB.BlockOffset o) = pretty o

instance MPretty (HeaderHash a) => MPretty (ImmutableDB.Tip a) where
  mpretty (ImmutableDB.Tip slot isEBB blockNo hash) =
    "ImmutableDBTip(slot:"
      <+> mpretty slot
      <> ", isEpochBoundaryBlock:"
      <+> mpretty isEBB
      <> ", hash:"
      <+> mpretty hash
      <> ", blockNo:"
      <+> mpretty blockNo
      <> ")"

instance MPretty IsEBB where
  mpretty IsEBB = "true"
  mpretty IsNotEBB = "false"

instance MPretty FsPath where
  mpretty = viaShow

instance MPretty (TraceEventMempool (MorphoBlock h c)) where
  mpretty
    ( TraceMempoolAddedTx
        tx
        (MempoolSize numTxsBefore numBytesBefore)
        (MempoolSize numTxsAfter numBytesAfter)
      ) =
      "Transaction"
        <+> mpretty tx
        <+> "added to mempool. Number of transactions in mempool goes from"
        <+> pretty numTxsBefore
        <+> "to"
        <+> pretty numTxsAfter
        <> ". Bytes of mempool goes from"
        <+> pretty numBytesBefore
        <+> "to"
        <+> pretty numBytesAfter
  mpretty (TraceMempoolRejectedTx tx err (MempoolSize numTxs numBytes)) =
    "Transaction"
      <+> mpretty tx
      <+> "rejected because:"
      <+> mpretty (snd err)
      <> ". Mempool has"
      <+> pretty numTxs
      <+> "transaction(s) and has size of"
      <+> pretty numBytes
      <+> "bytes"
  mpretty (TraceMempoolRemoveTxs txs (MempoolSize numTxs numBytes)) =
    "Transactions were removed because they're no longer valid:"
      <+> mpretty txs
      <> ". Mempool has"
      <+> pretty numTxs
      <+> "transaction(s) and has size of"
      <+> pretty numBytes
      <+> "bytes"
  mpretty (TraceMempoolManuallyRemovedTxs txIds txs (MempoolSize numTxs numBytes)) =
    "Transactions"
      <+> mpretty txIds
      <+> "were manually removed, causing transactions"
      <+> mpretty txs
      <+> "to no longer be valid, removing them as well. Mempool has"
      <+> pretty numTxs
      <+> "transaction(s) and has size of"
      <+> pretty numBytes
      <+> "bytes"

instance MPretty NtN.AcceptConnectionsPolicyTrace where
  mpretty (ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
    "Limit the current number of connections of"
      <+> pretty numOfConnections
      <+> "with a delay of"
      <+> mpretty delay
  mpretty (ServerTraceAcceptConnectionHardLimit limit) =
    "Wait until the current number of connection drops below the limit of"
      <+> pretty limit

instance MPretty DiffusionInitializationTracer where
  mpretty (RunServer addr) = "Run server at" <+> mpretty addr
  mpretty (RunLocalServer (NtC.LocalAddress fp)) =
    "Run local server at" <+> pretty fp
  mpretty (UsingSystemdSocket fp) = "Using systemd socket" <+> pretty fp
  mpretty (CreateSystemdSocketForSnocketPath fp) =
    "Created systemd socket for snocket path" <+> pretty fp
  mpretty (CreatedLocalSocket fp) = "Created local socket" <+> pretty fp
  mpretty (ConfiguringLocalSocket fp fd) =
    "Configuring local socket" <+> pretty fp <+> "with descriptor" <+> mpretty fd
  mpretty (ListeningLocalSocket fp fd) =
    "Listening to local socket" <+> pretty fp <+> "with descriptor" <+> mpretty fd
  mpretty (LocalSocketUp fp fd) =
    "Local socket up:" <+> pretty fp <+> "with descriptor" <+> mpretty fd
  mpretty (CreatingServerSocket addr) = "Creating server socket" <+> mpretty addr
  mpretty (ConfiguringServerSocket addr) =
    "Configuring server socket" <+> mpretty addr
  mpretty (ListeningServerSocket addr) =
    "Listening to server socket" <+> mpretty addr
  mpretty (ServerSocketUp addr) =
    "Server socket up:" <+> mpretty addr
  mpretty (UnsupportedLocalSystemdSocket addr) =
    "Unsupported local systemd socket" <+> mpretty addr
  mpretty UnsupportedReadySocketCase =
    "Unsupported ready socket case"
  mpretty (DiffusionErrored e) = "Diffusion error with:" <+> mpretty e

instance MPretty TraceLedgerPeers where
  mpretty (PickedPeer relayAddr (AccPoolStake accPoolStake) (PoolStake poolStake)) =
    "Picked peer"
      <+> mpretty relayAddr
      <+> "with a relative stake of"
      <+> mpretty poolStake
      <+> "in respect to the total amount staked and an accumulated relative stake of"
      <+> mpretty accPoolStake
      <+> "in respect to the total amount staked including all preceding pools"
  mpretty (PickedPeers n relayAddrs) =
    "Wanted to pick"
      <+> pretty n
      <+> "peer(s) and the following peers were picked:"
      <+> mpretty relayAddrs
  mpretty (FetchingNewLedgerState n) =
    "Fetching new ledger state with" <+> pretty n <+> "peer(s) returned"

instance MPretty RelayAddress where
  mpretty (RelayAddressDomain domainAddr) = mpretty domainAddr
  mpretty (RelayAddressAddr (IPv4 ip) port) = mpretty ip <> ":" <> mpretty port
  mpretty (RelayAddressAddr (IPv6 ip) port) = "[" <> mpretty ip <> "]:" <> mpretty port

instance MPretty DomainAddress where
  mpretty (DomainAddress domain port) = mpretty domain <> ":" <> mpretty port

instance MPretty FileDescriptor where
  mpretty = viaShow

instance MPretty SomeException where
  mpretty = viaShow

instance MPretty Rational where
  mpretty = viaShow

instance MPretty IP.IPv4 where
  mpretty = viaShow

instance MPretty IP.IPv6 where
  mpretty = viaShow

instance MPretty Domain where
  mpretty = viaShow

instance MPretty Socket.PortNumber where
  mpretty = viaShow

instance (MPretty peer, MPretty a) => MPretty (TraceLabelPeer peer a) where
  mpretty (TraceLabelPeer peer a) =
    "Peer" <+> mpretty peer <> ":" <+> mpretty a

instance MPretty FetchDecline where
  mpretty FetchDeclineChainNotPlausible =
    "Blocks not fetched from peer: chain not plausible"
  mpretty FetchDeclineChainNoIntersection =
    "Blocks not fetched from peer: chain has no intersection"
  mpretty FetchDeclineAlreadyFetched =
    "Blocks not fetched from peer: already fetched"
  mpretty FetchDeclineInFlightThisPeer =
    "Blocks not fetched from peer: in flight this peer"
  mpretty FetchDeclineInFlightOtherPeer =
    "Blocks not fetched from peer: in flight other peer"
  mpretty FetchDeclinePeerShutdown =
    "Blocks not fetched from peer: peer was shutdown"
  mpretty FetchDeclinePeerSlow =
    "Blocks not fetched from peer: peer is too slow"
  mpretty (FetchDeclineReqsInFlightLimit _) =
    "Blocks not fetched from peer: already at limit for requests in flight"
  mpretty FetchDeclineBytesInFlightLimit {} =
    "Blocks not fetched from peer: already at bytes limit for requests in flight"
  mpretty FetchDeclinePeerBusy {} =
    "Blocks not fetched from peer: busy"
  mpretty FetchDeclineConcurrencyLimit {} =
    "Blocks not fetched from peer: concurrency limit"

instance (AF.HasHeader h, MPretty (HeaderHash h)) => MPretty (TraceFetchClientState h) where
  mpretty (AddedFetchRequest fetchRequest _ _ _) =
    "Block fetch decision thread has a new fetch instruction consisting of:"
      <+> mpretty fetchRequest
  mpretty (AcknowledgedFetchRequest fetchRequest) =
    "Block fetch client thread picks up the request added by the block fetch decision thread:"
      <+> mpretty fetchRequest
  mpretty (StartedFetchBatch chainRange _ _ _) =
    "Block fetch client starts receiving a streaming batch of blocks:"
      <+> mpretty chainRange
  mpretty (CompletedBlockFetch point _ _ _ _) =
    "Block fetch client successfully received"
      <+> mpretty point
      <+> "within a streaming batch of blocks"
  mpretty (CompletedFetchBatch chainRange _ _ _) =
    "Block fetch client sucessfully received the full streaming batch of blocks:"
      <+> mpretty chainRange
  mpretty (RejectedFetchBatch chainRange _ _ _) =
    "Other peer rejects rejects block fetch client's request of blocks"
      <+> mpretty chainRange
  mpretty (ClientTerminating n) =
    "Block fetch client is terminating."
      <+> pretty n
      <+> "outstanding request(s)"

instance MPretty (TraceLocalTxSubmissionServerEvent (MorphoBlock h c)) where
  mpretty (TraceReceivedTx genTx) = "A transaction was received:" <+> mpretty genTx

instance
  ( AF.HasHeader h,
    MPretty (HeaderHash h)
  ) =>
  MPretty (FetchRequest h)
  where
  mpretty (FetchRequest fragments) = mpretty $ fmap AF.headPoint fragments

instance MPretty point => MPretty (ChainRange point) where
  mpretty (ChainRange p1 p2) = "(" <> mpretty p1 <> "," <+> mpretty p2 <> ")"

instance
  ( forall (st :: ps). MPretty (ClientHasAgency st),
    forall (st :: ps). MPretty (ServerHasAgency st),
    forall (st :: ps) (st' :: ps). MPretty (Message ps st st')
  ) =>
  MPretty (NtN.TraceSendRecv ps)
  where
  mpretty (NtN.TraceSendMsg msg) = "Send message:" <+> mpretty msg
  mpretty (NtN.TraceRecvMsg msg) = "Receive message:" <+> mpretty msg

instance
  ( forall (st :: ps). MPretty (ClientHasAgency st),
    forall (st :: ps). MPretty (ServerHasAgency st),
    forall (st :: ps) (st' :: ps). MPretty (Message ps st st')
  ) =>
  MPretty (AnyMessageAndAgency ps)
  where
  mpretty (AnyMessageAndAgency agency msg) = mpretty agency <> "," <+> mpretty msg

instance
  ( forall (st' :: ps). MPretty (ClientHasAgency st'),
    forall (st' :: ps). MPretty (ServerHasAgency st')
  ) =>
  MPretty (PeerHasAgency pr (st :: ps))
  where
  mpretty (ClientAgency stok) = "ClientAgency(" <> mpretty stok <> ")"
  mpretty (ServerAgency stok) = "ServerAgency(" <> mpretty stok <> ")"

instance MPretty (ClientHasAgency (st :: Handshake vNumber vParams)) where
  mpretty Handshake.TokPropose = "TokPropose"

instance MPretty (ServerHasAgency (st :: Handshake vNumber vParams)) where
  mpretty Handshake.TokConfirm = "TokConfirm"

instance MPretty (ClientHasAgency (st :: ChainSync.ChainSync header point tip)) where
  mpretty ChainSync.TokIdle = "TokIdle"

instance MPretty (ServerHasAgency (st :: ChainSync.ChainSync header point tip)) where
  mpretty (ChainSync.TokNext ChainSync.TokCanAwait) = "TokNext TokCanAwait"
  mpretty (ChainSync.TokNext ChainSync.TokMustReply) = "TokNext TokMustReply"
  mpretty ChainSync.TokIntersect = "TokIntersect"

instance MPretty (ClientHasAgency (st :: BlockFetch.BlockFetch block point)) where
  mpretty BlockFetch.TokIdle = "TokIdle"

instance MPretty (ServerHasAgency (st :: BlockFetch.BlockFetch block point)) where
  mpretty BlockFetch.TokBusy = "TokBusy"
  mpretty BlockFetch.TokStreaming = "TokStreaming"

instance MPretty (ClientHasAgency (st :: TxSubmission.TxSubmission txid tx)) where
  mpretty (TxSubmission.TokTxIds TxSubmission.TokBlocking) = "TokTxIds TokBlocking"
  mpretty (TxSubmission.TokTxIds TxSubmission.TokNonBlocking) = "TokTxIds TokNonBlocking"
  mpretty TxSubmission.TokTxs = "TokTxs"

instance MPretty (ServerHasAgency (st :: TxSubmission.TxSubmission txid tx)) where
  mpretty TxSubmission.TokIdle = "TokIdle"

instance
  (forall (st' :: ps). MPretty (ClientHasAgency st')) =>
  MPretty (ClientHasAgency (st :: Hello.Hello ps (stIdle :: ps)))
  where
  mpretty Hello.TokHello = "TokHello"
  mpretty (Hello.TokClientTalk tok) = "TokClientTalk" <+> mpretty tok

instance
  (forall (st' :: ps). MPretty (ServerHasAgency st')) =>
  MPretty (ServerHasAgency (st :: Hello.Hello ps stIdle))
  where
  mpretty (Hello.TokServerTalk tok) = "TokServerTalk" <+> mpretty tok

instance (MPretty vNumber, MPretty vParams) => MPretty (Message (Handshake vNumber vParams) st st') where
  mpretty (Handshake.MsgProposeVersions versions) =
    "Propose versions:" <+> mpretty versions
  mpretty (Handshake.MsgAcceptVersion vNumber vParams) =
    "Acception version"
      <+> mpretty vNumber
      <+> "with version parameters"
      <+> mpretty vParams
  mpretty (Handshake.MsgRefuse reason) = "Refuse to run any versions:" <+> mpretty reason

instance MPretty vNumber => MPretty (Handshake.RefuseReason vNumber) where
  mpretty (Handshake.VersionMismatch vNumbers rawTags) =
    "All of the prosed versions where not known to the server:"
      <+> mpretty vNumbers
      <> ". Versions raw tags:"
      <+> pretty rawTags
  mpretty (Handshake.HandshakeDecodeError vNumbers vParams) =
    "The server failed to decode following version parameters from version"
      <+> mpretty vNumbers
      <> ":"
      <+> pretty vParams
  mpretty (Handshake.Refused vNumbers vParams) =
    "The server refused to run the proposed version parameters from version"
      <+> mpretty vNumbers
      <> ":"
      <+> pretty vParams

instance
  (MPretty header, MPretty point, MPretty tip) =>
  MPretty (Message (ChainSync.ChainSync header point tip) st st')
  where
  mpretty ChainSync.MsgRequestNext = "Request the next update from the producer."
  mpretty ChainSync.MsgAwaitReply =
    "Acknowledge the request but require the consumer to wait for the next update."
  mpretty (ChainSync.MsgRollForward header tip) =
    "Consumer must extend their chain with header"
      <+> mpretty header
      <> ". Current tip:"
      <+> mpretty tip
  mpretty (ChainSync.MsgRollBackward point tip) =
    "Consumer must roll back to the following point on their chain:"
      <+> mpretty point
      <> ". Current tip:"
      <+> mpretty tip
  mpretty (ChainSync.MsgFindIntersect points) =
    "Producer must find an improved intersection point between the consumer and producer's chains."
      <+> "Here's a sequence of points from the consumer:"
      <+> mpretty points
  mpretty (ChainSync.MsgIntersectFound point tip) =
    "The producer found the intersection point:"
      <+> mpretty point
      <> ". Current tip:"
      <+> mpretty tip
  mpretty (ChainSync.MsgIntersectNotFound tip) =
    "The producer found no intersection: none of the points the consumer supplied are on the producer chain."
      <> "Current tip:"
      <+> mpretty tip
  mpretty ChainSync.MsgDone = "Terminating ChainSync messages"

instance MPretty NodeToNodeVersion where
  mpretty = viaShow

instance MPretty NodeToClientVersion where
  mpretty = viaShow

instance MPretty Term where
  mpretty = viaShow

instance MPretty (Serialised blk) where
  mpretty (Serialised s) = pretty $ TL.decodeUtf8 $ BL16.encode s

instance BftCrypto c => MPretty (SerialisedHeader (MorphoBlock h c)) where
  mpretty (SerialisedHeaderFromDepPair (GenDepPair (NestedCtxt CtxtMorpho) b)) = mpretty b

instance
  (MPretty block, MPretty point) =>
  MPretty (Message (BlockFetch.BlockFetch block point) st st')
  where
  mpretty (BlockFetch.MsgRequestRange range) = "Request range of blocks" <+> mpretty range
  mpretty BlockFetch.MsgStartBatch = "Start block streaming"
  mpretty BlockFetch.MsgNoBlocks = "No blocks"
  mpretty (BlockFetch.MsgBlock blk) = "Streaming block" <+> mpretty blk
  mpretty BlockFetch.MsgBatchDone = "End of block streaming"
  mpretty BlockFetch.MsgClientDone = "Terminating BlockFetch message"

instance (MPretty txid, MPretty tx) => MPretty (Message (TxSubmission.TxSubmission txid tx) st st') where
  mpretty (TxSubmission.MsgRequestTxIds tok n nTxIds) =
    "Request"
      <+> pretty nTxIds
      <+> "txids and can acknowledge"
      <+> pretty n
      <+> "outstanding txids with"
      <+> mpretty tok
  mpretty (TxSubmission.MsgReplyTxIds replyList) =
    "Reply with list of transactions identifiers with size in bytes:" <+> mpretty replyList
  mpretty (TxSubmission.MsgRequestTxs txids) =
    "Request transactions corresponding to the given transaction identifiers:"
      <+> mpretty txids
  mpretty (TxSubmission.MsgReplyTxs txs) =
    "Reply with the requested transactions:" <+> mpretty txs
  mpretty TxSubmission.MsgDone = "Terminating TxSubmission message from client"
  -- Should be
  --    mpretty TxSubmission.MsgKThxBye =
  -- but using _ to prevent deprecation error
  mpretty _ = "K Thanks Bye"

instance MPretty TxSubmission.TxSizeInBytes where
  mpretty = pretty

instance MPretty (TxSubmission.TokBlockingStyle blocking) where
  mpretty TxSubmission.TokBlocking = "TokBlocking"
  mpretty TxSubmission.TokNonBlocking = "TokNonBlocking"

instance (MPretty a) => MPretty (TxSubmission.BlockingReplyList blocking a) where
  mpretty (TxSubmission.BlockingReply nonEmptyList) = mpretty $ toList nonEmptyList
  mpretty (TxSubmission.NonBlockingReply list) = mpretty list

instance
  (forall (from' :: ps) (to' :: ps). MPretty (Message ps from' to')) =>
  MPretty (Message (Hello.Hello ps stIdle) st st')
  where
  mpretty Hello.MsgHello = "Client side hello message"
  mpretty (Hello.MsgTalk msg) = mpretty msg

instance MPretty (TraceTxSubmissionInbound txid tx) where
  mpretty (TraceTxSubmissionCollected n) = pretty n <+> "transaction(s) just about to be inserted"
  mpretty (TraceTxSubmissionProcessed (ProcessedTxCount nTxAccepted nTxRejected)) =
    pretty nTxAccepted
      <+> "accepted transaction(s) and"
      <+> pretty nTxRejected
      <+> "rejected transaction(s)"
  mpretty TraceTxInboundTerminated = "Server received MsgDone"
  mpretty (TraceTxInboundCanRequestMoreTxs _) = "There are more transactions that can be requested."
  mpretty (TraceTxInboundCannotRequestMoreTxs _) = "There are no more transactions that can be requested."

instance (MPretty txid, MPretty tx) => MPretty (TraceTxSubmissionOutbound txid tx) where
  mpretty (TraceTxSubmissionOutboundRecvMsgRequestTxs txIds) = "The IDs of the transactions requested:" <+> mpretty txIds
  mpretty (TraceTxSubmissionOutboundSendMsgReplyTxs txs) = "The transactions to be sent in the response:" <+> mpretty txs
  mpretty (TraceControlMessage msg) = "Transaction submission out bound control message:" <+> mpretty msg

instance MPretty ControlMessage where
  mpretty = viaShow

instance (MPretty addr, MPretty a) => MPretty (WithAddr addr a) where
  mpretty (WithAddr addr err) = mpretty addr <> ":" <+> mpretty err

instance MPretty ErrorPolicyTrace where
  mpretty (ErrorPolicySuspendPeer (Just err) _ _) =
    "Suspending peer with given exception:" <+> viaShow err
  mpretty (ErrorPolicySuspendPeer Nothing _ _) = "Suspending peer"
  mpretty (ErrorPolicySuspendConsumer err _) =
    "Suspending consumer with given exception:" <+> viaShow err
  mpretty (ErrorPolicyLocalNodeError err) =
    "Caught a local exception:" <+> viaShow err
  mpretty ErrorPolicyResumePeer = "Resuming a peer"
  mpretty ErrorPolicyKeepSuspended = "Consumer was suspended until producer will resume"
  mpretty ErrorPolicyResumeConsumer = "Resuming consumer"
  mpretty ErrorPolicyResumeProducer = "Resuming producer"
  mpretty (ErrorPolicyUnhandledApplicationException err) =
    "Unhandled application exception:"
      <+> mpretty err
  mpretty (ErrorPolicyUnhandledConnectionException err) =
    "Unhandled connect exception:"
      <+> mpretty err
  mpretty (ErrorPolicyAcceptException err) =
    "Accept throwed an exception:"
      <+> mpretty err

instance MPretty a => MPretty (WithDomainName a) where
  mpretty WithDomainName {wdnDomain, wdnEvent} =
    "Domain:" <+> pretty (decodeUtf8 wdnDomain) <+> mpretty wdnEvent

instance MPretty addr => MPretty (SubscriptionTrace addr) where
  mpretty (SubscriptionTraceConnectStart dst) =
    "Connection Attempt Start, destination" <+> mpretty dst
  mpretty (SubscriptionTraceConnectEnd dst res) =
    "Connection Attempt End, destination"
      <+> mpretty dst <> ", outcome:"
      <+> mpretty res
  mpretty (SubscriptionTraceSocketAllocationException dst e) =
    "Socket Allocation Exception, destination"
      <+> mpretty dst
      <> ", exception:"
      <+> viaShow e
  mpretty (SubscriptionTraceConnectException dst e) =
    "Connection Attempt Exception, destination"
      <+> mpretty dst
      <> ", exception:"
      <+> viaShow e
  mpretty (SubscriptionTraceApplicationException dst e) =
    "Application Exception, destination"
      <+> mpretty dst
      <> ", exception:"
      <+> viaShow e
  mpretty (SubscriptionTraceTryConnectToPeer addr) =
    "Trying to connect to" <+> mpretty addr
  mpretty (SubscriptionTraceSkippingPeer addr) =
    "Skipping peer" <+> mpretty addr
  mpretty SubscriptionTraceSubscriptionRunning =
    "Required subscriptions started"
  mpretty (SubscriptionTraceSubscriptionWaiting d) =
    "Waiting on" <+> pretty d <+> "active connections"
  mpretty SubscriptionTraceSubscriptionFailed =
    "Failed to start all required subscriptions"
  mpretty (SubscriptionTraceSubscriptionWaitingNewConnection delay) =
    "Waiting" <+> viaShow delay <+> "before attempting a new connection"
  mpretty (SubscriptionTraceStart val) =
    "Starting Subscription Worker, valency" <+> pretty val
  mpretty (SubscriptionTraceRestart duration desiredVal currentVal) =
    "Restarting Subscription after"
      <+> mpretty duration
      <+> "desired valency"
      <+> pretty desiredVal
      <+> "current valency"
      <+> pretty currentVal
  mpretty (SubscriptionTraceConnectionExist dst) =
    "Connection Existed to" <+> mpretty dst
  mpretty (SubscriptionTraceUnsupportedRemoteAddr dst) =
    "Unsupported remote target address" <+> mpretty dst
  mpretty SubscriptionTraceMissingLocalAddress =
    "Missing local address"
  mpretty (SubscriptionTraceAllocateSocket addr) =
    "Allocate socket to" <+> mpretty addr
  mpretty (SubscriptionTraceCloseSocket addr) =
    "Closed socket to" <+> mpretty addr

instance MPretty IOException where
  mpretty = viaShow

instance MPretty DiffTime where
  mpretty = viaShow

instance MPretty ConnectResult where
  mpretty ConnectSuccess = "Successful connection"
  mpretty ConnectSuccessLast = "Successfully connection, reached the valency target. Other ongoing connection attempts will be killed."
  mpretty ConnectValencyExceeded = "Someone else manged to create the final connection to a target before us."

instance MPretty DnsTrace where
  -- DnsTrace already has a nice humain readable Show instance
  mpretty = viaShow

instance MPretty Socket.SockAddr where
  mpretty = viaShow

instance (MPretty a) => MPretty (NtN.ConnectionId a) where
  mpretty cid = mpretty (NtN.remoteAddress cid)

instance MPretty a => MPretty (WithIPList a) where
  mpretty (WithIPList _ destAddrs event) =
    "With destinations" <+> mpretty destAddrs <> ":" <+> mpretty event

instance MPretty LocalAddress where
  mpretty (LocalAddress fp) = "LocalAddress(" <> pretty fp <> ")"

instance (MPretty peer, MPretty a) => MPretty (WithMuxBearer peer a) where
  mpretty (WithMuxBearer peer a) = "With mux bearer" <+> mpretty peer <> ":" <+> mpretty a

instance MPretty MuxTrace where
  mpretty = viaShow

instance MPretty a => MPretty [a] where
  mpretty = ppList . map mpretty

instance (MPretty a, MPretty b) => MPretty (Map a b) where
  mpretty m =
    "("
      <> hsep (punctuate comma (map ppTuple (Map.toList m)))
      <> ")"
    where
      ppTuple (a, b) = mpretty a <+> "->" <+> mpretty b

instance (MPretty a, MPretty b) => MPretty (a, b) where
  mpretty (a, b) = "(" <> mpretty a <> "," <+> mpretty b <> ")"

instance (MPretty a) => MPretty (Maybe a) where
  mpretty (Just a) = "Just(" <> mpretty a <> ")"
  mpretty Nothing = "Nothing"

instance (MPretty a, MPretty b) => MPretty (Either a b) where
  mpretty (Left e) = "Left(" <> mpretty e <> ")"
  mpretty (Right v) = "Right(" <> mpretty v <> ")"

instance MPretty Int where
  mpretty = pretty

instance MPretty Bool where
  mpretty = pretty

instance
  (MorphoStateDefaultConstraints h c, Signable (BftDSIGN c) (MorphoStdHeader h c)) =>
  MPretty (ChainDB.InvalidBlockReason (MorphoBlock h c))
  where
  mpretty = viaShow

instance MPretty (HeaderHash blk) => MPretty (Point blk) where
  mpretty GenesisPoint = "Genesis"
  mpretty (BlockPoint slot h) =
    "Block(slot:" <+> mpretty slot <> ", hash:" <+> mpretty h <> ")"

instance MPretty (HeaderHash blk) => MPretty (RealPoint blk) where
  mpretty (RealPoint slot headerHash) =
    "Block(slot:" <+> mpretty slot <> ", hash:" <+> mpretty headerHash <> ")"

ppList :: [Doc ann] -> Doc ann
ppList docs =
  "["
    <> concatWith (\d1 d2 -> d1 <> "," <+> d2) docs
    <> "]"

ppPublicKey :: Bytes -> Signature -> Doc ann
ppPublicKey bytes signature =
  maybe "<unknown>" mpretty (recoverPublicKey signature bytes)
