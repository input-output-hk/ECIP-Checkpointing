{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Tracing.Orphan.ExternalToJSONInstances where

import Cardano.Crypto.Hash.Class
import Cardano.Prelude hiding (show)
import Codec.CBOR.Read (DeserialiseFailure (..))
import Codec.CBOR.Term
import Control.Monad.Class.MonadTime
import Data.Aeson (Options (tagSingleConstructors, unwrapUnaryRecords), ToJSON (..), ToJSONKey, Value (..), defaultOptions, genericToJSON, object, (.=))
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.IP
import qualified Data.Text as Text
import qualified GHC.Exts
import qualified GHC.Stack.Types
import Network.DNS.Types
import Network.Mux (MuxTrace (..), WithMuxBearer (..))
import Network.Mux.Trace (MuxBearerState (..))
import Network.Mux.Types
import qualified Network.Socket as Socket
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.Fragment.Diff
import Ouroboros.Consensus.HardFork.History
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Mempool.API (MempoolSize (..), TraceEventMempool (..))
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server (TraceBlockFetchServerEvent (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientException (..),
    ChainSyncClientResult (..),
    Our (..),
    Their (..),
    TraceChainSyncClientEvent (..),
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server (TraceChainSyncServerEvent (..))
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server (TraceLocalTxSubmissionServerEvent (..))
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..), TraceLabelCreds (..))
import Ouroboros.Consensus.NodeId
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Storage.ChainDB hiding (InvalidBlock, getTipPoint)
import Ouroboros.Consensus.Storage.FS.API.Types
import Ouroboros.Consensus.Storage.ImmutableDB hiding (Tip, getTipPoint)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB.OnDisk
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Types as VolatileDB
import qualified Ouroboros.Consensus.Util as OCU
import Ouroboros.Consensus.Util.CBOR
-- import Ouroboros.Consensus.Util.Orphans ()
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Ouroboros.Network.AnchoredSeq as AS
import Ouroboros.Network.Block (MaxSlotNo, Serialised (..), getTipPoint)
import Ouroboros.Network.BlockFetch.ClientState
import Ouroboros.Network.BlockFetch.Decision
import Ouroboros.Network.BlockFetch.DeltaQ
import Ouroboros.Network.Codec (AnyMessageAndAgency (..), PeerHasAgency (..))
import Ouroboros.Network.Diffusion
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToNode
import Ouroboros.Network.PeerSelection.LedgerPeers
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import qualified Ouroboros.Network.Protocol.Handshake.Type as Handshake
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import qualified Ouroboros.Network.Protocol.Trans.Hello.Type as Hello
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TxSubmission
import Ouroboros.Network.Snocket (FileDescriptor, LocalAddress (..))
import Ouroboros.Network.Subscription
import Ouroboros.Network.TxSubmission.Inbound
  ( ProcessedTxCount (..),
    TraceTxSubmissionInbound (..),
  )
import Ouroboros.Network.TxSubmission.Outbound
  ( TraceTxSubmissionOutbound (..),
  )
import Prelude (show)

deriving instance Generic (TraceBlockchainTimeEvent t)

deriving instance Generic PastHorizonException

deriving instance Generic GHC.Stack.Types.SrcLoc

deriving instance Generic (TraceLabelCreds a)

deriving instance Generic (AF.ChainUpdate blk a)

deriving instance Generic (TraceChainSyncServerEvent blk)

deriving instance Generic (TraceLocalTxSubmissionServerEvent blk)

deriving instance Generic (TraceLabelPeer peer a)

deriving instance Generic (LedgerEvent b)

deriving instance Generic (VolatileDB.ParseError b)

deriving instance Generic ReadIncrementalErr

deriving instance Generic DeserialiseFailure

deriving instance Generic (ImmutableDB.ChunkFileError b)

deriving instance Generic (UnknownRange b)

deriving instance Generic Time

deriving instance Generic (ChainDiff b)

deriving instance Generic (WithIPList a)

deriving instance Generic ConnectResult

deriving instance Generic (LocalAddresses Socket.SockAddr)

deriving instance Generic (WithDomainName a)

deriving instance Generic DnsTrace

deriving instance Generic DNSError

deriving instance Generic MiniProtocolDir

deriving instance Generic MuxSDUHeader

deriving instance Generic MiniProtocolNum

deriving instance Generic RemoteClockModel

deriving instance Generic MuxBearerState

deriving instance Generic (WithAddr a b)

deriving instance Generic ErrorPolicyTrace

deriving instance Generic (TraceSendRecv a)

deriving instance Generic AcceptConnectionsPolicyTrace

deriving instance Generic DiffusionInitializationTracer

deriving instance Generic TraceLedgerPeers

deriving instance Generic PoolStake

deriving instance Generic AccPoolStake

deriving instance Generic RelayAddress

deriving instance Generic DomainAddress

deriving instance Generic (TraceChainSyncClientEvent a)

deriving instance Generic FetchDecline

deriving instance Generic FetchMode

deriving instance Generic (TraceFetchClientState a)

deriving instance Generic (ChainRange pt)

deriving instance Generic (PeerFetchStatus hdr)

deriving instance Generic IsIdle

deriving instance Generic (PeerFetchInFlight a)

deriving instance Generic (FetchRequest a)

deriving instance Generic PeerFetchInFlightLimits

deriving instance Generic (TraceBlockFetchServerEvent blk)

deriving instance Generic (TraceTxSubmissionInbound txid tx)

deriving instance Generic AcquireFailure

deriving instance Generic ProcessedTxCount

deriving instance Generic (TraceTxSubmissionOutbound txid tx)

deriving instance Generic ControlMessage

deriving instance Generic (TraceEventMempool blk)

deriving instance Generic MempoolSize

deriving instance Generic (TraceForgeEvent blk)

deriving instance Generic OutsideForecastRange

deriving instance Generic (Serialised blk)

deriving instance Generic Term

deriving instance Generic NodeToNodeVersion

deriving instance Generic NodeToClientVersion

deriving instance Generic (Handshake.RefuseReason vNumber)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { tagSingleConstructors = True,
      unwrapUnaryRecords = True
    }

deriving newtype instance ToJSON BlockNo

instance ToJSON SafeZone where
  toJSON = genericToJSON jsonOptions

instance ToJSON SlotLength where
  toJSON = String . Text.pack . show . getSlotLength

instance ToJSON EraParams where
  toJSON = genericToJSON jsonOptions

deriving newtype instance ToJSON RelativeTime

instance ToJSON Bound where
  toJSON = genericToJSON jsonOptions

instance ToJSON EraEnd where
  toJSON = genericToJSON jsonOptions

instance ToJSON EraSummary where
  toJSON = genericToJSON jsonOptions

instance ToJSON GHC.Stack.Types.SrcLoc where
  toJSON = genericToJSON jsonOptions

instance ToJSON PastHorizonException where
  toJSON = genericToJSON jsonOptions

deriving newtype instance ToJSON SystemStart

instance ToJSON DiskSnapshot where
  toJSON = genericToJSON jsonOptions

instance ToJSON t => ToJSON (TraceBlockchainTimeEvent t) where
  toJSON = genericToJSON jsonOptions

instance ToJSON a => ToJSON (TraceLabelCreds a) where
  toJSON (TraceLabelCreds _ a) = toJSON a

instance ToJSON a => ToJSON (WithOrigin a) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON peer, ToJSON a) => ToJSON (TraceLabelPeer peer a) where
  toJSON = genericToJSON jsonOptions

instance ToJSON Ouroboros.Consensus.Storage.FS.API.Types.FsPath where
  toJSON = genericToJSON jsonOptions

instance ToJSON DeserialiseFailure where
  toJSON = genericToJSON jsonOptions

instance ToJSON ReadIncrementalErr where
  toJSON = genericToJSON jsonOptions

deriving newtype instance ToJSON VolatileDB.BlockOffset

instance ToJSON IsEBB where
  toJSON = genericToJSON jsonOptions

instance ToJSON ChunkNo where
  toJSON = genericToJSON jsonOptions

instance ToJSON ImmutableDB.TraceCacheEvent where
  toJSON = genericToJSON jsonOptions

instance ToJSON BftValidationErr where
  toJSON = genericToJSON jsonOptions

instance ToJSON Time where
  toJSON = genericToJSON jsonOptions

instance ToJSON (HeaderHash b) => ToJSON (AF.Anchor b) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON b, HasHeader b, ToJSON (HeaderHash b)) => ToJSON (ChainDiff b) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (RealPoint b) => ToJSON (NewTipInfo b) where
  toJSON = genericToJSON jsonOptions

instance ToJSON ConnectResult where
  toJSON = genericToJSON jsonOptions

instance ToJSON (LocalAddresses Socket.SockAddr) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  toJSON = genericToJSON jsonOptions

instance ToJSON a => ToJSON (WithDomainName a) where
  toJSON (WithDomainName domain a) =
    object
      [ "tag" .= String "WithDomainName",
        "domain" .= String (decodeUtf8 domain),
        "value" .= a
      ]

instance ToJSON DNSError where
  toJSON = genericToJSON jsonOptions

instance ToJSON DnsTrace where
  toJSON = genericToJSON jsonOptions

instance (ToJSON peer, ToJSON a) => ToJSON (WithMuxBearer peer a) where
  toJSON = genericToJSON jsonOptions

instance ToJSON MiniProtocolDir where
  toJSON = genericToJSON jsonOptions

instance ToJSON MiniProtocolNum where
  toJSON = genericToJSON jsonOptions

instance ToJSON RemoteClockModel where
  toJSON = genericToJSON jsonOptions

instance ToJSON MuxSDUHeader where
  toJSON = genericToJSON jsonOptions

deriving newtype instance ToJSON CoreNodeId

instance ToJSON MuxBearerState where
  toJSON = genericToJSON jsonOptions

instance ToJSON i => ToJSON (ConnectionId i) where
  toJSON = genericToJSON jsonOptions

instance ToJSON LocalAddress where
  toJSON = genericToJSON jsonOptions

instance (ToJSON a, ToJSON b) => ToJSON (WithAddr a b) where
  toJSON = genericToJSON jsonOptions

instance Show (HeaderHash blk) => ToJSON (Point blk) where
  toJSON GenesisPoint = String "GenesisPoint"
  toJSON (BlockPoint (SlotNo s) h) =
    object
      [ "slot" .= s,
        -- The Show instance for `Hash h a` adds a " at the front and beginning
        -- And we can't use the ToJSON instance of it because of existential
        -- quantification in other parts only giving us the Show instance
        "hash" .= String (Text.filter (/= '"') $ Text.pack (show h))
      ]

instance ToJSON (AnyMessageAndAgency ps) => ToJSON (TraceSendRecv ps) where
  toJSON = genericToJSON jsonOptions

instance ToJSON (ClientHasAgency (st :: Handshake vNumber vParams)) where
  toJSON Handshake.TokPropose = "TokPropose"

instance ToJSON (ServerHasAgency (st :: Handshake vNumber vParams)) where
  toJSON Handshake.TokConfirm = "TokConfirm"

instance
  (ToJSON vNumber, ToJSONKey vNumber, ToJSON vParams) =>
  ToJSON (Message (Handshake vNumber vParams) from to)
  where
  toJSON (Handshake.MsgProposeVersions versions) =
    object
      [ "tag" .= String "MsgProposeVersions",
        "versions" .= versions
      ]
  toJSON (Handshake.MsgAcceptVersion num params) =
    object
      [ "tag" .= String "MsgAcceptVersion",
        "number" .= num,
        "params" .= params
      ]
  toJSON (Handshake.MsgRefuse reason) =
    object
      [ "tag" .= String "MsgRefuse",
        "reason" .= reason
      ]

instance ToJSON (ClientHasAgency (st :: ChainSync.ChainSync header point tip)) where
  toJSON ChainSync.TokIdle = "TokIdle"

instance ToJSON (ServerHasAgency (st :: ChainSync.ChainSync header point tip)) where
  toJSON (ChainSync.TokNext ChainSync.TokCanAwait) = "TokNext TokCanAwait"
  toJSON (ChainSync.TokNext ChainSync.TokMustReply) = "TokNext TokMustReply"
  toJSON ChainSync.TokIntersect = "TokIntersect"

instance
  (ToJSON header, ToJSON point, ToJSON tip) =>
  ToJSON (Message (ChainSync.ChainSync header point tip) from to)
  where
  toJSON ChainSync.MsgRequestNext = object ["tag" .= String "MsgRequestNext"]
  toJSON ChainSync.MsgAwaitReply = object ["tag" .= String "MsgAwaitReply"]
  toJSON (ChainSync.MsgRollForward h tip) =
    object
      [ "tag" .= String "MsgRollForward",
        "header" .= h,
        "tip" .= tip
      ]
  toJSON (ChainSync.MsgRollBackward p tip) =
    object
      [ "tag" .= String "MsgRollBackward",
        "point" .= p,
        "tip" .= tip
      ]
  toJSON (ChainSync.MsgFindIntersect ps) =
    object
      [ "tag" .= String "MsgFindIntersect",
        "points" .= ps
      ]
  toJSON (ChainSync.MsgIntersectFound p tip) =
    object
      [ "tag" .= String "MsgIntersectFound",
        "point" .= p,
        "tip" .= tip
      ]
  toJSON (ChainSync.MsgIntersectNotFound tip) =
    object
      [ "tag" .= String "MsgIntersectNotFound",
        "tip" .= tip
      ]
  toJSON ChainSync.MsgDone = object ["tag" .= String "MsgDone"]

instance ToJSON (ClientHasAgency (st :: BlockFetch.BlockFetch block point)) where
  toJSON BlockFetch.TokIdle = "TokIdle"

instance ToJSON (ServerHasAgency (st :: BlockFetch.BlockFetch block point)) where
  toJSON BlockFetch.TokBusy = "TokBusy"
  toJSON BlockFetch.TokStreaming = "TokStreaming"

instance
  (ToJSON block, ToJSON point) =>
  ToJSON (Message (BlockFetch.BlockFetch block point) from to)
  where
  toJSON (BlockFetch.MsgRequestRange range) =
    object
      [ "tag" .= String "MsgRequestRange",
        "range" .= range
      ]
  toJSON BlockFetch.MsgStartBatch = object ["tag" .= String "MsgStartBatch"]
  toJSON (BlockFetch.MsgBlock block) =
    object
      [ "tag" .= String "MsgBlock",
        "block" .= block
      ]
  toJSON BlockFetch.MsgNoBlocks = object ["tag" .= String "MsgNoBlocks"]
  toJSON BlockFetch.MsgBatchDone = object ["tag" .= String "MsgBatchDone"]
  toJSON BlockFetch.MsgClientDone = object ["tag" .= String "MsgClientDone"]

instance ToJSON (ClientHasAgency (st :: TxSubmission.TxSubmission txid tx)) where
  toJSON (TxSubmission.TokTxIds TxSubmission.TokBlocking) = "TokTxIds TokBlocking"
  toJSON (TxSubmission.TokTxIds TxSubmission.TokNonBlocking) = "TokTxIds TokNonBlocking"
  toJSON TxSubmission.TokTxs = "TokTxs"

instance ToJSON (ServerHasAgency (st :: TxSubmission.TxSubmission txid tx)) where
  toJSON TxSubmission.TokIdle = "TokIdle"

instance
  (ToJSON txid, ToJSON tx) =>
  ToJSON (Message (TxSubmission.TxSubmission txid tx) from to)
  where
  toJSON (TxSubmission.MsgRequestTxIds blocking ackCount reqCount) =
    object
      [ "tag" .= String "MsgRequestTxIds",
        "blocking" .= String (Text.pack (show blocking)),
        "ackCount" .= ackCount,
        "reqCount" .= reqCount
      ]
  toJSON (TxSubmission.MsgReplyTxIds (TxSubmission.BlockingReply txids)) =
    object
      [ "tag" .= String "MsgReplyTxIds",
        "txids" .= txids
      ]
  toJSON (TxSubmission.MsgReplyTxIds (TxSubmission.NonBlockingReply txids)) =
    object
      [ "tag" .= String "MsgReplyTxIds",
        "txids" .= txids
      ]
  toJSON (TxSubmission.MsgRequestTxs txids) =
    object
      [ "tag" .= String "MsgRequestTxs",
        "txids" .= txids
      ]
  toJSON (TxSubmission.MsgReplyTxs txs) =
    object
      [ "tag" .= String "MsgReplyTxs",
        "txs" .= txs
      ]
  toJSON TxSubmission.MsgDone =
    object
      [ "tag" .= String "MsgDone"
      ]
  -- Should be
  --    toJSON TxSubmission.MsgKThxBye =
  -- but using _ to prevent deprecation error
  toJSON _ =
    object
      [ "tag" .= String "MsgKThxBye"
      ]

instance
  (forall (from' :: ps) (to' :: ps). ToJSON (Message ps from' to')) =>
  ToJSON (Message (Hello.Hello ps stIdle) from to)
  where
  toJSON Hello.MsgHello = object ["tag" .= String "MsgHello"]
  toJSON (Hello.MsgTalk msg) =
    object
      [ "tag" .= String "MsgTalk",
        "msg" .= msg
      ]

instance
  (forall (st' :: ps). ToJSON (ClientHasAgency st')) =>
  ToJSON (ClientHasAgency (st :: Hello.Hello ps (stIdle :: ps)))
  where
  toJSON Hello.TokHello = object ["tag" .= String "TokHello"]
  toJSON (Hello.TokClientTalk tok) =
    object
      [ "tag" .= String "TokClientTalk",
        "token" .= tok
      ]

instance
  (forall (st' :: ps). ToJSON (ServerHasAgency st')) =>
  ToJSON (ServerHasAgency (st :: Hello.Hello ps stIdle))
  where
  toJSON (Hello.TokServerTalk tok) =
    object
      [ "tag" .= String "TokServerTalk",
        "token" .= tok
      ]

instance ToJSON (Serialised a) where
  toJSON (Serialised bytes) = toJSON bytes

instance ToJSON Term where
  toJSON = genericToJSON jsonOptions

instance ToJSON NodeToClientVersion where
  toJSON = genericToJSON jsonOptions

instance ToJSONKey NodeToClientVersion

instance ToJSON NodeToNodeVersion where
  toJSON = genericToJSON jsonOptions

instance ToJSONKey NodeToNodeVersion

instance ToJSON vNumber => ToJSON (Handshake.RefuseReason vNumber) where
  toJSON = genericToJSON jsonOptions

instance ToJSON AcceptConnectionsPolicyTrace where
  toJSON = genericToJSON jsonOptions

instance ToJSON FileDescriptor where
  toJSON = genericToJSON jsonOptions

instance ToJSON DiffusionInitializationTracer where
  toJSON = genericToJSON jsonOptions

instance ToJSON PoolStake where
  toJSON = genericToJSON jsonOptions

instance ToJSON AccPoolStake where
  toJSON = genericToJSON jsonOptions

instance ToJSON IPv6 where
  toJSON = genericToJSON jsonOptions

instance ToJSON IPv4 where
  toJSON = genericToJSON jsonOptions

instance ToJSON IP where
  toJSON = genericToJSON jsonOptions

instance ToJSON DomainAddress where
  toJSON = genericToJSON jsonOptions

instance ToJSON RelayAddress where
  toJSON = genericToJSON jsonOptions

instance ToJSON TraceLedgerPeers where
  toJSON = genericToJSON jsonOptions

instance ToJSON FetchMode where
  toJSON = genericToJSON jsonOptions

instance ToJSON FetchDecline where
  toJSON = genericToJSON jsonOptions

instance ToJSON pt => ToJSON (ChainRange pt) where
  toJSON = genericToJSON jsonOptions

instance ToJSON IsIdle where
  toJSON = genericToJSON jsonOptions

instance ToJSON (Point hdr) => ToJSON (PeerFetchStatus hdr) where
  toJSON = genericToJSON jsonOptions

instance ToJSON MaxSlotNo where
  toJSON = genericToJSON jsonOptions

instance ToJSON (Point a) => ToJSON (PeerFetchInFlight a) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON a, ToJSON (HeaderHash a), HasHeader a) => ToJSON (FetchRequest a) where
  toJSON = genericToJSON jsonOptions

instance ToJSON PeerFetchInFlightLimits where
  toJSON = genericToJSON jsonOptions

instance ToJSON (TraceBlockFetchServerEvent blk) where
  toJSON = genericToJSON jsonOptions

instance ToJSON ProcessedTxCount where
  toJSON = genericToJSON jsonOptions

instance ToJSON (TraceTxSubmissionInbound txid tx) where
  toJSON = genericToJSON jsonOptions

instance ToJSON ControlMessage where
  toJSON = genericToJSON jsonOptions

instance (ToJSON tx, ToJSON txid) => ToJSON (TraceTxSubmissionOutbound txid tx) where
  toJSON = genericToJSON jsonOptions

instance ToJSON MempoolSize where
  toJSON = genericToJSON jsonOptions

instance ToJSON OutsideForecastRange where
  toJSON = genericToJSON jsonOptions

instance ToJSON a => ToJSON (Their a) where
  toJSON (Their a) = toJSON a

instance ToJSON a => ToJSON (Our a) where
  toJSON (Our a) = toJSON a

instance
  ( forall (st' :: ps). ToJSON (ClientHasAgency st'),
    forall (st' :: ps). ToJSON (ServerHasAgency st')
  ) =>
  ToJSON (PeerHasAgency pr (st :: ps))
  where
  toJSON (ClientAgency stok) =
    object
      [ "tag" .= String "ClientAgency",
        "contents" .= stok
      ]
  toJSON (ServerAgency stok) =
    object
      [ "tag" .= String "ServerAgency",
        "contents" .= stok
      ]

instance
  ( forall (st :: ps). ToJSON (ClientHasAgency st),
    forall (st :: ps). ToJSON (ServerHasAgency st),
    forall (st :: ps) (st' :: ps). ToJSON (Message ps st st')
  ) =>
  ToJSON (AnyMessageAndAgency ps)
  where
  toJSON (AnyMessageAndAgency agency msg) = toJSON (agency, msg)

instance ToJSON (OCU.Some a) where
  toJSON _ = String "Some"

instance ToJSON CallStack where
  toJSON x = toJSON $ GHC.Exts.toList x

instance ToJSON ByteString where
  toJSON bs = String $ "0x" <> decodeUtf8 (B16.encode bs)

instance ToJSON LBS.ByteString where
  toJSON = toJSON . LBS.toStrict

instance (AS.Anchorable v a b, ToJSON a, ToJSON b) => ToJSON (AF.AnchoredSeq v a b) where
  toJSON (AF.Empty a) = toJSON a
  toJSON (l AF.:> b) =
    object
      [ "rest" .= toJSON l,
        "top" .= toJSON b
      ]

instance ToJSON Socket.SockAddr where
  toJSON = String . Text.pack . show

instance ToJSON PortNumber where
  toJSON p = Number $ fromIntegral p

instance ToJSON IOException where
  toJSON e = String $ Text.pack $ displayException e

instance ToJSON SomeException where
  toJSON e = String $ Text.pack $ displayException e

-- Can't derive an instance for this type because it uses constraints for some constructors
instance ToJSON (SubscriptionTrace Socket.SockAddr) where
  toJSON ev =
    object $
      ["tag" .= String tag]
        ++ address
        ++ extra
    where
      tag = case ev of
        SubscriptionTraceConnectStart _ -> "SubscriptionTraceConnectStart"
        SubscriptionTraceConnectEnd _ _ -> "SubscriptionTraceConnectEnd"
        SubscriptionTraceSocketAllocationException _ _ -> "SubscriptionTraceSocketAllocationException"
        SubscriptionTraceConnectException _ _ -> "SubscriptionTraceConnectException"
        SubscriptionTraceApplicationException _ _ -> "SubscriptionTraceApplicationException"
        SubscriptionTraceTryConnectToPeer _ -> "SubscriptionTraceTryConnectToPeer"
        SubscriptionTraceSkippingPeer _ -> "SubscriptionTraceSkippingPeer"
        SubscriptionTraceSubscriptionRunning -> "SubscriptionTraceSubscriptionRunning"
        SubscriptionTraceSubscriptionWaiting _ -> "SubscriptionTraceSubscriptionWaiting"
        SubscriptionTraceSubscriptionFailed -> "SubscriptionTraceSubscriptionFailed"
        SubscriptionTraceSubscriptionWaitingNewConnection _ -> "SubscriptionTraceSubscriptionWaitingNewConnection"
        SubscriptionTraceStart _ -> "SubscriptionTraceStart"
        SubscriptionTraceRestart {} -> "SubscriptionTraceRestart"
        SubscriptionTraceConnectionExist _ -> "SubscriptionTraceConnectionExist"
        SubscriptionTraceUnsupportedRemoteAddr _ -> "SubscriptionTraceUnsupportedRemoteAddr"
        SubscriptionTraceMissingLocalAddress -> "SubscriptionTraceMissingLocalAddress"
        SubscriptionTraceAllocateSocket _ -> "SubscriptionTraceAllocateSocket"
        SubscriptionTraceCloseSocket _ -> "SubscriptionTraceCloseSocket"
      address = case ev of
        SubscriptionTraceConnectStart adr -> ["addr" .= toJSON adr]
        SubscriptionTraceConnectEnd adr _ -> ["addr" .= toJSON adr]
        SubscriptionTraceSocketAllocationException adr _ -> ["addr" .= toJSON adr]
        SubscriptionTraceConnectException adr _ -> ["addr" .= toJSON adr]
        SubscriptionTraceApplicationException adr _ -> ["addr" .= toJSON adr]
        SubscriptionTraceTryConnectToPeer adr -> ["addr" .= toJSON adr]
        SubscriptionTraceSkippingPeer adr -> ["addr" .= toJSON adr]
        SubscriptionTraceSubscriptionRunning -> []
        SubscriptionTraceSubscriptionWaiting _ -> []
        SubscriptionTraceSubscriptionFailed -> []
        SubscriptionTraceSubscriptionWaitingNewConnection _ -> []
        SubscriptionTraceStart _ -> []
        SubscriptionTraceRestart {} -> []
        SubscriptionTraceConnectionExist adr -> ["addr" .= toJSON adr]
        SubscriptionTraceUnsupportedRemoteAddr adr -> ["addr" .= toJSON adr]
        SubscriptionTraceMissingLocalAddress -> []
        SubscriptionTraceAllocateSocket adr -> ["addr" .= toJSON adr]
        SubscriptionTraceCloseSocket adr -> ["addr" .= toJSON adr]
      extra = case ev of
        SubscriptionTraceConnectStart _ -> []
        SubscriptionTraceConnectEnd _ res ->
          ["connectionResult" .= toJSON res]
        SubscriptionTraceSocketAllocationException _ e ->
          ["exception" .= String (Text.pack $ show e)]
        SubscriptionTraceConnectException _ e ->
          ["exception" .= String (Text.pack $ show e)]
        SubscriptionTraceApplicationException _ e ->
          ["exception" .= String (Text.pack $ show e)]
        SubscriptionTraceTryConnectToPeer _ -> []
        SubscriptionTraceSkippingPeer _ -> []
        SubscriptionTraceSubscriptionRunning -> []
        SubscriptionTraceSubscriptionWaiting count -> ["numberWaitingOn" .= toJSON count]
        SubscriptionTraceSubscriptionFailed -> []
        SubscriptionTraceSubscriptionWaitingNewConnection delay ->
          ["waitingBeforeAttemptingNewConnection" .= toJSON delay]
        SubscriptionTraceStart val -> ["valency" .= toJSON val]
        SubscriptionTraceRestart delay valDes valCur ->
          [ "restartAfter" .= toJSON delay,
            "currentValency" .= toJSON valCur,
            "desiredValency" .= toJSON valDes
          ]
        SubscriptionTraceConnectionExist _ -> []
        SubscriptionTraceUnsupportedRemoteAddr _ -> []
        SubscriptionTraceMissingLocalAddress -> []
        SubscriptionTraceAllocateSocket _ -> []
        SubscriptionTraceCloseSocket _ -> []

instance ToJSON MuxTrace where
  toJSON ev =
    object
      [ "tag" .= String tag,
        "contents" .= contents
      ]
    where
      tag = case ev of
        MuxTraceRecvHeaderStart -> "MuxTraceRecvHeaderStart"
        MuxTraceRecvHeaderEnd _ -> "MuxTraceRecvHeaderEnd"
        MuxTraceRecvDeltaQObservation _ _ -> "MuxTraceRecvDeltaQObservation"
        MuxTraceRecvDeltaQSample {} -> "MuxTraceRecvDeltaQSample"
        MuxTraceRecvStart _ -> "MuxTraceRecvStart"
        MuxTraceRecvEnd _ -> "MuxTraceRecvEnd"
        MuxTraceSendStart _ -> "MuxTraceSendStart"
        MuxTraceSendEnd -> "MuxTraceSendEnd"
        MuxTraceState _ -> "MuxTraceState"
        MuxTraceCleanExit _ _ -> "MuxTraceCleanExit"
        MuxTraceExceptionExit {} -> "MuxTraceExceptionExit"
        MuxTraceChannelRecvStart _ -> "MuxTraceChannelRecvStart"
        MuxTraceChannelRecvEnd _ _ -> "MuxTraceChannelRecvEnd"
        MuxTraceChannelSendStart _ _ -> "MuxTraceChannelSendStart"
        MuxTraceChannelSendEnd _ -> "MuxTraceChannelSendEnd"
        MuxTraceHandshakeStart -> "MuxTraceHandshakeStart"
        MuxTraceHandshakeClientEnd _ -> "MuxTraceHandshakeClientEnd"
        MuxTraceHandshakeServerEnd -> "MuxTraceHandshakeServerEnd"
        MuxTraceHandshakeClientError _ _ -> "MuxTraceHandshakeClientError"
        MuxTraceHandshakeServerError _ -> "MuxTraceHandshakeServerError"
        MuxTraceSDUReadTimeoutException -> "MuxTraceSDUReadTimeoutException"
        MuxTraceSDUWriteTimeoutException -> "MuxTraceSDUWriteTimeoutException"
        MuxTraceStartEagerly _ _ -> "MuxTraceStartEagerly"
        MuxTraceStartOnDemand _ _ -> "MuxTraceStartOnDemand"
        MuxTraceStartedOnDemand _ _ -> "MuxTraceStartedOnDemand"
        MuxTraceTerminating _ _ -> "MuxTraceTerminating"
        MuxTraceShutdown -> "MuxTraceShutdown"

      contents = case ev of
        MuxTraceRecvHeaderStart -> []
        MuxTraceRecvHeaderEnd v1 -> [toJSON v1]
        MuxTraceRecvDeltaQObservation v1 v2 -> [toJSON v1, toJSON v2]
        MuxTraceRecvDeltaQSample v1 v2 v3 v4 v5 v6 v7 v8 -> [toJSON v1, toJSON v2, toJSON v3, toJSON v4, toJSON v5, toJSON v6, toJSON v7, toJSON v8]
        MuxTraceRecvStart v1 -> [toJSON v1]
        MuxTraceRecvEnd v1 -> [toJSON v1]
        MuxTraceSendStart v1 -> [toJSON v1]
        MuxTraceSendEnd -> []
        MuxTraceState v1 -> [toJSON v1]
        MuxTraceCleanExit v1 v2 -> [toJSON v1, toJSON v2]
        MuxTraceExceptionExit v1 v2 v3 -> [toJSON v1, toJSON v2, toJSON v3]
        MuxTraceChannelRecvStart v1 -> [toJSON v1]
        MuxTraceChannelRecvEnd v1 v2 -> [toJSON v1, toJSON v2]
        MuxTraceChannelSendStart v1 v2 -> [toJSON v1, toJSON v2]
        MuxTraceChannelSendEnd v1 -> [toJSON v1]
        MuxTraceHandshakeStart -> []
        MuxTraceHandshakeClientEnd v1 -> [toJSON v1]
        MuxTraceHandshakeServerEnd -> []
        MuxTraceHandshakeClientError v1 v2 -> [toJSON (SomeException v1), toJSON v2]
        MuxTraceHandshakeServerError v1 -> [toJSON (SomeException v1)]
        MuxTraceSDUReadTimeoutException -> []
        MuxTraceSDUWriteTimeoutException -> []
        MuxTraceStartEagerly v1 v2 -> [toJSON v1, toJSON v2]
        MuxTraceStartOnDemand v1 v2 -> [toJSON v1, toJSON v2]
        MuxTraceStartedOnDemand v1 v2 -> [toJSON v1, toJSON v2]
        MuxTraceTerminating v1 v2 -> [toJSON v1, toJSON v2]
        MuxTraceShutdown -> []

instance ToJSON ErrorPolicyTrace where
  toJSON ev =
    object
      [ "tag" .= String tag,
        "contents" .= contents
      ]
    where
      tag = case ev of
        ErrorPolicySuspendPeer {} -> "ErrorPolicySuspendPeer"
        ErrorPolicySuspendConsumer _ _ -> "ErrorPolicySuspendConsumer"
        ErrorPolicyLocalNodeError _ -> "ErrorPolicyLocalNodeError"
        ErrorPolicyResumePeer -> "ErrorPolicyResumePeer"
        ErrorPolicyKeepSuspended -> "ErrorPolicyKeepSuspended"
        ErrorPolicyResumeConsumer -> "ErrorPolicyResumeConsumer"
        ErrorPolicyResumeProducer -> "ErrorPolicyResumeProducer"
        ErrorPolicyUnhandledApplicationException _ -> "ErrorPolicyUnhandledApplicationException"
        ErrorPolicyUnhandledConnectionException _ -> "ErrorPolicyUnhandledConnectionException"
        ErrorPolicyAcceptException _ -> "ErrorPolicyAcceptException"
      contents = case ev of
        ErrorPolicySuspendPeer _ v2 v3 -> [toJSON v2, toJSON v3]
        ErrorPolicySuspendConsumer _ v2 -> [toJSON v2]
        ErrorPolicyLocalNodeError _ -> []
        ErrorPolicyResumePeer -> []
        ErrorPolicyKeepSuspended -> []
        ErrorPolicyResumeConsumer -> []
        ErrorPolicyResumeProducer -> []
        ErrorPolicyUnhandledApplicationException e -> [toJSON e]
        ErrorPolicyUnhandledConnectionException e -> [toJSON e]
        ErrorPolicyAcceptException e -> [toJSON e]

instance ToJSON ChainSyncClientResult where
  toJSON (ForkTooDeep int (Our our) (Their their)) =
    object
      [ "tag" .= String "ForkTooDeep",
        "intersection" .= int,
        "ourTip" .= getTipPoint our,
        "theirTip" .= getTipPoint their
      ]
  toJSON (NoMoreIntersection (Our our) (Their their)) =
    object
      [ "tag" .= String "NoMoreIntersection",
        "ourTip" .= getTipPoint our,
        "theirTip" .= getTipPoint their
      ]
  toJSON (RolledBackPastIntersection rollbackPoint (Our our) (Their their)) =
    object
      [ "tag" .= String "RolledBackPastIntersection",
        "rollbackPoint" .= rollbackPoint,
        "ourTip" .= getTipPoint our,
        "theirTip" .= getTipPoint their
      ]
  toJSON AskedToTerminate =
    object
      [ "tag" .= String "AskedToTerminate"
      ]

instance (BlockSupportsProtocol blk, ValidateEnvelope blk) => ToJSON (HeaderError blk) where
  toJSON (HeaderProtocolError validationErr) =
    object
      [ "tag" .= String "HeaderProtocolError",
        "err" .= String (Text.pack $ show validationErr)
      ]
  toJSON (HeaderEnvelopeError envelopeErr) =
    object
      [ "tag" .= String "HeaderEnvelopeError",
        "err" .= String (Text.pack $ show envelopeErr)
      ]

instance ToJSON ChainSyncClientException where
  toJSON (HeaderError pt er (Our our) (Their their)) =
    object
      [ "tag" .= String "HeaderError",
        "invalidHeader" .= pt,
        "error" .= toJSON er,
        "ourTip" .= getTipPoint our,
        "theirTip" .= getTipPoint their
      ]
  toJSON (InvalidIntersection int (Our our) (Their their)) =
    object
      [ "tag" .= String "InvalidIntersection",
        "intersection" .= int,
        "ourTip" .= getTipPoint our,
        "theirTip" .= getTipPoint their
      ]
  toJSON (DoesntFit received expected (Our our) (Their their)) =
    object
      [ "tag" .= String "DoesntFit",
        "received" .= String (Text.pack $ show received),
        "expected" .= String (Text.pack $ show expected),
        "ourTip" .= getTipPoint our,
        "theirTip" .= getTipPoint their
      ]
  toJSON (InvalidBlock pt reason) =
    object
      [ "tag" .= String "InvalidBlock",
        "received" .= pt,
        "reason" .= String (Text.pack $ show reason)
      ]
