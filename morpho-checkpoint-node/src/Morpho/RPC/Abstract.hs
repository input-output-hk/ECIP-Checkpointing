{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Morpho.RPC.Abstract where

import Cardano.BM.Data.Tracer
import Cardano.Prelude
import Data.Aeson
import qualified Data.Text as Text
import Morpho.Ledger.PowTypes (Checkpoint, PowBlockRef)
import Morpho.Tracing.Pretty (MPretty (..))
import Prettyprinter (Pretty (pretty), viaShow, (<+>))
import Prelude (show)

-- | Abstract definition of RPC methods supported
-- @i@ is the input type, @o@ the output type
data RpcMethod i o where
  -- | Get the latest checkpoint candidate block
  GetLatestBlock :: RpcMethod (Int, Maybe PowBlockRef) (Maybe PowBlockRef)
  -- | Push a checkpoint to the Pow chain
  PushCheckpoint :: RpcMethod Checkpoint Bool

instance Show (RpcMethod i o) where
  show GetLatestBlock = "GetLatestBlock"
  show PushCheckpoint = "PushCheckpoint"

instance ToJSON (RpcMethod i o) where
  toJSON v = String (Text.pack $ Prelude.show v)

data RpcEvent ev o
  = RpcStart
  | RpcSuccess o
  | RpcEvent ev
  deriving (Show, Generic, ToJSON)

data RpcTrace ev i o
  = RpcTrace (RpcMethod i o) i (RpcEvent ev o)

instance ToJSON ev => ToJSON (RpcTrace ev i o) where
  toJSON (RpcTrace method@GetLatestBlock request event) =
    object
      [ "kind" .= String "RpcTrace",
        "method" .= method,
        "request" .= request,
        "event" .= event
      ]
  toJSON (RpcTrace method@PushCheckpoint request event) =
    object
      [ "kind" .= String "RpcTrace",
        "method" .= method,
        "request" .= request,
        "event" .= event
      ]

instance Show e => MPretty (RpcTrace e i o) where
  mpretty (RpcTrace GetLatestBlock (ckptInterval, powBlock) RpcStart) =
    "Starting RPC call GetLatestBlock(interval:"
      <+> pretty ckptInterval
      <> ", parent:"
      <+> mpretty powBlock
      <> ")"
  mpretty (RpcTrace GetLatestBlock (ckptInterval, powBlock) (RpcEvent ev)) =
    "Event for RPC call GetLatestBlock(interval:"
      <+> pretty ckptInterval
      <> ", parent:"
      <+> mpretty powBlock
      <> ")"
      <+> "returned:"
      <+> viaShow ev
  mpretty (RpcTrace GetLatestBlock (ckptInterval, powBlock) (RpcSuccess o)) =
    "Successful RPC call GetLatestBlock(interval:"
      <+> pretty ckptInterval
      <> ", parent:"
      <+> mpretty powBlock
      <> ")"
      <+> "returned:"
      <+> mpretty o
  mpretty (RpcTrace PushCheckpoint checkpoint RpcStart) =
    "Starting RPC call PushCheckpoint(" <+> mpretty checkpoint <> ")"
  mpretty (RpcTrace PushCheckpoint checkpoint (RpcEvent ev)) =
    "Event for RPC call PushCheckpoint("
      <+> mpretty checkpoint
      <> ")"
      <+> "returned:"
      <+> viaShow ev
  mpretty (RpcTrace PushCheckpoint checkpoint (RpcSuccess o)) =
    "Successful RPC call PushCheckpoint("
      <+> mpretty checkpoint
      <> ")"
      <+> "returned:"
      <+> mpretty o

-- | An upstream where the RPC calls can be issued to
-- An implementation of this is in Morpho.RPC.JsonRpc
newtype RpcUpstream ev m
  = RpcUpstream
      ( forall i o.
        Tracer m ev ->
        RpcMethod i o ->
        i ->
        (o -> m ()) ->
        m ()
      )

rpcCall ::
  Monad m =>
  Tracer m (RpcTrace ev i o) ->
  RpcUpstream ev m ->
  RpcMethod i o ->
  i ->
  (o -> m ()) ->
  m ()
rpcCall tracer (RpcUpstream rpcUpstream) method request cont = do
  traceWith tracer' RpcStart
  rpcUpstream (contramap RpcEvent tracer') method request $ \result -> do
    traceWith tracer' (RpcSuccess result)
    cont result
  where
    tracer' = contramap (RpcTrace method request) tracer
