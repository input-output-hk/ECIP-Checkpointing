{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Morpho.Config.Topology
  ( NetworkTopology (..),
    NodeSetup (..),
    NodeAddress (..),
    RemoteAddress (..),
    NodeHostAddress (..),
    remoteAddressToNodeAddress,
    readTopologyFile,
  )
where

import Cardano.Prelude
import qualified Control.Exception as Exception
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Morpho.Config.Types
import Network.Socket
import Ouroboros.Consensus.Util.Condense (Condense (..))
import Prelude (String)

-- | Domain name with port number
data RemoteAddress = RemoteAddress
  { -- | either a dns address or ip address
    raAddress :: !String,
    -- | port number of the destination
    raPort :: !(Maybe PortNumber),
    -- | if a dns address is given valency governs
    -- to how many resolved ip addresses
    -- should we maintain acctive (hot) connection;
    -- if an ip address is given valency is used as
    -- a boolean value, @0@ means to ignore the address;
    raValency :: !Int
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON NodeHostAddress where
  parseJSON (String ipStr) = case readMaybe $ T.unpack ipStr of
    Just ip -> pure . NodeHostAddress $ Just ip
    Nothing -> pure $ NodeHostAddress Nothing
  parseJSON invalid =
    panic $
      "Parsing of IP failed due to type mismatch. "
        <> "Encountered: "
        <> T.pack (show invalid)

instance Condense NodeAddress where
  condense (NodeAddress addr port) = show addr ++ ":" ++ show port

instance FromJSON NodeAddress where
  parseJSON = withObject "NodeAddress" $ \v -> do
    NodeAddress
      <$> (NodeHostAddress <$> readMaybe <$> v .: "addr")
      <*> ((fromIntegral :: Int -> PortNumber) <$> v .: "port")

data NodeSetup = NodeSetup
  { nodeId :: !Word64,
    producers :: ![RemoteAddress]
  }
  deriving (Show, Eq)

newtype NetworkTopology = NetworkTopology [NodeSetup]
  deriving (Eq, Show)

deriveFromJSON defaultOptions ''NodeSetup

deriveFromJSON defaultOptions ''NetworkTopology

-- | Parse 'raAddress' field as an IP address; if it parses and the valency is
-- non zero return corresponding NodeAddress.
remoteAddressToNodeAddress :: RemoteAddress -> Maybe NodeAddress
remoteAddressToNodeAddress (RemoteAddress _ Nothing _) = Nothing
remoteAddressToNodeAddress (RemoteAddress addrStr (Just port) val) =
  case readMaybe addrStr of
    Nothing -> Nothing
    Just addr ->
      if val /= 0
        then Just $ NodeAddress (NodeHostAddress $ Just addr) port
        else Nothing

instance Condense RemoteAddress where
  condense (RemoteAddress addr Nothing val) =
    addr ++ " (" ++ show val ++ ")"
  condense (RemoteAddress addr (Just port) val) =
    addr ++ show port ++ " (" ++ show val ++ ")"

instance FromJSON RemoteAddress where
  parseJSON = withObject "RemoteAddress" $ \v ->
    RemoteAddress
      <$> v .: "addr"
      <*> (fmap (fromIntegral :: Int -> PortNumber) <$> v .:? "port")
      <*> (v .: "valency")

readTopologyFile :: FilePath -> IO (Either String NetworkTopology)
readTopologyFile topo = do
  eBs <- Exception.try $ BS.readFile topo
  pure $ case eBs of
    Left (e :: IOException) ->
      Left $ "Failed to read topology file " <> topo <> ": " <> displayException e
    Right bs -> case eitherDecode $ LBS.fromStrict bs of
      Left e ->
        Left $ "Failed to decode topology file " <> topo <> ": " <> e
      Right topology -> Right topology
