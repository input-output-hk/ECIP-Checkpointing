{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Blockchain.Config.Protocol (
  TraceConstraints,
  SomeProtocol(..),
  extractNodeInfo,
  fromProtocol,
  mockSecurityParam
) where

import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import Cardano.Prelude
import Codec.CBOR.Read
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTx, GenTxId)
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Node.Run (RunNode)
import Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import Ouroboros.Consensus.Util
import Control.Monad.Trans.Except.Extra
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Network.Block
import qualified Cardano.Chain.Genesis as Genesis
import qualified Ouroboros.Consensus.Protocol as Consensus

import Blockchain.Config.Types

-- | Tracing-related constraints for monitoring purposes.
--
-- When you need a 'Show' or 'Condense' instance for more types, just add the
-- appropriate constraint here. There's no need to modify the consensus
-- code-base, unless the corresponding instance is missing.
type TraceConstraints blk =
    ( Condense blk
    , Condense [blk]
    , Condense (Header blk)
    , Condense (HeaderHash blk)
    , Condense (GenTx blk)
    , Show (ApplyTxErr blk)
    , Show (GenTx blk)
    , Show (GenTxId blk)
    , Show blk
    , Show (Header blk)
    )

data SomeProtocol where
  SomeProtocol :: (RunNode blk, TraceConstraints blk)
               => Consensus.Protocol blk -> SomeProtocol

extractNodeInfo
  :: Maybe NodeId
  -> Maybe Int
  -> Either ProtocolInstantiationError (CoreNodeId, NumCoreNodes)
extractNodeInfo mNodeId mnumCoreNodes  = do
    coreNodeId <- case mNodeId of
                      Just (CoreId coreNodeId) -> pure coreNodeId
                      _ -> Left MissingCoreNodeId
    numCoreNodes <- maybe (Left MissingNumCoreNodes) Right mnumCoreNodes
    return (CoreNodeId coreNodeId , NumCoreNodes numCoreNodes)

-- | Helper for creating a 'SomeProtocol' for a mock protocol that needs the
-- 'CoreNodeId' and NumCoreNodes'. If one of them is missing from the
-- 'CardanoConfiguration', a 'MissingNodeInfo' exception is thrown.
mockSomeProtocol
  :: (RunNode blk, TraceConstraints blk)
  => Maybe NodeId
  -> Maybe Int
  -- ^ Number of core nodes
  -> (CoreNodeId -> NumCoreNodes -> Consensus.Protocol blk)
  -> Either ProtocolInstantiationError SomeProtocol
mockSomeProtocol nId mNumCoreNodes mkConsensusProtocol =  do
    (cid, numCoreNodes) <- extractNodeInfo nId mNumCoreNodes
    let p = mkConsensusProtocol cid numCoreNodes
    case Consensus.runProtocol p of
      Dict -> return $ SomeProtocol p

data ProtocolInstantiationError =
    ByronLegacyProtocolNotImplemented
  | CanonicalDecodeFailure Text
  | DelegationCertificateFilepathNotSpecified
  | LedgerConfigError Genesis.ConfigurationError
  | MissingCoreNodeId
  | MissingNumCoreNodes
  | SigningKeyDeserialiseFailure DeserialiseFailure
  | SigningKeyFilepathNotSpecified
  deriving Show

fromProtocol
  :: Text
  -> Maybe NodeId
  -> Maybe Int
  -- ^ Number of core nodes
  -> GenesisFile
  -> RequiresNetworkMagic
  -> Maybe Double
  -> Maybe DelegationCertFile
  -> Maybe SigningKeyFile
  -> Update
  -> Protocol
  -> SlotLength
  -> ExceptT ProtocolInstantiationError IO SomeProtocol
fromProtocol _ nId mNumCoreNodes _ _ _ _ _ _ MockedBFT slotLength =
  hoistEither $ mockSomeProtocol nId mNumCoreNodes $ \cid numCoreNodes ->
    Consensus.ProtocolMockBFT numCoreNodes
                              cid
                              mockSecurityParam
                              (singletonSlotLengths slotLength)
  -- TODO add TimeSlot here

mockSecurityParam :: Consensus.SecurityParam
mockSecurityParam = Consensus.SecurityParam 5
