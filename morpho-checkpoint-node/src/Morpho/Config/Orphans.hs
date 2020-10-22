{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Config.Orphans where

import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Cardano.Prelude
import Data.Aeson
import Data.Maybe (maybe)
import Data.Scientific (isInteger, toBoundedInteger)
import qualified Data.Text as T
import Ouroboros.Consensus.Block (Header (..))
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Network.Block (HeaderHash)
import Prelude (show)

deriving instance Show TracingVerbosity

instance FromJSON SlotLength where
  parseJSON (Number i)
    | isInteger i =
      maybe
        (panic $ "Paring of slot duration failed: integer overflow.")
        pure
        (slotLengthFromSec . toInteger <$> (toBoundedInteger i :: Maybe Int))
    | otherwise =
      panic $
        "Parsing of slot duration failed: "
          <> "the specified duration is not an integer."
  parseJSON invalid =
    panic $
      "Parsing of slot duration failed due to type mismatch. "
        <> "Encountered: "
        <> (T.pack $ Prelude.show invalid)

-- | Tracing-related constraints for monitoring purposes.
--
-- When you need a 'Show' or 'Condense' instance for more types, just add the
-- appropriate constraint here. There's no need to modify the consensus
-- code-base, unless the corresponding instance is missing.
type TraceConstraints blk =
  ( Condense blk,
    Condense [blk],
    Condense (Header blk),
    Condense (HeaderHash blk),
    Show blk,
    Show (Header blk)
  )
