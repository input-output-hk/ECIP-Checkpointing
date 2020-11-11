{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Morpho.Golden
  ( goldenTests,
  )
where

import Codec.CBOR.FlatTerm (TermToken (..))
import Codec.Serialise (Serialise (..))
import Morpho.Config.Topology
import Morpho.Config.Types
import Morpho.Ledger.Serialise ()
import Test.Morpho.Examples
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.Golden
import Prelude

goldenTests :: TestTree
goldenTests =
  testGroup
    "Golden tests"
    [ testCase "LedgerState" test_golden_LedgerState,
      testCase "GenTx" test_golden_GenTx,
      testCase "Block" test_golden_Block,
      testCase "ApplyTxErr" test_golden_ApplyTxErr,
      testCase "NodeConfig" test_golden_parseNodeConfiguration,
      testCase "Topology" test_golden_parseTopology
    ]

test_golden_LedgerState :: Assertion
test_golden_LedgerState =
  goldenTestCBOR
    encode
    exampleLedgerState
    [ TkListLen 5,
      TkInt 0,
      TkListLen 3,
      TkInt 0,
      TkListLen 3,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkInt 10,
      TkListLen 2,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListBegin,
      TkListLen 4,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListLen 2,
      TkInt 0,
      TkBytes "\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US ",
      TkInt 5,
      TkBreak,
      TkListLen 2,
      TkInt 4,
      TkListLen 2,
      TkInt 0,
      TkBytes "\173\231\160\220",
      TkMapLen 1,
      TkListLen 2,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "j\EOT\171\152\217\228wJ\216\ACK\227\STX\221\222\182;\234\SYN\181\203_\">\231tx\232a\187X>\179\&6\182\251\203`\181\179\212\241U\SUB\196^_\252I6Fn}\152\246\199\192\236se9\247F\145\166",
      TkListLen 3,
      TkInt 0,
      TkListLen 3,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkInt 10,
      TkListLen 2,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListLen 4,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListLen 2,
      TkInt 0,
      TkBytes "\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US ",
      TkInt 5,
      TkListLen 2,
      TkInt 4,
      TkListLen 2,
      TkInt 0,
      TkBytes "\173\231\160\220"
    ]

test_golden_GenTx :: Assertion
test_golden_GenTx =
  goldenTestCBOR
    encode
    exampleGenTx
    [ TkListLen 3,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkListLen 3,
      TkInt 0,
      TkListLen 3,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkInt 10,
      TkListLen 2,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListLen 4,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListLen 2,
      TkInt 0,
      TkBytes "\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US ",
      TkInt 5,
      TkListLen 2,
      TkInt 0,
      TkBytes "0\SUBm\189\142\131\172e&\146\198$ \172+\168\t\206O\171\250\226\234\151l\NULt\NAK\134W)p"
    ]

test_golden_Block :: Assertion
test_golden_Block =
  goldenTestCBOR
    encode
    exampleBlock
    [ TkListLen 3,
      TkInt 0,
      TkListLen 2,
      TkListLen 6,
      TkInt 0,
      TkListLen 0,
      TkInt 5,
      TkInt 3,
      TkListLen 2,
      TkInt 0,
      TkBytes "U\165@\b",
      TkInt 100,
      TkBytes "h\179)\218\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC4",
      TkListLen 2,
      TkInt 0,
      TkListBegin,
      TkListLen 3,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkListLen 3,
      TkInt 0,
      TkListLen 3,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkInt 10,
      TkListLen 2,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListLen 4,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListLen 2,
      TkInt 0,
      TkBytes "\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US ",
      TkInt 5,
      TkListLen 2,
      TkInt 0,
      TkBytes "0\SUBm\189\142\131\172e&\146\198$ \172+\168\t\206O\171\250\226\234\151l\NULt\NAK\134W)p",
      TkBreak
    ]

test_golden_ApplyTxErr :: Assertion
test_golden_ApplyTxErr =
  goldenTestCBOR
    encode
    exampleApplyTxErr
    [ TkListLen 2,
      TkInt 1,
      TkListLen 3,
      TkInt 0,
      TkListLen 3,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkInt 10,
      TkListLen 2,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListLen 4,
      TkInt 0,
      TkListLen 2,
      TkInt 0,
      TkBytes "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI",
      TkListLen 2,
      TkInt 0,
      TkBytes "\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US ",
      TkInt 5
    ]

test_golden_parseNodeConfiguration :: Assertion
test_golden_parseNodeConfiguration = do
  cfg <- parseNodeConfiguration "tests/configuration/Golden/Config.yaml"
  assertEqual "NodeConfiguration" cfg exampleNodeConfig

test_golden_parseTopology :: Assertion
test_golden_parseTopology = do
  topology <- readTopologyFile "tests/configuration/Golden/Topology.json"
  assertEqual "Topology" topology (Right exampleTopology)
