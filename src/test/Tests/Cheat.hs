module Tests.Cheat (cheatTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract', solcV, solved, passed)
import Echidna.Types.Worker (WorkerType(..))

cheatTests :: TestTree
cheatTests =
  testGroup "Cheatcodes Tests"
    [ testContract' "cheat/ffi.sol" (Just "TestFFI") (Just (> solcV (0,5,0))) (Just "cheat/ffi.yaml") False FuzzWorker
        [ ("echidna_ffi passed", solved "echidna_ffi") ]
    , testContract' "cheat/ffi2.sol" (Just "TestFFI") (Just (> solcV (0,5,0))) (Just "cheat/ffi.yaml") False FuzzWorker
        [ ("echidna_ffi passed", solved "echidna_ffi") ]
    , testContract' "cheat/gas.sol" (Just "TestCheatGas") (Just (> solcV (0,5,0))) (Just "cheat/ffi.yaml") False FuzzWorker
        [ ("echidna_gas_zero passed", solved "echidna_gas_zero") ]
    , testContract' "cheat/prank.sol" (Just "TestPrank") (Just (> solcV (0,6,0))) (Just "cheat/prank.yaml") False FuzzWorker
        [ ("withPrank failed",               passed "withPrank")
        , ("withStartPrank failed",          passed "withStartPrank")
        , ("withStartPrankStopPrank failed", passed "withStartPrankStopPrank")
        , ("withNothing failed",             passed "withNothing")
        , ("withDoubleDeploy failed",        passed "withDoubleDeploy")
        ]
    , testContract' "cheat/getCode.sol" (Just "TestGetCode") (Just (> solcV (0,5,0))) (Just "cheat/getCode.yaml") False FuzzWorker
        [ ("echidna_getCode_success_paths failed", passed "echidna_getCode_success_paths")
        , ("echidna_getCode_rejects_bad_inputs failed", passed "echidna_getCode_rejects_bad_inputs")
        ]
    , testContract' "cheat/getCode.sol" (Just "TestGetCodeNoFFI") (Just (> solcV (0,5,0))) (Just "cheat/getCode_noffi.yaml") False FuzzWorker
        [ ("echidna_getCode_reverts_without_allowffi failed", passed "echidna_getCode_reverts_without_allowffi")
        ]
    ]
