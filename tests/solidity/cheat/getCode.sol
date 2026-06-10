pragma experimental ABIEncoderV2;

interface Hevm {
  function getCode(string calldata) external returns (bytes memory);
}

contract TestGetCode {
  address constant HEVM_ADDRESS = 0x7109709ECfa91a80626fF3989D68f67F5b1DD12D;

  function _callGetCode(string memory artifactRef) internal returns (bool ok, bytes memory ret) {
    return HEVM_ADDRESS.call(abi.encodeWithSignature("getCode(string)", artifactRef));
  }

  function _equalsExpected(bytes memory encoded, bytes memory expected) internal pure returns (bool) {
    bytes memory got = abi.decode(encoded, (bytes));
    return keccak256(got) == keccak256(expected);
  }

  function echidna_getCode_success_paths() public returns (bool) {
    bool ok;
    bytes memory ret;

    (ok, ret) = _callGetCode("tests/solidity/cheat/getcode_artifacts/GetCodeWidget.0.8.18.json");
    if (!ok || !_equalsExpected(ret, hex"60016000556002600055")) return false;

    (ok, ret) = _callGetCode("GetCodeWidget.sol:GetCodeWidget");
    if (!ok || !_equalsExpected(ret, hex"60016000556002600055")) return false;

    (ok, ret) = _callGetCode("GetCodeWidget.sol");
    if (!ok || !_equalsExpected(ret, hex"60016000556002600055")) return false;

    (ok, ret) = _callGetCode("GetCodeWidget");
    if (!ok || !_equalsExpected(ret, hex"60016000556002600055")) return false;

    (ok, ret) = _callGetCode("GetCodeVersioned.sol:0.8.18");
    if (!ok || !_equalsExpected(ret, hex"60086000556009600055")) return false;

    (ok, ret) = _callGetCode("GetCodeVersioned:0.8.19");
    if (!ok || !_equalsExpected(ret, hex"60186000556019600055")) return false;

    return true;
  }

  function echidna_getCode_rejects_bad_inputs() public returns (bool) {
    bool ok;

    (ok, ) = _callGetCode("GetCodeVersioned");
    if (ok) return false;

    (ok, ) = _callGetCode("GetCodeVersioned:0.8.99");
    if (ok) return false;

    (ok, ) = _callGetCode("bad:selector:shape");
    if (ok) return false;

    (ok, ) = _callGetCode("tests/solidity/cheat/getcode_artifacts/InvalidJson.json");
    if (ok) return false;

    (ok, ) = _callGetCode("tests/solidity/cheat/getcode_artifacts/MissingBytecode.json");
    if (ok) return false;

    (ok, ) = _callGetCode("tests/solidity/cheat/getcode_artifacts/InvalidHex.json");
    if (ok) return false;

    (ok, ) = _callGetCode("tests/solidity/cheat/getcode_artifacts/Unlinked.json");
    if (ok) return false;

    (ok, ) = _callGetCode("../solidity_project/fixtures/json/sample.json");
    if (ok) return false;

    return true;
  }
}

contract TestGetCodeNoFFI {
  address constant HEVM_ADDRESS = 0x7109709ECfa91a80626fF3989D68f67F5b1DD12D;

  function echidna_getCode_reverts_without_allowffi() public returns (bool) {
    (bool ok, ) = HEVM_ADDRESS.call(
      abi.encodeWithSignature("getCode(string)", "tests/solidity/cheat/getcode_artifacts/GetCodeWidget.0.8.18.json")
    );
    return ok == false;
  }
}
