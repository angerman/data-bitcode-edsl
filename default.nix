{ mkDerivation, base, base16-bytestring, binary, bytestring
, data-bitcode, data-bitcode-llvm, pretty, stdenv, transformers
}:
mkDerivation {
  pname = "data-bitcode-edsl";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base16-bytestring binary bytestring data-bitcode
    data-bitcode-llvm pretty transformers
  ];
  homepage = "https://github.com/angerman/bitcode-playground#readme";
  description = "bitcode experimentation";
  license = stdenv.lib.licenses.bsd3;
}
