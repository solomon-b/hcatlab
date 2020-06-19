{ mkDerivation, base, ghc-prim, integer-gmp, stdenv }:
mkDerivation {
  pname = "hCatLab";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ghc-prim integer-gmp ];
  testHaskellDepends = [ base ghc-prim integer-gmp ];
  homepage = "https://github.com/ssbothwell/hcatlab#readme";
  license = stdenv.lib.licenses.bsd3;
}
