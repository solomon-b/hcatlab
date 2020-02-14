{ mkDerivation, base, ghc-prim, hpack, integer-gmp, stdenv }:
mkDerivation {
  pname = "myPrelude";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ghc-prim integer-gmp ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [ base ghc-prim integer-gmp ];
  prePatch = "hpack";
  homepage = "https://github.com/ssbothwell/myPrelude#readme";
  license = stdenv.lib.licenses.bsd3;
}
