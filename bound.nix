{ mkDerivation, base, bifunctors, binary, bytes, cereal, comonad
, deriving-compat, directory, doctest, fetchgit, filepath
, functor-classes-compat, hashable, profunctors, stdenv
, template-haskell, transformers, transformers-compat, vector, void
}:
mkDerivation {
  pname = "bound";
  version = "2";
  src = fetchgit {
    url = "git://github.com/ekmett/bound.git";
    sha256 = "1r11p67j13z3xpvvx26v2dbjh323fz7bmplj672z667wiqhpd3ck";
    rev = "742d511091300f203a9f0e1b22ce02e30de55dc9";
  };
  libraryHaskellDepends = [
    base bifunctors binary bytes cereal comonad hashable profunctors
    template-haskell transformers transformers-compat
  ];
  testHaskellDepends = [
    base deriving-compat directory doctest filepath
    functor-classes-compat transformers transformers-compat vector void
  ];
  homepage = "http://github.com/ekmett/bound/";
  description = "Making de Bruijn Succ Less";
  license = stdenv.lib.licenses.bsd3;
}
