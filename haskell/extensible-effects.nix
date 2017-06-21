{ mkDerivation, base, fetchgit, HUnit, QuickCheck, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, test-framework-th, transformers, transformers-base, type-aligned
, void
}:
mkDerivation {
  pname = "extensible-effects";
  version = "1.11.0.4";
  src = fetchgit {
    url = "https://github.com/suhailshergill/extensible-effects.git";
    sha256 = "0rlyhbwmp4c9v5rkmrpivqmmi4qiz5r8i6n91c16s8kj328sifcb";
    rev = "9977c412a8952cc445632d577d063fe93b8a9233";
  };
  libraryHaskellDepends = [
    base transformers transformers-base type-aligned void
  ];
  testHaskellDepends = [
    base HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th void
  ];
  jailbreak = true;
  homepage = "https://github.com/suhailshergill/extensible-effects";
  description = "An Alternative to Monad Transformers";
  license = stdenv.lib.licenses.mit;
}
