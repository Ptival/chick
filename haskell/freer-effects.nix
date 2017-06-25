{ mkDerivation, base, criterion, extensible-effects, fetchgit, free
, mtl, QuickCheck, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "freer-effects";
  version = "0.3.0.0";
  src = fetchgit {
    url = "https://github.com/IxpertaSolutions/freer-effects.git";
    sha256 = "1iqs45fqp0lbqr7iiy0sjhhys9wk59jbabnwdkxz5lkwq8z7pa9d";
    rev = "39a7e62cc049d43d36ece4ac24ba19e545f0b726";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    base criterion extensible-effects free mtl
  ];
  jailbreak = true;
  homepage = "https://github.com/IxpertaSolutions/freer-effects#readme";
  description = "Implementation of effect system for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
