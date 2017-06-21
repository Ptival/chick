{ mkDerivation, base, criterion, fetchgit, free, mtl, QuickCheck
, stdenv, tasty, tasty-hunit, tasty-quickcheck, template-haskell
}:
mkDerivation {
  pname = "freer";
  version = "0.2.4.1";
  src = fetchgit {
    url = "https://github.com/TaktInc/freer.git";
    sha256 = "0miyjbfmcpnzlv1ck1ch4qdkf3jxpfb7g15jb3h1lzi6jrx3mv36";
    rev = "5e4b2db086e48814d59c5d630449b54e0239d850";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base template-haskell ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base criterion free mtl ];
  jailbreak = true;
  homepage = "https://github.com/TaktInc/freer";
  description = "Implementation of the Freer Monad";
  license = stdenv.lib.licenses.bsd3;
}
