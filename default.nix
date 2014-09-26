{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
with haskellPackages;
cabal.mkDerivation (self: {
  pname = "hike";
  version = "0.1.0";

  src = ./.;

  buildDepends = [ lens mtl free either exceptions text spawn dlist ];

  meta = {
    description = "A build system.";
    license = stdenv.lib.licenses.bsd3;
  };
})
