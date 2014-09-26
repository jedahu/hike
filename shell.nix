let pkgs = import <nixpkgs> {};
    hike = pkgs.callPackage ./. {haskellPackages = pkgs.haskellPackages;};
in
with pkgs;
with haskellPackages;
pkgs.lib.overrideDerivation hike (attrs: {
  src = ./.;
  buildInputs = [ zlib cabalInstall ghc ghcMod_5_0_1_1 hlint ] ++ attrs.buildInputs;
  shellHook = ''
  '';
})

