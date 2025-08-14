{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.shellFor {
    packages = hpkgs: [ (hpkgs.callCabal2nix "SOE.cabal" ./. {}) ];

    nativeBuildInputs = [
        pkgs.cabal-install
        pkgs.cabal2nix
        pkgs.haskell-language-server
    ];
}
