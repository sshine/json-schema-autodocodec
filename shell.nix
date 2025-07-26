{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    just
    ghc
    cabal-install
    haskellPackages.autodocodec
    haskellPackages.autodocodec-schema
    haskellPackages.aeson
    haskellPackages.aeson-pretty
  ];
}
