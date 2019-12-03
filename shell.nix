{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [pkgs.ghcid
                (pkgs.haskellPackages.ghcWithPackages (p: [
                   p.split
                   p.vector
                   p.containers
                   ]))];
}
