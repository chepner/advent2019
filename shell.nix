# For my own benefit...
# Rather than a function with an argument
# that defaults to import <nixpkgs> {}, this could have been
# written as an expreesion like
#
#   let pkgs = import <nixpakgs> {};
#   in pkgs.mkShell {
#        buildInputs = [pkgs.ghcid
#                      (pkgs.haskellPackages.ghcWithPackages (p: [
#                          p.split
#                          p.vector
#                          p.containers
#                      ]))];
#
#
# or simply
#
#  with import <nixpkgs> {};
#
#  mkShell {
#    buildInputs = [
#      ghcid
#      (haskellPackages.ghcWithPackages (p: [...]))
#    ]
#
# In either case, the packages involved would have to come from
# <nixpkgs>, with no option to override which set of packages to use.

{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [pkgs.ghcid
                (pkgs.haskellPackages.ghcWithPackages (p: [
                   p.split
                   p.vector
                   p.containers
                   ]))];
}
