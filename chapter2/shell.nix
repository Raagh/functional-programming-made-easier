{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    packages = with pkgs; [
      nodejs
      nodePackages.npm
      purescript
      spago
    ];
}
