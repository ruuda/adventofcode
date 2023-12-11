{
  description = "Advent of Code 2023 devenv";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.rcl.url = "github:ruuda/rcl";
  inputs.rcl.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, rcl }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      # Extend Tokei with support for counting RCL.
      tokei = pkgs.tokei.overrideAttrs (oldAttrs: rec {
        src = pkgs.fetchFromGitHub {
          owner = "XAMPPRocky";
          repo = "tokei";
          rev = "e2625c02d85a6b8cd9d74c809fa3c51cd40ccbc1";
          hash = "sha256-reAsv2vzr7zJvNEky0OH9ze4Bw8trfhe0p2a2FyteVI=";
        };
        cargoDeps = oldAttrs.cargoDeps.overrideAttrs {
          inherit src;
          outputHash = "sha256-RqDpA2NS6uUSokPlRFlekfmc7A1JiWwbQqcvrLzZyIY=";
        };
        patches = [ ./tokei-rcl.patch ];
      });
    in
      {
        devShells.${system}.default = pkgs.mkShell {
          name = "aoc2023";
          nativeBuildInputs = [
            pkgs.clang
            pkgs.dmd
            pkgs.fsharp
            pkgs.ghc
            pkgs.go
            pkgs.hare
            pkgs.nim
            pkgs.ocaml-ng.ocamlPackages_5_1.ocaml
            pkgs.ponyc
            pkgs.python3
            pkgs.rustc
            pkgs.zig
            rcl.packages.${system}.default
            tokei
          ];
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        };
      };
}
