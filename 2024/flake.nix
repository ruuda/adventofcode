{
  description = "Advent of Code 2024 devenv";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.rcl.url = "github:ruuda/rcl?ref=master";
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
        patches = [ ../2023/tokei-rcl.patch ];
      });
    in
      {
        devShells.${system}.default = pkgs.mkShell {
          name = "aoc2024";
          nativeBuildInputs = [
            pkgs.clojure
            pkgs.dotnet-sdk
            pkgs.go
            pkgs.hare
            pkgs.nim
            pkgs.ocaml-ng.ocamlPackages_5_2.ocaml
            pkgs.ocaml-ng.ocamlPackages_5_2.ocamlformat
            pkgs.python3
            pkgs.zig
            rcl.packages.${system}.default
            tokei
          ];
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
          DOTNET_ROOT = "${pkgs.dotnet-sdk_8}";
        };
      };
}
