{
  description = "Advent of Code 2023 devenv";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.rcl.url = "github:ruuda/rcl";
  inputs.rcl.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, rcl }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
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
            pkgs.nim
            pkgs.ocaml
            pkgs.ponyc
            pkgs.python3
            pkgs.rustc
            pkgs.zig
            rcl.packages.${system}.default
          ];
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        };
      };
}
