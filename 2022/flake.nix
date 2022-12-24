{
  description = "Advent of Code 2022 devenv";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.unison.url = "github:ceedubs/unison-nix";

  outputs = { self, nixpkgs, unison }: 
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
      {
        devShells.${system}.default = pkgs.mkShell {
          name = "aoc2022";
          nativeBuildInputs = [
            pkgs.ghc
            pkgs.go
            pkgs.python3
            pkgs.racket
            pkgs.rustc
            pkgs.ponyc
            unison.packages.${system}.ucm
          ];
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        };
      };
}
