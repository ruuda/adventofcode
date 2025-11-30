{
  description = "Advent of Code 2025 devenv";

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
          rev = "5b597c070c2b0ffea86c4f89b8349dd8a979a839";
          hash = "sha256-GOMKLA8g5H/jxTokS/iyH9kxSg/s/+Z/YY+Vo6IZw2Y=";
        };
        cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
          inherit src;
          hash = "sha256-R9PI85ay7DoOTM4iwAv24X70W4vU0Y2gg+lQtaTLj/s=";
        };
        patches = [ ../2023/tokei-rcl.patch ../2024/tokei-hoon.patch ];
      });
    in
      {
        devShells.${system}.default = pkgs.mkShell {
          name = "aoc2025";
          nativeBuildInputs = [
            pkgs.nim
            pkgs.ponyc
            pkgs.zig
            rcl.packages.${system}.default
            tokei
          ];
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
        };
      };
}
