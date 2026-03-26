{
  description = "A small utilities for SAT solver benchmark";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  outputs =
    { self, nixpkgs }:
    {
      packages = builtins.listToAttrs (
        map
          (
            system: with import nixpkgs { system = "${system}"; }; {
              name = system;
              value = {
                default = rustPlatform.buildRustPackage rec {
                  name = "sat-bench-${version}";
                  pname = "sat-bench";
                  version = "0.17.1-20260326-1";
                  src = fetchFromGitHub {
                    name = "SAT-bench";
                    owner = "shnarazk";
                    repo = "SAT-bench";
                    rev = "4f6f70f510be36ae5efcd65c12d75ce40e4cf184";
                    hash = "sha256-pviywT4R8sGb6SPRUvzG5iaxkhdrWb2LUJGjODpZbnQ=";
                  };
                  cargoHash = "sha256-2qq1BhG33VI1NdxAS+5pF/T8BZ5HqlNQQ7WZ5jYlcIQ=";
                  buildInputs = rustc.buildInputs ++ [
                    cargo
                    rustc
                    libiconv
                    openssl
                    pkg-config
                  ];
                  buildPhase = "cargo build --release";
                  installPhase = ''
                    mkdir -p $out/bin;
                    install -t $out/bin target/release/sat-bench target/release/benchm
                    mkdir -p $out/lib
                    cp -r 3-SAT SAT09 SatRace2015 SC21 $out/lib/
                  '';
                  patchPhase = ''
                    sed -i "s|long = \"lib\", default_value = \"\"|long = \"lib\", default_value = \"$out/lib\"|" src/bin/sat-bench.rs
                  '';
                  SATBENCHLIB = "$out/lib";
                };
              };
            }
          )
          [
            "x86_64-linux"
            "aarch64-linux"
            "x86_64-darwin"
            "aarch64-darwin"
          ]
      );
    };
}
