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
                  version = "0.17.2-20260403-1";
                  src = fetchFromGitHub {
                    name = "SAT-bench";
                    owner = "shnarazk";
                    repo = "SAT-bench";
                    rev = "b9e773ba1bc8adf6d01c8bf59f6c8237ee710c24";
                    hash = "sha256-VUqbCkXpEFtMP6lIUjOAsXH7Cy8GHtQdQTxwsU99t7I=";
                  };
                  cargoHash = "sha256-D9WVhT0GaW7Ix7lhRvsLoEYrUEZ6RtHwg7w6B2FTSKM=";
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
