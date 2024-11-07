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
                  version = "0.16.1-20240926-1";
                  src = fetchFromGitHub {
                    name = "SAT-bench";
                    owner = "shnarazk";
                    repo = "SAT-bench";
                    rev = "0388ee0b5ec5ddca46eca36c063457d342adad6b";
                    hash = "sha256-ldmHS9LZ+iRy06QV1JtcUb50SusjdyXTpbS7Aj21Jto=";
                  };
                  cargoHash = "sha256-05Pv4mRtd7bBgxPjr0pz3wmxnBGh8mpAmqFScaQyS9A=";
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
