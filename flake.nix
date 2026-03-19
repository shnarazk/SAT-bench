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
                  version = "0.17.0-20260320-1";
                  src = fetchFromGitHub {
                    name = "SAT-bench";
                    owner = "shnarazk";
                    repo = "SAT-bench";
                    rev = "4ae892687c6401e716367b0acb9fd818c3f586c5";
                    hash = "sha256-t2MfCv6oKrdGUbEA3lNcftMNnBO3ZNUnAg3l1EzbpwI";
                  };
                  cargoHash = "sha256-nB4evO7Ea6Oy7SUjyX3FUjasL/D+5lT87cxHdigrq4Q=";
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
