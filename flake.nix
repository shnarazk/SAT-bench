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
                  version = "0.16.1-20240812-1";
                  src = fetchFromGitHub {
                    name = "SAT-bench";
                    owner = "shnarazk";
                    repo = "SAT-bench";
                    rev = "aac357ec5399e4778787ccd7ebe4bb3e3ad586e2";
                    hash = "sha256-Uf5pbyWhKGB9szs1fH40cmn5CBjVPGTvJaFESRtQxzI=";
                  };
                  cargoHash = "sha256-rPm1FxCui5ucxyhCN15jtvYI2o02+Y4TjR3ibS5Ogmk=";
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
