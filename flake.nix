{
  description = "A small utilities for SAT solver benchmarking";
  inputs.nixpkgs.url = github:NixOS/nixpkgs;
  outputs = { self, nixpkgs }:
  {
    packages = builtins.listToAttrs
      (map
        (system:
           with import nixpkgs { system = "${system}"; };
           {
             name = system;
             value = {
               default =
                 stdenv.mkDerivation rec {
                   name = "sat-bench-${version}";
                   pname = "sat-bench";
                   version = "0.15.0-20221015-1";
                   src = self;
                   buildInputs = rustc.buildInputs ++ [ cargo rustc libiconv openssl pkg-config ];
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
                   SATBENCHLIB="$out/lib";
                 };
           };
        })
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ]
      );
  };
}
