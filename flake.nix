{
  description = "A small utilities for SAT solver benchmarking";
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    flake-utils.url = github:numtide/flake-utils;
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      defaultPackage =
        with import nixpkgs { system = "${system}"; };
        stdenv.mkDerivation rec {
          name = "sat-bench-${version}";
          pname = "sat-bench";
          version = "0.11.6";
          src = self;
          buildInputs = rustc.buildInputs ++ [ cargo rustc libiconv openssl pkgconfig ];
          buildPhase = "cargo build --release";
          installPhase = "mkdir -p $out/bin; install -t $out/bin target/release/sat-bench target/release/benchm";
        }
      ;
    })
  ;
}
