{
  description = "A small utilities for SAT solver benchmarking";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      defaultPackage =
        with import nixpkgs { system = "${system}"; };
        stdenv.mkDerivation {
          name = "SAT-bench";
          src = self;
          buildInputs = [ cargo rustc ];
          buildPhase = "cargo build --release";
          installPhase = "mkdir -p $out/bin; install -t $out/bin target/release/sat-bench target/release/benchm";
        }
      ;
    })
  ;
}
