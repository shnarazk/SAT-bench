# SATbench, small utilities about SAT benchmark

## Nix Overlay

- Modify the following nix with right values:
   - `rev` is the latest commit id.
   - the value for `sha256` is provided by `nix-prefetch-url https://gitlab.com/satisfiability01/SATbench/repository/archive.tar.gz?ref=$rev` .
   - the value for `cargoSha256` is provided by a failure install.
   - See: https://nixos.org/nixpkgs/manual/#compiling-rust-applications-with-cargo
-  Save it as ~/.config/nixpkgs/overlay/sat-bench.nix,

```nix
self: super:
{
    sat-bench = super.rustPlatform.buildRustPackage rec {
        name = "satbench-${version}";
        version = "0.3";
        src = super.fetchFromGitLab {
            owner = "satisfiability01";
            repo = "SATbench";
            rev = "FIXME";
            sha256 = "FIXME";
        };
        cargoSha256 = "FIXME";
        meta = with super.stdenv.lib; {
            description = "Small utilities for SAT benchmark";
            homepage = "https://gitlab.com/satisfiability01/SATbench";
        };
    };
}
```

