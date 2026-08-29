{ inputs, ... }:

{
  imports = [
    ./dev-devenv/packages.nix
    ./dev-devenv/environment.nix
  ];

  overlays = [
    inputs.rust-overlay.overlays.default
    (import ./dev-devenv/haskell.nix)
    (import ./dev-devenv/node.nix)
    (import ./dev-devenv/rust.nix)
  ];
}
