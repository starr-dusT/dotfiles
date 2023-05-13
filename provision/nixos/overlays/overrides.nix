# This overlay is used for overriding upstream packages.

self: super:

let
  nixpkgs-master = import <nixpkgs-master> { inherit (super) config; };
  nixpkgs-unstable = import <channels-nixos-unstable> { inherit (super) config; };

in {
  inherit nixpkgs-master;
  inherit nixpkgs-unstable;

  qtile = super.qtile.overrideAttrs(oldAttrs: {
    propagatedBuildInputs = oldAttrs.passthru.unwrapped.propagatedBuildInputs ++ (with self.python3Packages; [
      pyyaml
    ]);
  });
}
