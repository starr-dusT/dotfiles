{ inputs, ... }:
{
  flake.modules.nixos.cachy =
    { pkgs, ... }:
    {
      nixpkgs.overlays = [
        inputs.nix-cachyos-kernel.overlays.default
      ];

      # Use cachy kernel
      boot.kernelPackages = pkgs.cachyosKernels.linuxPackages-cachyos-latest;
    };
}
