{ ... }:
{
  flake.modules.nixos.nix =
    { pkgs, ... }:
    {
      nix = {
        package = pkgs.nixVersions.stable;
        extraOptions = "experimental-features = nix-command flakes";
        settings.auto-optimise-store = true;
        gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 7d";
        };
      };

      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = import ../../../lib/overlays.nix;
    };
}
