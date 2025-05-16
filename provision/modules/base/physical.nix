{ config, pkgs, user, lib, inputs, ... }:

let cfg = config.modules.physical;
in {
  options.modules.physical.enable = lib.mkEnableOption "physical";
  config = lib.mkIf cfg.enable {
    # use the systemd-boot EFI boot loader
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
