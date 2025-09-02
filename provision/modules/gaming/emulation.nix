{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.emulation;
in {
  options.modules.gaming.emulation.enable = lib.mkEnableOption "emulation";

  config = lib.mkIf cfg.enable {
    services.flatpak.packages = [
      "net.retrodeck.retrodeck"
    ];
  };
}
