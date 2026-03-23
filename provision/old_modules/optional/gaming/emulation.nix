{ config, lib, ... }:

let
  cfg = config.modules.optional.gaming.emulation;
in
{
  options.modules.optional.gaming.emulation.enable = lib.mkEnableOption "emulation";

  config = lib.mkIf cfg.enable {
    services.flatpak.packages = [
      "net.retrodeck.retrodeck"
    ];
  };
}
