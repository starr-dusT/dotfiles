{ config, lib, pkgs, user, ... }:

let cfg = config.modules.services.peripherals;
in {
  options.modules.services.peripherals.enable = lib.mkEnableOption "peripherals";
  config = lib.mkIf cfg.enable {

    hardware.openrazer.enable = true;
    hardware.openrazer.users = ["tstarr"];

    environment.systemPackages = with pkgs; [
        polychromatic
        pulseaudio
    ];

    # rtkit is optional but recommended
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };
}
