{ config, lib, pkgs, user, ... }:

let cfg = config.modules.services.peripherals;
in {
  options.modules.services.peripherals.enable = lib.mkEnableOption "peripherals";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      pulseaudio # Sound server for Linux and other Unix-like operating systems.
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
