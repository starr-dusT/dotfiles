{ config, lib, pkgs, user, ... }:

let cfg = config.modules.services.peripherals;
in {
  options.modules.services.peripherals.enable = lib.mkEnableOption "peripherals";
  config = lib.mkIf cfg.enable {

    hardware.openrazer.enable = true;
    hardware.openrazer.users = ["tstarr"];

    environment.systemPackages = with pkgs; [
      polychromatic # Graphical utility for configuring RGB lighting effects on Razer peripherals.
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
