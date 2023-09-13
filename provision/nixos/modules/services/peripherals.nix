{ config, lib, pkgs, ... }:

let cfg = config.modules.services.peripherals;
in {
  options.modules.services.peripherals.enable = lib.mkEnableOption "peripherals";
  config = lib.mkIf cfg.enable {
    # Enable sound.
    sound.enable = true;
    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.support32Bit = true;

    services = {
      gvfs.enable = true;
      blueman.enable = true;
      printing.enable = true;
      printing.drivers = [ pkgs.hplip ];
      avahi.enable = true;
      avahi.nssmdns = true;
    };
  };
}
