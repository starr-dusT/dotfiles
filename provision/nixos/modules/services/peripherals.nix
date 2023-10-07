{ config, lib, pkgs, ... }:

let cfg = config.modules.services.peripherals;
in {
  options.modules.services.peripherals.enable = lib.mkEnableOption "peripherals";
  config = lib.mkIf cfg.enable {

    # rtkit is optional but recommended
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;
    };

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
