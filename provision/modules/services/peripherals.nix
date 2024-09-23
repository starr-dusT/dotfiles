{ config, lib, pkgs, user, ... }:

let cfg = config.modules.services.peripherals;
in {
  options.modules.services.peripherals.enable = lib.mkEnableOption "peripherals";
  config = lib.mkIf cfg.enable {

    services.usbmuxd.enable = true; # for iOS mounting as storage
    environment.systemPackages = with pkgs; [
      pulseaudio # Sound server for Linux and other Unix-like operating systems.
      pavucontrol # Simple GTK based mixer for the PulseAudio sound server
      libimobiledevice # Library to support iPhone, iPod Touch and iPad devices on Linux
      ifuse # fuse filesystem implementation to access the contents of iOS devices
      usbtop
    ];

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };
}
