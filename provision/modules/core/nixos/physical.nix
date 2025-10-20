{ config, pkgs, user, lib, inputs, hostname, ... }:

let cfg = config.modules.physical;
in {
  options.modules.physical.enable = lib.mkEnableOption "physical";

  config = lib.mkIf cfg.enable {
    boot = {
      loader.systemd-boot.enable = true;
      loader.efi.canTouchEfiVariables = true;

      consoleLogLevel = 3;
      initrd.verbose = false;
      initrd.systemd.enable = true;
      kernelParams = [
          "quiet"
          "splash"
          "intremap=on"
          "boot.shell_on_fail"
          "udev.log_priority=3"
          "rd.systemd.show_status=auto"
      ];

      plymouth = {
        enable = true;
        theme = "nixos-bgrt";
        themePackages = with pkgs; [ nixos-bgrt-plymouth ];
      };
    };
  };
}
