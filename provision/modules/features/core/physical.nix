{ ... }:
{
  flake.modules.nixos.physical =
    { config, pkgs, ... }:
    {
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

      services.ddccontrol = {
        enable = true;
        package = pkgs.ddcutil-service;
      };
      hardware.i2c.enable = true;
      users.users.${config.preferences.user}.extraGroups = [ "i2c" ];
    };
}
