{ ... }:
{
  flake.modules.nixos.kde-connect =
    { pkgs, ... }:
    {
      networking.firewall = rec {
        allowedTCPPortRanges = [
          {
            from = 1714;
            to = 1764;
          }
        ];
        allowedUDPPortRanges = allowedTCPPortRanges;
      };

      programs.kdeconnect = {
        enable = true;
        package = pkgs.valent-custom; # Implementation of the KDE Connect protocol, built on GNOME platform libraries
      };
    };
}
