{ ... }:
{
  flake.modules.nixos.valent =
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
        package = pkgs.valent; # Implementation of the KDE Connect protocol, built on GNOME platform libraries
      };
    };
}
