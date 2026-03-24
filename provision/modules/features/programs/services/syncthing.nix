{ ... }:
{
  flake.modules.nixos.syncthing =
    { pkgs, config, ... }:
    let
      user = "${config.preferences.user}";
    in
    {
      environment.systemPackages = with pkgs; [
        syncthing # File sync program for multiple devices in real-time
      ];

      networking.firewall.allowedTCPPorts = [
        8384
        22000
      ];
      networking.firewall.allowedUDPPorts = [
        22000
        21027
      ];

      services.syncthing = {
        enable = true;
        user = "${user}";
        dataDir = "/home/${user}/.local/share/syncthing";
        configDir = "/home/${user}/.config/syncthing";
        guiAddress = "0.0.0.0:8384";
        key = "/run/agenix/syncthing/key.pem";
        cert = "/run/agenix/syncthing/cert.pem";
        overrideFolders = false; # don't delete user added folders
      };
    };
}
