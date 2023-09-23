{ ... }:
{
  imports = [ ./wireguard-server.nix ./syncthing.nix ./samba-server.nix ./samba-client.nix ./jellyfin.nix ./virt-manager.nix ./peripherals.nix ];
}
