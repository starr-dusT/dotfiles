{ ... }:
{
  imports = [
    ./gnome-remote-desktop.nix
    ./samba-client.nix 
    ./ssh.nix
    ./syncthing.nix
  ];
}
