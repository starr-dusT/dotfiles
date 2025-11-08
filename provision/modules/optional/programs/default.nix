{ ... }:
{
  imports = [
    ./_plus.nix
    ./beancount.nix
    ./borg.nix
    ./chrome.nix
    ./docker.nix
    ./firefox.nix
    ./k3s.nix
    ./kitty.nix
    ./virt-manager.nix
  ];
}
