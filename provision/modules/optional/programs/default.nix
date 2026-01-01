{ ... }:
{
  imports = [
    ./_plus.nix
    ./beancount.nix
    ./chrome.nix
    ./docker.nix
    ./firefox.nix
    ./virt-manager.nix
  ];
}
