{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    neovim # Fork of Vim aiming to improve extensibility and usability
  ];
}
