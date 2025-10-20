{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    neovim # Fork of Vim aiming to improve extensibility and usability
    pyright # Latest version of the Pyright package, a static type checker for Python
  ];
}
