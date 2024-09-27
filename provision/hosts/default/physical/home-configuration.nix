{ config, pkgs, user, ... }:
{
  home-manager.users.${user} = {
    programs.vscode = {
      enable = true;
      package = pkgs.vscode.fhs;
    };
  };
}
