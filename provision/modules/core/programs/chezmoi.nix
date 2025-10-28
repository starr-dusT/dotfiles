{
  pkgs,
  user,
  home-manager,
  ...
}:
{
  environment.systemPackages = with pkgs; [
    chezmoi # Manage your dotfiles across multiple machines, securely
  ];

  home-manager.users.${user} = {
    home.activation.chezmoi = home-manager.lib.hm.dag.entryAfter [ "installPackages" ] ''
      PATH="${pkgs.git}/bin:$PATH"
      run ${pkgs.chezmoi}/bin/chezmoi apply --force
    '';
  };
}
