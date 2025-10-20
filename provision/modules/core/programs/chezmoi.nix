{ pkgs, user, home-manager, ... }:
{
  environment.systemPackages = with pkgs; [ 
    chezmoi # Manage your dotfiles across multiple machines, securely
  ];

  home-manager.users.${user} = {
    home.activation.chezmoi = home-manager.lib.hm.dag.entryAfter [ "installPackages" ] ''
      _saved_path=$PATH
      PATH="${pkgs.git}/bin:$PATH"
      run ${pkgs.chezmoi}/bin/chezmoi apply --force
      PATH=$_saved_path
    '';
  };
}
