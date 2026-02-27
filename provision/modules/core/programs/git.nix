{ pkgs, user, ... }:
{
  environment.systemPackages = with pkgs; [
    git # Version control system for tracking changes in source code during software development
    git-lfs # Git extension for versioning large files
    git-annex # Manages files with git, without checking the file contents into git
    lazygit # Terminal-based GUI for git, making it easier to use and visualize git repositories
  ];

  age.secrets."git/github_personal" = {
    file = ../../../secrets/git/github_personal.age;
    owner = "${user}";
    group = "users";
  };

  programs.ssh.extraConfig = ''
    Host github.com
      AddKeysToAgent yes
      IdentityFile /run/agenix/git/github_personal 
  '';
}
