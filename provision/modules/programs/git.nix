{ config, lib, pkgs, user, ... }:

let cfg = config.modules.programs.git;
in {
  options.modules.programs.git = with lib; {
    enable = lib.mkOption {
      type = with types; bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      git # Version control system for tracking changes in source code during software development.
      git-annex # Manages files with git, without checking the file contents into git.
      lazygit # Terminal-based GUI for git, making it easier to use and visualize git repositories.
    ];

    age.secrets."git/github_personal" = {
      file = ../../secrets/git/github_personal.age;
      owner = "${user}";
      group = "users";
    };
  };
}
