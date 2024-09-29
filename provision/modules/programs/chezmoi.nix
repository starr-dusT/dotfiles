{ config, lib, pkgs, user, home-manager, ... }:

let cfg = config.modules.programs.chezmoi;
in {
  options.modules.programs.chezmoi = with lib; {
    enable = lib.mkEnableOption "chezmoi";
    apply = lib.mkOption {
      type = with types; bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      chezmoi # Manages your dotfiles across multiple machines, ensuring consistency and version control
    ];

    # Optionally apply chezmoi dotfiles with home-manager activation
    home-manager.users.${user} = lib.mkIf cfg.apply {
      home.activation.chezmoi = home-manager.lib.hm.dag.entryAfter [ "installPackages" ] ''
        _saved_path=$PATH
        PATH="${pkgs.git}/bin:$PATH"
        run ${pkgs.chezmoi}/bin/chezmoi apply --force
        PATH=$_saved_path
      '';
    };
  };
}
