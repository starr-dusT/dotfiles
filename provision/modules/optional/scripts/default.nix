{ lib, ... }:
{
  imports = [ 
    ./init-bash-script.nix
    ./mount-engi.nix
  ];

  options.modules.scripts = with lib; {
    enable = mkEnableOption "scripts";
    blacklist = mkOption {
      type = types.listOf types.str;
      default = []; 
      description = ''
        list of scripts to blacklist for host.
      '';
    };
  };
}
