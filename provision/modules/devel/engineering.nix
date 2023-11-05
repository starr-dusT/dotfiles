# CAD and 3d printing and everything nice

{ config, lib, pkgs, pkgs-unstable, user, ... }:

let
  cfg = config.modules.devel.engineering;
in {
  options.modules.devel.engineering.enable = lib.mkEnableOption "engineering";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [ 
      super-slicer 
      blender 
    ] ++ [
      # Freecad is broken right now (https://github.com/NixOS/nixpkgs/issues/263452)
      #pkgs-unstable.freecad 
    ];
  };
}
