# CAD and 3d printing and everything nice

{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.engineering;
in {
  options.modules.devel.engineering.enable = lib.mkEnableOption "engineering";
  config = lib.mkIf cfg.enable {

    # Install packages
    environment.systemPackages = with pkgs; [ super-slicer freecad blender ];
  };
}
