# CAD and 3d printing and everything nice

{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.engineering;
in {
  options.modules.devel.engineering.enable = lib.mkEnableOption "engineering";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [ 
      super-slicer # 3D printing slicing software with advanced features and customization options.
      prusa-slicer # Slicing software optimized for Prusa 3D printers, offering user-friendly interface and settings.
      freecad # Open-source parametric 3D CAD modeler.
      blender # Open-source 3D creation suite for modeling, animation, rendering, and more.
    ];
  };
}
