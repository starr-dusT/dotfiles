{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.optional.development.engineering;
in
{
  options.modules.optional.development.engineering.enable = lib.mkEnableOption "engineering";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      blender # Open-source 3D creation suite for modeling, animation, rendering, and more
      freecad # Open-source parametric 3D CAD modeler
      openscad # Open-source script-only based modeller
      kicad # Open Source Electronics Design Automation suite
      prusa-slicer # Open-source, feature-rich, frequently updated tool that contains everything you need to export the perfect print files for your 3D printer.
    ];
  };
}
