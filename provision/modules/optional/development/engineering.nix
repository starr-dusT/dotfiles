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
    ];
    services.flatpak.packages = [
      "com.prusa3d.PrusaSlicer"
    ];
  };
}
