{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.optional.desktop;
in
{
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.jq
    ]
    ++ [
      (pkgs.writeShellScriptBin "sink-switch.sh" (builtins.readFile ./sink-switch.sh))
      (pkgs.writeShellScriptBin "display-switch.sh" (builtins.readFile ./display-switch.sh))
      (pkgs.writeShellScriptBin "subwoofer-volume.sh" (builtins.readFile ./subwoofer-volume.sh))
    ];
  };
}
