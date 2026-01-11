{
  config,
  lib,
  pkgs,
  user,
  ...
}:

let
  cfg = config.modules.optional.desktop;
in
{
  config = lib.mkIf cfg.enable {
    age.secrets."kube/homedb_pass" = {
      file = ../../../../secrets/kube/homedb_pass.age;
      owner = "${user}";
      group = "users";
    };

    environment.systemPackages = [
      pkgs.jq
      pkgs.postgresql
    ]
    ++ [
      (pkgs.writeShellScriptBin "sink-switch.sh" (builtins.readFile ./sink-switch.sh))
      (pkgs.writeShellScriptBin "display-switch.sh" (builtins.readFile ./display-switch.sh))
      (pkgs.writeShellScriptBin "subwoofer-volume.sh" (builtins.readFile ./subwoofer-volume.sh))
    ];
  };
}
