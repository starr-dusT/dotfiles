{ ... }:
{
  flake.modules.nixos.desktop-scripts =
    { pkgs, config, ... }:
    {
      age.secrets."kube/homedb_pass" = {
        file = ../../../../../secrets/kube/homedb_pass.age;
        owner = "${config.preferences.user}";
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
