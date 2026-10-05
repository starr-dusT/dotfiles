{ ... }:
{
  flake.modules.nixos.scripts =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        (pkgs.writeShellScriptBin "init-bash-script" (builtins.readFile ./init-bash-script.sh))
        (pkgs.writeShellScriptBin "mount-engi" (builtins.readFile ./mount-engi.sh))
      ];
    };
}
