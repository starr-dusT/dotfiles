{ ... }:
{
  flake.modules.nixos.scripts =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        (pkgs.writeShellScriptBin "init-bash-script.sh" (builtins.readFile ./init-bash-script.sh))
        (pkgs.writeShellScriptBin "mount-engi.sh" (builtins.readFile ./mount-engi.sh))
      ];
    };
}
