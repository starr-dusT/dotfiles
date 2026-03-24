{ ... }:
{
  flake.modules.nixos.notes =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        obsidian # Note-taking and knowledge management application
        pandoc # Universal document converter
      ];
    };
}
