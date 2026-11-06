{ ... }:
{
  flake.modules.nixos.kestrel =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        opencode # AI coding agent built for the terminal
      ];

      services.ollama = {
        enable = true;
        package = pkgs.ollama-cuda;
      };
    };
}
