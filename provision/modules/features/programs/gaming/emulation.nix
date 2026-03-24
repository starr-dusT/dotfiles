{ ... }:
{
  flake.modules.nixos.emulation =
    { ... }:
    {
      services.flatpak.packages = [
        "net.retrodeck.retrodeck"
      ];
    };
}
