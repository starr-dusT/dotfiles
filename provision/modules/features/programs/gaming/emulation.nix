{ ... }:
{
  flake.modules.nixos.emulation =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        ryubing # Experimental Nintendo Switch Emulator written in C# (community fork of Ryujinx)
      ];
      services.flatpak.packages = [
        "net.retrodeck.retrodeck" # All-in-One Retro Gaming Platform for Linux
      ];
    };
}
