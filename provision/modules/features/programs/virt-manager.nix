{ ... }:
{
  flake.modules.nixos.virt-manager =
    { pkgs, config, ... }:
    {
      environment.systemPackages = with pkgs; [
        virt-manager # Desktop application for managing virtual machines through libvirt
      ];

      virtualisation.libvirtd.enable = true;
      virtualisation.libvirtd.qemu.swtpm.enable = true;
      programs.dconf.enable = true;

      users.users.${config.preferences.user} = {
        extraGroups = [ "libvirtd" ];
      };
    };
}
