{ config, lib, pkgs, user, ... }:

let cfg = config.modules.programs.virt-manager;
in {
  options.modules.programs.virt-manager.enable = lib.mkEnableOption "virt-manager";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      virt-manager # Desktop application for managing virtual machines through libvirt
    ];
    
    virtualisation.libvirtd.enable = true;
    virtualisation.libvirtd.qemu.swtpm.enable = true;
    programs.dconf.enable = true;

    users.users.${user} = {
      extraGroups = [ "libvirtd" ];
    };
  };
}
