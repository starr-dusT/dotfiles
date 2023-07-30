# vfio setup for windows gaming with single gpu 

{ config, lib, pkgs, user, ... }:

let cfg = config.modules.services.vfio;
in {
  options.modules.services.vfio.enable = lib.mkEnableOption "samba";
  config = lib.mkIf cfg.enable {

    users.users.${user}.extraGroups = [ "qemu-libvirtd" "libvirtd" "kvm" ];

    # Boot configuration
    boot.kernelParams = [ "amd_iommu=on" "iommu=pt" ];
    boot.kernelModules = [ "kvm-amd" "vfio-pci" ];

    programs.dconf.enable = true;

    environment.systemPackages = with pkgs; [ virt-manager ];

    # Enable libvirtd
    virtualisation.libvirtd = {
      enable = true;
      onBoot = "ignore";
      onShutdown = "shutdown";
      qemu.ovmf.enable = true;
      qemu.runAsRoot = true;
    };

    # Place helper files where libvirt can get to them
    environment.etc = {
      "libvirt/hooks/qemu" = {
        source = "/home/${user}/.local/share/chezmoi/provision/local/gpu-passthrough/qemu";
        mode = "0755";
      };
      "libvirt/hooks/qemu.d/win10/prepare/begin/start.sh" = {
        source = "/home/${user}/.local/share/chezmoi/provision/local/gpu-passthrough/start.sh";
        mode = "0755";
      };
      "libvirt/hooks/qemu.d/win10/release/end/revert.sh" = {
        source = "/home/${user}/.local/share/chezmoi/provision/local/gpu-passthrough/revert.sh";
        mode = "0755";
      };
      "libvirt/qemu.conf" = {
        source = "/home/${user}/.local/share/chezmoi/provision/local/gpu-passthrough/qemu.conf";
        mode = "0755";
      };
      "libvirt/libvirtd.conf" = {
        source = "/home/${user}/.local/share/chezmoi/provision/local/gpu-passthrough/libvirtd.conf";
        mode = "0755";
      };
    };
  };
}
