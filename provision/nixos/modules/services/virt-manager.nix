{ config, lib, pkgs, ... }:

let cfg = config.modules.services.virt-manager;
in {
  options.modules.services.virt-manager.enable = lib.mkEnableOption "virt-manager";
  config = lib.mkIf cfg.enable {
    virtualisation.libvirtd.enable = true;
    programs.dconf.enable = true;
    environment.systemPackages = with pkgs; [ virt-manager ];
  };
}
