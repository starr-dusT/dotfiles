{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.nipr;
in {
  options.modules.system.nipr.enable = lib.mkEnableOption "nipr";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      opensc
      pcsc-tools
      pkcs11helper
      firefox
    ];
  };
}
