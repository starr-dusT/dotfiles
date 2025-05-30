{ config, lib, pkgs, user, ... }:

let cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh.enable = lib.mkEnableOption "ssh";

  config = lib.mkIf cfg.enable {
    services.openssh.enable = true;
    services.pcscd.enable = true;
    programs.gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-curses;
      enableSSHSupport = true;
    };
  };
}
