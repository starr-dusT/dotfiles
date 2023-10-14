{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.ssh;
in {
  options.modules.system.ssh.enable = lib.mkEnableOption "ssh";
  config = lib.mkIf cfg.enable {
    # Enable the OpenSSH daemon.
    services.openssh.enable = true;
    services.pcscd.enable = true;
    programs.gnupg.agent = {
       enable = true;
       pinentryFlavor = "curses";
       enableSSHSupport = true;
    };
  };
}
