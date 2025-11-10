{ pkgs, lib, ... }:
{
  services.openssh.enable = true;
  services.pcscd.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryPackage = lib.mkForce pkgs.pinentry-gnome3;
  };
}
