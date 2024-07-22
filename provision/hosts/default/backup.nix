{ config, pkgs, user, lib, ... }:
{
  environment.systemPackages = with pkgs; [
    restic # Fast and secure backup program
  ];
}
