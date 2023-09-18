# python with all the venom

{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.python;
  my-python-packages = ps: with ps; [
    virtualenv
    i3ipc
    ipython
    pip
  ];
  
in {
  options.modules.devel.python.enable = lib.mkEnableOption "python";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [ 
      (python3.withPackages my-python-packages)
      beancount 
      fava 
      nodePackages_latest.pyright
      distrobox
    ];
  };
}
