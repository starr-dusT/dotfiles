# python with all the venom

{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.python;
  my-python-packages = ps: with ps; [
    virtualenv # Tool for creating isolated Python environments.
    i3ipc # Python library for controlling i3 window manager via its IPC interface.
    ipython # Interactive computing environment for Python.
    pip # Package installer for Python.
  ];
  
in {
  options.modules.devel.python.enable = lib.mkEnableOption "python";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [ 
      (python3.withPackages my-python-packages)
      fava # Web interface for the double-entry bookkeeping software Beancount.
      beancount # Double-entry bookkeeping software for tracking financial transactions.
      nodePackages_latest.pyright # Latest version of the Pyright package, a static type checker for Python.
    ];
  };
}
