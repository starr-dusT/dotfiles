{ config, lib, pkgs, user, ... }:

let cfg = config.modules.programs.beancount;
in {
  options.modules.programs.beancount.enable = lib.mkEnableOption "beancount";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      fava # Web interface for the double-entry bookkeeping software Beancount
      beancount # Double-entry bookkeeping software for tracking financial transactions
    ];
  };
}
