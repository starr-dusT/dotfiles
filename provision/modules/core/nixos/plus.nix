# TODO: Think about this file maybe no including borg, beancount and rather adding
#       a subset of programs from terminal.nix then adding a plus-plus.nix.
{ lib, config, ... }:

let cfg = config.modules.core.plus;
in {
  options.modules.core.plus.enable = lib.mkEnableOption "plus";

  config = lib.mkIf cfg.enable {
    modules = {
        programs = {
          borg.enable = true;
          beancount.enable = true;
      };
    };
  };
}
