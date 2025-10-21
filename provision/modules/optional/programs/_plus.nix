# TODO: Think about this file maybe no including borg, beancount and rather adding
#       a subset of programs from terminal.nix then adding a plus-plus.nix.
{ lib, config, ... }:

let
  cfg = config.modules.optional.programs._plus;
in
{
  options.modules.optional.programs._plus.enable = lib.mkEnableOption "_plus";

  config = lib.mkIf cfg.enable {
    modules = {
      optional = {
        programs = {
          borg.enable = true;
          beancount.enable = true;
        };
      };
    };
  };
}
