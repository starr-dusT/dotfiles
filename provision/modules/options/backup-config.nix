{
  flake.modules.nixos.borgmatic = {lib, ...}: {
    options.preferences = {
      backup-config = lib.mkOption {
        type = lib.types.attrs;
        default = [ ];
      };
    };
  };
}
