{
  flake.modules.nixos.core = {lib, ...}: {
    options.preferences = {
      hostname = lib.mkOption {
        type = lib.types.str;
        default = "nixos";
      };
    };
  };
}
