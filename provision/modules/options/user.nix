{
  flake.modules.nixos.core = {lib, ...}: {
    options.preferences = {
      user = lib.mkOption {
        type = lib.types.str;
        default = "tstarr";
      };
    };
  };
}
