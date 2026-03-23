{
  flake.modules.nixos.core = {lib, ...}: {
    options.preferences = {
      user.name = lib.mkOption {
        type = lib.types.str;
        default = "tstarr";
      };
    };
  };
}
