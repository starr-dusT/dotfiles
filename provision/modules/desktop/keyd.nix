{ config, lib, pkgs, user, inputs, ... }:
{
  services.keyd = {
    enable = true;
    keyboards.true = {
      ids = [ "*" ];
      settings = { 
        main = {
          capslock = "overload(meta, esc)";
          leftalt = "layer(alt)";
        };
        alt = {
          h = "left";
          j = "down";
          k = "up";
          l = "right";
        };
      };
    };
  };
}
