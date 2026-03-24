{ ... }:
{
  flake.modules.nixos.keyd =
    { ... }:
    {
      services.keyd = {
        enable = true;
        keyboards.true = {
          ids = [ "*" ];
          settings = {
            main = {
              # Replace capslock with esc
              capslock = "overload(meta, esc)";
            };
            meta = {
              u = "oneshot(util)";
            };
            util = {
              d = "C-M-d"; # Kestrel display at desk
              l = "C-M-l"; # Kestrel display in living room
              k = "C-M-k"; # Kestrel display in Comet remote control
            };
            alt = {
              # Use hjkl for arrow keys with alt
              h = "left";
              j = "down";
              k = "up";
              l = "right";
            };
          };
        };
      };
    };
}
