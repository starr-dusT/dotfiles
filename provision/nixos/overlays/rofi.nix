let sources = import ../nix/sources.nix; in
self: super: {
    rofi = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };
}



