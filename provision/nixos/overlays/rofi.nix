self: super: {
    rofi = super.rofi.override { plugins = [ self.rofi-emoji ]; };
}


