self: super: {
  gnome-remote-desktop = super.gnome-remote-desktop.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [ super.autoAddDriverRunpath ];
  });
}
