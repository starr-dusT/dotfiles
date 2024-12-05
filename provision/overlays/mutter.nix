self: super: {
  gnome = super.gnome.overrideScope (gnomeFinal: gnomePrev: {
    mutter = gnomePrev.mutter.overrideAttrs ( old: {
      src = super.fetchgit {
        url = "https://gitlab.gnome.org/vanvugt/mutter.git";
        # GNOME 46: triple-buffering-v4-46
        rev = "94f500589efe6b04aa478b3df8322eb81307d89f";
        sha256 = "sha256-fkPjB/5DPBX06t7yj0Rb3UEuu5b9mu3aS+jhH18+lpI=";
      };
    });
  });
}
