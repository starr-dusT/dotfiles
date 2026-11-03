self: super: {
  mattermost-desktop = super.mattermost-desktop.overrideAttrs (oldAttrs: {
    postInstall = (oldAttrs.postInstall or "") + ''
            substituteInPlace $out/share/applications/Mattermost.desktop \
              --replace-fail \
                "[Desktop Entry]" \
                "[Desktop Entry]
      StartupWMClass=Mattermost.Desktop"
    '';
  });
}
