final: prev: {
  ftw = final.callPackage ../pkgs/ftw.nix {};
  gnome-set-panel-monitor = final.callPackage ../pkgs/gnome-set-panel-monitor.nix {};
  pySVS = final.callPackage ../pkgs/pySVS.nix {};
}
