final: prev: {
  advcpmv = final.callPackage ../pkgs/advcpmv.nix {};
  ftw = final.callPackage ../pkgs/ftw.nix {};
  gnome-set-panel-monitor = final.callPackage ../pkgs/gnome-set-panel-monitor.nix {};
  stacks-in-gnome = final.callPackage ../pkgs/stacks-in-gnome.nix {};
  pySVS = final.callPackage ../pkgs/pySVS.nix {};
}
