/* This contains various packages we want to overlay. Note that the
 * other ".nix" files in this directory are automatically loaded.
 */
final: prev: {
  sway-scratchpad = final.callPackage ../pkgs/sway-scratchpad.nix {};
  advcpmv = final.callPackage ../pkgs/advcpmv.nix {};
  taskopen = final.callPackage ../pkgs/taskopen.nix {};
  cookcli = final.callPackage ../pkgs/cookcli.nix {};
  nx_tzdb = final.callPackage ../pkgs/nx_tzdb.nix {};
  yuzu-archive = final.libsForQt5.callPackage ../pkgs/yuzu-archive.nix {};
  gnome-set-panel-monitor = final.callPackage ../pkgs/gnome-set-panel-monitor.nix {};
  gnome-maximize-lonely-window = final.callPackage ../pkgs/gnome-maximize-lonely-window.nix {};
  gnome-fullscreen-to-empty-workspace = final.callPackage ../pkgs/gnome-fullscreen-to-empty-workspace.nix {};
}
