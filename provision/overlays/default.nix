/* This contains various packages we want to overlay. Note that the
 * other ".nix" files in this directory are automatically loaded.
 */
final: prev: {
  sway-scratchpad = final.callPackage ../pkgs/sway-scratchpad.nix {};
  advcpmv = final.callPackage ../pkgs/advcpmv.nix {};
  taskopen = final.callPackage ../pkgs/taskopen.nix {};
}