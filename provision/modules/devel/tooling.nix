# coding stuff for all the languages

{ config, lib, pkgs, user, ... }:

let
  cfg = config.modules.devel.tooling;
in {
  options.modules.devel.tooling.enable = lib.mkEnableOption "tooling";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      cmake # Cross-platform build system generator.
      gcc # GNU Compiler Collection, a compiler system for programming languages.
      coreutils # Collection of basic file, shell, and text manipulation utilities.
      gnumake # GNU Make, a build automation tool.
      go # Programming language developed by Google, known for its simplicity and efficiency.
      nixpkgs-lint
      nixpkgs-fmt
    ];
  };
}
