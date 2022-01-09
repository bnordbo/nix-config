{ config, lib, pkgs, stdenv, ... }:

let
  packages = with pkgs; [
    pkgs.acpilight                      # ACPI backlight control
    pkgs.alacritty
    pkgs.bazelisk                       # A wrapper to use the right Bazel version
    pkgs.binutils                       # Linker, assembler etc. and utilities like strings
    pkgs.curl                           # Command line tool for transferring files with URL syntax
    pkgs.fd                             # Simple, fast and user-friendly alternative to find
    pkgs.file                           # Program that shows the type of files
    pkgs.firefox                        # Web browser built from Firefox source tree
    pkgs.font-manager
    pkgs.git                            # Distributed version control system
    pkgs.haskellPackages.xmobar         # A Minimalistic Text Based Status Bar
    pkgs.haskellPackages.xmonad-contrib # Extra modules for the XMonad window manager
    pkgs.jq                             # Lightweight and flexible command-line JSON processor
    pkgs.kubectl                        # Kubernetes CLI
    pkgs.niv                            # Easy dependency management for Nix projects
    pkgs.python3
    pkgs.ripgrep                        # Fast grep replacement
    pkgs.rofi                           # Window switcher, run dialog and dmenu replacement
    pkgs.xorg.xev                       # Capture X events
    pkgs.xorg.xmodmap                   # Tool for remapping keys in X
  ];
in

{
  imports = (import ./programs);

  # with?
  home-manager.users.bn.home = {
    packages = packages;
  };
}