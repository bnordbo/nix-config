{ config, lib, pkgs, stdenv, ... }:

let
  packages = with pkgs; [
    # Applicatins
    pkgs.firefox                        # Web browser built from Firefox source tree
    pkgs.font-manager

    # Desktop
    pkgs.rofi                           # Window switcher, run dialog and dmenu replacement
    pkgs.xclip                          # Command line access to the X clipboard
    pkgs.xorg.xev                       # Capture X events
    pkgs.xorg.xkbcomp
    pkgs.xorg.xmessage
    pkgs.xorg.xmodmap                   # Tool for remapping keys in X

    # Development
    pkgs.bazelisk                       # A wrapper to use the right Bazel version
    pkgs.binutils                       # Linker, assembler etc. and utilities like strings
    pkgs.docker
    pkgs.docker-compose
    pkgs.go
    pkgs.gocode
    pkgs.git                            # Distributed version control system
    pkgs.python3

    # Nix
    pkgs.niv                            # Easy dependency management for Nix projects

    # Operations
    pkgs.kubectl                        # Kubernetes CLI

    # System
    pkgs.acpilight                      # ACPI backlight control
    pkgs.usbutils

    # Utilities
    pkgs.curl                           # Command line tool for transferring files with URL syntax
    pkgs.fd                             # Simple, fast and user-friendly alternative to find
    pkgs.file                           # Program that shows the type of files
    pkgs.jq                             # Lightweight and flexible command-line JSON processor
    pkgs.ripgrep                        # Fast grep replacement
    pkgs.tree                           # Neatly indented directory tree listings
  ];

  haskellPackages = with pkgs; [
    pkgs.ghc                            # The Glasgow Haskell Compiler
    pkgs.haskell-language-server        # LSP server for GHC
  ];
in

{
  imports = (import ./programs) ++ (import ./services);

  # with?
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users.bn = {
      programs.home-manager.enable = true;

      home = {
        packages = packages ++ haskellPackages;

        shellAliases = {
          "ec" = "emacsclient -nq";
          "myip" = "curl http://ipinfo.io/ip";
        };
      };
    };
  };
}
