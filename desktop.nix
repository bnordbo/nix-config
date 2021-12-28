{ config, pkgs, ... }:

let
  myCustomLayout = pkgs.writeText "xkb-layout" ''
keycode 64  = Mode_switch Alt_L
keycode 133 = Meta_L      Super_L
keycode 134 = Meta_R      Super_R
keycode 108 = Mode_switch Alt_R

keycode 48  = apostrophe  quotedbl    ae          AE
keycode 32  = o           O           oslash      Oslash
keycode 38  = a           A           aring       Aring
'';
in

{
  services = {
    gnome.gnome-keyring.enable = true;

    dbus = {
      enable = true;
      packages = [ pkgs.gnome3.dconf ];
    };

    xserver = {
      enable = true;
      layout = "us";

      displayManager = {
        defaultSession = "none+xmonad";
        autoLogin.enable = false;
        autoLogin.user = "bn";
      };

      windowManager.xmonad = {
        enable = true;
      };
    };
  };

  home-manager.users.bn = {
    home.keyboard.options = [ "ctrl:swapcaps" ];
    xsession = {
      enable = true;
#      initExtra = "${pkgs.xorg.xmodmap}/bin/xmodmap ${myCustomLayout}\n";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ./config.hs;
      };
    };

    programs = {
      emacs = {
        enable = true;
        extraPackages = (epkgs: [
          epkgs.magit
          epkgs.org-pdftools
          epkgs.org-roam
          epkgs.plantuml-mode
          epkgs.ripgrep
        ]);
      };

      rofi = {
        enable = true;
      };
    };
  };
}
