{ config, pkgs, ... }:

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
    xsession = {
      enable = true;
       windowManager.xmonad = {
         enable = true;
       };
    };
  };
}
