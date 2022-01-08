{ pkgs, libs, ... }:

let
  extra = ''
    ${pkgs.xorg.xset}/bin/xset r rate 290 26
  '';
in

{
  home-manager.users.bn.xsession = {
    enable = true;
    initExtra = extra;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./config.hs;
    };
  };
}
