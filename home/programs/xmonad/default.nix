{ pkgs, libs, ... }:

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

  extra = ''
    ${pkgs.xorg.xset}/bin/xset r rate 290 26
    ${pkgs.xorg.xmodmap}/bin/xmodmap ${myCustomLayout}
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
