{ pkgs, ... }:

{
  home-manager.users.bn.programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
   };
}
