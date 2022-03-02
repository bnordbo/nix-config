{ pkgs, ... }:

{
  home-manager.users.bn.services.betterlockscreen = {
    enable = true;
    inactiveInterval = 10;
    arguments = ["--off" "10"];
  };
}
