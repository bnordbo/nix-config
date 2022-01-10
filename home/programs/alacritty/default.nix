{ pkgs, ... }:

{
  home-manager.users.bn.programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = {
          family = "Monoid";
          style = "Regular";
        };
        size = 9;
      };
    };
  };
}
