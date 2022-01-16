{ pkgs, ... }:

{
  home-manager.users.bn.services.dunst = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
      size = "16x16";
    };
    settings = {
      global = {
        font = "Monoid 12";
        line_height = 4;
        format = ''<b>%s</b>\n%b'';
      };
    };
  };
}
