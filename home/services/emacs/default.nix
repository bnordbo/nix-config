{ pkgs, ... }:

{
  home-manager.users.bn.services.emacs = {
    enable = true;
#    defaultEditor = true;

    client = {
      enable = true;
      arguments = ["-n" "-q"];
    };
  };
}