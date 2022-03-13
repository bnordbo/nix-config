{ pkgs, ... }:

{
  home-manager.users.bn.programs.git = {
    enable = true;
    userEmail = "bn@strangedays.no";
    userName = "Bjørn Nordbø";

    aliases = {
      br = "branch";
      co = "checkout";
    };
  };
}
