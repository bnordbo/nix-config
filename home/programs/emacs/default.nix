{ pkgs, ... }:

{
  home-manager.users.bn.programs = {
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
  };
}
