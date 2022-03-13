{ pkgs, ... }:

{
  home-manager.users.bn = {
    services.emacs = {
      enable = true;
      #    defaultEditor = true;

      client = {
        enable = true;
        arguments = ["-c" "-n" "-q"];
      };
    };

    systemd.user.targets.emacs = {
      Service = {
        ExecStop = "${pkgs.emacs}/bin/emacsclient --eval \"(save-buffers-kill-terminal t)\"";
      };
    };
  };
}
