{ pkgs, ... }:

{
  systemd.user.services.lobatt-notify = {
    description = "Post a notification when the battery is running low.";
    serviceConfig = {
      Type = "oneshot";
      User = "bn";
      ExecStart = pkgs.writeScript "notify-on-low-battery" ''
        #!${pkgs.bash}/bin/bash
        . <(${pkgs.systemd}/bin/udevadm info -q property -p /sys/class/power_supply/BAT0 |
          ${pkgs.gnugrep}/bin/egrep 'POWER_SUPPLY_(CAPACITY|STATUS)=')
        if [[ $POWER_SUPPLY_STATUS = Discharging && $POWER_SUPPLY_CAPACITY -lt 10 ]]; then
          ${pkgs.dunst}/bin/dunstify -u critical "Low battery: $POWER_SUPPLY_CAPACITY"
        fi
      '';
    };
    after = [ "display-manager.service" ];
    startAt = "minutely";
  };
}
