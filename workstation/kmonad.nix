# Adapted from https://github.com/pjones/tilde
{ config, lib, pkgs, ... }:

let
  cfg = config.workstation.kmonad;

  package = pkgs.callPackage ../pkgs/kmonad.nix { };

  # Per-keyboard options:
  keyboard = { name, ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        example = "laptop-internal";
        description = "Keyboard name.";
      };

      device = lib.mkOption {
        type = lib.types.path;
        example = "/dev/input/by-id/some-dev";
        description = "Path to the keyboard's device file.";
      };

      config = lib.mkOption {
        type = lib.types.lines;
        example = "(defsrc ...)";
        description = ''
          Keyboard configuration excluding the defcfg block.
        '';
      };
    };

    config = {
      name = lib.mkDefault name;
    };
  };

  # Create a complete KMonad configuration file:
  mkCfg = keyboard:
    let
      defcfg = ''
        (defcfg
          input  (device-file "${keyboard.device}")
          output (uinput-sink "kmonad-${keyboard.name}")
          cmp-seq KeyScrollLock ;; Effectively unused
          cmp-seq-delay 5
          fallthrough true
          allow-cmd true
        )
        '';

      # Mostly for command aliases in order to refer to the packages.
      defalias = ''
        (defalias
          bup (cmd-button "${pkgs.acpilight}/bin/xbacklight -inc 2")
          bdn (cmd-button "${pkgs.acpilight}/bin/xbacklight -dec 2")
        )
      '';
    in
    pkgs.writeTextFile {
      name = "kmonad-${keyboard.name}.cfg";
      text = defcfg + "\n" + defalias + "\n" + keyboard.config;
      checkPhase = "${cfg.package}/bin/kmonad -d $out";
    };

  # Build a systemd path config that starts the service below when a
  # keyboard device appears:
  mkPath = keyboard: rec {
    name = "kmonad-${keyboard.name}";
    value = {
      description = "KMonad trigger for ${keyboard.device}";
      wantedBy = [ "default.target" ];
      pathConfig.Unit = "${name}.service";
      pathConfig.PathExists = keyboard.device;
    };
  };

  # Build a systemd service that starts KMonad:
  mkService = keyboard: {
    name = "kmonad-${keyboard.name}";
    value = {
      description = "KMonad for ${keyboard.device}";
      script = "${cfg.package}/bin/kmonad -l info ${mkCfg keyboard}";
      serviceConfig.Restart = "no";
      serviceConfig.User = "kmonad";
      # Need video for brightnessctl command
      serviceConfig.SupplementaryGroups = [ "input" "uinput" "video" ];
      serviceConfig.Nice = -20;
    };
  };
in
{
  options.workstation.kmonad = {
    enable = lib.mkEnableOption "KMonad: An advanced keyboard manager.";

    package = lib.mkOption {
      type = lib.types.package;
      default = package;
      example = "pkgs.haskellPacakges.kmonad";
      description = "The KMonad package to use.";
    };

    keyboards = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule keyboard);
      default = { };
      description = "Keyboard configuration.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.groups.uinput = { };
    users.groups.kmonad = { };

    users.users.kmonad = {
      description = "KMonad system user.";
      group = "kmonad";
      isSystemUser = true;
    };

    services.udev.extraRules = ''
      # KMonad user access to /dev/uinput
      KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
    '';

    systemd.paths =
      builtins.listToAttrs
        (map mkPath (builtins.attrValues cfg.keyboards));

    systemd.services =
      builtins.listToAttrs
        (map mkService (builtins.attrValues cfg.keyboards));
  };
}
