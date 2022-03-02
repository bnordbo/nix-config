{ config, pkgs, lib, ... }:

# Quick fix to avoid checking in secrets
let
  confidential = import ./.confidential.nix;
in

{
  imports =
    [
      ./hardware-configuration.nix
      ./desktop.nix
      ./home
      ./workstation/kmonad.nix
      <home-manager/nixos>
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "nusse";

    wireless.enable = true;
    wireless.networks = {
      geitecasa = {
        psk = confidential.geitecasaPsk;
      };
    };

    useDHCP = false;
    interfaces.wlan0.useDHCP = true;

    hosts = {
      "172.16.8.20" = ["nurke" "nurke.local"];
    };
  };

  time.timeZone = "Europe/Amsterdam";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  console = {
    useXkbConfig = true;
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  };

  services.printing.enable = true;

  sound.enable = true;

  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.naturalScrolling = true;

  security.sudo.extraRules = [
    { users = [ "bn" ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];

  users.mutableUsers = false;

  users.users = {
    bn = {
      isNormalUser = true;
      description = "Bjørn Nordbø";
      hashedPassword = confidential.hashedPassword;
      openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDYU/rtF7vYFljR0womC+toIl9WlKs826pGR+fryHyu/rPE8ke+3t+j87XNZhRRrFFb7tNptGYik3+mAc6MRl3C9Zj87m+coKZ0aIrRff7/an+EwiPHhGtOAEbzMveEzb+7LrvaG3FpdOdAdWtSalFFveX81aDGZxdOp1vn3aOdJMG8BvWkxiwYhuoopeLeywbR10yhy9qd0w8IhD/tzzorcHJBBwqvqgHjTe+nVgZfvT3nqscRln9JYB7CrwQjz6/dK/EniU2nhMwWYxn/ChF96dxvGk/EcUdjxGyFw6ph7wmDPvkhxMUAMV+HqCWBFTCSy8mHGKBx1d1wxeB2r8Q9 bn@snapp.local" ];
      extraGroups = [
        "docker"
        "input"
        "uinput"
        "video"
        "wheel"
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    vim
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  services = {
    openssh.enable = true;

    syncthing = {
      enable = true;
      relay.enable = false;
      user = "bn";
      dataDir = "/home/bn";
      overrideDevices = true;
      overrideFolders = true;
      devices = {
        "nurke" = { id = "QOWD2RF-ZUG2ARY-EZBVUUT-U4MBPZY-DJRBYW6-JK3RBXY-BGEQHM3-SZXTQAT"; };
      };
      folders = {
        "Contexts" = {
          path = "/home/bn/Contexts";
          devices = [ "nurke" ];
        };
        "Repository" = {
          path = "/home/bn/Repository";
          devices = [ "nurke" ];
        };
      };
    };

    upower = {
      enable = true;
    };
  };

  workstation.kmonad = {
    enable = true;
    keyboards = {
      internal = {
        device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
        config = builtins.readFile ./support/keyboard/thinkpad_60_no.kbd;
      };
    };
  };

  fonts.fonts = with pkgs; [
    libre-baskerville
    monoid
  ];

  virtualisation = {
    docker = {
      enable = true;
    };
  };

  nix = {
    package = pkgs.nix_2_4; # TODO: Remove in NixOS 22.05
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
