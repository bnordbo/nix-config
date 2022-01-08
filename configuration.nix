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
        "input"
        "uinput"
        "video"
        "wheel"
      ];
    };
  };

  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.users.bn = { pkgs, ... }: {

    home.shellAliases = {
      "ec" = "emacsclient -nq";
      "myip" = "curl http://ipinfo.io/ip";
    };

    programs.bash = {
      enable = true;
    };

    programs.emacs = {
      enable = true;
      extraPackages = (epkgs: [
        epkgs.org-pdftools
      ]);
    };

    programs.home-manager.enable = true;
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
      user = "bn";
      dataDir = "/home/bn";
      overrideDevices = true;
      overrideFolders = true;
      devices = {
        "snapp" = { id = "KRPCRQH-QGKGF66-QPXOEZU-ILA6TZ3-CL7HTP4-6RPBWUA-GUPI6M5-ZK5ZYA5"; };
      };
      folders = {
        "Contexts" = {
          path = "/home/bn/Contexts";
          devices = [ "snapp" ];
        };
        "Repository" = {
          path = "/home/bn/Repository";
          devices = [ "snapp" ];
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
      planck = {
        device = "/dev/input/by-id/usb-OLKB_Planck_0-event-kbd";
        config = builtins.readFile ./support/keyboard/planck.kbd;
      };
    };
  };

  fonts.fonts = with pkgs; [
    libre-baskerville
    monoid
  ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # nix = {
  #   package = pkgs.nix_2_4; # TODO: Remove in NixOS 22.05
  #   extraOptions = ''
  #     experimental-features = nix-command flakes
  #   '';
  #  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
}
