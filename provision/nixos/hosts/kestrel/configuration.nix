# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, user, lib, ... }:
{
  nix = {
    # Flakes!
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";

    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  # Use zen kernel
  boot.kernelPackages = pkgs.linuxPackages_zen;

  hardware.bluetooth.enable = true;
  hardware.sensor.iio.enable = true;
  services.blueman.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "kestrel"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  programs.seahorse.enable = lib.mkForce false;
  programs.gnupg.agent = {                                                      
    enable = true;
    enableSSHSupport = true;
  };
  programs.ssh.askPassword = "";


  services.xserver = {
    enable = true;
    layout = "us";
    #displayManager.lightdm.greeters.mini = {
    #         enable = true;
    #         user = "${user}";
    #         extraConfig = ''
    #           # LightDM Mini Greeter Configuration
    #           #
    #           # To test your configuration out, run: lightdm --test-mode
    #           
    #           [greeter]
    #           # The user to login as.
    #           user = tstarr 
    #           # Whether to show the password input's label.
    #           show-password-label = true
    #           # The text of the password input's label.
    #           password-label-text = Password:
    #           # The text shown when an invalid password is entered. May be blank.
    #           invalid-password-text = Invalid Password
    #           # Show a blinking cursor in the password input.
    #           show-input-cursor = true
    #           # The text alignment for the password input. Possible values are:
    #           # "left", "center", or "right"
    #           password-alignment = right
    #           # The number of characters that should fit into the password input.
    #           # A value of -1 will use GTK's default width.
    #           # Note: The entered width is a suggestion, GTK may render a narrower input.
    #           password-input-width = -1
    #           # Show the background image on all monitors or just the primary monitor.
    #           show-image-on-all-monitors = false
    #           # Show system info above the password input.
    #           # `<user>@<hostname>` is shown on the left side, & current time on the right.
    #           show-sys-info = false
    #           
    #           
    #           [greeter-hotkeys]
    #           # The modifier key used to trigger hotkeys. Possible values are:
    #           # "alt", "control" or "meta"
    #           # meta is also known as the "Windows"/"Super" key
    #           mod-key = meta
    #           # Power management shortcuts (single-key, case-sensitive)
    #           shutdown-key = s
    #           restart-key = r
    #           hibernate-key = h
    #           suspend-key = u
    #           # Cycle through available sessions
    #           session-key = e
    #           
    #           
    #           [greeter-theme]
    #           # A color from X11's `rgb.txt` file, a quoted hex string(`"#rrggbb"`) or a
    #           # RGB color(`rgb(r,g,b)`) are all acceptable formats.
    #           
    #           # The font to use for all text
    #           font = "Sans"
    #           # The font size to use for all text
    #           font-size = 1em
    #           # The font weight to use for all text
    #           font-weight = bold
    #           # The font style to use for all text
    #           font-style = normal
    #           # The default text color
    #           text-color = "#080800"
    #           # The color of the error text
    #           error-color = "#F8F8F0"
    #           # An absolute path to an optional background image.
    #           # Note: The file should be somewhere that LightDM has permissions to read
    #           #       (e.g., /etc/lightdm/).
    #           background-image = ""
    #           # Background image size:
    #           # auto: unscaled
    #           # cover: scale image to fill screen space
    #           # contain: scale image to fit inside screen space
    #           # (more options: https://www.w3.org/TR/css-backgrounds-3/#background-size)
    #           background-image-size = auto
    #           # The screen's background color.
    #           background-color = "#1B1D1E"
    #           # The password window's background color
    #           window-color = "#F92672"
    #           # The color of the password window's border
    #           border-color = "#080800"
    #           # The width of the password window's border.
    #           # A trailing `px` is required.
    #           border-width = 2px
    #           # The pixels of empty space around the password input.
    #           # Do not include a trailing `px`.
    #           layout-space = 15
    #           # The character used to mask your password. Possible values are:
    #           # "-1", "0", or a single unicode character(including emojis)
    #           # A value of -1 uses the default bullet & 0 displays no characters when you
    #           # type your password.
    #           password-character = -1
    #           # The color of the text in the password input.
    #           password-color = "#F8F8F0"
    #           # The background color of the password input.
    #           password-background-color = "#1B1D1E"
    #           # The color of the password input's border.
    #           # Falls back to `border-color` if missing.
    #           password-border-color = "#080800"
    #           # The width of the password input's border.
    #           # Falls back to `border-width` if missing.
    #           password-border-width = 2px
    #           # The border radius of the password input.
    #           password-border-radius = 0.341125em
    #           # Override font for system info
    #           # Falls back to `font` if missing.
    #           sys-info-font = "Mono"
    #           # Set font size of system info
    #           # Falls back to `font-size` if missing.
    #           sys-info-font-size = 0.8em
    #           # Override color for system info text
    #           # Falls back to `text-color` if missing.
    #           #sys-info-color = "#080800"
    #           # Margins around the system info section
    #           # The default `-5px -5px -5px` works well with the password label enabled.
    #           # If you have the label disabled, you might want to try `-5px -5px 0px`
    #           sys-info-margin = -5px -5px -5px
    #         '';
    #     };

    desktopManager.xterm.enable = false;

    # Use nvidia drivers
    videoDrivers = [ "nvidia" ];
    screenSection = ''
      Option "metamodes" "DP-4: 2560x1440_144 +2560+0, HDMI-0: 2560x1440_144 +0+0"
    '';

    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;

    # Use the xmonad wm
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
  };


  hardware.opengl.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];
  services.avahi.enable = true;
  services.avahi.nssmdns = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Add non-free packages
  nixpkgs.config.allowUnfree = true;


  fonts.fonts = with pkgs; [
    nerdfonts
  ];

  services = {
    # USB automount
    gvfs.enable = true;

    # File sync
    syncthing = {
      enable = true;
      user = "tstarr";
      dataDir = "/home/tstarr/sync";
      configDir = "/home/tstarr/.config/syncthing";
    };
  };

  programs.steam = {
    enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    neovim
    git
    haskellPackages.xmobar
    killall
    pciutils
    syncthing
    nnn
    xidlehook
    pamixer
    vifm
    play-with-mpv
    mpv
    autokey
    gnome-extension-manager
    gnome.gnome-tweaks
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  system.stateVersion = "22.05"; # Did you read the comment?

  programs.bash.shellAliases = {
    # Nix rebuild and switch
    nr = "cd ~/.setup && sudo nixos-rebuild switch --flake .# && cd -";
    sd = "cd ~/.setup/local/stow && stow . -t /home/${user} --no-folding && cd -";
  };

  virtualisation.docker.enable = true;

  imports = [ ../../modules ];

  modules = {
    stow.enable = true;
    services = {
      samba.enable = true;
      vfio.enable = true;
    };
    devel = {
      python.enable = true;
      engineering.enable = true;
    };
    editors = {
      emacs.enable = true;
    };
  };
}
