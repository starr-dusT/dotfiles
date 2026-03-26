{ inputs, ... }:
{
  flake.modules.nixos.core =
    {
      pkgs,
      config,
      ...
    }:
    {
      imports = with inputs.self.modules.nixos; [
        nix
        home-manager
        physical
        ssh
        terminal
        git
        secrets
      ];

      time.timeZone = "America/Los_Angeles";
      i18n.defaultLocale = "en_US.UTF-8";

      users.users.${config.preferences.user} = {
        isNormalUser = true;
        extraGroups = [
          "audio"
          "dialout"
          "wheel"
          "input"
        ];
        shell = pkgs.bash;
      };

      system.stateVersion = "23.11";
    };
}
