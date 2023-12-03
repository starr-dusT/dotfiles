{ config, lib, pkgs, user, ... }:
{
  networking.firewall.allowedTCPPorts = [ 1883 ];
  services.home-assistant.config.mqtt = { };

  services.mosquitto = {
    enable = true;
    listeners = [{
      users.tstarr = {
        acl = [ "readwrite #" ];
        hashedPassword = "$7$101$fWS64zbpdeUWd8fA$A31ESjDqooTB9BTYA9dvdU20Cwpxc3wmXfm7QLEJYZzd+UkbCVG1Ic7VsD9BBghka0WQZgkQczzag/t09BZ+5w==";
      };
    }];
  };
}
