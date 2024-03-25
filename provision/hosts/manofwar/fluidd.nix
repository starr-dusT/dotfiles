{ config, lib, pkgs, user, ... }:
  services.fluidd.enable = true;
  services.fluidd.nginx.locations."/webcam".proxyPass = "http://127.0.0.1:8080/stream";
  services.nginx.clientMaxBodySize = "1000m";
}
