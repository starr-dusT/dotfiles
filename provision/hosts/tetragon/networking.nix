{ pkgs, ... }:
{
  networking = {
    useDHCP = false;
    hostName = "tetragon";
    nameservers = [ "8.8.8.8" "4.4.4.4" ];
    
    interfaces = {
      
    };
  };
}
