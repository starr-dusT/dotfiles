{ config, lib, pkgs, ... }:
{
  # Enable NAT
  networking.nat = {
    enable = true;
    enableIPv6 = true;
    externalInterface = "enp4s0";
    internalInterfaces = [ "wg0" ];
  };
  
  # Open ports in the firewall
  networking.firewall = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 51820 ];
  };
    
  networking.wg-quick.interfaces = {
    # "wg0" is the network interface name. You can name the interface arbitrarily.
    wg0 = {
      # Determines the IP/IPv6 address and subnet of the client's end of the tunnel interface
      address = [ "192.168.2.1/24" ];
      # The port that WireGuard listens to - recommended that this be changed from default
      listenPort = 51820;
      # Path to the server's private key
      privateKeyFile = "/engi/apps/wireguard/private";
  
      # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
      postUp = ''
        ${pkgs.iptables}/bin/iptables -A FORWARD -i %i -j ACCEPT 
        ${pkgs.iptables}/bin/iptables -A FORWARD -o %i -j ACCEPT 
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o enp4s0 -j MASQUERADE
  
      '';
  
      # Undo the above
      preDown = ''
        ${pkgs.iptables}/bin/iptables -D FORWARD -i %i -j ACCEPT 
        ${pkgs.iptables}/bin/iptables -D FORWARD -o %i -j ACCEPT 
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -o enp4s0 -j MASQUERADE
      '';
  
      peers = [
        { 
          # Adjudicator
          publicKey = "r2/IeYCO1T+l248387wUBoNnc2DK9O8pHcIr/NQqezM=";
          allowedIPs = [ "192.168.2.2/32" ];
        }
        { 
          # Kestrel
          publicKey = "hPso657fppLYvBU31Rtqqg792JEoPv7r82JgLoF8S2Y=";
          allowedIPs = [ "192.168.2.3/32" ];
        }
        # More peers can be added here.
      ];
    };
  };
}
