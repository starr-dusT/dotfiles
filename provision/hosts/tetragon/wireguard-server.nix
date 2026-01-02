{ pkgs, ... }:
{
  networking.nat = {
    enable = true;
    enableIPv6 = true;
    externalInterface = "enp1s0f0";
    internalInterfaces = [ "wg0" ];
  };

  networking.firewall = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [
      53
      51820
    ];
  };

  age.secrets."wireguard/torus".file = ../../././secrets/wireguard/torus.age;

  networking.wg-quick.interfaces = {
    wg0 = {
      address = [ "69.69.2.1/24" ];
      listenPort = 51820;
      privateKeyFile = "/run/agenix/wireguard/torus";

      postUp = ''
        ${pkgs.iptables}/bin/iptables -A FORWARD -i %i -j ACCEPT 
        ${pkgs.iptables}/bin/iptables -A FORWARD -o %i -j ACCEPT 
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o enp4s0 -j MASQUERADE
      '';

      preDown = ''
        ${pkgs.iptables}/bin/iptables -D FORWARD -i %i -j ACCEPT 
        ${pkgs.iptables}/bin/iptables -D FORWARD -o %i -j ACCEPT 
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -o enp4s0 -j MASQUERADE
      '';

      peers = [
        {
          # Adjudicator
          publicKey = "xc8FkV/oElnHUBVLKKtWTseKVQHTZyhsHn2TfTxOGmU=";
          allowedIPs = [ "69.69.2.2/32" ];
        }
        {
          # Kestrel
          publicKey = "hPso657fppLYvBU31Rtqqg792JEoPv7r82JgLoF8S2Y=";
          allowedIPs = [ "69.69.2.3/32" ];
        }
        {
          # Macbook 
          publicKey = "wE9rlXtGXbmb9orNCAHGnYWafPRjwphrfc+yxAAzeiY=";
          allowedIPs = [ "69.69.2.4/32" ];
        }
      ];
    };
  };
}
