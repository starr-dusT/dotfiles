{ config, lib, pkgs, ... }:

let cfg = config.modules.services.wireguard-server;
in {
  options.modules.services.wireguard-server.enable = lib.mkEnableOption "wireguard-server";
  config = lib.mkIf cfg.enable {
    # enable NAT
    networking.nat.enable = true;
    networking.nat.externalInterface = "enp4s0";
    networking.nat.internalInterfaces = [ "wg0" ];
    networking.firewall = {
      allowedUDPPorts = [ 51820 ];
    };

    networking.wireguard.interfaces = {
      # "wg0" is the network interface name. You can name the interface arbitrarily.
      wg0 = {
        # Determines the IP address and subnet of the server's end of the tunnel interface.
        ips = [ "10.100.0.1/24" ];

        # The port that WireGuard listens to. Must be accessible by the client.
        listenPort = 51820;

        # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
        # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
        postSetup = ''
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';

        # This undoes the above command
        postShutdown = ''
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';

        # Path to the private key file.
        #
        # Note: The private key can also be included inline via the privateKey option,
        # but this makes the private key world-readable; thus, using privateKeyFile is
        # recommended.
        privateKeyFile = "/engi/apps/wireguard/torus-adjudicator";

        peers = [
          # List of allowed peers.
          { # Feel free to give a meaning full name
            # Public key of the peer (not a file path).
            publicKey = "boy07PYDJT8TuG6Zkwg1KqhKeoakc7GH7UxAw9NuSjE";
            # List of IPs assigned to this peer within the tunnel subnet. Used to configure routing.
            allowedIPs = [ "10.100.0.2/32" ];
          }
        ];
      };
    };
  };
}
