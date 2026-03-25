{ self, ... }:
{
  flake.modules.nixos.vortex =
    { config, pkgs, ... }:
    {
      imports = [
        self.modules.nixos.core
        self.modules.nixos.programming
        self.modules.nixos.node-exporter
      ];

      # Use normal kernel
      boot.kernelPackages = pkgs.linuxPackages_6_6;

      networking = {
        tempAddresses = "disabled";
        firewall.enable = false;
        useDHCP = false;
        defaultGateway = {
          address = "69.69.1.1";
        };
        defaultGateway6 = {
          address = "fe80::1";
        };
        nameservers = [
          "69.69.1.10"
        ];
      };

      users.users."${config.preferences.user}".openssh.authorizedKeys.keyFiles = [
        ../../../secrets/ssh/pubs/kestrel.pub
      ];

      # k3s setup
      networking.firewall.allowedTCPPorts = [
        6443 # Kubernetes API
        10250 # Kubelet API
        2379
        2380
      ];
      networking.firewall.allowedUDPPorts = [
        8472 # Flannel VXLAN
      ];

      environment.systemPackages = with pkgs; [
        age
        sops
        kubernetes-helm
        fluxcd
        jq
      ];

      age.secrets."kube/token".file = ../../../secrets/kube/token.age;

      # Needed for flux
      environment.variables.KUBECONFIG = "/etc/rancher/k3s/k3s.yaml";

      # Allow user to read k3s.yaml
      systemd.tmpfiles.rules = [
        "f /etc/rancher/k3s/k3s.yaml 0600 ${config.preferences.user} users -"
      ];

      services.k3s = {
        enable = true;
        role = "server";
        tokenFile = config.age.secrets."kube/token".path;
      };
    };
}
