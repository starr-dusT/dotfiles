{
  config,
  pkgs,
  user,
  lib,
  hostname,
  ...
}:
let
  interface = if "${hostname}" == "vortex-1" then "enp2s0" else "enp1s0f0";
  address4 =
    if "${hostname}" == "vortex-1" then
      "69.69.1.11"
    else if "${hostname}" == "vortex-2" then
      "69.69.1.12"
    else
      "69.69.1.13";
  address6 =
    if "${hostname}" == "vortex-1" then
      "2607:9b00:620a:9c00:ca4a:2454:50c7:4e2e"
    else if "${hostname}" == "vortex-2" then
      "2607:9b00:620a:9c00:a3c1:32df:32fe:6418"
    else
      "2607:9b00:620a:9c00:7385:15ff:6b2:d8de";
in
{
  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages_6_6;

  # Set networking options
  networking = {
    tempAddresses = "disabled";
    firewall.enable = false;
    useDHCP = false;
    defaultGateway = {
      address = "69.69.1.1";
      interface = "${interface}";
    };
    defaultGateway6 = {
      address = "fe80::1";
      interface = "${interface}";
    };
    nameservers = [
      "69.69.1.10"
    ];

    interfaces = {
      "${interface}" = {
        useDHCP = false;
        ipv4.addresses = [
          {
            address = "${address4}";
            prefixLength = 24;
          }
        ];
        ipv6.addresses = [
          {
            address = "${address6}";
            prefixLength = 64;
          }
        ];
      };
    };
  };

  users.users."${user}".openssh.authorizedKeys.keyFiles = [
    ../../secrets/ssh/pubs/kestrel.pub
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

  age.secrets."kube/token".file = ../../secrets/kube/token.age;

  # Needed for flux
  environment.variables.KUBECONFIG = "/etc/rancher/k3s/k3s.yaml";

  # Allow user to read k3s.yaml
  systemd.tmpfiles.rules = [
    "f /etc/rancher/k3s/k3s.yaml 0600 ${user} users -"
  ];

  services.k3s = {
    enable = true;
    role = if (lib.strings.hasInfix "vortex" "${hostname}") then "server" else "agent";
    clusterInit = if "${hostname}" == "vortex-1" then true else false;
    serverAddr = if "${hostname}" == "vortex-1" then "" else "https://69.69.1.11:6443";
    tokenFile = config.age.secrets."kube/token".path;
    nodeIP = "${address4},${address6}";
  };

  # Modules
  modules = {
    core = {
      physical.enable = true;
    };
    optional = {
      development = {
        programming.enable = true;
      };
      services = {
        node-exporter.enable = true;
      };
    };
  };
}
