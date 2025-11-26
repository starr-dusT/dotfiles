{
  config,
  lib,
  user,
  hostname,
  pkgs,
  ...
}:

let
  cfg = config.modules.optional.programs.k3s;
in
{
  options.modules.optional.programs.k3s = with lib; {
    enable = lib.mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
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
      kubernetes-helm
      fluxcd
      jq
    ];

    age.secrets."kube/token" = {
      file = ../../../secrets/kube/token.age;
      owner = "${user}";
      group = "users";
    };

    services.k3s = {
      enable = true;
      role = if (lib.strings.hasInfix "vortex" "${hostname}") then "server" else "agent";
      clusterInit = if "${hostname}" == "vortex-1" then true else false;
      serverAddr = if "${hostname}" == "vortex-1" then "" else "https://192.168.2.88:6443";
      tokenFile = config.age.secrets."kube/token".path;
    };
  };
}
