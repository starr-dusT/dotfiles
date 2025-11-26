let
  kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJgmH6IJCOMHs/FMCOeKu3tcMxICkjAXTJmIJNuLUn5d";
  shivan = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMo2VKfITJn+noaGhah1O7JvH0oRl11YbZprwdISKtMe";
  stormwalker = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHvAGrh01zvH8dbj8NdrNxkRcQ/pRt27WjK6uHNNoG4n";
  tetragon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKAUEk/LRycmNstE6RhC7I7Ca8AgbK973ReVvGvcZwQP";
  torus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN71z5g6QyCn5Go0Wm+NOSF4f22xOOCvtIA3IM4KzSpG";
  vortex-1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJTiqwFJbjHx9WQY8V0YSK2cKwgUU3aPf8d7FX8a1h3P";
  vortex-2 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPEuih/qir6ygAEb8orN0AO4V8P1jmYQx6HmBF5IGbp4";
  vortex-3 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIC4fM5OTEHibyAO0P9b6MdYR8pD0cwQ7t4L6X5YDKd4";
  systems = [
    kestrel
    shivan
    stormwalker
    tetragon
    torus
    vortex-1
    vortex-2
    vortex-3
  ];
in
{
  "cloudflared/tstarr.json.age".publicKeys = systems;
  "git/github_personal.age".publicKeys = systems;
  "git/gitea-runner-1.age".publicKeys = systems;
  "wireguard/kestrel.age".publicKeys = systems;
  "wireguard/torus.age".publicKeys = systems;
  "ssh/kestrel.age".publicKeys = systems;
  "ssh/torus.age".publicKeys = systems;
  "borg/torus/password.age".publicKeys = systems;
  "borg/torus/discord_webhook.age".publicKeys = systems;
  "borg/rsync/id_rsa.age".publicKeys = systems;
  "borg/rsync/id_rsa.pub.age".publicKeys = systems;
  "kube/token.age".publicKeys = systems;
  "syncthing/kestrel/key.pem.age".publicKeys = systems;
  "syncthing/kestrel/cert.pem.age".publicKeys = systems;
  "syncthing/torus/key.pem.age".publicKeys = systems;
  "syncthing/torus/cert.pem.age".publicKeys = systems;
  "syncthing/stormwalker/key.pem.age".publicKeys = systems;
  "syncthing/stormwalker/cert.pem.age".publicKeys = systems;
  "smb/torus.age".publicKeys = systems;
}
