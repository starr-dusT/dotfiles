let
  kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJgmH6IJCOMHs/FMCOeKu3tcMxICkjAXTJmIJNuLUn5d";
  shivan = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMo2VKfITJn+noaGhah1O7JvH0oRl11YbZprwdISKtMe";
  stormwalker = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHvAGrh01zvH8dbj8NdrNxkRcQ/pRt27WjK6uHNNoG4n";
  tetragon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKAUEk/LRycmNstE6RhC7I7Ca8AgbK973ReVvGvcZwQP";
  torus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN71z5g6QyCn5Go0Wm+NOSF4f22xOOCvtIA3IM4KzSpG";
  vortex-1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHfi08D2wP+j+VCoivH5ji+ZUXcBnsevsLFMZnPoi8mf";
  vortex-2 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP37A1dzBXciv+MoB4vCrSerSHg5FX8Kp0yhJ0tE0azT";
  vortex-3 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHMzPDBm5oJs0Sc+hUMzbOvTnHPhjo2e0fsb7NAsG9L+";
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
  "ssh/kestrel/id_ed25519.age".publicKeys = systems;
  "ssh/kestrel/id_ed25519.pub.age".publicKeys = systems;
  "ssh/torus/id_ed25519.age".publicKeys = systems;
  "ssh/torus/id_ed25519.pub.age".publicKeys = systems;
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
