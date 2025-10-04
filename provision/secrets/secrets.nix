let
  kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJgmH6IJCOMHs/FMCOeKu3tcMxICkjAXTJmIJNuLUn5d";
  torus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN71z5g6QyCn5Go0Wm+NOSF4f22xOOCvtIA3IM4KzSpG";
  stormwalker = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHvAGrh01zvH8dbj8NdrNxkRcQ/pRt27WjK6uHNNoG4n";
  shivan = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMo2VKfITJn+noaGhah1O7JvH0oRl11YbZprwdISKtMe";
  systems = [ kestrel torus shivan stormwalker ];
in
{
  "cloudflared/tstarr.json.age".publicKeys = systems;
  "git/github_personal.age".publicKeys = systems;
  "git/gitea-runner-1.age".publicKeys = systems;
  "wireguard/kestrel.age".publicKeys = systems;
  "wireguard/torus.age".publicKeys = systems;
  "wireguard/bulwark.age".publicKeys = systems;
  "wireguard/osprey.age".publicKeys = systems;
  "ssh/kestrel/id_ed25519.age".publicKeys = systems;
  "ssh/kestrel/id_ed25519.pub.age".publicKeys = systems;
  "ssh/torus/id_ed25519.age".publicKeys = systems;
  "ssh/torus/id_ed25519.pub.age".publicKeys = systems;
  "borg/torus/password.age".publicKeys = systems;
  "borg/rsync/id_rsa.age".publicKeys = systems;
  "borg/rsync/id_rsa.pub.age".publicKeys = systems;
  "syncthing/kestrel/key.pem.age".publicKeys = systems;
  "syncthing/kestrel/cert.pem.age".publicKeys = systems;
  "syncthing/bulwark/key.pem.age".publicKeys = systems;
  "syncthing/bulwark/cert.pem.age".publicKeys = systems;
  "syncthing/torus/key.pem.age".publicKeys = systems;
  "syncthing/torus/cert.pem.age".publicKeys = systems;
}

