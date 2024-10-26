let
  kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM2iE16XVkriD0x6GhnqmvGDA1qNBibvHVIi5xY+c7Iu";
  torus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN71z5g6QyCn5Go0Wm+NOSF4f22xOOCvtIA3IM4KzSpG";
  bulwark = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG52QybtJrt0KU7iJGyiBBoDCcd0AXoy+wFi+9fBsopk";
  osprey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINpYnahS9+WKJrM3ZpjZlMLL5V7iwJJqZml337VuG7Jq";
  systems = [ kestrel torus bulwark osprey ];
in
{
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
  "nextcloud/password.age".publicKeys = systems;
  "syncthing/kestrel/key.pem.age".publicKeys = systems;
  "syncthing/kestrel/cert.pem.age".publicKeys = systems;
  "syncthing/bulwark/key.pem.age".publicKeys = systems;
  "syncthing/bulwark/cert.pem.age".publicKeys = systems;
}

