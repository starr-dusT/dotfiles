let
  kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM2iE16XVkriD0x6GhnqmvGDA1qNBibvHVIi5xY+c7Iu";
  torus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN71z5g6QyCn5Go0Wm+NOSF4f22xOOCvtIA3IM4KzSpG";
  bulwark = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG52QybtJrt0KU7iJGyiBBoDCcd0AXoy+wFi+9fBsopk";
  systems = [ kestrel torus bulwark ];

  tstarr_kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINr2BUUToMswbAbxZMXarl2pQEomM+jADyZbEK31VGu/";
  tstarr_torus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKhxsVgd8DH8c0zckjMUxSJrTimU709JLCgDGBMFoNxQ";
  tstarr_osprey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGQEjr+yK4zdnV9kBUMo9fopsJbvF+TfQlVQexBCwuwB";
  users = [ tstarr_kestrel tstarr_torus tstarr_osprey ];
in
{
  "git/github_personal.age".publicKeys = users ++ systems;
  "emu/switch/prod.keys.age".publicKeys = users ++ systems;
  "emu/switch/title.keys.age".publicKeys = users ++ systems;
  "wireguard/kestrel.age".publicKeys = users ++ systems;
  "wireguard/torus.age".publicKeys = systems;
  "wireguard/bulwark.age".publicKeys = systems;
  "wireguard/osprey.age".publicKeys = [ tstarr_osprey ] ++ systems;
  "git/gitea-runner-1.age".publicKeys = systems;
  "nextcloud/password.age".publicKeys = systems;
  "ssh/kestrel/id_ed25519.age".publicKeys = [ tstarr_kestrel ] ++ systems;
  "ssh/kestrel/id_ed25519.pub.age".publicKeys = users ++ systems;
  "ssh/torus/id_ed25519.age".publicKeys = [ tstarr_torus ] ++ systems;
  "ssh/torus/id_ed25519.pub.age".publicKeys = users ++ systems;
  "borg/torus/password.age".publicKeys = [ tstarr_torus ] ++ systems;
  "borg/rsync/id_rsa.age".publicKeys = users ++ systems;
  "borg/rsync/id_rsa.pub.age".publicKeys = users ++ systems;
}

