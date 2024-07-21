let
  kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM2iE16XVkriD0x6GhnqmvGDA1qNBibvHVIi5xY+c7Iu";
  torus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN71z5g6QyCn5Go0Wm+NOSF4f22xOOCvtIA3IM4KzSpG";
  systems = [ kestrel torus ];

  tstarr_kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINr2BUUToMswbAbxZMXarl2pQEomM+jADyZbEK31VGu/";
  tstarr_torus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKhxsVgd8DH8c0zckjMUxSJrTimU709JLCgDGBMFoNxQ";
  users = [ tstarr_kestrel tstarr_torus ];
in
{
  "git/github_personal.age".publicKeys = users ++ systems;
  "wireguard/kestrel.age".publicKeys = users ++ systems;
  "git/gitea-runner-1.age".publicKeys = [ torus tstarr_torus ];
  "nextcloud/password.age".publicKeys = [ torus tstarr_torus ];
}
