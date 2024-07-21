let
  kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM2iE16XVkriD0x6GhnqmvGDA1qNBibvHVIi5xY+c7Iu";
  systems = [ kestrel ];

  tstarr_kestrel = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINr2BUUToMswbAbxZMXarl2pQEomM+jADyZbEK31VGu/";
  users = [ tstarr_kestrel ];
in
{
  "git/github_personal.age".publicKeys = users ++ systems;
}
