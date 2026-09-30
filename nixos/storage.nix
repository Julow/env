{ config, ... }:

{
  # Don't record access times to improve performances, ssd lifespan and
  # privacy.
  fileSystems."/".options = [ "noatime" ];
  fileSystems."/boot".options = [ "noatime" ];

  boot.tmp.useTmpfs = true;

  # Automatic GC
  nix.gc = {
    automatic = true;
    dates = "weekly";
    delete_generations = "+5";
  };
}
