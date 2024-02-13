{ pkgs, config, lib, ... }:

let
in {
  programs.thunderbird = {
    enable = true;

    profiles.hm = { isDefault = true; };
  };
}
