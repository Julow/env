{ config, lib, pkgs, ... }:

# Remove older generations before running the garbage collector.
#
# Unlike the [--delete-older-than] option that can be passed to
# [nix.gc.options], this script uses [nix-env --delete-generations].
# This allows to specify a number of generation to keep, removing the risk of
# removing all older generations.
#
# This applies to every profiles (system, user profile, home-manager).
#
# This module requires [nix.gc.automatic = true;] to be set and must be enabled
# with [modules.gc_clean_profiles = true;].

with lib;

let
  conf = config.modules.gc_clean_profiles;

  clean_profiles = pkgs.writeShellScript "gc_clean_profiles.sh" ''
    PATH="${pkgs.nix}/bin:$PATH"
    . ${./clean_profiles.sh} ${escapeShellArg conf.delete_generations}
  '';

in {
  options.modules.gc_clean_profiles = {
    enable = mkEnableOption "gc_clean_profiles";

    delete_generations = mkOption {
      type = types.str;
      default = "+5";
      description =
        "Argument passed to [nix-env --delete-generations]. The default of '+5' means to keep the 5 most recent generations of each profiles.";
    };
  };

  config = mkIf conf.enable {
    assertions = [{
      assertion = conf.enable -> config.nix.gc.automatic;
      message = "modules.gc_clean_profiles requires nix.gc.automatic";
    }];

    systemd.services.gc_clean_profiles = {
      wantedBy = [ "nix-gc.service" ];
      before = [ "nix-gc.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = clean_profiles;
      };
    };
  };
}
