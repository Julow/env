{ pkgs, config, ... }: {
  programs.less = {
    enable = true;
    options = [
      "--ignore-case"
      "--tabs=4"
      "--use-color"
      "-DBr-$Cr-$"
      "--jump-target=5"
      "--mouse"
      "--wheel-lines=3"
    ];
  };

}
