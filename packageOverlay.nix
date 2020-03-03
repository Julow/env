self: super:

with super;

{
  dunst = dunst.override { dunstify = true; };

  xterm = xterm.overrideAttrs (attrs: {
    configureFlags = attrs.configureFlags ++ [ "--enable-exec-xterm" ];
  });
}
