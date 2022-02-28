{ pkgs, config, ... }:
{
  programs.mpv = {
    enable = true;
    config = {
      fullscreen = true;
      hwdec = "auto-copy";
      # Fix cutting and tearing because mpv can't decode fast enough
      vo = "xv";
      vd-lavc-fast = true;
      # Restrict resolution and prefer avc1. Otherwise mpv can't decode fast enough.
      ytdl-format = "bestvideo[height<=1080][vcodec^=avc1]+bestaudio/best";
    };
    profiles = {
      audio = {
        # Show progress for audio files
        osd-level = 3;
        osd-align-x = "center";
        osd-align-y = "center";
      };
      image = {
        pause = true; # Don't close images after 1 second
      };
      "extension.jpg".profile = "image";
      "extension.jpeg".profile = "image";
      "extension.png".profile = "image";
      "extension.mp3".profile = "audio";
    };
  };
}
