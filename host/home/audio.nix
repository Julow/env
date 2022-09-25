{ ... }:

{
  # Specify audio format, USB DAC is not reliable otherwise.
  # https://wiki.archlinux.org/title/PipeWire#High_latency_with_USB_DACs_(e.g._Schiit_DACs)
  services.pipewire.media-session.config.alsa-monitor.rules = [{
    # Matches all cards
    matches = [{ "node.name" = "~alsa_output.*"; }];
    actions.update-props = {
      "audio.format" = "S24_3LE";
      "audio.rate" = 96000;
      "audio.channels" = 2;
      "api.alsa.period-size" = 256;
    };
  }];
}
