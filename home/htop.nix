{ pkgs, config, ... }: {
  programs.htop.enable = true;
  programs.htop.settings = with config.lib.htop;
    {
      # Visual
      color_scheme = 3;
      highlight_base_name = 1;
      highlight_megabytes = 1;
      highlight_threads = 1;
      highlight_deleted_exe = 1;
      highlight_changes = 1;
      highlight_changes_delay_secs = 5;
      hide_function_bar = 2;
      # List
      tree_view = 1;
      show_program_path = 0;
      hide_kernel_threads = 1;
      hide_userland_threads = 1;
      sort_key = fields.CUTIME;
      fields = with fields; [
        PID
        CUTIME
        CSTIME
        M_PSS
        PERCENT_CPU
        PERCENT_MEM
        TIME
        IO_RATE
        COMM
      ];
      # Metters
      show_cpu_temperature = 1;
    } // (leftMeters [
      (bar "AllCPUs4")
      (bar "CPU")
      (text "Blank")
      (bar "Memory")
      (bar "Swap")
      (text "Blank")
      (text "Systemd")
    ]) // (rightMeters [
      (text "Uptime")
      (text "Battery")
      (text "Blank")
      (text "LoadAverage")
      (text "Tasks")
      (text "Blank")
      (text "DiskIO")
      (text "NetworkIO")
    ]);
}
