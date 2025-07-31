{ pkgs, config, lib, ... }:
with lib;
let
  home = config.home.homeDirectory;
  bookmarks = [ "Downloads" "notes" "notes/drive" ];
in {
  xdg.configFile."gtk-3.0/bookmarks".text =
    concatMapStringsSep "\n" (p: "file://${home}/${p}") bookmarks;

  xdg.configFile."pcmanfm/default/pcmanfm.conf".force = true;
  xdg.configFile."pcmanfm/default/pcmanfm.conf".text = ''
    [config]
    bm_open_method=0

    [volume]
    mount_on_startup=0
    mount_removable=0
    autorun=1

    [ui]
    always_show_tabs=0
    max_tab_chars=32
    splitter_pos=320
    media_in_new_tab=0
    desktop_folder_new_win=0
    side_pane_mode=places
    view_mode=list
    show_hidden=0
    sort=mtime;descending;
    toolbar=newtab;navigation;
    show_statusbar=1
    pathbar_mode_buttons=0
  '';

  xdg.configFile."libfm/libfm.conf".force = true;
  xdg.configFile."libfm/libfm.conf".text = ''
    [config]
    single_click=0
    use_trash=1
    confirm_del=1
    confirm_trash=0
    advanced_mode=0
    si_unit=0
    force_startup_notify=1
    backup_as_hidden=1
    no_usb_trash=1
    no_child_non_expandable=0
    show_full_names=0
    only_user_templates=0
    template_run_app=0
    template_type_once=0
    auto_selection_delay=600
    drop_default_action=auto
    defer_content_test=0
    quick_exec=0
    thumbnail_local=1
    thumbnail_max=16000
    smart_desktop_autodrop=0

    [ui]
    big_icon_size=48
    small_icon_size=16
    pane_icon_size=16
    thumbnail_size=256
    show_thumbnail=1
    shadow_hidden=0

    [places]
    places_home=1
    places_desktop=0
    places_root=1
    places_computer=0
    places_trash=1
    places_applications=0
    places_network=0
    places_unmounted=1
  '';
}
