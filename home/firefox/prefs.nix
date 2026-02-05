{
  # Behavior
  "browser.tabs.closeWindowWithLastTab" = false;
  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
  "reader.parse-on-load.force-enabled" = true;

  # Experimental features
  "browser.urlbar.suggest.calculator" = true;
  "browser.urlbar.unitConversion.enabled" = true;

  # Vertical tabs
  "browser.uiCustomization.navBarWhenVerticalTabs" = ''
    [
      "back-button", "vertical-spacer", "forward-button", "urlbar-container",
      "downloads-button", "fxa-toolbar-menu-button",
      "reset-pbm-toolbar-button", "unified-extensions-button"
    ]
  '';
  "sidebar.verticalTabs" = true;

  # UI
  "browser.uiCustomization.state" = ''
    {
        "placements": {
            "widget-overflow-fixed-list": [],
            "nav-bar": [ "back-button", "forward-button", "urlbar-container", "downloads-button", "fxa-toolbar-menu-button" ],
            "toolbar-menubar": [ "menubar-items" ],
            "TabsToolbar": [ ],
            "vertical-tabs": [ "tabbrowser-tabs" ],
            "PersonalToolbar": [ ]
        },
        "nav-bar": [
            "back-button",
            "vertical-spacer",
            "forward-button",
            "urlbar-container",
            "downloads-button",
            "reset-pbm-toolbar-button",
            "unified-extensions-button"
        ],
        "seen": [
          "redirector_einaregilsson_com-browser-action",
          "_d7742d87-e61d-4b78-b8a1-b469842139fa_-browser-action",
          "jid1-bofifl9vbdl2zq_jetpack-browser-action",
          "jid1-mnnxcxisbpnsxq_jetpack-browser-action",
          "ublock0_raymondhill_net-browser-action",
          "_74145f27-f039-47ce-a470-a662b129930a_-browser-action",
          "_85b42b8f-49cd-4935-aeca-a6b32dd6ac9f_-browser-action",
          "_a655a6b2-69a5-40de-a3b8-3f7f200c95a7_-browser-action",
          "developer-button",
          "fxa-toolbar-menu-button",
          "screenshot-button"
        ],
        "dirtyAreaCache": [
            "nav-bar",
            "PersonalToolbar",
            "toolbar-menubar",
            "TabsToolbar",
            "widget-overflow-fixed-list",
            "unified-extensions-area",
            "vertical-tabs"
        ],
        "currentVersion": 23,
        "newElementCount": 7
    }
  '';

  # Force HTTPS
  "dom.security.https_only_mode" = true;
  "dom.security.https_only_mode_ever_enabled" = true;

  # Disable AI
  "browser.ml.enable" = true;
  "browser.tabs.groups.smart.enabled" = true;
  "extensions.ml.enabled" = true;
  "sidebar.notification.badge.aichat" = true;

  # Disable push notifications while sites are closed
  "dom.webnotifications.serviceworker.enabled" = false;
  # Explicitly enable notifications
  "dom.webnotifications.enabled" = true;
  "dom.pushconnection.enabled" = true;
  "dom.push.enabled" = true;

  # Disable annoyances
  "extensions.htmlaboutaddons.recommendations.enabled" = false;
  "browser.urlbar.showSearchSuggestionsFirst" = false;
  "browser.startup.homepage" = "about:blank";
  "browser.disableResetPrompt" = true;
  "browser.fixup.alternate.enabled" = false;
  "browser.newtab.preload" = false;
  "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
  "browser.newtabpage.enabled" = false;
  "browser.newtabpage.enhanced" = false;
  "browser.shell.checkDefaultBrowser" = false;
  "extensions.pocket.enabled" = false;
  "services.sync.prefs.sync.browser.newtabpage.activity-stream.showSponsoredTopSite" =
    false;
  "signon.autofillForms" = false;
  "browser.search.widget.inNavBar" = false;
  "browser.newtabpage.pinned" = "[]"; # Google would be "pinned" otherwise
  "dom.event.contextmenu.enabled" = false; # Prevent sites disabling right-click
  "browser.tabs.firefox-view" = false;

  # Disable password manager
  "signon.rememberSignons" = false;
  "extensions.formautofill.creditCards.enabled" = false;

  # Disable telemetry
  "toolkit.telemetry.archive.enabled" = false;
  "toolkit.telemetry.bhrPing.enabled" = false;
  "toolkit.telemetry.cachedClientID" = "";
  "toolkit.telemetry.enabled" = false; # Doesn't disable telemetry
  "toolkit.telemetry.firstShutdownPing.enabled" = false;
  "toolkit.telemetry.hybridContent.enabled" = false;
  "toolkit.telemetry.newProfilePing.enabled" = false;
  "toolkit.telemetry.prompted" = 2;
  "toolkit.telemetry.rejected" = true;
  "toolkit.telemetry.reportingpolicy.firstRun" = false;
  "toolkit.telemetry.server" = "";
  "toolkit.telemetry.shutdownPingSender.enabled" = false;
  "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.unifiedIsOptIn" = false;
  "toolkit.telemetry.updatePing.enabled" = false;
  "datareporting.policy.dataSubmissionEnabled" = false;
  "datareporting.healthreport.service.enabled" = false;
  "datareporting.healthreport.uploadEnabled" = false;

  # Disable malwares
  "app.normandy.api_url" = "";
  "app.normandy.enabled" = false;
  "app.shield.optoutstudies.enabled" = false;
  "app.update.auto" = false;
  "beacon.enabled" = false;
  "breakpad.reportURL" = "";
  "experiments.activeExperiment" = false;
  "experiments.enabled" = false;
  "experiments.manifest.uri" = "";
  "experiments.supported" = false;
  "network.allow-experiments" = false;

  # Disable safebrowsing
  "browser.safebrowsing.appRepURL" = "";
  "browser.safebrowsing.blockedURIs.enabled" = false;
  "browser.safebrowsing.downloads.enabled" = false;
  "browser.safebrowsing.downloads.remote.enabled" = false;
  "browser.safebrowsing.downloads.remote.url" = "";
  "browser.safebrowsing.enabled" = false;
  "browser.safebrowsing.malware.enabled" = false;
  "browser.safebrowsing.phishing.enabled" = false;

  # Would break copy and paste
  "dom.event.clipboardevents.enabled" = true;

  # Improve privacy
  "browser.selfsupport.url" = "";
  "browser.send_pings" = false;
  "browser.sessionstore.privacy_level" = 2;
  "browser.startup.homepage_override.mstone" = "ignore";
  "browser.tabs.crashReporting.sendReport" = false;
  "browser.urlbar.groupLabels.enabled" = false;
  "browser.urlbar.quicksuggest.enabled" = false;
  "browser.urlbar.speculativeConnect.enabled" = false;
  "device.sensors.ambientLight.enabled" = false;
  "device.sensors.enabled" = false;
  "device.sensors.motion.enabled" = false;
  "device.sensors.orientation.enabled" = false;
  "device.sensors.proximity.enabled" = false;
  "dom.battery.enabled" = false;
  "extensions.getAddons.cache.enabled" = false;
  "extensions.getAddons.showPane" = false;
  "extensions.greasemonkey.stats.optedin" = false;
  "extensions.greasemonkey.stats.url" = "";
  "extensions.screenshots.upload-disabled" = true;
  "extensions.shield-recipe-client.api_url" = "";
  "extensions.shield-recipe-client.enabled" = false;
  "extensions.webservice.discoverURL" = "";
  "network.IDN_show_punycode" = true;
  "network.cookie.cookieBehavior" = 1;
  "network.dns.disablePrefetch" = true;
  "network.dns.disablePrefetchFromHTTPS" = true;
  "network.http.referer.XOriginPolicy" = 2;
  "network.http.speculative-parallel-limit" = 0;
  "network.predictor.enable-prefetch" = false;
  "network.predictor.enabled" = false;
  "network.prefetch-next" = false;
  "privacy.donottrackheader.enabled" = true;
  "privacy.donottrackheader.value" = 1;
  "privacy.trackingprotection.cryptomining.enabled" = true;
  "privacy.trackingprotection.enabled" = true;
  "privacy.trackingprotection.fingerprinting.enabled" = true;
  "privacy.trackingprotection.pbmode.enabled" = true;
  "privacy.usercontext.about_newtab_segregation.enabled" = true;
  "security.ssl.disable_session_identifiers" = true;
  "webgl.renderer-string-override" = " ";
  "webgl.vendor-string-override" = " ";
}
