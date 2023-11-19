{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop.firefox;
    style = import ../../lib/style.nix;

in {
  options.modules.desktop.firefox.enable = lib.mkEnableOption "firefox";
  config = lib.mkIf cfg.enable {
    programs.firefox = {
      enable = true;

      package = pkgs.wrapFirefox (pkgs.firefox-unwrapped.override {
        waylandSupport = false;
        privacySupport = true;
        webrtcSupport = true;
      }) { };

      profiles."test" = {
        id = 1;
        settings = {
          "browser.uidensity" = 1;
        };
      };

      profiles."tstarr" = with style.color; {
        id = 0;
        # user.js
        # https://github.com/ghacksuserjs/ghacks-user.js/blob/master/user.js
        settings = {

          ##### STARTUP #####

          # Disable default browser check
          "browser.shell.checkDefaultBrowser" = false;

          # Disable Activity Stream telemetry
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.newtabpage.activity-stream.telemetry" = false;

          # Disable Activity Stream Snippets
          "browser.newtabpage.activity-stream.feeds.snippets" = false;
          "browser.newtabpage.activity-stream.asrouter.providers.snippets" = "";

          # Disable Activity Stream Top Stories, Pocket-based and/or sponsored content
          "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
          "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
          "browser.newtabpage.activity-stream.showSponsored" = false;
          "browser.newtabpage.activity-stream.feeds.discoverystreamfeed" = false;

          # Disable Activity Stream recent Highlights in the Library
          "browser.library.activity-stream.enabled" = false;


          ##### GEOLOCATION / LANGUAGE / LOCALE #####

          # Use Mozilla geolocation service instead of Google when geolocation is enabled
          "geo.provider.network.url" = "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%";

          # Disable using the OS's geolocation service
          "geo.provider.use_gpsd" = false;

          # Disable geographically specific results/search engines
          "browser.search.geoSpecificDefaults" = false;
          "browser.search.geoSpecificDefaults.url" = "";

          # Set preferred language for displaying web pages
          "intl.accept_languages" = "fr";


          ##### QUIET FOX #####

          # Disable auto-installing Firefox update
          "app.update.auto" = false;

          # Disable search engine updates
          "browser.search.update" = false;

          # Disable sending Flash crash reports
          "dom.ipc.plugins.flash.subprocess.crashreporter.enabled" = false;

          # Disable sending the URL of the website where a plugin crashed
          "dom.ipc.plugins.reportCrashURL" = false;

          # Disable about:addons' Recommendations pane (uses Google Analytics)
          "extensions.getAddons.showPane" = false;

          # Disable recommendations in about:addons' Extensions and Themes panes
          "extensions.htmlaboutaddons.recommendations.enabled" = false;

          # Disable telemetry
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.server" = "data:,";
          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.newProfilePing.enabled" = false;
          "toolkit.telemetry.shutdownPingSender.enabled" = false;
          "toolkit.telemetry.updatePing.enabled" = false;
          "toolkit.telemetry.bhrPing.enabled" = false;
          "toolkit.telemetry.firstShutdownPing.enabled" = false;

          # Disable Telemetry Coverage
          "toolkit.telemetry.coverage.opt-out" = true;
          "toolkit.coverage.opt-out" = true;
          "toolkit.coverage.endpoint.base" = "";

          # Disable Health Reports
          "datareporting.healthreport.uploadEnabled" = false;

          # Disable new data submission, master kill switch
          "datareporting.policy.dataSubmissionEnabled" = false;

          # Disable Studies
          "app.shield.optoutstudies.enabled" = false;

          # Disable personalized Extension Recommendations in about:addons and AMO
          "browser.discovery.enabled" = false;

          # Disable Crash Reports
          "breakpad.reportURL" = "";
          "browser.tabs.crashReporting.sendReport" = false;
          "browser.crashReports.unsubmittedCheck.enabled" = false;

          # Disable backlogged Crash Reports
          "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;

          # Disable Captive Portal detection
          "captivedetect.canonicalURL" = "";
          "network.captive-portal-service.enabled" = false;

          # Disable Network Connectivity checks
          "network.connectivity-service.enabled" = false;


          ##### BLOCKLISTS / SAFE BROWSING #####

          # Enforce Firefox blocklist, but sanitize blocklist url
          "extensions.blocklist.enabled" = true;
          "extensions.blocklist.url" = "https://blocklists.settings.services.mozilla.com/v1/blocklist/3/%APP_ID%/%APP_VERSION%/";

          # Disable SB checks for downloads
          "browser.safebrowsing.downloads.remote.enabled" = false;
          "browser.safebrowsing.downloads.remote.url" = "";


          ##### SYSTEM ADD-ONS / EXPERIMENTS #####

          # Disable Normandy/Shield
          "app.normandy.enabled" = false;
          "app.normandy.api_url" = "";

          # Disable PingCentre telemetry
          "browser.ping-centre.telemetry" = false;

          # Disable Form Autofill
          "extensions.formautofill.addresses.enabled" = false;
          "extensions.formautofill.available" = "off";
          "extensions.formautofill.creditCards.enabled" = false;
          "extensions.formautofill.heuristics.enabled" = false;

          # Disable Web Compatibility Reporter
          "extensions.webcompat-reporter.enabled" = false;

          ##### BLOCK IMPLICIT OUTBOUND #####

          # Disable link prefetching
          "network.prefetch-next" = false;

          # Disable DNS prefetching
          "network.dns.disablePrefetch" = true;
          "network.predictor.enable-prefetch" = false;

          # Disable link-mouseover opening connection to linked server
          "network.http.speculative-parallel-limit" = 0;

          # Disable "Hyperlink Auditing" (click tracking) and enforce same host in case
          "browser.send_pings" = false;
          "browser.send_pings.require_same_host" = true;


          ##### HTTP* / TCP/IP / DNS / PROXY / SOCKS etc #####

          # Disable IPv6 => if VPN
          #"network.dns.disableIPv6" = true;

          # Disable HTTP Alternative Services
          "network.http.altsvc.enabled" = false;
          "network.http.altsvc.oe" = false;

          # Enforce the proxy server to do any DNS lookups when using SOCKS
          "network.proxy.socks_remote_dns" = true;

          # Disable using UNC (Uniform Naming Convention) paths
          "network.file.disable_unc_paths" = true;

          # Disable GIO as a potential proxy bypass vector
          "network.gio.supported-protocols" = "";


          ##### LOCATION BAR / SEARCH BAR / SUGGESTIONS / HISTORY / FORMS #####

          # Disable location bar domain guessing
          "browser.fixup.alternate.enabled" = false;

          # Display all parts of the url in the location bar
          "browser.urlbar.trimURLs" = false;

          # Disable coloring of visited links - CSS history leak
          "layout.css.visited_links_enabled" = false;

          # Disable location bar suggesting "preloaded" top websites
          "browser.urlbar.usepreloadedtopurls.enabled" = false;

          # Disable location bar making speculative connections
          "browser.urlbar.speculativeConnect.enabled" = false;

          # Disable location bar autofill
          "browser.urlbar.autoFill" = false;

          # Disable location bar one-off searches
          "browser.urlbar.oneOffSearches" = false;

          # Disable search and form history
          "browser.formfill.enable" = false;


          ##### PASSWORDS #####

          # Disable auto-filling username & password form fields
          "signon.autofillForms" = false;

          # Disable formless login capture for Password Manager
          "signon.formlessCapture.enabled" = false;

          # Disable HTTP authentication credentials dialogs triggered by cross-origin
          "network.auth.subresource-http-auth-allow" = 1;


          ##### CACHE / SESSION (RE)STORE / FAVICONS #####

          # Disable disk cache
          "browser.cache.disk.enable" = false;

          # Disable media cache from writing to disk in Private Browsing
          "browser.privatebrowsing.forceMediaMemoryCache" = true;
          "media.memory_cache_max_size" = 16384;

          # Disable storing extra session data on unencrypted sites
          "browser.sessionstore.privacy_level" = 1;

          # Disable favicons in shortcuts
          "browser.shell.shortcutFavicons" = false;


          ##### HTTPS (SSL/TLS / OCSP / CERTS / HPKP / CIPHERS) #####

          # Require safe negotiation
          "security.ssl.require_safe_negotiation" = true;

          # Enforce TLS 1.0 and 1.1 downgrades as session only
          "security.tls.version.enable-deprecated" = false;

          # Disable SSL session tracking
          "security.ssl.disable_session_identifiers" = true;

          # Disable SSL Error Reporting
          "security.ssl.errorReporting.automatic" = false;
          "security.ssl.errorReporting.enabled" = false;
          "security.ssl.errorReporting.url" = "";

          # Disable TLS1.3 0-RTT (round-trip time)
          "security.tls.enable_0rtt_data" = false;

          # Enable OCSP Stapling
          "security.ssl.enable_ocsp_stapling" = true;

          # Set OCSP fetch failures to hard-fail
          "security.OCSP.require" = true;

          # Disable SHA-1 certificates
          "security.pki.sha1_enforcement_level" = 1;

          # Enforce strict pinning
          "security.cert_pinning.enforcement_level" = 2;

          # Disable insecure active content on https pages
          "security.mixed_content.block_active_content" = true;

          # Block unencrypted requests from Flash on encrypted pages to mitigate MitM attacks
          "security.mixed_content.block_object_subrequest" = true;

          # Control "Add Security Exception" dialog on SSL warnings
          "browser.ssl_override_behavior" = 1;

          # Display advanced information on Insecure Connection warning pages
          "browser.xul.error_pages.expert_bad_cert" = true;


          ##### FONTS #####

          # Disable websites choosing fonts
          "browser.display.use_document_fonts" = 0;

          # Disable rendering of SVG OpenType fonts
          "gfx.font_rendering.opentype_svg.enabled" = false;

          # Disable graphite
          "gfx.font_rendering.graphite.enabled" = false;


          ##### HEADERS / REFERERS #####

          # Send a referer only if base domains match
          "network.http.referer.XOriginPolicy" = 1;

          # Trim the referer
          "network.http.referer.XOriginTrimmingPolicy" = 2;


          ##### PLUGINS #####

          # Disable Flash plugin
          "plugin.state.flash" = 0;

          # Disable widevine CDM (Content Decryption Module)
          "media.gmp-widevinecdm.visible" = false;
          "media.gmp-widevinecdm.enabled" = false;

          # Disable all DRM content (EME: Encryption Media Extension)
          "media.eme.enabled" = false;


          ##### MEDIA / CAMERA / MIC #####

          # Disable WebRTC (Web Real-Time Communication)
          "media.peerconnection.enabled" = true;

          # Limit WebRTC IP leaks if using WebRTC
          "media.peerconnection.ice.default_address_only" = true;
          "media.peerconnection.ice.no_host" = true;
          "media.peerconnection.ice.proxy_only_if_behind_proxy" = true;

          # Disable WebGL (Web Graphics Library)
          "webgl.disabled" = true;
          "webgl.enable-webgl2" = false;

          # Disable screensharing
          "media.getusermedia.screensharing.enabled" = false;
          "media.getusermedia.browser.enabled" = false;
          "media.getusermedia.audiocapture.enabled" = false;

          # Disable autoplay of HTML5 media
          "media.autoplay.default" = 5;

          # Disable autoplay of HTML5 media if you interacted with the site
          "media.autoplay.enabled.user-gestures-needed" = false;


          ##### WINDOW MEDDLING & LEAKS / POPUPS #####

          # Prevent websites from disabling new window features
          "dom.disable_window_open_feature.close" = true;
          "dom.disable_window_open_feature.location" = true;
          "dom.disable_window_open_feature.menubar" = true;
          "dom.disable_window_open_feature.minimizable" = true;
          "dom.disable_window_open_feature.personalbar" = true;
          "dom.disable_window_open_feature.resizable" = true;
          "dom.disable_window_open_feature.status" = true;
          "dom.disable_window_open_feature.titlebar" = true;
          "dom.disable_window_open_feature.toolbar" = true;

          # Prevent scripts from moving and resizing open windows
          "dom.disable_window_move_resize" = true;

          # Open links targeting new windows in a new tab instead
          "browser.link.open_newwindow" = 3;
          "browser.link.open_newwindow.restriction" = 0;

          # Block popup windows
          "dom.disable_open_during_load" = true;

          # Limit events that can cause a popup
          "dom.popup_allowed_events" = "click dblclick";


          ##### WEB WORKERS #####

          # Disable service workers
          "dom.serviceWorkers.enabled" = false;

          # Disable Push Notifications
          "dom.push.enabled" = false;
          "dom.push.userAgentID" = "";

          
          ##### DOM (DOCUMENT OBJECT MODEL) & JAVASCRIPT #####

          # Disable website access to clipboard events/content
          "dom.event.clipboardevents.enabled" = false;

          # Disable clipboard commands (cut/copy) from "non-privileged" content
          "dom.allow_cut_copy" = false;

          # Disable shaking the screen
          "dom.vibrator.enabled" = false;

          # Enable window.opener protection
          "dom.targetBlankNoOpener.enabled" = true;


          ##### HARDWARE FINGERPRINTING #####

          # Disable Battery Status API
          "dom.battery.enabled" = false;

          # Disable media device enumeration
          "media.navigator.enabled" = false;

          # Disable Web Audio API
          "dom.webaudio.enabled" = false;

          # Disable virtual reality devices
          "dom.vr.enabled" = false;


          ##### MISCELLANEOUS #####

          # Prevent accessibility services from accessing your browser
          "accessibility.force_disabled" = 1;

          # Disable sending additional analytics to web servers
          "beacon.enabled" = false;

          # Remove temp files opened with an external application
          "browser.helperApps.deleteTempFileOnExit" = true;

          # Disable page thumbnail collection
          "browser.pagethumbnails.capturing_disabled" = true;

          # Block web content in file processes
          "browser.tabs.remote.allowLinkedWebInFileUriProcess" = false;

          # Disable UITour backend so there is no chance that a remote page can use it
          "browser.uitour.enabled" = false;
          "browser.uitour.url" = "";

          # Disable various developer tools in browser context
          "devtools.chrome.enabled" = false;

          # Disable remote debugging
          "devtools.debugger.remote-enabled" = false;

          # Disable middle mouse click opening links from clipboard
          "middlemouse.contentLoadURL" = false;

          # Limit HTTP redirect
          "network.http.redirection-limit" = 10;

          # Remove special permissions for certain mozilla domains
          "permissions.manager.defaultsUrl" = "";

          # Remove webchannel whitelist
          "webchannel.allowObject.urlWhitelist" = "";

          # Enforce Punycode for Internationalized Domain Names to eliminate possible spoofing
          "network.IDN_show_punycode" = true;

          # Enforce Firefox's built-in PDF reader
          "pdfjs.disabled" = false;

          # Enforce no system colors; they can be fingerprinted
          "browser.display.use_system_colors" = false;

          # Disable permissions delegation
          "permissions.delegation.enabled" = false;

          # Save do the downloader folder
          "browser.download.folderList" = 1;

          # Set download directory
          "browser.download.dir" = "/home/lucas/Téléchargements";

          # Disable adding downloads to the system's "recent documents" list
          "browser.download.manager.addToRecentDocs" = false;

          # Disable hiding mime types (Options>General>Applications) not associated with a plugin
          "browser.download.hide_plugins_without_extensions" = false;


          # Lock down allowed extension directories
          "extensions.enabledScopes" = 5;
          "extensions.autoDisableScopes" = 15;

          # Disable webextension restrictions on certain mozilla domains
          "extensions.webextensions.restrictedDomains" = "";

          # Enforce CSP (Content Security Policy)
          "security.csp.enable" = true;

          # Enforce a security delay on some confirmation dialogs such as install, open/save
          "security.dialog_enable_delay" = 700;


          ##### PERSISTENT STORAGE #####

          # Block cross-site and social media trackers
          "network.cookie.cookieBehavior" = 4;

          # Accept cookies for the current session only
          "network.cookie.thirdparty.sessionOnly" = true;
          "network.cookie.lifetimePolicy" = 2;


          ##### SHUTDOWN #####

          # Enable Firefox to clear items on shutdown
          "privacy.sanitize.sanitizeOnShutdown" = true;
          "privacy.clearOnShutdown.cache" = false;
          "privacy.clearOnShutdown.cookies" = true;
          "privacy.clearOnShutdown.downloads" = false;
          "privacy.clearOnShutdown.formdata" = false;
          "privacy.clearOnShutdown.history" = false;
          "privacy.clearOnShutdown.offlineApps" = false;
          "privacy.clearOnShutdown.sessions" = false;
          "privacy.clearOnShutdown.siteSettings" = false;


          ##### FPI (FIRST PARTY ISOLATION) #####

          # Enable First Party Isolation
          "privacy.firstparty.isolate" = true;

          # Enforce FPI restriction for window.opener
          "privacy.firstparty.isolate.restrict_opener_access" = true;
          "privacy.firstparty.isolate.block_post_message" = true;


          ##### RFP (RESIST FINGERPRINTING) #####

          "privacy.resistFingerprinting" = true;

          # Disable mozAddonManager Web API
          "privacy.resistFingerprinting.block_mozAddonManager" = true;

          # Disable showing about:blank as soon as possible during startup
          "browser.startup.blankWindow" = false;


          ##### PERSONAL #####

          # Load userChrome.css and userContent.css at startup
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

          # Warnings
          "browser.aboutConfig.showWarning" = false;
          "general.warnOnAboutConfig" = false;
          "browser.tabs.warnOnClose" = false;
          "browser.tabs.warnOnCloseOtherTabs" = false;
          "browser.tabs.warnOnOpen" = false;
          "full-screen-api.warning.delay" = 0;
          "full-screen-api.warning.timeout" = 0;

          # Always show downloads button
          "browser.download.autohideButton" = false;

          # Compact density
          "browser.uidensity" = 1;

          # Restore session at startup
          "browser.startup.page" = 3;

          # Set homepage
          "browser.startup.homepage" = "about:blank";

          "browser.newtabpage.enabled" = false;
          "browser.newtab.preload" = false;

          # Hide Pocket
          "extensions.pocket.enabled" = false;
        };

        # userContent.css
        userContent = with style.color; /* css */ ''
          /* Hide the scrollbars */
          *{scrollbar-width:none !important}

          /* Change the about:blank page background */
          @-moz-document url("about:blank") {
            * {
              background: ${background} !important;
            }
          }
        '';

        # userChrome.css
        userChrome = /* css */ ''
          /* Hide the tab close button */
          .tab-close-button {
            display: none !important;
          }
        '';
      };
    };
  };
}
