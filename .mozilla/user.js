//
// Akkana's personal prefs which I don't want overwritten.
//

// DNS prefetch, if I understand it correctly, prefetches the DNS
// for every link on the page every few minutes (because the cache
// expires that fast).
// https://bugzilla.mozilla.org/show_bug.cgi?id=453403
user_pref("network.dns.disablePrefetch", true);

// Silly Ubuntu turns off middleclick load!
user_pref("middlemouse.contentLoadURL", true);

// Force frame resizability
user_pref("layout.frames.force_resizability", true);

// Allow dragging into the middle of a text block, like a link:
user_pref("browser.drag_out_of_frame_style", 0);

// Set cursive and fantasy fonts
user_pref("font.name.cursive.x-western", "Allegro");
user_pref("font.name.fantasy.x-western", "Dragonwick");
user_pref("font.minimum-size.x-western", 12);

// Make backspace not go back to the previous page.
// See bug 219203 and bug 262905
user_pref("browser.backspace_action", 2);

// Override the default user-agent string:
//user_pref("general.useragent.venfor", "Firefox/Kitfox");
//user_pref("general.useragent.override", "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.0.0; hi, Mom) Gecko/20020604");
//user_pref("general.useragent.override", "Mozilla/5.0 (Linux: Penguins Rule!) Gecko blah blah");

// Use google for keyword searches:
//user_pref("keyword.URL", "http://www.google.com/search?q=");
// but insist that it show only pages containing all the search terms
// (why isn't that the default?):
// http://www.googleguide.com/advanced_operators.html#allintext
// Adding complete=0 seems to turn off Instant.
// &nfpr=1 disables automatically going to the "did you mean? ..."
// but allintext: disables "did you mean? ..." anyway.
user_pref("keyword.URL", "http://www.google.com/search?complete=0&q=allintext%3A+");
// Try providers other than google since it's running horrible UI
// experiments on me:
//user_pref("keyword.URL", "http://duckduckgo.com/html/?q=");
//
// Supposedly keyword.URL no longer exists as of firefox 23
// https://bugzilla.mozilla.org/show_bug.cgi?id=738818
// but they copied it tobrowser.search.defaulturl and
// browser.search.selectedEngine and use one or the other of them
// if keyword.enabled is set, which it is by default.

// Show JS warnings:
// Turned off for Firefox 3.5 because its JS engine has bugs
// and reports strict warnings for things that aren't wrong.
//user_pref("javascript.options.strict", true);

// Don't normally warn on close; force it here so that
// I can occasionally turn it on when I need it:
user_pref("browser.tabs.warnOnClose", false);

// Image animation: normal, once, none
user_pref("image.animation_mode", "none");

// Turn off the obnoxious download manager:
//user_pref(("browser.downloadmanager.behavior", 1);

// Disable blinking text:
user_pref("browser.blink_allowed", false);

// Enable printing from print preview, so you don't have to do
// page setup, dismiss, print, page setup, dismiss to get one
// single landscape mode printout.
// However, it really doesn't work anyway, so ...
//user_pref("print.whileInPrintPreview", true);

// Always start up with printing set to portrait.
// That way, I don't have to remember to run Print Setup again
// every time I want to print one page in landscape mode.
// Unfortunately, you have to do this for every printer that's registered.
// To find out which orientation is which for each printer,
//    grep orientation user.js | grep name
// The first looks like a default, but is ignored since it's
// overridden by all the printer-specific ones.
user_pref("print.print_orientation", 0);
user_pref("print.printer_Brother.print_orientation", 0);
user_pref("print.printer_HP_Deskjet_F4280.print_orientation", 0);
//user_pref("print.printer_PostScript/default.print_orientation", 0);
user_pref("print.printer_Print_to_File.print_orientation", 0);
user_pref("print.print_to_filename", "/home/akkana/moz.pdf");

// Disable the blinking cursor too, while we're at it:
// unfortunately this makes the cursor invisible a lot of the time too.
user_pref("ui.caretBlinkTime", 0);

// Set the submenu delay to be really long.
// This means that menus will stay posted until I click somewhere,
// making them behave more like Motif menus instead of un-posting
// whenever my mouse strays one pixel off the menu or cuts
// across the border between a menu and a submenu:
// This worked in the suite but not in Firefox.
//user_pref("ui.submenuDelay", 7000);

// Browser background color, because the colorpicker is too coarse.
// This helps me notice when background colors aren't set and should be,
// and also gives me a more comfortable reading background on the
// rare web pages that don't force a color.
user_pref("browser.display.background_color", "#EEFFFF");

user_pref("browser.display.use_system_colors", false);

// Set select background for text widgets:
// Unfortunately this inexplicably sets firefox 3's
// urlbar selections to white on white.
//user_pref("ui.textSelectBackground", "green");
// Not clear when widgetSelectBackground ever gets called: let's find out.
//user_pref("ui.widgetSelectBackground", "orange");

// Don't prefetch, what a waste of bandwidth
user_pref("network.prefetch-next", false);

// Guard against a security bug:
// http://seclists.org/fulldisclosure/2007/Feb/0340.html
user_pref("capability.policy.default.Location.hostname.set", "noAccess");

// Make firefox2 tab preferences more livable:
user_pref("browser.tabs.closeButtons", 3);
user_pref("browser.tabs.tabMinWidth", 1);
user_pref("browser.tabs.selectOwnerOnClose", false);

// Keep the browser from stealing focus when it opens a new tab:
// http://labnol.blogspot.com/2006/07/secret-firefox-trick-prevent-browser.html
user_pref("browser.tabs.loadDivertedInBackground", true);

// Turn off the whizzy firefox 3 drag images that are so large and
// opaque that they make it impossible to see where you're dropping
user_pref("nglayout.enable_drag_images", false);

// Custom newline handling:
//   0. Paste newlines intact (old linux default)
//   1. Paste up to the first newline
//   2. Replace newlines with spaces (ff3 default, old win default)
//   3. Strip newlines
//   4. Replace with commas
//   5. Strip newlines and surrounding whitespace 
user_pref("editor.singleLine.pasteNewlines", 2);

// Tab to focus only to text fields, not links etc.:
user_pref("accessibility.tabfocus", 1);
