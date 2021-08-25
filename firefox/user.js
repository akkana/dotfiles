//
// Akkana's personal prefs which I don't want overwritten.
//
// (Note: many of these are very old and may no longer apply.)
//

///////////////// Some user-experience settings

// Custom newline handling:
//   0. Paste newlines intact (old linux default)
//   1. Paste up to the first newline
//   2. Replace newlines with spaces (ff3 default, old win default)
//   3. Strip newlines
//   4. Replace with commas
//   5. Strip newlines and surrounding whitespace
user_pref("editor.singleLine.pasteNewlines", 2);

// Silly Ubuntu turns off middleclick load!
// And a lot of flamers in bug 366945 want to do that everywhere.
user_pref("middlemouse.contentLoadURL", true);


// Make backspace not go back to the previous page.
// See bug 219203 and bug 262905
user_pref("browser.backspace_action", 2);

// Use google for keyword searches:
user_pref("keyword.URL", "http://www.google.com/search?q=");
//user_pref("keyword.URL", "http://duckduckgo.com/html/?q=");

// Supposedly keyword.URL no longer exists as of firefox 23
// https://bugzilla.mozilla.org/show_bug.cgi?id=738818
// but they copied it tobrowser.search.defaulturl and
// browser.search.selectedEngine and use one or the other of them
// if keyword.enabled is set, which it is by default.

// Show JS warnings:
//user_pref("javascript.options.strict", true);

// Image animation: normal, once, none
user_pref("image.animation_mode", "none");

// Disable blinking text:
user_pref("browser.blink_allowed", false);

// Disable the blinking cursor:
// unfortunately this makes the cursor invisible sometimes.
user_pref("ui.caretBlinkTime", 0);

// Browser background color, because the colorpicker is too coarse.
// This helps me notice when background colors aren't set and should be,
// and also gives me a more comfortable reading background on the
// rare web pages that don't force a color.
user_pref("browser.display.background_color", "#EEFFFF");

// Prevent Firefox from opening new tabs when I didn't request them:
// http://kb.mozillazine.org/Browser.link.open_newwindow
// http://kb.mozillazine.org/Browser.link.open_newwindow.restriction
// Quantum defaults:
//   browser.link.open_newwindow: 3 (open targeted links in a new tab)
//   browser.link.open_newwindow.disabled_in_fullscreen: false
//   browser.link.open_newwindow.override_external: -1
//   browser.link.open_newwindow.restriction: 2
// Maybe obsolete: user_pref("browser.link.open_newwindow", 1)
// user_pref("browser.link.open_newwindow.override_external", 1);
// https://support.mozilla.org/en-US/questions/1014541 claims the possible
// values are 1, 2 and 3 but quantum's default is -1.


///////////////// Security/Privacy Issues

// Show attemps at spoofed international domain names.
// Every browser except mozilla does this by default.
user_pref("network.IDN_show_punycode", true);

// DNS prefetch, if I understand it correctly, prefetches the DNS
// for every link on the page every few minutes (because the cache
// expires that fast).
// https://bugzilla.mozilla.org/show_bug.cgi?id=453403
user_pref("network.dns.disablePrefetch", true);
user_pref("network.prefetch-next", false);

// Guard against a security bug:
// http://seclists.org/fulldisclosure/2007/Feb/0340.html
user_pref("capability.policy.default.Location.hostname.set", "noAccess");

// Override the default user-agent string:
//user_pref("general.useragent.venfor", "Firefox/Kitfox");
//user_pref("general.useragent.override", "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.0.0; hi, Mom) Gecko/20020604");
//user_pref("general.useragent.override", "Mozilla/5.0 (Linux: Penguins Rule!) Gecko blah blah");


///////////////// Misc. Not tested recently, May or may not be obsolete

// Don't autoplay videos. Now seems to be the default.
// user_pref("media.autoplay.enabled", false);

// Set cursive and fantasy fonts, just in case anyone ever uses them.
user_pref("font.name.cursive.x-western", "Allegro");
user_pref("font.name.fantasy.x-western", "Dragonwick");
user_pref("font.minimum-size.x-western", 12);

// Always add new tabs at the far right, not adjacent to whatever the
// current tab is.
//user_pref("browser.tabs.insertRelatedAfterCurrent", false);

// On strings typed in the urlbar, first see if they can be
// interpreted as a URL, e.g. from localhost, and if so, don't
// pass them along to a search engine.
//
// """When browser.fixup.dns_first_for_single_words is true, we first
// dns-lookup and then if that fails we execute a search.
// When browser.fixup.dns_first_for_single_words is false, we first
// execute a search, and then dns-lookup, if the lookup is successful
// we ask to the user whether he wanted to go to "typedstring"."""
// https://bugzilla.mozilla.org/show_bug.cgi?id=1578856#c43
user_pref("browser.fixup.dns_first_for_single_words", true);

// Force frame resizability
user_pref("layout.frames.force_resizability", true);

// Allow dragging into the middle of a text block, like a link:
user_pref("browser.drag_out_of_frame_style", 0);

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

user_pref("browser.display.use_system_colors", false);

// Make firefox2 tab preferences more livable:
// (some of these are now ignored, like tabMinWidth, and need an extension)
user_pref("browser.tabs.closeButtons", 3);
user_pref("browser.tabs.tabMinWidth", 1);
user_pref("browser.tabs.selectOwnerOnClose", false);

// Keep the browser from stealing focus when it opens a new tab:
// http://labnol.blogspot.com/2006/07/secret-firefox-trick-prevent-browser.html
user_pref("browser.tabs.loadDivertedInBackground", true);

// Turn off the whizzy firefox 3 drag images that are so large and
// opaque that they make it impossible to see where you're dropping
user_pref("nglayout.enable_drag_images", false);

// Tab to focus only to text fields, not links etc.:
user_pref("accessibility.tabfocus", 1);
