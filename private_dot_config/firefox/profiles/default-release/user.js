user_pref("browser.ctrlTab.sortByRecentlyUsed", true);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("findbar.highlightAll", true);
user_pref("browser.startup.homepage", "about:newtab");

// no password filling
user_pref("signon.rememberSignons", false);
user_pref("signon.storeWhenAutocompleteOff", false);
user_pref("signon.autofillForms", false);
user_pref("services.sync.prefs.sync.signon.rememberSignons", false);

// no new window, especially not windows without toolbars
user_pref("browser.link.open_newwindow", 3);
user_pref("browser.link.open_newwindow.override.external", -1);
user_pref("browser.link.open_newwindow.restriction", 0);

// search
user_pref("browser.search.suggest.enabled", false);
