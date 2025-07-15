lockPref("privacy.resistFingerprinting", false);
lockPref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
lockPref("browser.uidensity", 0); // compact UI density

// const { Services } = ChromeUtils.import("resource://gre/modules/Services.jsm");

// function addKeybinding() {
//     const keyset = document.createXULElement("keyset");
//     keyset.id = "my-keyset";

//     const key = document.createXULElement("key");
//     key.setAttribute("id", "my-new-key");
//     key.setAttribute("key", "N"); // Change this to your desired key
//     key.setAttribute("modifiers", "accel"); // Use "accel" for Ctrl, "shift" for Shift, etc.
//     key.setAttribute("oncommand", "gBrowser.selectedTab = gBrowser.addTab();");

//     keyset.appendChild(key);
//     document.documentElement.appendChild(keyset);
// }

// addKeybinding();
