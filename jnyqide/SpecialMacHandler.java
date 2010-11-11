package jnyqide;

import jnyqide.*;
import com.apple.eawt.ApplicationAdapter;
import com.apple.eawt.ApplicationEvent;
import com.apple.eawt.Application;
/* import com.apple.mrj.*; */
import javax.swing.SwingUtilities;

/* OLD CODE -- the MRJ classes are deprecated.
public class SpecialMacHandler
    implements MRJQuitHandler, MRJPrefsHandler, MRJAboutHandler {
    MainFrame us;
    public SpecialMacHandler(jnyqide.MainFrame theProgram) {
        us = theProgram;
        System.setProperty("com.apple.macos.useScreenMenubar", "true");
        System.setProperty("com.apple.mrj.application.apple.menu.about.name", "jNyqIDE");
        MRJApplicationUtils.registerAboutHandler(this);
        MRJApplicationUtils.registerPrefsHandler(this);
        MRJApplicationUtils.registerQuitHandler(this);
        System.out.println("\n\n\nRegistered handlers for Mac Application Menu\n\n\n");
    }
    public void handleAbout() {
        us.About();
    }
    public void handlePrefs() {
        us.Prefs();
    }
    public void handleQuit() {
        System.out.println("handleQuit in SpecialMacHandler.java called");
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                us.Quit();
            }
        });
        throw new IllegalStateException("Let the quit handler do it");
    }
}
*/

public class SpecialMacHandler extends ApplicationAdapter {

    MainFrame us;

    public SpecialMacHandler(jnyqide.MainFrame theProgram) {
        System.out.println("SpecialMacHandler created");
        us = theProgram;
        Application app = Application.getApplication();
        app.addApplicationListener(this);
        app.setEnabledPreferencesMenu(true);
    }

    public void handleAbout(ApplicationEvent e) {
        e.setHandled(true);
        us.About();
    }

    public void handlePreferences(ApplicationEvent e) {
        us.Prefs();
    }

    public void handleQuit(ApplicationEvent e) {
        System.out.println("handleQuit in SpecialMacHandler called");
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                us.Quit();
            }
        });
    }
}
