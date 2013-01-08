package jnyqide;
/////////////////////////////////////////////////////////
//  Bare Bones Browser Launch                          //
//  Version 1.5                                        //
//  December 10, 2005                                  //
//  Modified by RBD, 2008 to test for htmlview in linux//
//  Supports: Mac OS X, GNU/Linux, Unix, Windows XP    //
//  Example Usage:                                     //
//     String url = "http://www.centerkey.com/";       //
//     BareBonesBrowserLaunch.openURL(url);            //
//  Public Domain Software -- Free to Use as You Like  //
/////////////////////////////////////////////////////////



import java.lang.reflect.Method;
import javax.swing.JOptionPane;

public class BareBonesBrowserLaunch {

    private static final String errMsg = 
            "Error attempting to launch web browser";

    public static void openURL(String url) {
        String osName = System.getProperty("os.name");
        System.out.println("BareBonesBrowserLaunch: url=" + url + 
                           " osName:" + osName);
        try {
            if (osName.startsWith("Mac OS")) {
                System.out.println("BareBonesBrowserLaunch: Mac OS detected");
                // try a new way
                Runtime.getRuntime().exec(new String[] {"/usr/bin/open", url});
                // here's the old way that does not handle #name suffix on url
//                Class fileMgr = Class.forName("com.apple.eio.FileManager");
//                System.out.println("fileMgr=" + fileMgr);
//                Method openURL = fileMgr.getDeclaredMethod("openURL",
//                                               new Class[] {String.class});
//                openURL.invoke(null, new Object[] {url});
                System.out.println("openURL invoked with " + url);
            } else if (osName.startsWith("Windows")) {
                Runtime.getRuntime().exec(
                        "rundll32 url.dll, FileProtocolHandler " + 
			"\"" + url + "\""); // quote url or lose "#" suffixes
            } else { //assume Unix or Linux
                String[] browsers = {
                    "htmlview", "firefox", "opera", "konqueror", 
                    "epiphany", "mozilla", "netscape" };
                int count;
                for (count = 0; count < browsers.length; count++) {
                    if (Runtime.getRuntime().exec(
                            new String[] {"which", 
                                          browsers[count]}).waitFor() == 0) {
                        break;
                    }
                }
                if (count >= browsers.length)
                    throw new Exception("Could not find web browser");
                System.out.println("Found browser: " + browsers[count]);
                System.out.println("sending url: " + url);
                Runtime.getRuntime().exec(new String[] {browsers[count], url});
            }
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null, errMsg + ":\n" + 
                                                e.getLocalizedMessage());
        }
    }
}
