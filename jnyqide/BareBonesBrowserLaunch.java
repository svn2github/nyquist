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



import java.awt.Desktop;
import java.io.File;
import java.lang.reflect.Method;
import java.net.URI;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;

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
                // try a even newer way
                Desktop.getDesktop().browse(new URI(url));
                // try a new way
                // Runtime.getRuntime().exec(new String[] {"/usr/bin/open", "\"" + url + "\""});
                // here's the old way that does not handle #name suffix on url
//                Class fileMgr = Class.forName("com.apple.eio.FileManager");
//                System.out.println("fileMgr=" + fileMgr);
//                Method openURL = fileMgr.getDeclaredMethod("openURL",
//                                               new Class[] {String.class});
//                openURL.invoke(null, new Object[] {url});
                System.out.println("openURL invoked with " + url);
            } else if (osName.startsWith("Windows")) {
				// The new method -- ZEYU
				if (loadURL(url) == false) {
				// use the old ways is unsuccessful
					url = url.replace('\\','/'); // in browser, \ is not supported. --ZEYU
					System.out.println(" Win 7 open: " + url);
					Desktop.getDesktop().browse(java.net.URI.create(url));
				}
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
            e.printStackTrace();
        }
    }
	
	static class StreamReader extends Thread {
		private InputStream is;
		private StringWriter sw = new StringWriter();

		public StreamReader(InputStream is) {
			this.is = is;
		}

		public void run() {
			try {
				int c;
				while ((c = is.read()) != -1)
					sw.write(c);
			} catch (IOException e) {
			}
		}

		public String getResult() {
			return sw.toString();
		}
	}

	/**
	 * 
	 * @param location
	 *            path in the registry
	 * @param key
	 *            registry key
	 * @return registry value or null if not found
	 */
	public static final String readRegistry(String location, String key) {
		try {
			// Run reg query, then read output with StreamReader (internal
			// class)
			Process process;
			if (key.length() > 0)
				process = Runtime.getRuntime().exec(
						"reg query " + '"' + location + "\" /v " + key);
			else
				process = Runtime.getRuntime().exec(
						"reg query " + '"' + location + "\" /ve");

			StreamReader reader = new StreamReader(process.getInputStream());
			reader.start();
			process.waitFor();
			reader.join();

			// Parse out the value
			String[] parsed = reader.getResult().split("\\s+");
			if (parsed.length > 1 && key.length() > 0) {
				return parsed[parsed.length - 1];
			} else {
				return reader.getResult().split("REG_SZ")[1].split("\"%1\"")[0].split("--")[0].trim(); 
				// This is ugly... but it removes some useless options that would produce errors in Win Vista.
			}
		} catch (Exception e) {
		}

		return null;
	}

	public static boolean loadURL(String url) {
		// Get name for default browser
		String valueDefaultBrowser = readRegistry(
				"HKEY_CURRENT_USER\\SOFTWARE\\MICROSOFT\\WINDOWS\\CUrrentVersion\\Explorer\\FileExts\\.html\\UserChoice",
				"Progid");
		if (valueDefaultBrowser == null) {
			return false;
		}
		System.out.println("Default Browser: " + valueDefaultBrowser);

		// Get Path
		String regLocation = "HKEY_CLASSES_ROOT\\" + valueDefaultBrowser
				+ "\\Shell\\open\\command";
		if (regLocation == null) {
			return false;
		}	
		String command = readRegistry(regLocation, "");

		System.out.println("Command: " + command);
		try {
			Runtime.getRuntime().exec(command + " " + url);
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
}
