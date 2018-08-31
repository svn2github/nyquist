// WindowsRegistry -- used on Windows to read XLISPPATH from
//    registry, necessary to find the nyquistDir used by
//    nyquist executable on Windows.

package jnyqide;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

/**
 * @author Oleg Ryaboy, based on work by Miguel Enriquez 
 */
public class WindowsRegistry {
    /**
     * 
     * @param location path in the registry
     * @param key registry key
     * @return registry value or null if not found
     */
    public static final String readRegistry(String location, String key){
        try {
            // Run reg query, then read output with StreamReader
            // (internal class)
            System.out.println("readRegistry command " + "reg query " + 
                               '"'+ location + "\" /v " + key);
            Process process = Runtime.getRuntime().exec("reg query " + 
                    '"'+ location + "\" /v " + key);

            StreamReader reader = new StreamReader(process.getInputStream());
            reader.start();
            process.waitFor();
            reader.join();
            String output = reader.getResult();
            System.out.println("read registry result " + output);
            // output is a bunch of stuff followed by REG_SZ followed by
            // path. Since path could have spaces, be careful.
            int start = output.indexOf("REG_SZ");
            if (start < 0) return null;
            output = output.substring(start + 6).trim();
            return output;
        }
        catch (Exception e) {
            return null;
        }

    }

    static class StreamReader extends Thread {
        private InputStream is;
        private StringWriter sw= new StringWriter();

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

//        // Sample usage
//        String value = WindowsRegistry.readRegistry(
//                "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\" +
//                "Explorer\\Shell Folders", "Personal");
//        System.out.println(value);

}
