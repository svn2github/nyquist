package jnyqide;


import java.io.File;
import java.io.FilenameFilter;


public class NyquistFileFilter implements FilenameFilter {

    public NyquistFileFilter() {
    }

    public boolean accept(File dir, String name) {
        return name.endsWith(".lsp") || name.endsWith(".sal") ||
               (name.indexOf('.') < 0);
    }

    public String getDescription() {
        return "Nyquist source files.";
    }
}
