package jnyqide;


import java.io.File;
import java.io.FilenameFilter;


public class LispFileFilter implements FilenameFilter {

    public LispFileFilter() {
    }

    public boolean accept(File dir, String name) {
        return name.endsWith(".lsp") || (name.indexOf('.') < 0);
    }

    public String getDescription() {
        return "Lisp files.";
    }
}
