package jnyqide;


import javax.swing.filechooser.*;
import java.io.File;


/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2002</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class LispFileFilter extends FileFilter {

  public LispFileFilter() {
  }

  public boolean accept(File f) {
    if (f.getName().endsWith(".lsp")) return true;
    for (int x = 0; x < f.getName().length(); x++) {
      if ((f.getName().charAt(x) == '.'))
        return false;
    }
    return true;
  }

  public String getDescription() {
    return "Lisp files.";
  }
}
