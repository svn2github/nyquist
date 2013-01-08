package jnyqide;
//
//  SalFileFilter.java
//  nyquist
//
//  Created by Roger Dannenberg on 12/23/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//



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

public class SalFileFilter extends FileFilter {

  public SalFileFilter() {
  }

  public boolean accept(File f) {
    if (f.getName().endsWith(".sal")) return true;
    for (int x = 0; x < f.getName().length(); x++) {
      if ((f.getName().charAt(x) == '.'))
        return false;
    }
    return true;
  }

  public String getDescription() {
    return "Sal files.";
  }
}
