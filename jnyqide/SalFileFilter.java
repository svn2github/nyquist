package jnyqide;
//
//  SalFileFilter.java
//  nyquist
//
//  Created by Roger Dannenberg on 12/23/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//



import java.io.File;
import java.io.FilenameFilter;


public class SalFileFilter implements FilenameFilter {

    public SalFileFilter() {
    }

    public boolean accept(File dir, String name) {
        return name.endsWith(".sal") || (name.indexOf('.') < 0);
    }

    public String getDescription() {
        return "Sal files.";
    }
}
