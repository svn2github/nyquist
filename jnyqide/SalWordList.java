//
//  SalWordList.java
//  nyquist
//
//  Created by Roger Dannenberg on 12/18/07.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

package jnyqide;

import java.util.HashMap;
import java.util.Map;

    
public class SalWordList {
    public static String[] reservedWords = {
        "below", "by", "else", "finally", "for", "function", "in", "over",
        "repeat", "then", "from", "to", "unless", "until", "variable", 
        "when", "while", "with" };
        
    public static String[] commandWords = {
        "begin", "end", "chdir", "define", "open", "exec", "exit", "display",
        "load", "loop", "open", "print", "if", "return", "set", // "system",
        "play" };
        
    public static Map<String, String> reservedWordsTable;
            
    public static Map<String, String> commandWordsTable;


    static public void init() {
        int i;
        reservedWordsTable = new HashMap<String, String>();
        for (i = 0; i < reservedWords.length; i++) {
            reservedWordsTable.put(reservedWords[i], null);
        }
        commandWordsTable = new HashMap<String, String>();
        for (i = 0; i < commandWords.length; i++) {
            commandWordsTable.put(commandWords[i], null);
        }
    }
        
        
    static public boolean isReservedWord(String word) {
        boolean rslt = reservedWordsTable.containsKey(word);
        // System.out.println("Sal:isReservedWord " + word + " -> " + rslt);
        return rslt;
    }


    static public boolean isCommandWord(String word) {
        boolean rslt = commandWordsTable.containsKey(word);
        // System.out.println("Sal:isCommandWord " + word + " -> " + rslt);
        return rslt;
    }

}
