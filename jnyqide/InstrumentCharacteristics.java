package jnyqide;

import java.util.ArrayList;
import java.io.BufferedReader;

/***
** Class: InstrumentCharacteristics
** Author: Priyanka Raghavan and Roger B. Dannenberg
** Description: The instrument characteristics class reads from 
** the instruments.txt file and stores the
** instrument characteristics such as the subcategory,name,library.
** The characteristics include the implementation and parameter
** description. The different parameters are added as an arraylist.
*
* Syntax for the instruments.txt file:
*  (see instruments.txt for examples, it should be clear)
*  Each sound is described by a function declaration preceded by
*  a category like this:
*    category:subcategory[function](parameters)
*  where category and subcategory determine what appears in the 
*  pull-down lists in the browser, and function is the lisp
*  function to call (if omitted, then subcategory is also the
*  function name). parameters is a comma-separated list of 
*  parameter declarations. Each parameter is of the form
*    type name = default (low:high)
*  where type is "int" or "float", name is the parameter name
*  (if the parameter is a keyword parameter, it is prefixed with
*  a colon), and low:high gives the range for a slider.
* After the function declaration, there can be specifications for
* the implementation of the function. There should be either
* a pair of implementations for LISP and SAL, or a single
* REQUIRE. The LISP and SAL implementation is specified as follows:
*  LISP-SOURCE
*    <any number of lines of LISP source code>
*  SAL-SOURCE
*    <any number of lines of SAL source code>
* and the REQUIRE is specified as follows:
*  REQUIRE "path to implementation file to be loaded"
* After the implementation specifications (if any), the 
* sound description is terminated by the following line:
*  END-SOUND
* There may be any number of these sound specifications (instruments)
* in the file.
* When a user selects an instrument in the browser, the appropriate
* implementation is constructed (using SAL-SOURCE or LISP-SOURCE if
* present, otherwise using REQUIRE to load a file) and the function
* is called with parameters chosen by sliders.
*
**/

public class InstrumentCharacteristics {
    
    private String categoryName;
    private String subcategoryName;
    private String functionName;
    private ArrayList parameterList;
    private String lispImplementation;
    private String salImplementation;
    private String require;

    private static char buffer;
    private static boolean bufferp;
    private static String line;

    InstrumentCharacteristics() {
        bufferp = false;
	parameterList = new ArrayList();
    }

    public String getCategoryName() { return categoryName; }

    public void setCategoryName(String name) { categoryName = name; }

    public void setSubcategoryName(String subname) {
	subcategoryName = subname;
    }

    public String getSubcategoryName() { return subcategoryName; }

    public String getFunctionName() { return functionName; }


    public void addParameter(String name, String minValue, String maxValue, 
                             String defaultValue, String type) {
	Parameter parameter = new Parameter(name, minValue, maxValue, 
					    defaultValue, type);
	parameterList.add(parameter);
    }
    
    public ArrayList getParameters(){ return parameterList; }

    public String getLispImplementation() { return lispImplementation; }

    public String getSalImplementation() { return salImplementation; }

    public String getRequire() { return require; }

    public String readImplementation(BufferedReader br) throws Exception {
        String implementation = "";
        while ((line = br.readLine()) != null && 
               line.indexOf("REQUIRE") != 0 &&
               line.indexOf("LISP-SOURCE") != 0 &&
               line.indexOf("SAL-SOURCE") != 0 &&
               line.indexOf("END-SOUND") != 0) {
            implementation = implementation + line + "\n";
        }
        return implementation;
    }

    public boolean readData(BufferedReader br) {
	categoryName = getToken(br); // category
	if (categoryName == null) return false;
	if (getNonSpace(br) != ':') {
	    System.out.println("expected : after " + categoryName);
	    return false;
	}
	subcategoryName = getToken(br);
	int c = getNonSpace(br);
	functionName = subcategoryName;
	if (c == '[') {
	    functionName = getToken(br);
	    if (getNonSpace(br) != ']') {
		System.out.println("expected ] after " + functionName);
		return false;
	    }
	} else ungetChar(c);
	if (getNonSpace(br) != '(') {
	    System.out.println("no ( after " + functionName);
	    return false;
	}
	while ((c = getNonSpace(br)) != ')') {
	    ungetChar(c);
	    Parameter p = readParameter(br);
	    if (p == null) {
		System.out.println("syntax error for parameter in " + 
				   subcategoryName);
		return false;
	    }
	    parameterList.add(p);
	}
        // get a file to load or an implementation to execute
        require = null;
        lispImplementation = null;
        salImplementation = null;
        try {
            // read eol after function spec
            line = br.readLine();
            line = ""; // force a readline on first entry to loop
            while (true) {
                while (line != null && line.length() < 3) {
                    // skip blank lines -- we're not checking too carefully
                    // but a char count of 3 allows only CRLF and maybe a
                    // space or tab
                    line = br.readLine();
                }
                if (line == null) {
                    System.out.println(
                        "expected LISP-SOURCE or SAL-SOURCE, REQUIRE or " +
                        "END-SOUND, not " + line);
                    return false;
                }

                int reqPos = line.indexOf("REQUIRE");
                if (reqPos >= 0) {
                    reqPos += 8; // length of "REQUIRE "
                    require = line.substring(reqPos, line.length());
                    line = br.readLine();
                } else if (line.indexOf("LISP-SOURCE") == 0) {
                    // read until LISP-END
                    lispImplementation = readImplementation(br);
                } else if (line.indexOf("SAL-SOURCE") == 0) {
                    // read until SAL-END
                    salImplementation = readImplementation(br);
                } else if (line.indexOf("END-SOUND") == 0) {
                    return true;
                } else {
                    System.out.println(
                        "expected REQUIRE, LISP-SOURCE, SAL-SOURCE, or " +
                        "END-SOUND, not " + line);
                    return false;
                }
            }
	} catch (Exception e) {
	    return false;
	}	    
    }

    private Parameter readParameter(BufferedReader br) {
	Parameter p = new Parameter();
	String tok = getToken(br);
	if (tok == null) {
	    System.out.println("expected parameter type: " + tok);
	    return null;
	}
	p.setType(tok);
	int param1 = getNonSpace(br);
	if (param1 != ':') {
	    ungetChar(param1);
	}
	tok = getToken(br);
	if (tok == null) {
	    System.out.println("expected parameter name: " + tok);
	    return null;
	}
	if (param1 == ':') tok = ":" + tok;
	p.setName(tok);

	if (getNonSpace(br) != '=') {
	    System.out.println("expected = after parameter: " + tok);
	    return null;
	}

	tok = getToken(br);
	if (tok == null) {
	    System.out.println("expected default value: " + tok);
	    return null;
	}
	p.setDefaultValue(tok);

	if (getNonSpace(br) != '(') {
	    System.out.println("expected ( after default value: " + tok);
	    return null;
	}
	
	tok = getToken(br);
	if (tok == null) {
	    System.out.println("expected min value: " + tok);
	    return null;
	}
	p.setMinValue(tok);

	if (getNonSpace(br) != ':') {
	    System.out.println("expected : after min value: " + tok);
	    return null;
	}
	
	tok = getToken(br);
	if (tok == null) {
	    System.out.println("expected max value: " + tok);
	    return null;
	}
	p.setMaxValue(tok);

	if (getNonSpace(br) != ')') {
	    System.out.println("expected ) after max value: " + tok);
	    return null;
	}
	
	int c = getNonSpace(br);
	if (c != ',') ungetChar(c);

	return p;
    }

    private int getNonSpace(BufferedReader br) {
        int c;
        while ((c = getChar(br)) != -1 && Character.isWhitespace(c));
	return c;
    }

    private int getChar(BufferedReader br) {
        if (bufferp) {
	    bufferp = false;
	    return buffer;
	}
	try {
	    return br.read();
	} catch (Exception e) {
	    return -1;
	}
    }

    private void ungetChar(int c) {
	if (c == -1) return; // ignore EOF
	buffer = (char) c;
	bufferp = true;
    }

    private String getToken(BufferedReader br) {
	int c = getNonSpace(br);
	StringBuffer token = new StringBuffer();
        while (c != -1 && (Character.isLetterOrDigit(c) || 
			   c == '-' || c == '.')) {
	    token.append((char) c);
	    c = getChar(br);
	}
	ungetChar(c);
	String s = new String(token);
	if (s.length() == 0) return null;
	// System.out.println("gettoken: " + token);
        return s;
    }
}


/**
 ** Class: Parameter
 ** Author: Priyanka Raghavan
 ** Description: This class is used to store parameter values like 
 **    name, minvalue, maxvalue, default value, and type (integer,string, etc.)
 **
 **/

class Parameter{
    String name;
    String minValue;
    String maxValue;
    String defaultValue;
    String type;
    float value;
    
    Parameter() { }

    Parameter(String name, String minValue, String maxValue,
	      String defaultValue, String type) {
	this.name = name;
	this.minValue = minValue;
	this.maxValue = maxValue;
	this.defaultValue = defaultValue;
	this.type=type;
	value = 0.0f;
    }

    public void setName(String name) {
	this.name=name;
    }

    public void setMinValue(String value) {
	minValue = value;
    }

    public void setMaxValue(String value) {
	maxValue = value;
    }

    public void setDefaultValue(String defaultvalue) {
	defaultValue = defaultvalue;
    }

    public String getName() {
	return name;
    }

    public String getMinValue() {
	return minValue;
    }

    public String getMaxValue() {
	return maxValue;
    }
    
    public String getType() { return type; }
    
    public void setType(String type) { this.type = type; }
    
    public String getDefaultValue() { return defaultValue; }

    public float getValue() { return value; }

    public void setValue(float f) { value = f; }

}


