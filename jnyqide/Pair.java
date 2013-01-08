package jnyqide;


import java.io.*;
import java.util.*;

public class Pair
{
	private double time, value;

	public Pair(){
		time = 0;
		value = 0;
	}

	public Pair(double t, double v){

		if (t < 0) {
			System.out.println("Warning: negative time scale input.");
		}

	    // plotted functions are not signals, so you shouldn't expect them to stay within [-1, 1]
		//if (v < -1 || v > 1){
		//	System.out.println("Warning: Value is out of bounds.");
		//}

		time = t;
		value = v;
	}

	public double getTime(){
		return time;
	}

	public double getValue(){
		return value;
	}

}