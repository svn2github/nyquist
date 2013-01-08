package jnyqide;
/*
 * Ming Yang Koh (mykoh@andrew.cmu.edu)
 * Eugene Ang (ewee@andrew.cmu.edu)
 * Priscilla (cyp@andrew.cmu.edu)
 * 
 * PianoRoll application which takes in an array of strings
 * and creates a graphical representation of a piano roll
 * 
 */
 
 /* SOME NOTES BY ROGER B. DANNENBERG:
 
 This software is not ready for real use.
 Some problems include:
 1) there's no way to zoom in on scores or scroll the score view
 2) there's no display of pitch information
 3) there's no way to edit anything but pitch and time
 4) there's no way to copy a note and its attributes, so any new note
    will lack attributes that might be necessary
 5) there's no color coding of note types, e.g. instrumentation
 6) everytime you modify the score, the score is written to a file;
    instead the file should be written explicitly
 7) when the file is written, you should tell Nyquist to evaluate
    (score-restore) to read in the score and assign it to the global
    variable it came from
 8) the score write command probably does not put the score name
    (a lisp variable name) as the first line
 9) it looks like there's one representation for the score data
    as read from the file and another for a display list -- this
    should be simplified to one structure with accessor methods
    if necessary to translate into different representation for display
 10) the score window should be resizable
 11) should the score window be a subwindow to the jNyqIDE main window?
 
 How does it work? See xm.lsp for SCORE-EDIT, the function that you
 call to open a score editor, and SCORE-RESTORE, the function this 
 editor should call to return score data to Nyquist. Also see
 NyquistThread.java for code that opens the score editor. Look for
 "score-edit: writing " around line 230. 
 */


import java.io.*; 
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowListener;
import java.awt.event.WindowEvent;
import java.awt.*;
import javax.swing.*;
import java.util.StringTokenizer;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class Piano_Roll {
	
    // global variable to store the String
    // for the time being, the Invariant will be, each String will
    // contain 3 ints: x, y, and duration.
    
    // let user click and select centre left or right. center is to move to new location
    // left and right is for user to drag along the axis
	// start of global variables in Piano_Roll class
    static ScoreManipulator sm = new ScoreManipulator();
    static Piano_Roll.ScoreManipulator.Event[] inputArray = new Piano_Roll.ScoreManipulator.Event[0];
    static int currentOption = 2; // 0 = add, 1 = delete, 2 = arrange
    
    //static Piano_Roll.ScoreManipulator.Event[] inputArray = new Piano_Roll.ScoreManipulator.Event[0];
    static String filename;
    static String scoreName;
    
    // START OF BAOPANEL CLASS
	static class BaoPanel extends JPanel implements MouseListener 
     {
             private static final long serialVersionUID = 1L;
             // for dragNotStretch : 0 = drag, 1 = stretch from 
             // left(change start time), 2 = stretch from right(change dur)
             private int dragNotStretch;
             private int pitchStartCoord = 500,
                                     pitchHeight = 10,
                                     WINDOWWIDTH = 1024,
                                     maxClickableWidth = 946,
									 lengthUnit = 3;
             /*
              * each box shown on screen is 3 pixels wide
              * each box represents 0.1 seconds
              * the screen will hold approximately 90 seconds of music             
             */
             private int selectedIndex = -1, lastClickedX;
             
             /*drawing horizontal lines of grid
              * if pitchHeight is 10, then there will be... 
              * 49 segments. 49 pitches + 1 pitchless
              * A1 to G7
              */
             
             
             // START OF PAINTCOMPONENT FUNCTION TO DRAW OUT THE SCREEN
             protected void paintComponent (Graphics g) {
                 	int inputArrayLength = inputArray.length;
                 	int tempX, tempY, tempDur;
                     super.paintComponent(g);
                     Graphics2D g2 = (Graphics2D) g;                        
                     Color oldColor = g2.getColor();                        
                     g2.setColor(Color.BLACK);                                 
                     
                     // drawing horizontal lines of grid
                     for(int i=20; i <= pitchStartCoord; i+= pitchHeight){
                             g2.draw3DRect(0, i-10, WINDOWWIDTH, pitchHeight, false);                                
                     }                
                     
                     /* drawing vertical lines of grid,
                      * as well as the blue vertical markers 
                      * to mark out every second
                     */
                     for(int i=0, j=0; i <= WINDOWWIDTH; i += lengthUnit, j++){
                    	 	 if(j == 15){
                    	 		 g2.setColor(Color.BLUE);
                    	 		 g2.draw3DRect(i, 10, 1, pitchStartCoord, false);
                    	 		 j = 0;
                    	 		 g2.setColor(Color.BLACK);
                    	 	 }
                             g2.draw3DRect(i, 10, lengthUnit, pitchStartCoord, false);
                     }
                     
                     
                     g2.setColor(Color.BLUE);

                     //drawing the notes onto the grid
                     for(int i = 0; i < inputArrayLength; i++){
                             tempX = inputArray[i].getTime();
                             tempY = pitchHeight * (inputArray[i].getPitch());
                             tempDur = inputArray[i].getDur();
                             
                             /* the lowest pitch starts from bottom
                              * need to check if its zero, if so, then plot it in special area.
                              * this for loop is also responsible to draw the yellow sides
                              * for user to recognise that its the area for stretching notes
                             */
                             if(tempY != 0) {
                                     g2.fill3DRect(tempX+lengthUnit, (tempY+10), 
                                    		 (tempDur-(2*lengthUnit)), 10, false);                                     
                                     g2.setColor(Color.YELLOW);
                                     g2.fill3DRect(tempX, (tempY+10), lengthUnit, 10, false);
                                     g2.fill3DRect((tempX+tempDur-lengthUnit), 
                                    		 (tempY+10), lengthUnit, 10, false);
                                     g2.setColor(Color.BLUE);
                             }
                             else{
                                     g2.setColor(Color.BLACK);
                                     g2.fill3DRect(tempX, (tempY+10), tempDur, 10, false);                                        
                                     g2.setColor(Color.BLUE);                                        
                             } 
                     }
                     
                     
                     // setting back colour to prevent future 
                     // screw-ups, eg, the border colours etc..
                     g2.setColor(oldColor);
                     addMouseListener(this);
             }

             // START OF DETERMINEACTION FUNCTION FOR DIFFERENTIATING
             // DRAG AND STRETCH..
             private void determineAction(MouseEvent e){
            	 int arraySize = inputArray.length;
                 for(int i = 0; i < arraySize; i++){
                	 if(inputArray[i].checkWithinDuration(e.getX(), e.getY())){
                     selectedIndex = i;
                     dragNotStretch = inputArray[i].checkDragNotStretch(e.getX(), e.getY()); 

                     return;                        
                     }                                
                 }                                        
             }
             
             /* whenever theres a change in the array,
              * 1) change the array,
              * 2) as well as call the function to notify's eugene's part
              */ 
             
             
             /* 
              * START OF MOUSELISTENERS 
              * RESPONSIBLE FOR ALL THE USER INPUTS/CHANGES
              * TO THE SCORE
             */
             
             /*
              *  mouseReleased basically first differentiates
              *  different actions before doing anthing, that
              *  refers to: add, delete or drag/stretch
              *  
              *  then, it will proceed to do the specifed action
              *  within the case of drag/stretch, then the code
              *  will differentiate drag or stretch and proceed
              *  on respectively
              *  
              *  the check of selectedIndex != -1 ensures the user
              *  is operating on a certain notes.
              *  
              */
             public void mouseReleased(MouseEvent e) {
            	 
            	// *** IF DRAG/STRETCH
                if(currentOption == 2){
                	/*
                	 * the following are done in drag/stretch action:
                	 * a)ensure that bounds are kept, ie no
                	 * 		notes are dragged out of the screen, or 
                	 * 		'dragged into itself..'
                	 * b)changing the Event in the array and subsequently
                	 *  	send it to the score through the changeScore,
                	 *  	add_Event or delete_Event functions
                	 * 
                	 */
                	
                	if(selectedIndex != -1){
                        int originalTime = inputArray[selectedIndex].getTime(),
                        originalDur = inputArray[selectedIndex].getDur(),
                        newX = e.getX(),
                        newY = e.getY();
                        
                        
                        if(newX >= maxClickableWidth){
                        	selectedIndex = -1;
                        	return;
                        }                       	 
                        // *** DRAG action
                        if(dragNotStretch == 0){ 
                        	int originalPitch = inputArray[selectedIndex].getPitch();
                            int newStartTime = newX - lastClickedX + originalTime;
                            int newPitch = ((newY-10)/10);
                                
                            if(newStartTime + inputArray[selectedIndex].getDur() 
                            		>= maxClickableWidth){
                            	selectedIndex = -1;
                               	return;
                            }
                               	 
                            if((originalPitch == 0) && (((newY - 10)/10) != 0)){
                            	selectedIndex = -1;
                                return;
                            }
                                
                                
                            // the following ensures the notes stay within bounds
                            if(newStartTime < 0)
                            	newStartTime = 0;
                            if(newPitch > 49)
                            	newPitch = 49;
                            else if(newPitch < 0)
                            	newPitch = 1;
                                
                            inputArray[selectedIndex].setStartTime(newStartTime); 
                            inputArray[selectedIndex].setPitch(newPitch);

                        }
                        else if(dragNotStretch == 1){ // stretch from left
                        	if(newX > originalTime + originalDur - 12)
                        		newX = originalTime + originalDur - 12;
                            inputArray[selectedIndex].setDur(
                            		originalDur + originalTime - newX);
                            inputArray[selectedIndex].setStartTime(newX);                                        
                                
                        }
                        else { // stretch from right    
                        	if(newX < originalTime + 12)
                        		newX = originalTime + 12;
                        	inputArray[selectedIndex].setDur(newX - originalTime);
                               
                        }
                        //sends the data back to The middleman to send it back to nyquist

                        inputArray = sm.my_score.changeScore(
                        		inputArray[selectedIndex].getIndex(),
                        		inputArray[selectedIndex].getTime(), 
                        		inputArray[selectedIndex].getDur(),
								inputArray[selectedIndex].getPitch());
                        repaint();
                        revalidate();
                        selectedIndex = -1; //end of drag or stretch. selectedIndex back to -1..
                             
                        sm.writeFile();
                     }
                }           
                // *** IF DELETE
                else if(currentOption == 1){ 
                	if(selectedIndex != -1){
               		
                		inputArray = sm.my_score.del_Event(inputArray[selectedIndex]);
                        repaint();
                        revalidate();
                        //at end. selectedIndex back to -1
                        selectedIndex = -1;
                        sm.writeFile();
                        return;
                	}
                }
                // *** IF ADD
                else if(currentOption == 0){ 
                	int tempY = (e.getY()-10)/10;
                	if((e.getX() + 20) > maxClickableWidth){
                		return;
                	}
                	if( !(tempY <= 49 && tempY >=0)){
                		return;
                	}
                	
                	inputArray = sm.my_score.add_Event(new 
                			Piano_Roll.ScoreManipulator.Event(e.getX(), tempY, 20 , -1));
                    repaint();
                    revalidate();
                    //at end. selectedIndex back to -1
                    selectedIndex = -1;
                    sm.writeFile();
                    return;               	
                }
                
          }
         /*
          * mousePressed just does 2 things
          *  a) determine if its a drag or stretch action, 
          *  	as well as determining the Event manipulated
          *  b) logging down te x-coordinate of the click,
          *  	for use of dragging nicely later
          */
         public void mousePressed(MouseEvent e) {
        	 	determineAction(e);
                lastClickedX = e.getX();
         }

         public void mouseEntered(MouseEvent e) {/* do nothing*/}
         public void mouseExited(MouseEvent e) { /* do nothing*/}
         public void mouseClicked(MouseEvent e) { /* do nothing*/}

    }             
		/*
		 *  addComponentsToPane function deals with the layout
		 *  and components outside the grid, such as text, buttons
		 *  and xaxis and yaxis scales..
		 * 
		 */
	    public static void addComponentsToPane (Container pane) {
            // zero is center alignment.... 
            // can use HTML ways to edit the font/type
	    	JLabel topLabel = new JLabel("<html><font face=Verdana size=3>" +
	    			"Piano roll display:</font></html>", 0);
	    	JLabel bottomLabel = new JLabel(
	    		"<html><font face=Verdana size=2><ul><li>To stretch, " + 
        		"click on the yellow sections of notes</li><li>To transpose," +
        		" click on blue section of notes</li></ul></font></html>", 0);
 
	    	topLabel.setPreferredSize(new Dimension(1024, 50));
        
            JLabel leftPanel = new JLabel();
            JLabel bottomCenterLabel = new JLabel();
                
            ImageIcon xAxis = new ImageIcon("xAxis.jpg");
            ImageIcon yAxis = new ImageIcon("xxx.jpg");
            bottomCenterLabel.setIcon(xAxis);
                
               
            JPanel mainPanel = new JPanel();
            mainPanel.setLayout(new BorderLayout());
              
            // for center panel
            JPanel subCenterPanel = new BaoPanel();
            mainPanel.add(bottomCenterLabel, BorderLayout.SOUTH);
            mainPanel.add(subCenterPanel, BorderLayout.CENTER);
                
                
            // for left panel
            leftPanel.setIcon(yAxis);                
            leftPanel.setPreferredSize(new Dimension(30, 800));
                
            //for bottom panel
            JPanel bottomPanel = new JPanel(); 
            JPanel subBottomPanel = new JPanel();
            bottomPanel.setLayout(new BorderLayout());

            // setting up the buttons, and adding listeners            
            JButton okButton = new JButton("Add");
            JButton deleteButton = new JButton("Delete");
            JButton arrangeButton = new JButton("Drag/Stretch");
                
            okButton.addActionListener(new okButtonListener(
            		okButton, deleteButton, arrangeButton));
            deleteButton.addActionListener(new deleteButtonListener(
            		okButton, deleteButton, arrangeButton));
            arrangeButton.addActionListener(new arrangeButtonListener(
            		okButton, deleteButton, arrangeButton));
            arrangeButton.setEnabled(false);
            subBottomPanel.setLayout(new FlowLayout());
            subBottomPanel.add(okButton);
            subBottomPanel.add(deleteButton);
            subBottomPanel.add(arrangeButton);
            
            
            // combining the 2 sub panels together to form the bottomPanel
            bottomPanel.add(bottomLabel, BorderLayout.CENTER);
            bottomPanel.add(subBottomPanel, BorderLayout.EAST);
                
            JLabel fillerLabel2 = new JLabel("");
                
            fillerLabel2.setSize(10, 600);
            fillerLabel2.setVisible(true);
                
            // combining all the panels together to 
            // form the bulk of the screen
            pane.add(topLabel, BorderLayout.NORTH);                
            pane.add(mainPanel, BorderLayout.CENTER);
            pane.add(bottomPanel, BorderLayout.SOUTH);
            pane.add(leftPanel, BorderLayout.WEST); 
            pane.add(fillerLabel2, BorderLayout.EAST); 
    }
	    
   /*
    *  below are the 3 listener classes that deal with the 3 main
    *  operations: add, delete, drag/drop. 
    * 
    *  what they do is to initialise the buttons and to set 
    *  the respective buttons to become enabled/disabled
    *  when clicked. 
    *  
    *  the MouseReleased function will deal with user inputs
    *  eg, drags or deletes instead of the listeners below
    *  
    */
    private static class okButtonListener implements ActionListener {
	   	private JButton ok, delete, arrange;
	   
	   	public okButtonListener(JButton ok, JButton delete, JButton arrange){
	   		this.ok = ok;
	   		this.delete = delete;
	   		this.arrange = arrange;
	   	}
	   
    	public void actionPerformed(ActionEvent e){
    		currentOption = 0;
    		ok.setEnabled(false);
    		delete.setEnabled(true);
    		arrange.setEnabled(true);
    	}
    }
    
	private static class deleteButtonListener implements ActionListener {
		private JButton ok, delete, arrange;
   		public deleteButtonListener(JButton ok, JButton delete, JButton arrange){
   			this.ok = ok;
   			this.delete = delete;
   			this.arrange = arrange;
   		}
		public void actionPerformed(ActionEvent e){
			currentOption = 1;
			ok.setEnabled(true);
			delete.setEnabled(false);
			arrange.setEnabled(true);			
		}
	}	

	private static class arrangeButtonListener implements ActionListener {
		private JButton ok, delete, arrange;
   		public arrangeButtonListener(JButton ok, JButton delete, JButton arrange){
   			this.ok = ok;
   			this.delete = delete;
   			this.arrange = arrange;
   		}
		public void actionPerformed(ActionEvent e){
			currentOption = 2;
			ok.setEnabled(true);
			delete.setEnabled(true);
			arrange.setEnabled(false);			
		}
	}		
	
	
    /**
     * Create the GUI and show it.  For thread safety,
     * this method should be invoked from the
     * event-dispatching thread.
     */
	    private static void createAndShowGUI() {
	        //Create and set up the window.
	        JFrame.setDefaultLookAndFeelDecorated(true);
	        JFrame frame = new JFrame("Piano Bao : by Priscilla, Eugene and Damon. ");
	        frame.setResizable(false);
	        
	        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	        frame.addWindowListener(new prepareToComeBackListener());
	        //Set up the content pane.
	        //frame is the most most outermost enclosure
	        frame.getContentPane().setLayout(new BorderLayout());
	        addComponentsToPane(frame.getContentPane());

	        //Display the window, crops off exactly where the grid stops printing
	        frame.setSize(984, 672);
	        frame.setVisible(true);
	    }
	    

    public static void scoreEdit(String scoresaveFilename) {
    	
    //Schedule a job for the event-dispatching thread:
    //creating and showing this application's GUI.

	if (scoresaveFilename == null)
	{
		System.out.println("Error. No file argument.");
		return;
	}
		
	filename = scoresaveFilename;
	
	try 
	{
        BufferedReader in = new BufferedReader(new FileReader(filename));
        scoreName = in.readLine();
        String str;
        //make score object
        while ((str = in.readLine()) != null) 
        {	
            sm.my_score.addNote(sm.makeNote(str));
        }

        in.close();
	    
        //parse it to Event list, for GUI functions
        inputArray = sm.my_score.toEventList();

    } 
	catch (IOException e) 
	{
		System.out.println("IOException caught: " + e.getMessage());			
    }
    	
   	javax.swing.SwingUtilities.invokeLater(new Runnable() {
        public void run() {            	
           	createAndShowGUI();
        }
            
    });

}
	private static class prepareToComeBackListener implements WindowListener{
		public void windowActivated(WindowEvent e){}
		public void windowClosed(WindowEvent e){
			currentOption = 2;
			
		
		}
		public void windowClosing(WindowEvent e){/*do nothing*/}
		public void windowDeactivated(WindowEvent e){/*do nothing*/}
		public void windowDeiconified(WindowEvent e){/*do nothing*/}
		public void windowIconified(WindowEvent e){/*do nothing*/}
		public void windowOpened(WindowEvent e){/*do nothing*/}
	}
    
// This is a data structure that stores and 
// changes notes from a Nyquist score. It
// nests another data structure, ScoreNote.
public static class ScoreManipulator 
{

	public ScoreNote my_score; 	// a collection of notes, with functions to
								// add, delete and change the collection.

	public ScoreManipulator()
	{
		my_score = new ScoreNote();
	}

	// takes in a String of the following format:
	// start_time duration pitch option_str1 ... option_strn
	// and parses it into a Note object.
	public Note makeNote(String str)
	{
		int numTokens;
		StringTokenizer st = new StringTokenizer(str);
		if ( (numTokens = st.countTokens()) < 3)
		{
			System.out.println("ERROR in makeNote: some string too short: "
					           + str);
			return null;
		}
		
		String st_time, dur, pitch;
		
		// the remainder of the string must be the options
		String []opts = new String[numTokens - 3];
		
		int numOpts = 0;
	
		st_time = st.nextToken();
		dur = st.nextToken();
		pitch = st.nextToken();
	
		while (st.hasMoreTokens()) {
			opts[numOpts] = st.nextToken();
			numOpts++;
		}
        
        //System.out.println("st_time " + st_time + " dur " + dur +
        //                   " pitch " + pitch + " opts " + opts);
                           
		double stime = Double.parseDouble(st_time);
		double duration = Double.parseDouble(dur);
		int ptch = -9999;
        if (!pitch.equals("NIL")) ptch = Integer.parseInt(pitch);
	
		return new Note(stime,duration,ptch,opts,numOpts);
	}

	public void writeFile()
	{
		try {
			FileWriter fwr = new FileWriter(filename);
			fwr.write(my_score.writeScore());
			fwr.close();
		}
		catch (IOException e) {
			System.out.println("writeFile: IOException.");
		}
		
	}
	

	// The Note class stores information about a Nyquist
	// score note such that it is easy to access the fields
	// that we need in this piano roll application.
	// Notice that the field pitch can be zero (0).
	public class Note
	{

		// fields
		private double start_time; // Note's starting time
		private double duration;
		private int pitch, numopts;
		private String []options; 	// all other information that the piano roll will
									// not use, but that we must preserve

		public Note(double st_time, double dur, int pitch, String []opts, int numopts)
		{
			start_time = st_time;
			duration = dur;
			this.pitch = pitch;
			this.numopts = numopts;
			options = opts;
		}
		
		//accessors
		public double get_start()
		{
			return start_time;
		}
		
		public double get_dur()
		{
			return duration;
		}
		
		public int get_pitch()
		{
			return pitch;
		}
		
		public int get_numopts()
		{
			return numopts;
		}
		
		public String [] get_opts()
		{
			return options;
		}
		
		// for debugging
		public void PrintNote()
		{
			System.out.print("Start: " + start_time + " Duration: " + duration 
								+ " Pitch: " + pitch);
			for (int i = 0; i < numopts; i++)
			{
				System.out.print(" Opt string " + i + ": " + options[i]);
			}
			System.out.println();
		}

	}


	// ScoreNote is a collection of notes.
	// It also has a collection of functions
	// such as add, delete and change that
	// is used by the GUI functions to change
	// the score when the user drags/shifts/adds/
	// deletes notes.
	public class ScoreNote
	{
		private Note[] score; // the collection of notes
		private int num_notes; // capacity of the score
		private int notes_used; // number of notes in the 
								// collection that are
		                        // presently non-empty.

		public ScoreNote()
		{
			num_notes = 0;
			notes_used = 0;
			score = new Note[0];
		}

		//adds a Note to the data structure sorted
		//in order of start time.
		//Invariant: score starts out sorted. This is
		//true at the start => invariant is preserved
		//each time we call this
		public void addNote(Note newnote)
		{
			// If there exists a note in the score of the exact
			// pitch duration and start time, then don't add it
			// in.
			for(int j = 0; j < notes_used; j++) {
				if(score[j].get_start() == newnote.get_start() 
						&& score[j].get_pitch() == newnote.get_pitch()
						&& score[j].get_dur() == newnote.get_dur()){
					return;
					
				}
			}

			if (notes_used >= num_notes)
			{
				if (notes_used == 0)
				{
					score = new Note[10];
					num_notes = 10;
				}
				else
				{
					Note [] tempscore = new Note[num_notes * 2];
					for (int i = 0; i < notes_used; i++)
					{
						tempscore[i] = score[i];
					}  			
					score = tempscore;	  			
					num_notes *= 2;
				}
			}
			
			for (int i = 0; i < notes_used; i++)
			{
				// when we find the index that newnote should be in,
				// place it there and return
				if (score[i].get_start() >= newnote.get_start())
				{
					shift_right(i);
					score[i] = newnote;
					notes_used++;
					return;
				}
			}
			
			// otherwise, newnote's start time is
			// the largest in the score, and we
			// put it at the back of the array.
			score[notes_used] = newnote;
			notes_used++;
		}
		
		// shifts all notes in the array after and including
		// index by one to the right.
		public void shift_right(int index)
		{
			for (int i = notes_used - 1; i >= index; i--)
			{
				score[i+1] = score[i];
			}
			
		}

		// deletes the note specified by index from the score
		// while maintaining the invariant that notes in score
		// are sorted by start time.
		public void delNote(int index)
		{
			if (index < 0 || index >= notes_used)
			{
				System.out.println("del_note: Error! index out of bounds!");
				return;
			}
			
			shift_left(index);
			notes_used--;
		}
		
		// shifts all notes in score after
		// index by one to the left.
		public void shift_left(int index)
		{
			for (int i = index; i < notes_used; i++)
			{
				score[i] = score[i+1];
			}
			
		}

		// accessor for number of used notes in the score
		public int getLength()
		{
			return notes_used;
		}

		// accessor for a note at the particular index
		public Note getNote(int index)
		{
			if (index < 0 || index >= notes_used)
			{
				System.out.println("getNote: trying to index note out of bounds!");
				return null;
			}
		
			return score[index];
		}

		// makes an incremental change to the score, but makes sure to
		// preserve the invariant that score remains sorted by start time.
		public Event[] changeScore(int index, int new_start, int new_dur, int new_pitch)
		{
			if (index < 0 || index >= notes_used)
			{
				System.out.println("changeScore: index out of bounds!");
				return null;
			}

			// make the new note
			Note note = new Note( (double)(new_start / 10), (double)(new_dur / 10), 
					              convertPitch(new_pitch), score[index].get_opts(), 
								  score[index].get_numopts() );

			// delNote and addNote both preserve the invariant that
			// the notes in score are sorted in order of start time.
			delNote(index);
			addNote(note);
			
			// add one more layer of abstraction for the convenience of
			// the GUI functions, by using Event objects rather than
			// Note objects. The difference is basically in the standards
			// of time and note values, documented below.
			return toEventList();
		}
		
		// converts from a MIDI note to a note in 
		// our representation and vice versa.
		public int convertPitch(int pitch)
		{
			if (pitch == 0)
				return pitch;
			
			return 84 - (pitch - 1);
		}
		
		// Gets the list for the GUI functions to manipulate.
		// Invariant: the size of the event list returned is
		//            exactly the number of notes in our score. 
		public Event [] toEventList()
		{
			Event [] result = new Event[notes_used];
			int num_events = 0;
			
			for (int i = 0; i < notes_used; i++)
			{
				Note temp = score[i];
				
				// Note: we multiply by 10 for convenience of computation,
				// as Event stores time and duration as integers, not doubles.
				int start = (int)(temp.get_start() * 10);
				int dur = (int)(temp.get_dur() * 10);
				Event new_event = new Event(start,convertPitch(temp.get_pitch()),dur,i);
				result[i] = new_event;
				num_events++;
				
			}
			
			// size the array
			return shape_Event_List(result,num_events);
		}

		// The original event list may have some redundant
		// blank entries at the end. We use this shape_Event_List
		// to size it nicely to the number of events that are actually
		// needed.
		public Event [] shape_Event_List(Event [] original, int num_events)
		{
			if (original == null)
			{
				System.out.println("shape_Event_List: null array given.");
				return null;
			}
			
			Event [] result = new Event [num_events];
			
			for (int i = 0; i < num_events; i++)
			{
				result[i] = original[i];
			}
			return result;
		}
		
		// When the user clicks to add a new note,
		// this function is called to add an event into the
		// existing Event array.
		public Event [] add_Event(Event e)
		{
			
			// create new note
			String [] def_string = new String[1];
			def_string[0] = "(NOTE)";
			Note newnote = new Note(e.getTime()/10,
					                e.getDur()/10,
									convertPitch(e.getPitch()),
									def_string,
									1);
			
			// add it to score
			addNote(newnote);
						
			// return new list of events
			return toEventList();
		}
		
		// When user clicks to delete a note from
		// the piano roll, this function is called
		// to cleanly remove the event, maintaining
		// the invariant that the notes in score are
		// sorted in order of start time.
		public Event [] del_Event(Event e)
		{
			delNote(e.getIndex());
			return toEventList();
		}

		// This function outputs a string
		// that represents the score
		// in the format of the default file
		// from which we parsed the original score
		public String writeScore()
		{
			String result = scoreName + "\n";
			for (int i = 0; i < notes_used; i++)
			{
				result += Double.toString(score[i].get_start());
				result += " ";
				result += Double.toString(score[i].get_dur());
				result += " ";
                if (score[i].get_pitch() == -9999) result += "NIL";
                else result += Integer.toString(score[i].get_pitch());
				for (int j = 0; j < score[i].get_numopts(); j++)
				{
					result += " ";
					result += (score[i].get_opts())[j];
				}
				result += "\n";
			}
			return result;
		}
		
		// debugging function
		public void PrintScore()
		{
			System.out.println("Printing Score..");
			System.out.println("Variables: notes_used = " + notes_used
								+ " num_notes = " + num_notes);
			for (int i = 0; i < notes_used; i++)
			{
				Note temp = getNote(i);
				temp.PrintNote();
			}
			System.out.println("Done Printing Score!");
		}

	}


	/*
	the Event class, which defines a stretch of note on the piano roll
	15-392 - Pianobao project
	
	Most of the fields in the Event object are derived from 
	those in the Note class. The GUI functions use Events as
	the representation of the actual Note, for simplicity.
	For example, start time and duration are ints in Event
	but are doubles in Note. Also, the values of start time and
	duration are 10 times larger than its counterparts in Note
	because the scaling prevents the notes from being too small
	in the piano roll picture.
	
	Another point to note is that the pitch in Event is not
	in standard MIDI format, while that in Note is. This is
	because it was easier to use a range of 0-49 (these are 
	graphical coordinates) when plotting the pitch-axis in 
	a GUI, and this range corresponds to the MIDI pitches 
	36-84 (C2 -> C4).
	
	The index field the index of the array in which this Event
	resides. This is for book-keeping purposes.
 	the values stored in this Event object is a graphical location 
 	on the interface. A set of value converters will parse these
 	coordinates into actual musical values when we need to update
 	the score in Nyquist.

	*/


	public static class Event {
		private int time, pitch, dur, index;


		public Event(int startTime, int givenPitch, int duration, int index) {
	        time = startTime;
	        pitch = givenPitch;
	        dur = duration;
	        this.index = index;
		}

		public Event(int startTime, int givenPitch) {
	        time = startTime;
	        pitch = givenPitch;
	        dur = 1; 
		}

		/*
		 	checks whether the given x and y coordiante is within this particular event
		 	20 is the pitchHeight. Basically works by checking whether the coordinates
		 	clicked is within the rectangle of the Event on screen
		
		*/
	    public boolean checkWithinDuration(int timeX, int pitchY) {
	      
	    	return (((pitchY) <= ((pitch*10) + 20 )) && 
	    			(pitchY >= ((10*pitch) + 10))   &&
	    			(timeX >= time)                 && 
	    			(timeX <= (time + dur))
			   		);
	    } 

	    // uses coordinates of user's click to determine whether
	    // user intends to drag the note or stretch its duration.
	    // returns : 0 if drag, 
	    //           1 if stretch from left, 
	    //           2 if stretch from right (change dur)
	    public int checkDragNotStretch(int timeX, int pitchY) {
	    	if ((timeX <= (time+3)) && (timeX >= time)) {
	    		return 1;
	    	}
	        if ((timeX <= (time + dur)) && (timeX >= dur + time - 3)){
	            return 2;
	        }
	        return 0;
	    }        
	    
	    //accessors
		public int getTime() {
	        return time;
		}

		public int getPitch() {
	        return pitch;
		}

		public int getDur() {
	        return dur;
		}

		public int getIndex() {
		return index;
		}

		//mutators
		public void editEvent(int startTime, int givenPitch, int duration) {
	        time = startTime;
	        pitch = givenPitch;
	        dur = duration;
	        return;
		}

		public void setDur(int duration) {dur = duration;}
		public void setStartTime(int startTime) {time = startTime;}
	    public void setPitch(int newPitch) {pitch = newPitch;}
	    public void setIndex(int newIndex) {index = newIndex;}
	    
		public String toString(){
	        return "Event of Pitch: " + pitch + " , start time = " + time + " , duration = " +
					dur + ", index = " + index + "\n";
		}

	}
    // end of Event class


	}


}

