package jnyqide;


import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.Dialog;

import javax.print.attribute.standard.OutputDeviceAssigned;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JFrame;
import javax.swing.SwingWorker;
import javax.swing.JLabel;
import javax.swing.border.EmptyBorder;
import javax.swing.JTable;
import javax.swing.JScrollPane;
import javax.swing.AbstractListModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.ListSelectionModel;
import javax.swing.JOptionPane;
import javax.swing.JInternalFrame;
import javax.swing.event.InternalFrameListener;
import javax.swing.event.InternalFrameEvent;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.io.File;
import java.io.PrintWriter;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.net.HttpURLConnection;
import java.net.URL;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.Files;
import java.nio.file.FileVisitResult;
import java.nio.file.attribute.BasicFileAttributes;

import java.util.Vector;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.ArrayList;

import java.security.MessageDigest;

import java.util.Base64;

public class ExtensionManager extends JNonHideableInternalFrame 
                              implements ActionListener {
	/**
	 * URL of the Extension list. Should directly point to the text file.
	 */
	private static String EXTENSION_LIST_URL = 
        "https://www.cs.cmu.edu/~music/nyquist/extensions/extlist.txt";
        /* "file:///Users/rbd/nyquist/extensions/extlist.txt"; */

	private final JPanel contentPanel = new JPanel();
	private JTable table;
	private JPanel buttonPane = new JPanel();
	private DefaultTableModel dtm;
    private MainFrame mainFrame;
    private boolean updating; // this is selected after user clicks Update
    public enum Cols { EXTENSION, VERSION, DATE, INSTALLED, DESCRIPTION, URL, CHECKSUM };
    public enum ExtList { EXTENSION, URL, VERSION, DATE, CHECKSUM, DESCRIPTION };
    // and it disables the prompt to see if you really want to install
    // over an existing extension.
    // This is cleared after each button press other than Update.
    
    String[] extensions;
    List<String> installedExtensions;
    List<String> failedExtensions;
    int[] selectedRows;
    int selectedRow;
    JLabel progressText;
    JDialog progress;

    public class Downloader extends SwingWorker<Void, String> {
        String task, ext, url, filepath, tmpdir, extdir;

        Downloader(String task, String ext, String url, String filepath, 
                   String tmpdir, String extdir) {
            this.task = task;
            this.ext = ext;
            this.url = url;
            this.filepath = filepath;
            this.tmpdir = tmpdir;
            this.extdir = extdir;
        }

        @Override
        protected Void doInBackground() {
            System.out.println("doLongTask::doInBackground called");
            if (task == "test") {
                System.out.println("doLongTask::doInBackground called for test");
                try {
                    for (int i = 0; i < 5; i++) {
                        Thread.sleep(1000);
                        System.out.println("doLongTask::doInBackground sleep finished, i = " + i);
                        publish("test finished sleep at i = " + i);
                    }
                } catch (Exception e) {
                }
            } else if (task == "extlist") {
                System.out.println("calling loadExtensionData " + url);
                loadExtensionData(url);
            } else if (task == "files") {
                System.out.println("calling downloadExtension " + url);
                downloadExtension(ext, url, filepath, tmpdir, extdir, this);
            } else {
                System.out.println(
                                   "doLongTask: task not recognized - " + task);
            }
            System.out.println("doLongTask::doInBackground returning");
            return null;
        }

        public void status(String msg) { publish(msg); }

        protected void process(List<String> msgs) {
            // probably we could just take the last (or first? but could be empty?)
            // not sure of the possibilities here, so I'm being complete here by
            // iterating over all messages
            for (String msg : msgs) {
                System.out.println("doLongTask::process got msg " + msg);
                progressText.setText(msg);
                progressText.invalidate();
            }
        }

        @Override
        protected void done() {
            System.out.println("doLongTask::done called");
            progress.dispose();
        }
    };

	/**
	 * Create the dialog for Extension Manager
	 */
	public ExtensionManager(final MainFrame parent) 
	{
        mainFrame = parent;
        updating = false;
		// Set the extension manager window properties
		setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
		setTitle("Extension Manager");
        setResizable(true);
        setVisible(true);
        setClosable(true);
        setMaximizable(true);
        setIconifiable(true);
		setBounds(100, 100, 450, 300);
        setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
        final ExtensionManager extManFrame = this;
        addInternalFrameListener(
            new InternalFrameListener() {
                public void internalFrameClosing(InternalFrameEvent e) {
                    System.out.println("ExtensionManager Closing");
                        extManFrame.dispose();
                }
                public void internalFrameOpened(InternalFrameEvent e) {
                }
                public void internalFrameClosed(InternalFrameEvent e) {
                    updating = false;
                    parent.disconnectExtensionManager();
                    System.out.println("Extension Manager Closed");
                }
                public void internalFrameIconified(InternalFrameEvent e) {
                }
                public void internalFrameDeiconified(InternalFrameEvent e) {
                }
                public void internalFrameActivated(InternalFrameEvent e) {
                }
                public void internalFrameDeactivated(InternalFrameEvent e) {
                }
            }
        );

		getContentPane().setLayout(new BorderLayout());
		contentPanel.setLayout(new FlowLayout());
		contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        
		// Add the table for the list of extensions
		addTableToWindow();
        
		// Add the lower panel for buttons
		buttonPane = new JPanel();
		buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
		getContentPane().add(buttonPane, BorderLayout.SOUTH);
        
		// Add the buttons
        addVisitURLButton();
		addInstallButton();
		addUpdateButton();
		addCancelButton();
	}
    
	// ==================== Window-creation related functions ==================
    
	/**
	 * This function adds a table grid for the list of extensions on the window.
	 */
	private void addTableToWindow()
	{
		getContentPane().add(contentPanel, BorderLayout.CENTER);
		{
			table = new JTable();
			table.setSelectionMode(
                    ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			table.setCellSelectionEnabled(false);
			table.setColumnSelectionAllowed(false);
			table.setRowSelectionAllowed(true);
			String header[] = new String[] { "Name", "Ver.", "Date", "Installed",
                                             "Description", "URL", "Checksum" };
			dtm = new DefaultTableModel(0, 0)
                {
                    Class[] columnTypes = new Class[] {
						String.class, String.class, String.class, Boolean.class,
                        String.class, String.class, String.class
                    };
                    public Class getColumnClass(int columnIndex) {
                        return columnTypes[columnIndex];
                    }
/*                    boolean[] columnEditables = new boolean[] {
                        false, false, false, false, false, false, false
                    }; */
                    public boolean isCellEditable(int row, int column) {
                        return false;
                    }
                };
			dtm.setColumnIdentifiers(header);
			table.setModel(dtm);
			{
				table.getColumnModel().getColumn(Cols.EXTENSION.ordinal()).
                        setPreferredWidth(182);
				table.getColumnModel().getColumn(Cols.VERSION.ordinal()).
                        setPreferredWidth(40);
				table.getColumnModel().getColumn(Cols.DATE.ordinal()).
                        setPreferredWidth(120);
				table.getColumnModel().getColumn(Cols.INSTALLED.ordinal()).
                        setPreferredWidth(90);
				table.getColumnModel().getColumn(Cols.DESCRIPTION.ordinal()).
                        setPreferredWidth(302);
				table.getColumnModel().getColumn(Cols.URL.ordinal()).
                        setPreferredWidth(212);
				table.getColumnModel().getColumn(Cols.CHECKSUM.ordinal()).
                        setPreferredWidth(100);
				getContentPane().add(table, BorderLayout.NORTH);
			}
			getContentPane().add(new JScrollPane(table));
            
			loadExtensionDataToTable();
		}
	}
    
    
	/**
	 * This function adds 'Visit URL' button on the window.
	 * Pressing this button will open the extension root file in a browser.
	 */
	private void addVisitURLButton()
	{
		JButton visitButton = new JButton("Visit URL");
        
		// Define 'Visit URL' button functionality 
		visitButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                try {
                    updating = false;
                    int selectedRow = table.getSelectedRow();
                    if (selectedRow == -1) {
                        JOptionPane.showMessageDialog(contentPanel,
                         "Select an extension before clicking the " +
                          "Visit URL button",
                         "Unknown Problem", JOptionPane.ERROR_MESSAGE);
                        return;
                    }
                    // get the URL
                    String url = table.getModel().getValueAt(
                            selectedRow, Cols.URL.ordinal()).toString();
                    // open the URL
                    mainFrame.openPage(url);
                } catch (Exception e) {
                    JOptionPane.showMessageDialog(contentPanel,
                            "An unknown error happened opening the URL.",
                             "No extension selected",
                             JOptionPane. INFORMATION_MESSAGE);
                }
            }});
		// visitButton.setActionCommand("Visit");
		buttonPane.add(visitButton);
	}


	/**
	 * This function adds 'Install' button on the window.
	 * Pressing this button will install the extensions selected in the table. 
	 */
	private void addInstallButton() {
		JButton installButton = new JButton("Install");
        
		// Define 'Install' button functionality 
		installButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                performInstall();
            }
            });
        
		// installButton.setActionCommand("Install");
		buttonPane.add(installButton);
		getRootPane().setDefaultButton(installButton);
	}
    

    // Install all extensions that are selected. This is called
    // by the "Install" button
    private void performInstall() {
        try {
            // Create the extensions directory at the path defined 
            // in the environment variable
            String extDir = createExtDirectory();
            if (extDir == null) return;
            
            // Define lists for installed and failed extensions
            // (used for the final message). These are filled by
            // a worker thread, but there should be a handoff
            // where this (Swing) thread does not touch these
            // ArrayLists until the worker thread is done.
            installedExtensions = new ArrayList<String>();
            failedExtensions = new ArrayList<String>();
            
            // Retrieve the selected rows from table
            selectedRows = table.getSelectedRows();
            if (selectedRows.length == 0) {
                JOptionPane.showMessageDialog(contentPanel, 
                        "No extensions selected to install!", 
                        "Extensions", JOptionPane.WARNING_MESSAGE);
                updating = false;
                return;
            }
            
            // Install the selected extensions
            for (int i = 0; i < selectedRows.length; ++i) {
                selectedRow = selectedRows[i];
                // Create a sub-directory in extensions directory with 
                // extension name
                String extensionName = table.getModel().getValueAt(
                        selectedRow, Cols.EXTENSION.ordinal()).toString();
                String extensionDir = extDir + extensionName;
                String tmpPackageDir = extDir + "tmp-" + extensionName +
                                       File.separator;

                // Warn user if package already exists or tmp exists
                boolean pdExists = (new File(extensionDir)).exists();
                boolean tpdExists = (new File(tmpPackageDir)).exists();
                if (!updating && (pdExists || tpdExists)) {
                    String exists;
                    if (pdExists && tpdExists) {
                        exists = extensionDir + " and \n" + tmpPackageDir;
                    } else if (pdExists) {
                        exists = extensionDir;
                    } else {
                        exists = tmpPackageDir;
                    }
                    if (JOptionPane.showConfirmDialog(contentPanel,
                            "Warning: Installing " + extensionName +
                                " will require the deletion of the existing\n" +
                                exists + "\nOK?",
                            "Directory Conflict", JOptionPane.YES_NO_OPTION) !=
                            JOptionPane.YES_OPTION) {
                        failedExtensions.add(extensionName);
                        continue;
                    };
                    if (tpdExists) {
                        deleteDirectory(tmpPackageDir);
                    }
                }

                if (makeDirectory(tmpPackageDir) == false) {
                    JOptionPane.showMessageDialog(contentPanel, 
                       "Error creating directory at the following " +
                       "path:\n'" + tmpPackageDir + "'\n\nPackage '" +
                       extensionName + "' not installed!",
                       "Directory Creation Problem", JOptionPane.ERROR_MESSAGE);
                    failedExtensions.add(extensionName);
                    continue;
                }
                
                // Read the contents of the selected package file
                String url = table.getModel().getValueAt(
                        selectedRow, Cols.URL.ordinal()).toString();
                String filepath = fileLocalPathFromURL(url, tmpPackageDir);

                doLongTask("files", "Installing " + extensionName, 
                           extensionName, url, filepath, tmpPackageDir, 
                           extensionDir);
            }
            // Show a message about the completion of installation
            showPostInstallationMessage(installedExtensions, failedExtensions);
            updating = false;
        } catch (Exception e) {
            updating = false;
            JOptionPane.showMessageDialog(contentPanel,
                "An unknown error happened installing the packages!\n" +
                "Some packages may have been installed!\nError message: " +
                e.toString(), "Unknown Problem", JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
        }
    }        


    // download a single extension - this runs in a worker thread and is 
    //   called by doLongTask()
    void downloadExtension(String ext, String url, String filepath, 
                           String tmpPackageDir, String extensionDir,
                           Downloader worker) {
        String fileContent = (filepath == null) ? null :
            saveFromURLtoFile(url, filepath);
        if (fileContent == null) {
            JOptionPane.showMessageDialog(contentPanel,
                    "Error reading the following URL:\n" + url + 
                        "\n\nPackage " + ext + "' not installed!", 
                    "Error", JOptionPane.ERROR_MESSAGE);
            failedExtensions.add(ext);
            // remove tmp dir:
            deleteDirectory(tmpPackageDir);
            return;
        }
                
        // Keep the list of all the files for checksum
        List<String> packageFiles = new ArrayList<String>();
        packageFiles.add(fileLocalPathFromURL(url, tmpPackageDir));

        // Parse the extension file to read the additional files  
        String[] otherFiles = extractOtherFilesFromSAL(fileContent);
        if (otherFiles == null) {
            JOptionPane.showMessageDialog(contentPanel,
                    "Unknown error happened while parsing the extension " + 
                    "file!\n" + url + "\n\nPackage " + ext + 
                    " not installed!\n", "Error", JOptionPane.ERROR_MESSAGE);
            failedExtensions.add(ext);
            deleteDirectory(tmpPackageDir);
            return;
        }
                
        // Download the additional files
        String baseURL = dirFromPackageURL(url);
        for (int j = 0; j < otherFiles.length; ++j) {
            String fileLink = baseURL + otherFiles[j];
            String pathname = tmpPackageDir + otherFiles[j];
            worker.status("Downloading " + otherFiles[j] + " (" +
                          (j + 1) + " of " + otherFiles.length + ")");
            fileContent = saveFromURLtoFile(fileLink, pathname);
            packageFiles.add(pathname);
        }
                
        // Compare the checksum of the downloaded files 
        // with the reference checksum 
        String referenceChecksum = table.getModel().
            getValueAt(selectedRow,
                       Cols.CHECKSUM.ordinal()).toString();
        String downloadedChecksum = calculateFileChecksum(packageFiles);
        if (downloadedChecksum == null) {
            JOptionPane.showMessageDialog(contentPanel,
                    "Failure computing checksum; maybe a downloaded file\n" +
                        "was deleted or not stored for Extension " + ext,
                    "Checksum Error", JOptionPane.ERROR_MESSAGE);
        } else {
            System.out.println("ref " + referenceChecksum + " downloaded " + 
                               downloadedChecksum);
        }
        // closeProgressInfo();
        if (!downloadedChecksum.equalsIgnoreCase(referenceChecksum)) {
            // remove the downloaded package if checksum is
            // different
            JOptionPane.showMessageDialog(contentPanel,
                    "Checksum of the available package is different than " + 
                        "the verified version!\nExtension " + ext + 
                        " not installed!\n" + "Downloaded checksum: " + 
                        downloadedChecksum,
                    "Checksum Error", JOptionPane.ERROR_MESSAGE);
            downloadedChecksum = null; // enables common failure code below
            System.out.println("Checksum: " + downloadedChecksum);
        }
        // common failure code if there was a checksum problem:
        if (downloadedChecksum == null) { 
            removeFiles(packageFiles);
            failedExtensions.add(ext);
            deleteDirectory(tmpPackageDir);
            return;
        }
                
        // save the checksum
        if (!saveToFile(tmpPackageDir + "checksum.txt",
                        downloadedChecksum)) {
            JOptionPane.showMessageDialog(contentPanel,
                    "Extension checksum for " + ext + 
                        " could not be written to " + tmpPackageDir +
                        "checksum.txt\n" + "Extension '" + ext + 
                        "' not installed!\n", 
                    "Checksum Error", JOptionPane.ERROR_MESSAGE);
            failedExtensions.add(ext);
            deleteDirectory(tmpPackageDir);
            return;
        }
                
        // everything looks good, replace the old directory
        deleteDirectory(extensionDir);
        String tmp = tmpPackageDir; // remove trailing "/"
        // tmp = tmp.substring(0, tmp.length() - 1);
        if (!renameDirectory(tmp, extensionDir)) {
            JOptionPane.showMessageDialog(contentPanel,
                        "Could not rename " + tmp + "\nto " + extensionDir +
                        "\nLeaving the temporary directory, which contains " +
                        "good package files.", "Installation Failure", 
                    JOptionPane.ERROR_MESSAGE);
            failedExtensions.add(ext);
            return;
        }
        installedExtensions.add(ext);
        // Three things happen at startup:
        // (1) autoload.lsp is loaded if found
        for (String name : otherFiles) {
            if (name.equals("autoload.lsp")) {
                mainFrame.callFunction("lisp-loader", "\"" + ext + 
                                       "/" + "autoload.lsp\"");
                return;
            }
        }
        // (2) nyquistwords.txt is processed if found
        WordList.processExtension(extensionDir);
        // (3) Installed field in table is set to true
        for (int row = 0; row < dtm.getRowCount(); row++) {
            String name = (String) 
                dtm.getValueAt(row, Cols.EXTENSION.ordinal());
            if (name.equals(ext)) {
                dtm.setValueAt(true, row, Cols.INSTALLED.ordinal());
                return;
            }
        }
    }


    // delete a directory and all contents just to be sure
    // (does not follow symbolic links -- that would be dangerous
    //
    void deleteDirectory(String name) {
        System.out.println("deleteDirectory: " + name);
        Path directory = Paths.get(name);
        try {
            Files.walkFileTree(directory, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file, 
                        BasicFileAttributes attrs) throws IOException {
                    Files.delete(file);
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult postVisitDirectory(Path dir, 
                        IOException exc) throws IOException {
                    Files.delete(dir);
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (Exception e) {
            System.out.println("WARNING in deleteDirectory: " + e.toString());
            return;
        }
    }


    // rename directory
    //
    boolean renameDirectory(String fromDir, String toDir) {
        File oldDir = new File(fromDir);
        File newDir = new File(toDir);
        try {
            return oldDir.renameTo(newDir);
        } catch(Exception e) {
            System.out.println("Exception in rename directory from " + 
                               fromDir + " to " + toDir + ": " + 
                               e.toString());
            return false;
        }
    }


	/**
	 * This function adds 'Cancel' button on the window.
	 * Pressing this button will close the Extension Manager window.
	 */
	private void addCancelButton()
	{
		JButton cancelButton = new JButton("Done");
        
		cancelButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent arg0) {
                    dispose();
                    updating = true;
                }
            });
        
		// cancelButton.setActionCommand("Cancel");
		buttonPane.add(cancelButton);
	}
    
    
	/**
	 * This function adds 'Update' button on the window.
	 * Pressing this button will update the extensions list 
     * from the remote server.
	 */
	private void addUpdateButton()
	{
		JButton updateButton = new JButton("Update");
        
		updateButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent arg0) {
                    // This is a test of background processing/reporting:
                    // doLongTask("test", "Initial msg for test", null,
                    //           null, null, null, null);
                    loadExtensionDataToTable();
                    checkForUpdates();
                }
            });
        
		// updateButton.setActionCommand("Update");
		buttonPane.add(updateButton);
	}
    
	// ====================== Project-dependent functions ===============
    
	/**
	 * Downloads the extension data and inserts it into the grid table.
	 */
	private void loadExtensionDataToTable() {
        dtm.setRowCount(0);
        String extDir = createExtDirectory();
        if (extDir == null) return;
                
        doLongTask("extlist", "Downloading list of extensions", null, 
                   EXTENSION_LIST_URL, null, null, null);
        if (extensions == null) {
            JOptionPane.showMessageDialog(contentPanel,
                    "Error loading the extension list from the following " +
                      "path: \n'" + EXTENSION_LIST_URL + "'",
                     "Error Loading Extensions", JOptionPane.ERROR_MESSAGE);
            return;
        }                
        System.out.println("loadExtensionDataToTable extDir: " + extDir);
        for (int i = 0; i < extensions.length; ++i) {
            // Ignore the line if it is a comment or if it is empty
            if (extensions[i].trim().isEmpty()) continue;
            if (extensions[i].trim().startsWith("#")) continue;
                
            String[] ext = SplitExtensionData(extensions[i], 6);
            boolean pdExists = (new File(extDir + 
                    ext[ExtList.EXTENSION.ordinal()])).exists();
            String installed = pdExists ? "Yes" : "No";

            dtm.addRow(new Object[] { ext[ExtList.EXTENSION.ordinal()], 
                    ext[ExtList.VERSION.ordinal()], ext[ExtList.DATE.ordinal()],
                    pdExists, ext[ExtList.DESCRIPTION.ordinal()],
                    ext[ExtList.URL.ordinal()], 
                    ext[ExtList.CHECKSUM.ordinal()] });
        }
	}

    
    public static String[] getExtensionDirs(String extDir) {
        System.out.println("getExtensionDirs " + extDir);
        File file = new File(extDir);
        String[] directories = file.list(new FilenameFilter() {
            @Override
            public boolean accept(File current, String name) {
                return !name.equals("piano") &&
                       new File(current, name).isDirectory();
            }
            });
        return directories;
    }


    /*
     * find all installed extensions by searching lib directory
     * compare their checksums to the expected up-to-date one
     */
	private void checkForUpdates() {
        String[] directories = getExtensionDirs(mainFrame.extDir);
        // search directories in extDir to find packages to update
        Vector<Integer> needToUpdate = new Vector<Integer>();
        Vector<String> unknownExtensions = new Vector<String>();
        if (directories != null) {
            for (String dir : directories) {
                System.out.println("Update checks " + dir);
                try {
                    // check checksum in each directory
                    String checksum = "";
                    FileInputStream fis = new FileInputStream(mainFrame.extDir +
                            dir + File.separator + "checksum.txt");
                    byte[] dataBytes = new byte[1024];
                    int nread = 0;
                    if ((nread = fis.read(dataBytes)) == -1) 
                        continue;
                    checksum = new String(dataBytes);
                    checksum = checksum.trim(); // remove newlines etc.
                    
                    // find row in table
                    File packageFile = new File(dir);
                    String extensionName = packageFile.getName();
                    int r;
                    for (r = 0; r < table.getModel().getRowCount(); r++) {
                        String s = table.getModel().getValueAt(r, 
                                Cols.EXTENSION.ordinal()).toString();
                        if (s.equalsIgnoreCase(extensionName)) {
                            break;
                        }
                    }
                    if (r == table.getModel().getRowCount()) {
                        unknownExtensions.add(extensionName);
                        System.out.println("Found unknown package: " +
                                           extensionName);
                        continue; // package not an official one
                    }
                    String referenceChecksum = table.getModel().
                            getValueAt(r, Cols.CHECKSUM.ordinal()).toString();
                    System.out.println("refsum " + referenceChecksum +
                                       "\ncursum " + checksum);
                    if (!checksum.equalsIgnoreCase(referenceChecksum)) {
                        System.out.println("NOT EQUAL: \n" +
                                           "refsum " + referenceChecksum +
                                           "\ncursum " + checksum);
                        needToUpdate.add(r);
                    }
                } catch (Exception e) {
                    System.out.println("ERROR: " + e.toString());
                }
            }
        }
        // show unknown packages
        if (unknownExtensions.size() > 0) {
            String unknown = unknownExtensions.get(0);
            for (int i = 1; i < unknownExtensions.size(); i++) {
                unknown = unknown + " " + unknownExtensions.get(i);
            }
            JOptionPane.showMessageDialog(contentPanel,
                "Discovered unknown packages: " + unknown,
                "Warning: Unknown Extensions", JOptionPane.WARNING_MESSAGE);
        }
        
        if (needToUpdate.size() == 0) {
            JOptionPane.showMessageDialog(contentPanel,
                "All packages are up-to-date.",
                "Extension Update", JOptionPane.INFORMATION_MESSAGE);
        } else {
            // mark out-of-date packages in table
            table.clearSelection(); // remove all current selections
            for (int r : needToUpdate) {
                table.addRowSelectionInterval(r, r);
            }
            // prompt to (re)install selections
            JOptionPane.showMessageDialog(contentPanel,
                "Out-of-date packages have been selected.\n" +
                "Use the Install button to update the selections.",
                "Extension Update", JOptionPane.INFORMATION_MESSAGE);
            updating = true;
        }            
    }


	/**
	 * Shows a message after installation of the packages.
	 */
	private void showPostInstallationMessage(List<String> installedExtensions, 
                                             List<String> failedExtensions)
	{
		// Announce the failed and installed packages
		String endMessage = "Completed installation!";
		int messageType = -1;
		if (installedExtensions.size() > 0) {
            endMessage += 
                "\n\nThe following package(s) were successfully installed:\n";
            for (String pkg : installedExtensions) 
                endMessage += "'" + pkg + "'  ";
            messageType = JOptionPane.INFORMATION_MESSAGE;
        }
		if (failedExtensions.size() > 0) {
            endMessage += 
                "\n\nInstallation of the following package(s) failed:\n";
            for (String pkg : failedExtensions) 
                endMessage += "'" + pkg + "'  ";
            if (messageType == -1)
                messageType = JOptionPane.ERROR_MESSAGE;
            else
                messageType = JOptionPane.WARNING_MESSAGE;
        }
        
		if (messageType == JOptionPane.ERROR_MESSAGE)
			JOptionPane.showMessageDialog(contentPanel,
                  "No packages were installed!", "Job Completed", messageType);
		else
			JOptionPane.showMessageDialog(contentPanel, endMessage, 
                                          "Job Completed", messageType);
	}
    
    
	/**
	 * Create the extensions directory
	 * Returns the directory path
	 * Returns null on any type of error.
	 */
	private String createExtDirectory()
	{
		if (!makeDirectory(mainFrame.extDir)) {
            JOptionPane.showMessageDialog(contentPanel, 
                    "Error creating directory at the following path:\n'" + 
                     mainFrame.extDir + "'\n\nNo packages installed!", 
                    "Directory Creation Problem", JOptionPane.ERROR_MESSAGE);
            return null;
        }
        
		// Return the path if everything was ok
		return mainFrame.extDir;
	}
    
    
	/**
	 * This function retrieves and returns the list of additional files 
     * mentioned in a Nyquist extension file. The function assumes the 
     * following conditions:
	 * 
	 * 1- The name of each additional file is in a separate line with 
     *    the following format:
	 *    ...  Additional File:  <additional-file-name><end>
     *    where <end> is either ";" or a newline
	 * 
	 * 2- The function only reads the extension information section of
     *    the input file and stops parsing on the first line that 
     *    contains the string "End Metadata"
	 * 
	 * Returns null on any type of error or if no additional files found.
	 */
	private static String[] extractOtherFilesFromSAL(String fileContent) {
		try {
            // Extract lines from the SAL/HTML file
            String[] lines = fileContent.split("\n");
            // NOTE: line.separator doesn't work because we have to
            // have Unix conventions here or checksums won't match
            // Parse the header of SAL/HTML file to find the additional files 
            List<String> fileNames = new ArrayList<String>();
            for (int i = 0; i < lines.length; ++i) {
                String line = lines[i].trim();
                        
                try {
                    int loc = line.indexOf("Additional File:");
                    if (loc != -1) {
                        loc += 16; // length of Additional File:
                        String file = line.substring(loc);
                        loc = file.indexOf(";");
                        if (loc != -1) {
                            file = file.substring(0, loc);
                        }
                        fileNames.add(file.trim());
                    } else if (line.indexOf("End Metadata") != -1) {
                        break;
                    }
                } catch (Exception e) {
                    /* ignore exceptions */ ;
                }
            }
                
            // Convert the file list to an array
            String[] result = new String[fileNames.size()];
            result = fileNames.toArray(result);
                
            return result;
        } catch(Exception e) {
            return null;
        }
	}
    
    
	/**
	 * This function obtains extension information from a string line 
     * read from the extension list file. It assumes the following format:
	 * info[1], info[2], ... , info[numOfCells]
	 * Last cell can contain ',' or any other character, while the first 
     * numOfCells-1 cells should not contain any commas.
	 * Returns null on any type of error.
  	 */
	private static String[] SplitExtensionData(String line, int numOfCells) {
		try {
            String[] result = new String[numOfCells];
            result = line.split(",", numOfCells);
            for (int i = 0; i < result.length; ++i)
                result[i] = result[i].trim();
            return result;
        } catch(Exception e) {
            return null;
        }
	}
    
    
	/**
	 * This function reads the extension list from the given URL and returns an 
	 * array of extension data (information for each extension in one cell.
	 * Assumes that the extension list has info for each extension on a 
     * separate line.
	 * Returns null on any type of error.
	 */
	private void loadExtensionData(String url) {
        extensions = null;
		try {
            extensions = readFromURL(url).split("\n");
            // note: using System.getProperty("line.separator"));
            // FAILED because text from URL had unix newlines
        } catch (Exception e) {
            System.out.println("loadExtensionData error " + e.toString());
            ;
        }
	}
    
    
    
	// ================ Project-independent functions ===============
    
	/**
	 * Reads a text file from the given URL and returns it as a String object.
	 * Returns null on any type of error.
	 */
	private String readFromURL(String urlString) {
        System.out.println("readFromURL: " + urlString);
		try {
            if (urlString.startsWith("file:")) {
                FileInputStream stream = new FileInputStream(urlString.substring(7));
                String s = getStringFromStream(stream);
                stream.close();
                return s;
            } else {
                URL url = new URL(urlString);
                HttpURLConnection http = (HttpURLConnection) url.openConnection();
                Map<String, List<String>> header = http.getHeaderFields();
                
                InputStream stream = http.getInputStream();
                String s = getStringFromStream(stream);
                stream.close();
                return s;
            }
        } catch (Exception e) {
            return null;
        }
	}
    
    
	/**
	 * Reads a text file from a stream and returns it as a String object.
	 * Returns null on any type of error.
	 */
	private static String getStringFromStream(InputStream stream) {
		if (stream != null) {
            Writer writer = new StringWriter();
                
            char[] buffer = new char[2048];
            try {
                Reader reader = new BufferedReader(
                        new InputStreamReader(stream, "UTF-8"));
                int counter;
                while ((counter = reader.read(buffer)) != -1) 
                    writer.write(buffer, 0, counter);
                        
                stream.close();
                return writer.toString();
            } catch (Exception e) {
                return null;
            }
        } 
		return null;
	}
    
    
    private void doLongTask(String task, String firstMsg, String ext, 
            String url, String filepath, String tmpdir, String extdir) {
        // place to report progress even before dialog is created:
        progressText = new JLabel(firstMsg);
        Downloader worker = new Downloader(task, ext, url, filepath, 
                                           tmpdir, extdir);

        System.out.println("Calling worker.execute.");
        worker.execute();
        System.out.println("Returning from worker.execute.");

        progress = new JDialog();
        progress.setModalityType(Dialog.ModalityType.APPLICATION_MODAL);
        JPanel progressPanel = new JPanel();
        progressPanel.add(progressText);
        progress.getContentPane().add(progressPanel);
        progress.setResizable(false);
        progress.pack();
        progress.setSize(500, 60);
        progress.setLocationRelativeTo(null);
        progress.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        progress.setAlwaysOnTop(true);
        progress.setVisible(true);
        progressText.setBackground(progressPanel.getBackground());

    }


	/**
	 * Reads a file from a stream and saves it in a local directory.
     * If the filename ends with ".b64", the extension is removed and
     * the data is decoded before writing.
	 * Additionally, returns the read and decoded stream as a String
     * object (only if saved successfully).
	 * Returns null on any type of error.
     *
     * To prepare a binary file as an extension file, do this on OS X
     * (example uses demo.mid as the binary file in the extension):
     *     openssl base64 -in demo.mid -out demo.mid.b64
	 */
 	private String saveFromURLtoFile(String url, String filename) {
        String fileContent =  readFromURL(url);
        if (fileContent == null) return null;
        byte[] bytes;

        // .b64 extensions are removed and content is decoded
        if (filename.endsWith(".b64")) {
            bytes = Base64.getMimeDecoder().decode(fileContent);
            filename = filename.substring(0, filename.length() - 4);
            if (saveToBinaryFile(filename, bytes)) {
                return "saveToBinaryFile success";
            } else {
                return null;
            }
        } else {
            if (saveToFile(filename, fileContent)) {
                return fileContent;
            } else {
                return null;
            }
        }
    }

    
    /**
     * Writes string to file
     */
    private boolean saveToFile(String filename, String content) {
        try {
            PrintWriter out = new PrintWriter(filename);
            out.print(content);
            out.close();
            return true;
        } catch (Exception e) { 
            return false; 
        }
    }


    /*
     * Writes bytes to binary file
     */
    private boolean saveToBinaryFile(String filename, byte[] bytes) {
		try {
            Files.write(Paths.get(filename), bytes);
            return true;
        } catch (Exception e) { 
            return false; 
        }
    }


 	/**
 	 * Generates the local path for a file from its URL and its local directory.
	 * Returns null on any type of error.
 	 */
 	private String fileLocalPathFromURL(String url, String dir) {
		try {
            String filename = url.substring(url.lastIndexOf('/') + 1);
            return dir + File.separator + filename;
        } catch(Exception e) {
            return null;
        }
 	}
    
    
	/**
	 * Extract the URL for directory from an URL, i.e. removes the file name 
     * from the input URL and returns the rest, including the '/' at the end. 
     * Only works if the directory is the part of the URL right before the 
     * last slash ('/').  E.g. this function removes 'extlist.txt' from the
     * URL below:
	 *     https://cs.cmu.edu/~music/nyquist/extlist.txt
	 */
	private String dirFromPackageURL(String url) {
		try {
            return url.substring(0, url.lastIndexOf('/') + 1);
        } catch (Exception e) {
            return null;
        }
	}
    
    
	/**
	 * Create a local directory at the given path.
	 * Returns false on any kind of error.
 	 */
	private boolean makeDirectory(String path)
	{
		File theDir = new File(path);
        
		// If the directory does not exist, create it
		if (!theDir.exists()) {
            try {
                theDir.mkdir();
                return true;
            } catch (Exception e) {
                return false;
            }        
        }
		return true;
	}
    
    
	/**
	 * Calculate the SHA-1 checksum of a given list of files.
	 * Returns null on any kind of error.
 	 */
	public String calculateFileChecksum(List<String> filenames) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA1");
                
            for (String filename : filenames) {
                if (filename.endsWith(".b64")) {
                    // by now, .b64 has been removed and file decoded
                    filename = filename.substring(0, filename.length() - 4);
                }
                FileInputStream fis = new FileInputStream(filename);
                        
                byte[] dataBytes = new byte[1024];
                int nread = 0;
                while ((nread = fis.read(dataBytes)) != -1) 
                    md.update(dataBytes, 0, nread);
                fis.close();
            }
                
            byte[] mdbytes = md.digest();
                
            // Convert the bytes to hex format
            StringBuffer sb = new StringBuffer("");
            for (int i = 0; i < mdbytes.length; i++) 
                sb.append(Integer.toString((mdbytes[i] & 0xff) + 0x100, 
                                           16).substring(1));
            return sb.toString();
        } catch (Exception e) {
            return null;
        }
	}
    
    
	/**
	 * This function removes a list of given files. 
	 */
	private void removeFiles(List<String> filenames) {
		for (String filename : filenames) {
            try {
                File file = new File(filename); 
                file.delete();
            } catch (Exception e) {
                continue;
            }
        }
	}

    public void actionPerformed(ActionEvent e)
    {
        String cmd = e.getActionCommand();
        System.out.println("ACTION NOT HANDLED: " + cmd);
    }
}
