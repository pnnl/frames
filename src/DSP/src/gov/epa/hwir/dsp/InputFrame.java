package gov.epa.hwir.dsp;

import java.sql.*;
import java.util.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class InputFrame
    extends JFrame {

  public static final boolean DEBUG = false;

  private JLabel InputDatabaseLabel, seedLabel;
  private JButton cancelButton;
  private JFileChooser fileChooser;
  private JButton okButton, selectFileButton;
  private String mOutputRegionalDb;
  private String mOutputSiteDb;
  private String mOutputNationDb;
  private String mInputDatabase;
  private Component container;
  private JTextField seedTextField;

  public InputFrame(String site, String regional, String national) {

    super("DSP:Select Input Database");
    initComponents();
    mOutputSiteDb = site;
    mOutputRegionalDb = regional;
    mOutputNationDb = national;
    this.setSize(300, 160);
    container = this.getContentPane();

  }

  private void initComponents() {

    GridBagConstraints gridBagConstraints;

    InputDatabaseLabel = new JLabel();
    fileChooser = new JFileChooser(".");

    seedLabel = new JLabel ("Enter seed value");
    seedTextField = new JTextField ( "1000" );

    okButton = new JButton();
    cancelButton = new JButton();
    selectFileButton = new JButton();

    getContentPane().setLayout(new GridBagLayout());

    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent evt) {
        exitForm(evt);
      }
    });

    gridBagConstraints = new GridBagConstraints();
    gridBagConstraints.insets = new Insets(2, 2, 2, 2);
    gridBagConstraints.anchor = GridBagConstraints.NORTHWEST;
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 0;
    getContentPane().add(seedLabel, gridBagConstraints);

    gridBagConstraints = new GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 0;
    gridBagConstraints.insets = new Insets(2, 2, 2, 2);
    getContentPane().add(seedTextField, gridBagConstraints);

    InputDatabaseLabel.setText("Please select input DataBase");
    gridBagConstraints = new GridBagConstraints();
    gridBagConstraints.insets = new Insets(2, 2, 2, 2);
    gridBagConstraints.anchor = GridBagConstraints.WEST;
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 1;
    getContentPane().add(InputDatabaseLabel, gridBagConstraints);

    selectFileButton.setText("...");
    gridBagConstraints = new GridBagConstraints();
    gridBagConstraints.insets = new Insets(2, 2, 2, 2);
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 1;
    getContentPane().add(selectFileButton, gridBagConstraints);

    selectFileButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        int returnVal = fileChooser.showOpenDialog(container);

        if (returnVal == JFileChooser.APPROVE_OPTION) {

          System.out.println("You chose to open this file: " +
                             fileChooser.getSelectedFile().getName());
          mInputDatabase = fileChooser.getSelectedFile().getAbsolutePath();

        }
        else if (returnVal == JFileChooser.CANCEL_OPTION) {

        }

      }
    });

    okButton.setText("OK");
    gridBagConstraints = new GridBagConstraints();
    gridBagConstraints.gridx = 0;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.insets = new Insets(2, 2, 2, 2);
    getContentPane().add(okButton, gridBagConstraints);

    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        processDatabaseValues();
      }
    });

    cancelButton.setText("Cancel");
    gridBagConstraints = new GridBagConstraints();
    gridBagConstraints.gridx = 1;
    gridBagConstraints.gridy = 2;
    gridBagConstraints.insets = new Insets(2, 2, 2, 2);
    getContentPane().add(cancelButton, gridBagConstraints);

    pack();
  }

  /** Exit the Application */
  private void exitForm(java.awt.event.WindowEvent evt) {
    System.exit(0);
  }

  private String[] getCategoriesFromInputDatabase(Connection con) {

    String[] catg;
    try {

      Statement categories = con.createStatement(
          ResultSet.TYPE_SCROLL_INSENSITIVE,
          ResultSet.CONCUR_READ_ONLY);
      String catQuery =
          "Select SettingID from (select distinct SettingID from DistributionData)";
      ResultSet categorySet = categories.executeQuery(catQuery);
      categorySet.last();
      int count1 = categorySet.getRow();
      categorySet.beforeFirst();
      catg = new String[count1];

      for (int i = 0; i < count1; i++) {
        categorySet.absolute(i + 1);
        catg[i] = categorySet.getString("SettingID");
        System.out.println(catg[i]);
      }

      return catg;
    }
    catch (SQLException ex) {
      ex.printStackTrace();
      return null;
    }

  }

  /** Do all the database processing **/
  private void processDatabaseValues() {

    // should build the categories from the input database
    String[] categories = new String[] {
        "Regional", "National"};

    if (DEBUG) {
      System.out.println("Input database is " + mInputDatabase);
      System.out.println("Looping for categories... ");
      for (int i = 0; i < categories.length; i++) {
        System.out.println(categories[i]);
      }
    }
    // first get a connection to the input database
    //Connection inputConnection = DBAccess.getConnection( mInputDatabase );
    Connection inputConnection = DBAccess.getConnection(
        "C:\\DSP\\InputDatabase\\NationDB.mdb");

    //Get the list of categories from the input database
    // use those categories to populate output databases
    //categories = getCategoriesFromInputDatabase(inputConnection);

    try {

      // iterate through the categories array
      for (int i = 0; i < categories.length; i++) {
        // select a statement to select variables and datagroupnames for
        // particular category

        try {

          Statement inputStmt = inputConnection.createStatement(
              ResultSet.TYPE_SCROLL_INSENSITIVE,
              ResultSet.CONCUR_READ_ONLY);

          //construct query to select distinct combinations of variableName and DataGroupName
          String inputQuery =
              "select * from (select distinct VariableName, DataGroupName from "
              + "DistributionData WHERE SettingID='"
              + categories[i] + "') ";

          //get all datagroupnames and variablenames for this category
          ResultSet inputSet = inputStmt.executeQuery(inputQuery);

          // for the input result set
          while ( inputSet.next() ) {

            try {
              //get the variablename and the datagroupname
              System.out.println("****************") ;
              System.out.println("Starting new set") ;
              System.out.println("****************") ;
              String variableName = inputSet.getString("VariableName");
              String dataGroupName = inputSet.getString("DataGroupName");

              System.out.println("Fetching all records with Variable Name "
                                 + variableName +
                                 ", DataGroupName: " + dataGroupName
                                 + " and SettingId: " + categories[i]);

              // fetch all records with variablename and datagroupname for category
              // from input database
              Statement stmt2 = inputConnection.createStatement(
                  ResultSet.TYPE_SCROLL_INSENSITIVE,
                  ResultSet.CONCUR_READ_ONLY);

              //Reuse the inputQuery variable since it's only a placeholder
              inputQuery =
                  "Select * from DistributionData where VariableName='"
                  + variableName + "' And DataGroupName='" + dataGroupName +
                  "' And SettingID = '"
                  + categories[i] + "'";

              ResultSet inputResult = stmt2.executeQuery(inputQuery);
              int inputCount = printCountForResultSet ( inputResult );

              Vector inputVector = buildVectorFromResultSet ( inputResult );

              String outputDatabase = getOutputDbForCategory(categories[i]);

              //Open an output connection for writing to output database
              Connection outputConnection = DBAccess.getConnection(
                  outputDatabase);

              Statement outputStmt = outputConnection.createStatement(
                  ResultSet.TYPE_SCROLL_INSENSITIVE,
                  ResultSet.CONCUR_READ_ONLY);

              //create table name based on category of data
              String tableName = categories[i] + "_Variable_Distribution_Data";


             //get all the records with similar variablename and datagroupname
              String outputQuery = "SELECT * FROM " + tableName +
                  " WHERE Variable_Name = '" + variableName +
                  "' And Data_Group_Name = '" + dataGroupName + "'";

              ResultSet outputSet = outputStmt.executeQuery(outputQuery);

              long seed = new Long ( seedTextField.getText( )).longValue();

              Random rand = new Random ( seed );

              while ( outputSet.next() ){

                int rowId = outputSet.getInt( "RowID");

                //index the Vector ...
                int index = rand.nextInt( inputVector.size() );
                System.out.println("Selecting records number " + index ) ;

                DatabaseRow indexRow = (DatabaseRow) inputVector.elementAt( index );

                //from this position get all values
                double max = indexRow.getMax();
                double min = indexRow.getMin();
                double var = indexRow.getVar();

                System.out.println("Max " + max + "  Min " + min + " var " +
                                   var);

                String outputUpdate = "Update " + tableName + " SET Maximum ='" +
                  max +
                  "', Minimum= '" + min + "', Variance = '" + var +
                  "' WHERE Variable_Name = '" + variableName +
                  "' And Data_Group_Name = '" + dataGroupName +
                  "' And RowID = " + rowId;

                PreparedStatement pstmt = outputConnection.prepareStatement(
                    outputUpdate );

                int outputRows = pstmt.executeUpdate();

                // report the number of rows affected in output database
                System.out.println("Number of rows affected " + outputRows);

              }


            }
            catch (SQLException ex) {
              ex.printStackTrace();
            }
          }

        }
        catch (SQLException ex) {
          ex.printStackTrace();
        }

      }
    }
    catch (NullPointerException e) {

      e.printStackTrace();

    }

  }

  /**
   * buildVectorFromResultSet
   *
   * @param inputResult ResultSet
   * @return Vector
   */
  private Vector buildVectorFromResultSet(ResultSet inputResult) {

    try {
      Vector inputVect = new Vector ();
      while (inputResult.next()) {

        String variableName = inputResult.getString( "VariableName" );
        String dataGroupName = inputResult.getString( "DataGroupName" );
        double max = inputResult.getDouble( "Maximum" );
        double min = inputResult.getDouble( "Minimum" );
        double var = inputResult.getDouble( "Variance" );

        DatabaseRow inputRow = new DatabaseRow(variableName, dataGroupName, max, min, var);
        inputVect.add( inputRow );
      }

      inputVect.trimToSize();
      return inputVect;
    }
    catch (SQLException ex) {
      return null;
    }

  }

  private int printCountForResultSet ( ResultSet rs ){

    // get the count of this result set for each category
    try {
      rs.last();
      int count1 = rs.getRow();
      System.out.println("We have " + count1 + " rows in inputdatabase");
      rs.beforeFirst();
      return count1;
    }
    catch (SQLException ex) {
      ex.printStackTrace();
      return 0;
    }


  }

  /**
   * getOutputDbForCategory
   *
   * @param string String
   * @return String
   */
  private String getOutputDbForCategory(String category) {

    if (category.equals("Site")) {
      return mOutputSiteDb;
    }
    else if (category.equals("Regional")) {
      System.out.println("Output Region database is " + mOutputRegionalDb);
      return mOutputRegionalDb;
    }
    else if (category.equals("National")) {
      System.out.println("Output National database is " + mOutputNationDb);
      return mOutputNationDb;
    }
    else
      return "";
  }

}
