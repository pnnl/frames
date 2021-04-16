package FramesStatus;
/**
 * @author d3k076
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
/*
 * DBUtils.java
 *
 * Created on March 25, 2002, 9:16 AM
 */

import java.sql.*;

/**
 *
 * @author  d3k076
 */
public class DBUtils {

    /** Creates a new instance of DBUtils */
    public DBUtils() {
    }

    // add a public method to return a connection to the local database
    public Connection getLocalConnection() {
        // this method returns a connection to the local database

        // initialize a connection object (to be returned)
        Connection con = null;
        try {
            // create connection to JDBC datasource
            String driverName = "com.microsoft.jdbc.sqlserver.SQLServerDriver";
            String urlString = "jdbc:microsoft:sqlserver://localhost:1433;DatabaseName=FramesStatus";
            String dbUName = "framesstatus";
            String dbPassword = "frames2003";

            // make a connection to the target database to extract the
            //  database type and version
            Class.forName(driverName);
            // create a connection object
            con = DriverManager.getConnection(urlString,dbUName,dbPassword);
        } catch (Exception e) {
            e.printStackTrace();
        }

        // return the connection object
        return con;
    }



}
