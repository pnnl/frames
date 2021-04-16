package gov.epa.hwir.dsp;

import java.sql.DriverManager;
import java.sql.Connection;


public class DBAccess {

  public DBAccess() {
 }


 public static Connection getConnection ( String databaseName ){

   try{

     try {
       Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");
     }
     catch (Exception e) {
       System.out.println("Failed to load JDBC/ODBC driver.");
       return null;
     }

     String database =
         "jdbc:odbc:Driver={Microsoft Access Driver (*.mdb)};DBQ=";

     database += databaseName.trim();

     // now we can get the connection from the DriverManager
     Connection con = DriverManager.getConnection(database, "", "");


     return con;

   }
   catch ( Exception e ){

     e.printStackTrace();
     return null;

   }


 }
}
