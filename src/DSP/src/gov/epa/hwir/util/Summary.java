package gov.epa.hwir.util;

import java.io.*;
import java.util.*;
//import CSVReader;
import java.sql.*;
import java.net.*;

public class Summary implements Summarizable, JDBCSummarizable, Runnable
{
  Vector Summaries;
  ArrayHWIRIO hio;
  String RSOFDirectory,filename,scriptFile;
  int retries,sleep;
  Thread mySQL;
  public void addValues(StringBuffer sb)
  {
    Enumeration e=Summaries.elements();
    boolean first=true;
    while (e.hasMoreElements())
    {
      if (!first) sb.append(", ");
      first=false;
      JDBCSummarizable js=(JDBCSummarizable)e.nextElement();
      js.addValues(sb);
    }
  }
  public void addColumns(StringBuffer sb)
  {
    Enumeration e=Summaries.elements();
    boolean first=true;
    while (e.hasMoreElements())
    {
      if (!first) sb.append(", ");
      first=false;
      JDBCSummarizable js=(JDBCSummarizable)e.nextElement();
      js.addColumns(sb);
    }
  }
  public void addDescription(Statement s,String prefix)
  {
    Enumeration e=Summaries.elements();
    while (e.hasMoreElements())
    {
      JDBCSummarizable js=(JDBCSummarizable)e.nextElement();
      js.addDescription(s,prefix);
    }
  }
  public void createColumns(StringBuffer sb)
  {
    Enumeration e=Summaries.elements();
    boolean first=true;
    while (e.hasMoreElements())
    {
      if (!first) sb.append(", ");
      first=false;
      JDBCSummarizable js=(JDBCSummarizable)e.nextElement();
      js.createColumns(sb);
    }
  }
  public Summary()
  {
    Summaries=new Vector();
  }
  public void add(Summarizable s)
  {
    Summaries.add(s);
  }
  public void add(Summary s)
  {
    Enumeration e=s.Summaries.elements();
    while (e.hasMoreElements())
      Summaries.add(e.nextElement());
  }
  public void writeHeader1(PrintStream ps)
  {
    Summarizable s;
    Enumeration e=Summaries.elements();
    while (e.hasMoreElements())
    {
      s=(Summarizable)e.nextElement();
      s.writeHeader1(ps);
    }
    ps.println();
  }
  public void writeHeader2(PrintStream ps)
  {
    Summarizable s;
    Enumeration e=Summaries.elements();
    while (e.hasMoreElements())
    {
      s=(Summarizable)e.nextElement();
      s.writeHeader2(ps);
    }
    ps.println();
  }
  public void writeValue(PrintStream ps)
  {
    Summarizable s;
    Enumeration e=Summaries.elements();
    while (e.hasMoreElements())
    {
      s=(Summarizable)e.nextElement();
      s.writeValue(ps);
    }
    ps.println();
  }
  public void setArgs(String ssf,String grf,String header,String script)
  {
    hio=new ArrayHWIRIO(ssf,grf,header);
    RSOFDirectory=hio.readString(header,"RSOFDirectory","");
    scriptFile=script;
  }
  public void readScript(String filename)
  {
     CSVReader csv=new CSVReader(filename);
     String fun;
     int nx=0,ny=0,nz=0,nv=0,nw=0;
     do
     {
       fun=csv.readString();
       if (fun.equalsIgnoreCase("variable"))
       {
         Extraction e=new Extraction(hio,csv);
         e.setNumX(nx);
         e.setNumY(ny);
         e.setNumZ(nz);
         e.setNumV(nv);
         e.setNumW(nw);
         e.extractIntoSummary(this);
       }
       else if (fun.equalsIgnoreCase("nx"))
       {
         String temp=csv.readString();
         if (temp.equalsIgnoreCase("extract"))
         {
           Extraction e=new Extraction(hio,csv);
           e.setNumX(nx);
           e.setNumY(ny);
           e.setNumZ(nz);
           e.setNumV(nv);
           e.setNumW(nw);
           nx=e.getCount();
         }
         else
         {
           nx=Integer.valueOf(temp).intValue();
           csv.readLine();
         }
       }
       else if (fun.equalsIgnoreCase("ny"))
       {
         String temp=csv.readString();
         if (temp.equalsIgnoreCase("extract"))
         {
           Extraction e=new Extraction(hio,csv);
           e.setNumX(nx);
           e.setNumY(ny);
           e.setNumZ(nz);
           e.setNumV(nv);
           e.setNumW(nw);
           ny=e.getCount();
         }
         else
         {
           ny=Integer.valueOf(temp).intValue();
           csv.readLine();
         }
       }
       else if (fun.equalsIgnoreCase("nz"))
       {
         String temp=csv.readString();
         if (temp.equalsIgnoreCase("extract"))
         {
           Extraction e=new Extraction(hio,csv);
           e.setNumX(nx);
           e.setNumY(ny);
           e.setNumZ(nz);
           e.setNumV(nv);
           e.setNumW(nw);
           nz=e.getCount();
         }
         else
         {
           nz=Integer.valueOf(temp).intValue();
           csv.readLine();
         }
       }
       else if (fun.equalsIgnoreCase("nv"))
       {
         String temp=csv.readString();
         if (temp.equalsIgnoreCase("extract"))
         {
           Extraction e=new Extraction(hio,csv);
           e.setNumX(nx);
           e.setNumY(ny);
           e.setNumZ(nz);
           e.setNumV(nv);
           e.setNumW(nw);
           nv=e.getCount();
         }
         else
         {
           nv=Integer.valueOf(temp).intValue();
           csv.readLine();
         }
       }
       else if (fun.equalsIgnoreCase("nw"))
       {
         String temp=csv.readString();
         if (temp.equalsIgnoreCase("extract"))
         {
           Extraction e=new Extraction(hio,csv);
           e.setNumX(nx);
           e.setNumY(ny);
           e.setNumZ(nz);
           e.setNumV(nv);
           e.setNumW(nw);
           nw=e.getCount();
         }
         else
         {
           nw=Integer.valueOf(temp).intValue();
           csv.readLine();
         }
       }
       else if (fun.equalsIgnoreCase("parameterize"))
       {
         String temp=csv.readString();
         Extraction e=new Extraction(hio,csv);
         e.setNumX(nx);
         e.setNumY(ny);
         e.setNumZ(nz);
         e.setNumV(nv);
         e.setNumW(nw);
         Summarizable s=(Summarizable)e.parameterize(temp);
         add(s);
       }
       else if (fun.equalsIgnoreCase("percentiles"))
       {
         int n=csv.readInt();
         double percents[]=new double[n];
         for (int i=0;i<n;i++)
           percents[i]=csv.readFloat();
         Extraction e=new Extraction(hio,csv);
         e.setNumX(nx);
         e.setNumY(ny);
         e.setNumZ(nz);
         e.setNumV(nv);
         e.setNumW(nw);
         Summarizable s=(Summarizable)e.percentiles(percents);
         add(s);
       }
     }
     while (!fun.equalsIgnoreCase("end") && fun.length()>0);
     hio.closeGroups();
  }
  private void doASCII(String filename)
  {
    boolean exists;
    File f=new File(filename);
    exists=f.exists();
    try
    {
      PrintStream ps=new PrintStream(new FileOutputStream(filename,exists));
      if (!exists)
      {
        this.writeHeader1(ps);
        this.writeHeader2(ps);
      }
      this.writeValue(ps);
    }
    catch (IOException e)
    {
    }
  }
  public boolean tableExists(DatabaseMetaData dbmd,String catalog,String name)
  {
    try
    {
      String types[]={"TABLE"};
      ResultSet r=dbmd.getTables(catalog,(String)null,name,types);
      return r.next();
    }
    catch(SQLException e) {
      System.out.println(e.toString());
    }
    return false;
  }
  public void registerJDBCODBC()
  {
    try
    {
      Class.forName("sun.jdbc.odbc.JdbcOdbcDriver").newInstance();
    }
    catch (ClassNotFoundException e)
    {
      System.out.println("JDBC-ODBC bridge driver missing");
    }
    catch (IllegalAccessException e2)
    {
      System.out.println("Illegal access to JDBC-ODBC bridge");
    }
    catch (InstantiationException e3)
    {
      System.out.println("Could not instantiate JDBC-ODBC driver");
    }
  }
  private void doDBWork(Connection Conn,String catalog) throws SQLException
  {
      Statement Stmt=Conn.createStatement();
      DatabaseMetaData dbmd=Conn.getMetaData();
      File f=new File(scriptFile);
      scriptFile=f.getName();
      if (scriptFile.indexOf(".")>0) scriptFile=scriptFile.substring(0,scriptFile.indexOf("."));
      if (!tableExists(dbmd,catalog,scriptFile+"_description"))
        Value.createTable(Conn.createStatement(),scriptFile);
      if (!tableExists(dbmd,catalog,scriptFile+"_results"))
      {
        StringBuffer columns=new StringBuffer("create table ");
        columns.append(scriptFile);
        columns.append("_results (");
        createColumns(columns);
        columns.append(", MachineName char(40));");
        System.out.println(columns.toString());
        Stmt.execute(columns.toString());
        addDescription(Stmt,scriptFile);
      }
      StringBuffer insert=new StringBuffer("insert into ");
      insert.append(scriptFile);
      insert.append("_results (");
      addColumns(insert);
      insert.append(", MachineName) values (");
      addValues(insert);
      insert.append(", '");
      try
      {
        insert.append(InetAddress.getLocalHost().getHostName());
      }
      catch (UnknownHostException e)
      {
        insert.append("NameError");
      }
      insert.append("');");
      System.out.println(insert.toString());
      Stmt.execute(insert.toString());
  }
  public void doAccess(String filename,String header)
  {
    Connection Conn;
    File f=new File(filename);
    registerJDBCODBC();
    StringBuffer DBURL=new StringBuffer("jdbc:odbc:MS Access Database");
    DBURL.append(";DBQ=");
    DBURL.append(f.getName());
    DBURL.append(";DefaultDir=");
    DBURL.append(RSOFDirectory);
    System.out.println("Connecting to "+DBURL.toString());
    try
    {
      Conn=DriverManager.getConnection(DBURL.toString(),"admin","");
      doDBWork(Conn,(String)null);
      Conn.commit();
      Conn.close();
    }
    catch(SQLException e) {
      System.out.println(e.toString());
    }
  }
  private void registerMySQL()
  {
    // REGISTER DRIVER
    try
    {
        Driver d = (Driver)Class.forName("org.gjt.mm.mysql.Driver").newInstance();
    }
    catch (ClassNotFoundException e)
    {
      System.out.println("MySQL driver missing");
    }
    catch (IllegalAccessException e2)
    {
      System.out.println("Illegal access to MySQL driver class");
    }
    catch (InstantiationException e3)
    {
      System.out.println("Could not instantiate MySQL driver class");
    }
  }
  private boolean mysqlHasDatabase(String table,Connection c) throws SQLException
  {
    Statement s=c.createStatement();
    ResultSet r=s.executeQuery("show databases;");
    int idx=r.findColumn("Database");
    while(r.next())
    {
      if (r.getString(idx).equalsIgnoreCase(table)) return true;
    }
    return false;
  }
  public void run()
  {
    boolean done=false;
    int count=retries;
    Connection Conn;
    File f=new File(filename);
    registerMySQL();

    int idx=filename.indexOf("\\",3);
    String machine=filename.substring(2,idx);
    String db=filename.substring(idx+1,filename.length()-6);
    StringBuffer DBURL=new StringBuffer("jdbc:mysql://");
    DBURL.append(machine);
    DBURL.append("/");
    System.out.println("Connecting to "+DBURL.toString());
    while (!done)
    {
      try
      {
        Conn=DriverManager.getConnection(DBURL.toString(),"root","");
        Statement s=Conn.createStatement();
        if (!mysqlHasDatabase(db,Conn)) s.execute("create database "+db+";");
        s.execute("use "+db+";");
        doDBWork(Conn,db);
        s.close();
   //     Conn.commit();
        Conn.close();
        done=true;
      }
      catch(SQLException e) {
        System.out.println(e.toString());
        if (count==0) done=true;
        count--;
        try
        {
          mySQL.sleep(sleep);
        }
        catch(InterruptedException e2)
        {

        }
      }
    }
  }
  public void doMySQL(String mysqlfilename,int newRetries,int newSleep)
  {
    filename=mysqlfilename;
    sleep=newSleep;
    retries=newRetries;
    mySQL=new Thread(this);
    mySQL.start();
  }
  public static void main(String args[])
  {
    Summary s=new Summary();
    if (args.length>=5)
    {
      s.setArgs(args[0],args[1],args[2],args[3]);
      s.readScript(args[3]);
      if (args[4].endsWith(".csv")) s.doASCII(args[4]);
      else if (args[4].endsWith(".mdb")) s.doAccess(args[4],args[2]);
      else if (args[4].endsWith(".mysql")) s.doMySQL(args[4],Integer.parseInt(args[5]),Integer.parseInt(args[6]));
    }
    else
    {
      System.out.println("Usage Summary <ssfdir> <grfdir> <header> <script> <table> <retry> <delay>");
      System.out.println("<ssfdir> ssf directory Ex: c:\\hwir\\ssf");
      System.out.println("<grfdir> grf directory Ex: c:\\hwir\\grf");
      System.out.println("<header> header filenam Ex: hdprod.ssf");
      System.out.println("<script> Script filename Ex: TimeTrialsScript.csv");
      System.out.println("<table> Output table filename Ex: TimeTrialsData.csv");
      System.out.println("        or \\\\0100data\\TimeTrials.mdb or //0100data/Study1.mysql");
      System.out.println("for MySQL only");
      System.out.println("<retry> Number of times to retry connection to server");
      System.out.println("<delay> Time delay in milliseconds between retries");
    }
  }
}
