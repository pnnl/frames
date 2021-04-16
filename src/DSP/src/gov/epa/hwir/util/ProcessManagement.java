package gov.epa.hwir.util;

import java.io.*;
import java.util.*;

import java.util.*;
import java.io.*;

class StreamGobbler extends Thread
{
  InputStream is;
  String type;

  StreamGobbler(InputStream is, String type)
  {
    this.is = is;
    this.type = type;
  }

  public void run()
  {
    try
    {
      InputStreamReader isr = new InputStreamReader(is);
      BufferedReader br = new BufferedReader(isr);
      String line=null;
      while ( (line = br.readLine()) != null)
        System.out.println(type + ">" + line);
    }
    catch (IOException ioe)
    {
      ioe.printStackTrace();
    }
  }
}

public class ProcessManagement// implements Runnable
{
  Vector listeners;
  BufferedReader br;
  BufferedReader be;
  BufferedWriter bw;
  Process p;
  boolean running;
  String cmd;
  /*
  public void run()
  {
    Runtime r=Runtime.getRuntime();
    try
    {
      System.out.println("Before Runtime.exec");
      System.out.flush();
      running=true;
      p = r.exec(cmd);
      p.waitFor();
      System.out.println("After WaitFor");
      System.out.flush();
      running=false;
    }
    catch(IOException e)
    {
      System.out.println(e);
      System.out.flush();
    }
    catch(InterruptedException ie)
    {
      System.out.println(ie);
      System.out.flush();
    }
    System.out.println("Returning from run");
    System.out.flush();
  }
  */
  public ProcessManagement()
  {
    listeners=new Vector();
    br=null;
    be=null;
    bw=null;
    running=false;
    p=null;
  }
  public void addProcessListener(ProcessListener ps)
  {
    listeners.add(ps);
  }
  public void removeProcessListener(ProcessListener ps)
  {
    listeners.remove(ps);
  }
  public void informListeners(int nextChar)
  {
    System.out.print('.');
    System.out.flush();
    Enumeration e=listeners.elements();
    String line=String.valueOf(nextChar);
    while (e.hasMoreElements())
    {
      ProcessListener ps=(ProcessListener) e.nextElement();
      ps.outputString(line);
    }
  }
  public boolean exec(String command)
  {
    int exitVal=-1;
    try
    {
      String osName = System.getProperty("os.name" );
      String[] cmd = new String[1];
      /*
      String[] cmd = new String[3];

      if( osName.equalsIgnoreCase("Windows NT" ) ||
          osName.equalsIgnoreCase("Windows 2000") ||
          osName.equalsIgnoreCase("Windows XP") )
      {
        cmd[0] = "c:\\winnt\\system32\\cmd.exe" ;
        cmd[1] = "/C" ;
        cmd[2] = command;
      }
      else if( osName.equalsIgnoreCase("Windows 95" ) ||
               osName.equalsIgnoreCase("Windows ME" ) ||
               osName.equalsIgnoreCase("Windows SE" ) ||
               osName.equalsIgnoreCase("Windows 98" ) )
      {
        cmd[0] = "c:\\command.com" ;
        cmd[1] = "/C" ;
        cmd[2] = command;
      }
      else
      {
        System.out.println("Unrecongnized OS: "+osName);
        cmd[0] = "c:\\command.com" ;
        cmd[1] = "/C" ;
        cmd[2] = command;
      }
      */
      cmd[0]=command;
      Runtime rt = Runtime.getRuntime();
//      System.out.println("Execing " + cmd[] + " " + cmd[1]
//      + " " + cmd[2]);
      System.out.println("Execing " + command );
      File f=new File("c:\\ParError.txt");
      System.out.println("Creating file: "+f.toString());
      PrintStream ps=new PrintStream(new FileOutputStream(f));
      ps.println("Error");
      ps.close();
//      Process proc = rt.exec(cmd);
      f=new File(command);
      Process proc = rt.exec(cmd,null,f.getAbsoluteFile().getParentFile());
      // any error message?
      StreamGobbler errorGobbler = new
      StreamGobbler(proc.getErrorStream(), "ERROR");

      // any output?
      StreamGobbler outputGobbler = new
      StreamGobbler(proc.getInputStream(), "OUTPUT");

      // kick them off
      errorGobbler.start();
      outputGobbler.start();

      // any error???
      exitVal = proc.waitFor();
      proc.destroy();
      proc=null;
//      outputGobbler.destroy();
      outputGobbler=null;
//      errorGobbler.destroy();
      errorGobbler=null;
      rt.gc();
      if (f.exists()) exitVal=1;
      else exitVal=0;
      System.out.println("ExitValue: " + exitVal);
    }
    catch (Throwable t)
    {
      t.printStackTrace();
    }
    System.out.println("Done");
    if (exitVal==0) return true;
    return false;
  }
}
