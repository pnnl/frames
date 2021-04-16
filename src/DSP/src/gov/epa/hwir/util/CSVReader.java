package gov.epa.hwir.util;

import java.io.*;

public class CSVReader {

  private boolean eol;
  private boolean eof;
  private FileInputStream fis;
  private char delimiter,separator;
  private char pushchar;
  private boolean pushed;

  private void pushback(char c)
  {
    pushchar=c;
    pushed=true;
  }

  private char next()
  {
    byte b[]=new byte[1];
    int n;
    if (fis==null)
    {
      eof=true;
      return 0;
    }
    if (eof) return 0;
    if (pushed)
    {
      pushed=false;
      return pushchar;
    }
    else
    {
      try
      {
        n=fis.read(b);
        if (n==b.length) return (char)b[0];
        else
        {
          eof=true;
          return 0; // eof
        }
      }
      catch (IOException e)
      {
        eof=true;
        return 0;
      }
    }
  }
  public void setDelimiter(char newDelimiter)
  {
    delimiter=newDelimiter;
  }
  public void setSeparator(char newSeparator)
  {
    separator=newSeparator;
  }
  public CSVReader(File f)
  {
    eol=false;
    eof=false;
    setDelimiter('"');
    setSeparator(',');
    try
    {
      fis=new FileInputStream(f);
    }
    catch (FileNotFoundException fnfe)
    {
      fis=null;
    }
  }
  public CSVReader(String filepath) {
    eol=false;
    eof=false;
    setDelimiter('"');
    setSeparator(',');
    try
    {
      fis=new FileInputStream(filepath);
    }
    catch (FileNotFoundException fnfe)
    {
      fis=null;
    }
  }
  public void close()
  {
    try
    {
      if (fis!=null) fis.close();
      fis=null;
    }
    catch (IOException e)
    {
    }
  }
  public boolean endOfFile()
  {
    return eof;
  }
  public void readLine()
  {
    char c;
    if (!eol)  // go until either a 10 or 13 is found
    {
      c=next();
      while (c!=(char)10 && c!=(char)13 && !endOfFile())
        c=next();
    }
    c=next(); // get one more character
    if (c!=(char)10 && c!=(char)13) pushback(c);
    eol=false;
  }
  public float readFloat()
  {
    String s=readString();
    try
    {
      return Float.valueOf(s).floatValue();
    }
    catch (NumberFormatException nfe)
    {
      return 0.0f;
    }
  }
  public int readInt()
  {
    String s=readString();
    try
    {
      return Integer.valueOf(s).intValue();
    }
    catch (NumberFormatException nfe)
    {
      return 0;
    }
  }
  public String readString()
  {
    StringBuffer sb=new StringBuffer();
    char c;
    if (eol) return "";
    c=next();
    while (c!=separator && c!=(char)10 && c!=(char)13 && !endOfFile())
    {
      if (c==delimiter)
      {
        c=next();
        while (c!=delimiter && !endOfFile())
        {
          sb.append(c);
          c=next();
        }
        c=next();
      }
      else
      {
        sb.append((char)c);
        c=next();
      }
    }
    if (c==separator && separator==' ')
    {
      while (c==' ' && !endOfFile())
        c=next();
      pushback(c);
      c=' ';
    }
    if (c==(char)10 || c==(char)13) eol=true;
    return sb.toString();
  }
}
