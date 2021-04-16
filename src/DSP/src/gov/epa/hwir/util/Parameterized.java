package gov.epa.hwir.util;

import java.io.*;
import java.util.*;
import java.sql.*;

public class Parameterized extends Distribution {
  String type;
  double mean,sd,min,max;
  int count;
  public void addValues(StringBuffer sb)
  {
    calculate();
    sb.append(count);
    sb.append(", ");
    sb.append(mean);
    sb.append(", ");
    sb.append(sd);
    sb.append(", ");
    sb.append(min);
    sb.append(", ");
    sb.append(max);
  }
  private String newShortName(int column)
  {
    StringBuffer sb=new StringBuffer();
    sb.append(ShortName);
    sb.append('_');
    if (column==0) sb.append("n");
    if (column==1) sb.append("mean");
    if (column==2) sb.append("sd");
    if (column==3) sb.append("min");
    if (column==4) sb.append("max");
    return sb.toString();
  }
  public void addColumns(StringBuffer sb)
  {
    sb.append(newShortName(0));
    sb.append(", ");
    sb.append(newShortName(1));
    sb.append(", ");
    sb.append(newShortName(2));
    sb.append(", ");
    sb.append(newShortName(3));
    sb.append(", ");
    sb.append(newShortName(4));
  }
  public void createColumns(StringBuffer sb)
  {
    for (int i=0;i<5;i++)
    {
      if (i>0) sb.append(", ");
      sb.append(newShortName(i));
      sb.append(" double");
    }
  }
  private void addPercentDescription(Statement s,int column)
  {
    StringBuffer sb=new StringBuffer("insert into description ");
    sb.append("(alias ,summary, datagroup, variable, units, idx1, idx2, idx3, idx4, idx5, idx6) values (");
    sb.append("'");
    sb.append(newShortName(column));
    sb.append("', '");
    sb.append(type);
    sb.append("', '");
    sb.append(group);
    sb.append("', '");
    sb.append(name);
    sb.append("', '");
    sb.append(units);
    sb.append("'");
    for (int i=0;i<6;i++)
    {
      sb.append(", ");
      if (idx==null)
        sb.append(0);
      else
        if (i<idx.length)
          sb.append(idx[i]);
        else
          sb.append(0);
    }
    sb.append(");");
    System.out.println(sb.toString());
    try
    {
      s.execute(sb.toString());
    }
    catch(SQLException e)
    {
      System.out.println(e);
    }
  }
  public void addDescription(Statement s)
  {
    for (int i=0;i<5;i++)
      addPercentDescription(s, i);
  }
  public Parameterized(String group,String name,String units,int idx[],String newType)
  {
    super(group,name,units,idx);
    type=newType;
    count=0;
    mean=0;
    sd=0;
    min=0;
    max=0;
  }
  private void calculate()
  {
    boolean lognormal=false;
    boolean first=true;
    if (type.equalsIgnoreCase("lognormal"))
      lognormal=true;
    Enumeration e=values.elements();
    while (e.hasMoreElements())
    {
      double d=((Double)e.nextElement()).doubleValue();
      if (first)
      {
        min=d;
        max=d;
        first=false;
      }
      else
      {
        if (d<min) min=d;
        if (d>max) max=d;
      }
      if (lognormal)
      {
        if (d>0.0)
        {
          mean+=Math.log(d)/Math.log(10.0);
          count++;
        }
      }
      else
      {
        mean+=d;
        count++;
      }
    }
    mean/=(double)count;
    e=values.elements();
    while (e.hasMoreElements())
    {
      double d=((Double)e.nextElement()).doubleValue();
      double er;
      if (lognormal)
      {
        if (d>0.0)
        {
          er=mean-Math.log(d)/Math.log(10.0);
          sd+=(er*er);
        }
      }
      else
      {
        er=mean-d;
        sd+=(er*er);
      }
    }
    sd=Math.sqrt(sd/(count-1));
    if (lognormal)
    {
      sd=Math.pow(10,sd);
      mean=Math.pow(10,mean);
    }
  }
  public void writeHeader1(PrintStream ps)
  {
    ps.print("\"");
    ps.print(LongName);
    ps.print(type);
    ps.print("_n\",\"");
    ps.print(LongName);
    ps.print(type);
    ps.print("_mean\",\"");
    ps.print(LongName);
    ps.print(type);
    ps.print("_sd\",\"");
    ps.print(LongName);
    ps.print(type);
    ps.print("_min\",\"");
    ps.print(LongName);
    ps.print(type);
    ps.print("_max\",");
  }
  public void writeHeader2(PrintStream ps)
  {
    ps.print("\"");
    ps.print(newShortName(0));
    ps.print("\",\"");
    ps.print(newShortName(1));
    ps.print("\",\"");
    ps.print(newShortName(2));
    ps.print("\",\"");
    ps.print(newShortName(3));
    ps.print("\",\"");
    ps.print(newShortName(4));
    ps.print("\",");
  }
  public void writeValue(PrintStream ps)
  {
    calculate();
    ps.print(count);
    ps.print(',');
    ps.print(mean);
    ps.print(',');
    ps.print(sd);
    ps.print(',');
    ps.print(min);
    ps.print(',');
    ps.print(max);
    ps.print(',');
  }
}