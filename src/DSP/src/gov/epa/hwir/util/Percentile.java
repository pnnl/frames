package gov.epa.hwir.util;

import java.io.*;
import java.sql.*;

public class Percentile extends Distribution
{
  double percents[];
  double pervalues[];
  public void addValues(StringBuffer sb)
  {
    calc();
    for (int i=0;i<percents.length;i++)
    {
      if (i>0) sb.append(", ");
      sb.append(pervalues[i]);
    }
  }
  private String newShortName(double per)
  {
    StringBuffer sb=new StringBuffer();
    sb.append(ShortName);
    sb.append('_');
    sb.append((int)per);
    sb.append("per");
    return sb.toString();
  }
  public void createColumns(StringBuffer sb)
  {
    for (int i=0;i<percents.length;i++)
    {
      if (i>0) sb.append(", ");
      sb.append(newShortName(percents[i]));
      sb.append(" double");
    }
  }
  public void addColumns(StringBuffer sb)
  {
    for (int i=0;i<percents.length;i++)
    {
      if (i>0) sb.append(", ");
      sb.append(newShortName(percents[i]));
    }
  }
  private void addPercentDescription(Statement s,double per)
  {
    StringBuffer sb=new StringBuffer("insert into description ");
    sb.append("(alias ,summary, datagroup, variable, units, idx1, idx2, idx3, idx4, idx5, idx6) values (");
    sb.append("'");
    sb.append(newShortName(per));
    sb.append("', '");
    sb.append("Percentiles");
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
    for (int i=0;i<percents.length;i++)
      addPercentDescription(s, percents[i]);
  }
  public Percentile(String group,String name,String units,int idx[], double newPercents[])
  {
    super(group,name,units,idx);
    percents=newPercents;
    setType("Percentiles");
  }
  public void writeHeader1(PrintStream ps)
  {
    for (int i=0;i<percents.length;i++)
    {
      if (i>0) ps.print(',');
      ps.print('"');
      ps.print(LongName);
      ps.print('_');
      ps.print(percents[i]);
      ps.print('%');
      ps.print('"');
    }
  }
  public void writeHeader2(PrintStream ps)
  {
    for (int i=0;i<percents.length;i++)
    {
      if (i>0) ps.print(',');
      ps.print('"');
      ps.print(newShortName(percents[i]));
      ps.print('"');
    }
  }
  private void sort(Object o[],int m[])
  {
    int t;
    for (int i=0;i<o.length;i++)
      for (int j=0;j<i;j++)
        if (((Double)o[m[i]]).doubleValue()<
            ((Double)o[m[j]]).doubleValue())
        {
          t=m[i];
          m[i]=m[j];
          m[j]=t;
        }
  }
  public void calc()
  {
    int idx;
    Object objs[]=values.toArray();
    pervalues=new double[percents.length];
    if (objs.length==0)  {
      for (int i=0;i<percents.length;i++)
        pervalues[i]=0.0;
      return;
    }
    int map[]=new int[objs.length];
 //   System.out.println(objs.length);
    for (int i=0;i<objs.length;i++)
      map[i]=i;
    sort(objs,map);
    for (int i=0;i<percents.length;i++)
    {
      idx=(int)(percents[i]*values.size()/100.0);
      if (idx<0) idx=0;
      else if (idx>=objs.length) idx=objs.length-1;
      pervalues[i]=((Double)objs[map[idx]]).doubleValue();
    }
  }
  public void writeValue(PrintStream ps)
  {
    calc();
    for (int i=0;i<percents.length;i++)
    {
      if (i>0) ps.print(',');
      ps.print(pervalues[i]);
    }
  }
}
