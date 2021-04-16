package gov.epa.hwir.util;

import java.io.*;
import java.sql.*;

public abstract class Value implements Summarizable, JDBCSummarizable
{
  protected String LongName;
  protected String ShortName;
  static int count=0;
  String group;
  String name;
  String units;
  int idx[];
  String type;
  protected void setType(String newType)
  {
    type=newType;
  }
  public static void createTable(Statement s,String prefix)
  {
    StringBuffer sb=new StringBuffer("create table ");
    sb.append(prefix);
    sb.append("_description (");
    sb.append(" alias char(32),");
    sb.append(" summary char(32),");
    sb.append(" datagroup char(32),");
    sb.append(" variable char(32),");
    sb.append(" units char(32),");
    sb.append(" idx1 int,");
    sb.append(" idx2 int,");
    sb.append(" idx3 int,");
    sb.append(" idx4 int,");
    sb.append(" idx5 int,");
    sb.append(" idx6 int);");
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
  public void addColumns(StringBuffer sb)
  {
//    sb.append(", ");
    sb.append(ShortName);
  }
  public void addDescription(Statement s,String prefix)
  {
    StringBuffer sb=new StringBuffer("insert into ");
    sb.append(prefix);
    sb.append("_description ");
    sb.append("(alias ,summary, datagroup, variable, units, idx1, idx2, idx3, idx4, idx5, idx6) values (");
    sb.append("'");
    sb.append(ShortName);
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
    catch (SQLException e)
    {
      System.out.println(e);
    }
  }
  public static String getShortName()
  {
    String ShortName="Var"+String.valueOf(count);
    count++;
    return ShortName;
  }
  private String makeLong()
  {
    StringBuffer sb=new StringBuffer();
    sb.append(group);
    sb.append('.');
    sb.append(name);
    if (units.length()>0)
    {
      sb.append('.');
      sb.append(units);
    }
    if (idx!=null && idx.length>0)
    {
      sb.append('(');
      for (int i=0;i<idx.length;i++)
      {
        if (i>0) sb.append(',');
        sb.append(idx[i]);
      }
      sb.append(')');
    }
    return sb.toString();
  }
  public Value(String newGroup,String newName,String newUnits,int newIdx[])
  {
    group=newGroup;
    name=newName;
    units=newUnits;
    idx=newIdx;
    LongName=makeLong();
    ShortName=getShortName();
    type="Variable";
  }
  public void writeHeader1(PrintStream ps)
  {
    ps.print('"');
    ps.print(LongName);
    ps.print('"');
    ps.print(',');
  }
  public void writeHeader2(PrintStream ps)
  {
    ps.print(ShortName);
    ps.print(',');
  }
}
