package gov.epa.hwir.util;

import java.io.*;

public class StringValue extends Value
{
  String Value;
  boolean isNULL;
  public StringValue(String group,String name,String units,int idx[],String newValue)
  {
    super(group,name,units,idx);
    isNULL=false;
    Value=newValue;
  }
  public StringValue(String group,String name,String units,int idx[])
  {
    super(group,name,units,idx);
    isNULL=true;
  }
  public void writeValue(PrintStream ps)
  {
    if (isNULL) ps.print("NULL");
    else ps.print(Value);
    ps.print(',');
  }
  public void addValues(StringBuffer sb)
  {
    if (isNULL) sb.append("NULL");
    else
    {
      sb.append('\'');
      sb.append(Value);
      sb.append('\'');
    }
  }
  public void createColumns(StringBuffer sb)
  {
    sb.append(ShortName);
    sb.append(" char(80)");
  }
}
