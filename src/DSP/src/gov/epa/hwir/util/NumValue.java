package gov.epa.hwir.util;

import java.io.*;

public class NumValue extends Value
{
  double value;
  boolean isNULL;
  public NumValue(String group,String name,String units,int idx[],double newValue)
  {
    super(group,name,units,idx);
    isNULL=false;
    value=newValue;
  }
  public NumValue(String group,String name,String units,int idx[])
  {
    super(group,name,units,idx);
    isNULL=true;
  }
  public void writeValue(PrintStream ps)
  {
    if (isNULL) ps.print("NULL");
    else        ps.print(value);
    ps.print(',');
  }
  public void addValues(StringBuffer sb)
  {
    if (isNULL) sb.append("NULL");
    else sb.append(value);
  }
  public void createColumns(StringBuffer sb)
  {
    sb.append(ShortName);
    sb.append(" double");
  }
}