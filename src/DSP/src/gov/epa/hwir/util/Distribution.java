package gov.epa.hwir.util;

import java.util.*;
import java.io.*;

public abstract class Distribution extends Value implements Summarizable, JDBCSummarizable
{
  protected Vector values;
  public Distribution(String group,String name,String units,int idx[])
  {
    super(group,name,units,idx);
    values=new Vector();
    setType("Distribution");
  }
  public void add(double d)
  {
    values.add(new Double(d));
  }
}
