package gov.epa.hwir.util;

import java.io.*;

public interface Summarizable
{
  public void writeHeader1(PrintStream ps);
  public void writeHeader2(PrintStream ps);
  public void writeValue(PrintStream ps);
}
