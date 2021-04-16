package gov.epa.hwir.util;

import java.sql.*;

public interface JDBCSummarizable
{
  public void addValues(StringBuffer sb);
  public void addColumns(StringBuffer sb);
  public void addDescription(Statement s,String prefix);
  public void createColumns(StringBuffer sb);
}
