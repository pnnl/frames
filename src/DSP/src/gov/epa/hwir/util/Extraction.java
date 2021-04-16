package gov.epa.hwir.util;

import java.io.*;
//import CSVReader;

public class Extraction
{
  public static final int NOTUSED=0;
  public static final int XCOUNT=-1;
  public static final int YCOUNT=-2;
  public static final int ZCOUNT=-3;
  public static final int VCOUNT=-4;
  public static final int WCOUNT=-5;

  public static final int REAL=0;
  public static final int INTEGER=1;
  public static final int STRING=2;
  public static final int LOGICAL=3;
  String group,name,units;
  int numX,numY,numZ,numV,numW;
  ArrayHWIRIO hio;
  int indexs[];
  int type;
  private static int getDim(int indexs[])
  {
    int retval=0;
    for (int i=0;i<indexs.length;i++)
      if (indexs[i]!=NOTUSED) retval++;
    return retval;
  }
  private static int[] getIndexs(int curX,int curY, int curZ,int curV,int curW,int indexs[])
  {
    int retval[]=new int[getDim(indexs)];
    int count=0;
    for (int i=0;i<indexs.length;i++)
      if (indexs[i]!=NOTUSED)
      {
        if (indexs[i]>0) retval[count]=indexs[i];
        else if (indexs[i]==XCOUNT) retval[count]=curX;
        else if (indexs[i]==YCOUNT) retval[count]=curY;
        else if (indexs[i]==ZCOUNT) retval[count]=curZ;
        else if (indexs[i]==VCOUNT) retval[count]=curV;
        else if (indexs[i]==WCOUNT) retval[count]=curW;
        count++;
      }
    return retval;
  }
  private int[] getIndexs(int curX,int curY,int curZ,int curV,int curW)
  {
    return getIndexs(curX,curY,curZ,curV,curW,indexs);
  }
  public Extraction(ArrayHWIRIO io,String newGroup,String newName,String newUnits,int newIndexs[],int newType)
  {
    group=newGroup;
    name=newName;
    units=newUnits;
    hio=io;
    indexs=newIndexs;
    type=newType;
  }
  public Extraction(ArrayHWIRIO io,CSVReader csv)
  {
    String temp;
    int count;
    int idx[]=new int[6];
    group=csv.readString();
    name=csv.readString();
    units=csv.readString();
    hio=io;
    temp=csv.readString();
    type=STRING;
    if (temp.equalsIgnoreCase("real") || temp.equalsIgnoreCase("float") ||
        temp.equalsIgnoreCase("double"))
      type=REAL;
    else if (temp.equalsIgnoreCase("integer") || temp.equalsIgnoreCase("int"))
      type=INTEGER;
    else if (temp.equalsIgnoreCase("boolean") || temp.equalsIgnoreCase("bool") ||
             temp.equalsIgnoreCase("logical"))
      type=LOGICAL;
    count=0;
    do
    {
      temp=csv.readString();
      if (temp.equalsIgnoreCase("X"))
      {
        idx[count]=XCOUNT;
        count++;
      }
      else if (temp.equalsIgnoreCase("Y"))
      {
        idx[count]=YCOUNT;
        count++;
      }
      else if (temp.equalsIgnoreCase("Z"))
      {
        idx[count]=ZCOUNT;
        count++;
      }
      else if (temp.equalsIgnoreCase("V"))
      {
        idx[count]=VCOUNT;
        count++;
      }
      else if (temp.equalsIgnoreCase("W"))
      {
        idx[count]=WCOUNT;
        count++;
      }
      else if (temp.length()>0)
      {
        idx[count]=Integer.valueOf(temp).intValue();
        count++;
      }
    }
    while (temp.length()>0);
    csv.readLine();
    indexs=new int[count];
    for (int i=0;i<count;i++)
      indexs[i]=idx[i];
  }
  public void setNumX(int newNumX)
  {
    numX=newNumX;
  }
  public void setNumX(Extraction e)
  {
    numX=e.getCount();
    if (numX==0) numX=1;
  }
  public void setNumY(int newNumY)
  {
    numY=newNumY;
  }
  public void setNumY(Extraction e)
  {
    numY=e.getCount();
    if (numY==0) numY=1;
  }
  public void setNumZ(int newNumZ)
  {
    numZ=newNumZ;
  }
  public void setNumZ(Extraction e)
  {
    numZ=e.getCount();
    if (numZ==0) numZ=1;
  }
  public void setNumV(int newNumV)
  {
    numV=newNumV;
  }
  public void setNumV(Extraction e)
  {
    numV=e.getCount();
    if (numV==0) numV=1;
  }
  public void setNumW(int newNumW)
  {
    numW=newNumW;
  }
  public void setNumW(Extraction e)
  {
    numW=e.getCount();
    if (numW==0) numW=1;
  }
  public String getLongName(String group,int x,int y,int z,int v,int w)
  {
    StringBuffer sb=new StringBuffer();
    sb.append(group);
    sb.append(',');
    sb.append(name);
    sb.append(",\"");
    sb.append(units);
    sb.append("\"(");
    for (int i=0;i<indexs.length;i++)
    {
      if (i>0) sb.append(',');
      if (indexs[i]==XCOUNT)
      {
        sb.append("X=");
        sb.append(x);
      }
      else if (indexs[i]==YCOUNT)
      {
        sb.append("Y=");
        sb.append(y);
      }
      else if (indexs[i]==ZCOUNT)
      {
        sb.append("Z=");
        sb.append(z);
      }
      else if (indexs[i]==VCOUNT)
      {
        sb.append("V=");
        sb.append(v);
      }
      else if (indexs[i]==WCOUNT)
      {
        sb.append("W=");
        sb.append(w);
      }
      else
        sb.append(indexs[i]);
    }
    sb.append(')');
    return sb.toString();
  }
  private void output(String s,int vals[])
  {
       System.out.print(s);
       if (vals!=null)
        for (int i=0;i<vals.length;i++)
        {
          System.out.print(vals[i]);
          System.out.print(" ");
        }
        System.out.println();
  }
  public void extractIntoSummary(Summary s)
  {
    boolean skipGroup;
    int idx[],actualIdx[],noVars[];
    String actualGroup;
    Summarizable v;
    if (numX==0) numX=1;
    if (numY==0) numY=1;
    if (numZ==0) numZ=1;
    if (numV==0) numV=1;
    if (numW==0) numW=1;
    for (int x=0;x<numX;x++)
      for (int y=0;y<numY;y++)
        for (int z=0;z<numZ;z++)
          for (int V=0;V<numV;V++)
            for (int w=0;w<numW;w++)
      {
        noVars=Extraction.getIndexs(x+1,y+1,z+1,V+1,w+1,indexs);
        idx=hio.getIndexs(group,noVars);
        if (idx==null && noVars!=null)
          actualGroup=hio.getGroupName(group,noVars[0]);  // Get the actual name
        else if (idx==null)
          actualGroup=hio.getGroupName(group,0);  // Get the actual name
        else if (idx.length!=noVars.length)
          actualGroup=hio.getGroupName(group,noVars[0]);  // Get the actual name
        else
          actualGroup=hio.getGroupName(group,0);
        hio.addRGroup(actualGroup);
        skipGroup=!hio.getStatus();
//        output(actualGroup+" ",idx);
        if (type==REAL)
        {
          double d=0.0;
          if (!skipGroup) d=hio.readReal(actualGroup,name,units,idx);
          if (hio.getStatus()) v=(Summarizable)new NumValue(actualGroup,name,units,idx,d);
          else v=(Summarizable)new NumValue(actualGroup,name,units,idx);
          s.add(v);
        }
        else if (type==INTEGER)
        {
          double d=0.0;
          if (!skipGroup) d=(double)hio.readInt(actualGroup,name,units,idx);
          if (hio.getStatus()) v=(Summarizable)new NumValue(actualGroup,name,units,idx,d);
          else v=(Summarizable)new NumValue(actualGroup,name,units,idx);
          s.add(v);
        }
        else if (type==STRING)
        {
          String temp="";
          if (!skipGroup) temp=hio.readString(actualGroup,name,units,idx);
          if (hio.getStatus()) v=(Summarizable)new StringValue(actualGroup,name,units,idx,temp);
          else v=(Summarizable)new StringValue(actualGroup,name,units,idx);
          s.add(v);
        }
        else if (type==LOGICAL)
        {
          boolean b=false;
          if (!skipGroup) b=hio.readLog(actualGroup,name,units,idx);
          if (hio.getStatus())
          {
            if (b) v=(Summarizable)new StringValue(actualGroup,name,units,idx,"Yes");
            else v=(Summarizable)new StringValue(actualGroup,name,units,idx,"No");
          }
          else v=(Summarizable)new StringValue(actualGroup,name,units,idx);
          s.add(v);
        }
        if (!hio.getStatus())
        {
          hio.resetArgs();
          hio.openGroups();
          if (!hio.getStatus())
          {
            hio.setArgs(hio.getArgs()+" hdSummary.grf");
            hio.openGroups();
          }
          if (hio.getStatus()) hio.warning("Error reading "+getLongName(actualGroup,x+1,y+1,z+1,V+1,w+1));
        }
      }
  }
  public Parameterized parameterize(String distType)
  {
    boolean skipGroup;
    Parameterized d=new Parameterized(group,name,units,indexs,distType);
    int idx[],noVars[];
    String actualGroup;
    double v=0.0;
    if (numX==0) numX=1;
    if (numY==0) numY=1;
    if (numZ==0) numZ=1;
    if (numV==0) numV=1;
    if (numW==0) numW=1;
    for (int x=0;x<numX;x++)
      for (int y=0;y<numY;y++)
        for (int z=0;z<numZ;z++)
          for (int V=0;V<numV;V++)
            for (int w=0;w<numW;w++)
      {
        noVars=Extraction.getIndexs(x+1,y+1,z+1,V+1,w+1,indexs);
        idx=hio.getIndexs(group,noVars);
        if (idx==null && noVars!=null)
          actualGroup=hio.getGroupName(group,noVars[0]);  // Get the actual name
        else if (idx==null)
          actualGroup=hio.getGroupName(group,0);  // Get the actual name
        else if (idx.length!=noVars.length)
          actualGroup=hio.getGroupName(group,noVars[0]);  // Get the actual name
        else
          actualGroup=hio.getGroupName(group,0);
        hio.addRGroup(actualGroup);
        skipGroup=!hio.getStatus();
        if (type==REAL)
        {
          if (!skipGroup) v=hio.readReal(actualGroup,name,units,idx);
          if (hio.getStatus()) d.add(v);
        }
        else if (type==INTEGER)
        {
          if (!skipGroup) v=(double)hio.readInt(actualGroup,name,units,idx);
          if (hio.getStatus()) d.add(v);
        }
        if (!hio.getStatus() )
        {
          hio.resetArgs();
          hio.openGroups();
          if (!hio.getStatus())
          {
            hio.setArgs(hio.getArgs()+" hdSummary.grf");
            hio.openGroups();
          }
          hio.warning("Error reading "+getLongName(actualGroup,x+1,y+1,z+1,V+1,w+1));
        }
      }
    return d;
  }
  public Percentile percentiles(double[] percents)
  {
    boolean skipGroup;
    Percentile d=new Percentile(group,name,units,indexs,percents);
    int idx[],noVars[];
    String actualGroup;
    double v=0.0;
    if (numX==0) numX=1;
    if (numY==0) numY=1;
    if (numZ==0) numZ=1;
    if (numV==0) numV=1;
    if (numW==0) numW=1;
    for (int x=0;x<numX;x++)
      for (int y=0;y<numY;y++)
        for (int z=0;z<numZ;z++)
          for (int V=0;V<numV;V++)
            for (int w=0;w<numW;w++)
      {
        noVars=Extraction.getIndexs(x+1,y+1,z+1,V+1,w+1,indexs);
        idx=hio.getIndexs(group,noVars);
        if (idx==null && noVars!=null)
          actualGroup=hio.getGroupName(group,noVars[0]);  // Get the actual name
        else if (idx==null)
          actualGroup=hio.getGroupName(group,0);  // Get the actual name
        else if (idx.length!=noVars.length)
          actualGroup=hio.getGroupName(group,noVars[0]);  // Get the actual name
        else
          actualGroup=hio.getGroupName(group,0);
        hio.addRGroup(actualGroup);
        skipGroup=!hio.getStatus();
        if (type==REAL)
        {
          if (!skipGroup) v=hio.readReal(actualGroup,name,units,idx);
          if (hio.getStatus()) d.add(v);
        }
        else if (type==INTEGER)
        {
          if (!skipGroup) v=(double)hio.readInt(actualGroup,name,units,idx);
          if (hio.getStatus()) d.add(v);
        }
        if (!hio.getStatus())
        {
          hio.resetArgs();
          hio.openGroups();
          if (!hio.getStatus())
          {
            hio.setArgs(hio.getArgs()+" hdSummary.grf");
            hio.openGroups();
          }
          hio.warning("Error reading "+getLongName(actualGroup,x+1,y+1,z+1,V+1,w+1));
        }
      }
    return d;
  }
  public int getCount()
  {
    int idx[],actualIdx[];
    int retval,next=0;
    boolean skipGroup;
    String actualGroup;
    actualIdx=hio.getIndexs(group,indexs);  // Shift them up if special group like sw
    retval=1;
    boolean first=false;
//    System.out.println(getLongName(group,numX,numY,numZ,numV,numW));
    if (numX==0) numX=1;
    if (numY==0) numY=1;
    if (numZ==0) numZ=1;
    if (numV==0) numV=1;
    if (numW==0) numW=1;
    for (int x=0;x<numX;x++)
      for (int y=0;y<numY;y++)
        for (int z=0;z<numZ;z++)
          for (int V=0;V<numV;V++)
            for (int w=0;w<numW;w++)
      {
        idx=Extraction.getIndexs(x+1,y+1,z+1,V+1,w+1,actualIdx);  // Replace X and Y with 0 and 0
        if (idx!=null && idx.length>0)
          actualGroup=hio.getGroupName(group,indexs[0]);  // Get the actual name
        else
          actualGroup=hio.getGroupName(group,0);
//        System.out.println(getLongName(actualGroup,numX,numY,numZ,numV,numW));
        hio.addRGroup(actualGroup);
        next=hio.readInt(actualGroup,name,units,idx);
//        if (!hio.getStatus()) System.out.println("Failed");
//        else System.out.println("Ok");
//        System.out.println(next);
        if (hio.getStatus()) {
          if (first)  { retval=next; first=false; }
          else if (next>retval) retval=next;
        }
        if (hio.getStatus())
          hio.removeGroup(actualGroup);

        if (!hio.getStatus())
        {
          hio.resetArgs();
          hio.openGroups();
          if (!hio.getStatus())
          {
            hio.setArgs(hio.getArgs()+" hdSummary.grf");
            hio.openGroups();
          }
          hio.warning("Error reading "+getLongName(actualGroup,x+1,y+1,z+1,V+1,w+1));
        }
    }
//    System.out.println(retval);
    return retval;
  }
}
