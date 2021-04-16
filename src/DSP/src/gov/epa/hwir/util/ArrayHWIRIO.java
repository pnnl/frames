package gov.epa.hwir.util;

public class ArrayHWIRIO extends hwirio
{
  public String header,site,source;
  public String vadid,aquid;
  private StringBuffer args;
  private String lastRGroup;
  private boolean calledOpen;
  public String getArgs()
  {
    return args.toString();
  }
  private void init(String ssf,String grf,String newHeader,boolean callopen)
  {
    args=new StringBuffer();
    header=newHeader;
    args.append("Time Date ");
    args.append(ssf);
    args.append(' ');
    args.append(grf);
    args.append(" 1 ");
    args.append(header);
    setArgsSystem(args.toString()+" hdSummary.grf");
    if (callopen) openGroups();
    site=readString(header,"SiteID","");
    source=readString(header,"Source","");
    if (source.length()>2) source=source.substring(0,2);
    vadid=source+site;
    aquid=source+site;
    lastRGroup="";
  }
  public void openGroups()
  {
    calledOpen=true;
    super.openGroups();
  }
  public void closeGroups()
  {
    calledOpen=false;
    super.closeGroups();
  }
  public void addRGroup(String group)
  {
    if (lastRGroup.equals(group)) return;  // No need to reopen and close for the group
    lastRGroup=group;
    if (calledOpen) closeGroups();
    StringBuffer sb=new StringBuffer(args.toString());
    sb.append(' ');
    sb.append(group);
    sb.append(" hdSummary.grf");
    setArgsSystem(sb.toString());
    openGroups();
    if (!getStatus())
    {
      calledOpen=false;
      lastRGroup="";
    }
  }
  public ArrayHWIRIO(String ssf,String grf,String newHeader)
  {
    super();
    init(ssf,grf,newHeader,true);
  }
  public ArrayHWIRIO(String ssf,String grf,String newHeader,boolean callopen)
  {
    super();
    init(ssf,grf,newHeader,callopen);
  }
  public void addRWGroup(String group)
  {
    if (!group.equalsIgnoreCase(header)) super.addRWGroup(group);
  }
  public void addGroup(String group)
  {
    if (!group.equalsIgnoreCase(header)) super.addGroup(group);
  }
  public void removeGroup(String group)
  {
    if (!group.equalsIgnoreCase(header)) super.removeGroup(group);
  }
  public void finalize()
  {
    if (calledOpen) closeGroups();
  }
  public String getGroupName(String shortName,int index1)
  {
    if (shortName.endsWith(".ssf"))
    {
      if (shortName.startsWith("vz"))
        return "vz"+vadid+".ssf";
      else if (shortName.startsWith("aq"))
        return "aq"+aquid+".ssf";
      else if (shortName.startsWith("cp"))
        return shortName;
      else if (shortName.startsWith("hd"))
        return header;
      else if (shortName.startsWith("sw"))
        return "sw"+String.valueOf(index1)+".ssf";
      else
        return shortName.substring(0,2)+source+site+".ssf";
    }
    else  if (shortName.endsWith(".grf"))
    {
      if (shortName.startsWith("vz"))
        return "vz"+vadid+".grf";
      else if (shortName.startsWith("aq"))
        return "aq"+aquid+".grf";
      else if (shortName.startsWith("sl"))
        return "sl"+aquid+".grf";
      else if (shortName.startsWith("sw"))
        return "sw"+String.valueOf(index1)+".grf";
      else
        return shortName;
    }
    return "error";
  }
  public int[] getIndexs(String shortname,int indexs[])
  {
    if (shortname.startsWith("sw"))
    {
      if (indexs.length-1==0) return null;
      int retval[]=new int[indexs.length-1];
      for (int i=0;i<indexs.length-1;i++)
        retval[i]=indexs[i+1];
      return retval;
    }
    else
      return indexs;
  }
  public int readInt(String group,String name,String units, int indexs[])
  {
    if (indexs==null) return readInt(group,name,units);
    else if (indexs.length==0) return readInt(group,name,units);
    else if (indexs.length==1) return readInt(group,name,units,indexs[0]);
    else if (indexs.length==2) return readInt(group,name,units,indexs[0],indexs[1]);
    else if (indexs.length==3) return readInt(group,name,units,indexs[0],indexs[1],indexs[2]);
    else if (indexs.length==4) return readInt(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3]);
    else if (indexs.length==5) return readInt(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3],indexs[4]);
    else if (indexs.length==6) return readInt(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3],indexs[4],indexs[5]);
    return 0;
  }
  public double readReal(String group,String name,String units, int indexs[])
  {
    if (indexs==null) return readReal(group,name,units);
    else if (indexs.length==0) return readReal(group,name,units);
    else if (indexs.length==1) return readReal(group,name,units,indexs[0]);
    else if (indexs.length==2) return readReal(group,name,units,indexs[0],indexs[1]);
    else if (indexs.length==3) return readReal(group,name,units,indexs[0],indexs[1],indexs[2]);
    else if (indexs.length==4) return readReal(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3]);
    else if (indexs.length==5) return readReal(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3],indexs[4]);
    else if (indexs.length==6) return readReal(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3],indexs[4],indexs[5]);
    return 0.0;
  }
  public String readString(String group,String name,String units, int indexs[])
  {
    if (indexs==null) return readString(group,name,units);
    else if (indexs.length==0) return readString(group,name,units);
    else if (indexs.length==1) return readString(group,name,units,indexs[0]);
    else if (indexs.length==2) return readString(group,name,units,indexs[0],indexs[1]);
    else if (indexs.length==3) return readString(group,name,units,indexs[0],indexs[1],indexs[2]);
    else if (indexs.length==4) return readString(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3]);
    else if (indexs.length==5) return readString(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3],indexs[4]);
    else if (indexs.length==6) return readString(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3],indexs[4],indexs[5]);
    return "";
  }
  public boolean readLog(String group,String name,String units, int indexs[])
  {
    if (indexs==null) return readLog(group,name,units);
    else if (indexs.length==0) return readLog(group,name,units);
    else if (indexs.length==1) return readLog(group,name,units,indexs[0]);
    else if (indexs.length==2) return readLog(group,name,units,indexs[0],indexs[1]);
    else if (indexs.length==3) return readLog(group,name,units,indexs[0],indexs[1],indexs[2]);
    else if (indexs.length==4) return readLog(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3]);
    else if (indexs.length==5) return readLog(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3],indexs[4]);
    else if (indexs.length==6) return readLog(group,name,units,indexs[0],indexs[1],indexs[2],indexs[3],indexs[4],indexs[5]);
    return false;
  }
}
