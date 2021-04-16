package gov.epa.hwir.util;

public class hwirio
{

  public native void setArgs(String args);
  public native void setArgsSystem(String args);
  public native boolean getStatus();
  public native void resetArgs();
  public native void addGroup(String group);
  public native void addRWGroup(String group);
  public native void removeGroup(String group);
  public native int numArgs();
  public native int getArgInt(int argIndex);
  public native String getArgString(int argIndex);
  public native void openGroups();
  public native void closeGroups();
  public native void error(String description);
  public native void warning(String description);
  // ReadInt# from HWIRIO.dll the # has been dropped because Java supports
  // Function overloading.
  public native int IOOk();
  public native String getArgs();
  public native int readInt(String group,String varname,String units);
  public native int readInt(String group,String varname,String units,int index1);
  public native int readInt(String group,String varname,String units,int index1,int index2);
  public native int readInt(String group,String varname,String units,int index1,int index2,int index3);
  public native int readInt(String group,String varname,String units,int index1,int index2,int index3,int index4);
  public native int readInt(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5);
  public native int readInt(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,int index6);

  public native void writeInt(String group,String varname,String units,int val);
  public native void writeInt(String group,String varname,String units,int index1,int val);
  public native void writeInt(String group,String varname,String units,int index1,int index2,int val);
  public native void writeInt(String group,String varname,String units,int index1,int index2,int index3,int val);
  public native void writeInt(String group,String varname,String units,int index1,int index2,int index3,int index4,int val);
  public native void writeInt(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,int val);
  public native void writeInt(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,int index6,int val);

  public native double readReal(String group,String varname,String units);
  public native double readReal(String group,String varname,String units,int index1);
  public native double readReal(String group,String varname,String units,int index1,int index2);
  public native double readReal(String group,String varname,String units,int index1,int index2,int index3);
  public native double readReal(String group,String varname,String units,int index1,int index2,int index3,int index4);
  public native double readReal(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5);
  public native double readReal(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,int index6);

  public native void writeReal(String group,String varname,String units,double val);
  public native void writeReal(String group,String varname,String units,int index1,double val);
  public native void writeReal(String group,String varname,String units,int index1,int index2,double val);
  public native void writeReal(String group,String varname,String units,int index1,int index2,int index3,double val);
  public native void writeReal(String group,String varname,String units,int index1,int index2,int index3,int index4,double val);
  public native void writeReal(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,double val);
  public native void writeReal(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,int index6,double val);

  public native boolean readLog(String group,String varname,String units);
  public native boolean readLog(String group,String varname,String units,int index1);
  public native boolean readLog(String group,String varname,String units,int index1,int index2);
  public native boolean readLog(String group,String varname,String units,int index1,int index2,int index3);
  public native boolean readLog(String group,String varname,String units,int index1,int index2,int index3,int index4);
  public native boolean readLog(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5);
  public native boolean readLog(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,int index6);

  public native void writeLog(String group,String varname,String units,boolean val);
  public native void writeLog(String group,String varname,String units,int index1,boolean val);
  public native void writeLog(String group,String varname,String units,int index1,int index2,boolean val);
  public native void writeLog(String group,String varname,String units,int index1,int index2,int index3,boolean val);
  public native void writeLog(String group,String varname,String units,int index1,int index2,int index3,int index4,boolean val);
  public native void writeLog(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,boolean val);
  public native void writeLog(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,int index6,boolean val);

  public native String readString(String group,String varname,String units);
  public native String readString(String group,String varname,String units,int index1);
  public native String readString(String group,String varname,String units,int index1,int index2);
  public native String readString(String group,String varname,String units,int index1,int index2,int index3);
  public native String readString(String group,String varname,String units,int index1,int index2,int index3,int index4);
  public native String readString(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5);
  public native String readString(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,int index6);

  public native void writeString(String group,String varname,String units,String val);
  public native void writeString(String group,String varname,String units,int index1,String val);
  public native void writeString(String group,String varname,String units,int index1,int index2,String val);
  public native void writeString(String group,String varname,String units,int index1,int index2,int index3,String val);
  public native void writeString(String group,String varname,String units,int index1,int index2,int index3,int index4,String val);
  public native void writeString(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,String val);
  public native void writeString(String group,String varname,String units,int index1,int index2,int index3,int index4,int index5,int index6,String val);

  static
  {

    System.loadLibrary( "javaHWIRIO" );
  //System.load( "C:\\Documents and Settings\\d3m293\\jbproject\\DSP\\src\\gov\\epa\\hwir\\util\\javaHWIRIO\\Debug\\javaHWIRIO");

  }
}
