package gov.epa.hwir.util;

public class hwircp {
  public static final String  MEDIA_SOIL="SOIL";
  public static final String  MEDIA_SEDIMENT="Sediment";
  public static final String  MEDIA_SURFACEWATER="Surface Water";

  public native void generateSSF(String fname);
  public native void setDebug(boolean state);
  public native void setPath(String path);
  public native void setEnvironment(double temp,double ph, String media, double foc);
  public native void setCASID(String casid);
  public native int getNumChem();
  public native String getCASID(int index);
  public native String getName(int index);
  public native int getType(int index);
  // HWIR Specific functions below
  public native int getNumSolCW();
  public native double getSolCW(int index);
  public native int getNumLiqCW();
  public native double getLiqCW(int index);
  static
  {
    System.loadLibrary("javaHWIRCP");
  }
}
