package gov.epa.hwir.util;

public class StatDll {
  public StatDll() {
  }

  public native void StatSeed(int n);

  public native void StatDebugOn();

  public native void StatClear();

  public native void StatNumDist(int NumDist);

  public native void StatNumCor(int NumCor);

  public native void StatCor(int Index1, int Index2, float Cor);

  public native void StatSample(int Num, double[] Values);

  public native int StatNormal(double mean, double sd, double min, double max);

  public native int StatLogNormal(double mean, double sd, double min,
                                  double max);

  public native int StatExponential(double ct, double min, double max);

  public native int StatUniform(double min, double max);

  public native int StatSB(double ct, double var, double min, double max);

  public native int StatSU(double ct, double var, double min, double max);

  public native int StatEmpirical ( double min, double max, int Num,
                                   double[] Values, double[] CumProb);

  /**
   * this is used only for the discrete
   * @param min double
   * @param max double
   * @return int
   */
  public native int StatIntUniform(double min, double max);

  public native int StatTriangular(double mode, double min, double max);

  public native int StatTrnLogNormal(double tmean, double tsd, double min,
                                     double max);

  public native int StatGamma(double shape, double scale, double min,
                              double max);

  public native int StatWeibull(double shape, double scale, double min,
                                double max);

  public native int StatJohnsonSB( double mean, double sd, double min, double max);


  //added MSVCRT.dll and DFORRT.dll to the system32 directory.

  static {

    System.loadLibrary("StatDll");

  }

}
