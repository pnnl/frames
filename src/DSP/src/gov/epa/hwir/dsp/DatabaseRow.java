package gov.epa.hwir.dsp;

public class DatabaseRow {

  String mVariableName;
  String mDataGroupName;
  double mMax, mMin, mVar;


  public DatabaseRow(String var, String grp, double max, double min,
                     double varn) {
    mVariableName = var;
    mDataGroupName = grp;
    mMax = max;
    mMin = min;
    mVar = varn;
  }

  public double getMax() {
    return mMax;
  }

  public String getVariableName() {
    return mVariableName;
  }

  public double getVar() {
    return mVar;
  }

  public String getDataGroupName() {
    return mDataGroupName;
  }

  public void setMin(double mMin) {
    this.mMin = mMin;
  }

  public void setMax(double mMax) {
    this.mMax = mMax;
  }

  public void setVariableName(String mVariableName) {
    this.mVariableName = mVariableName;
  }

  public void setVar(double mVar) {
    this.mVar = mVar;
  }

  public void setDataGroupName(String mDataGroupName) {
    this.mDataGroupName = mDataGroupName;
  }

  public double getMin() {
    return mMin;
  }







}
