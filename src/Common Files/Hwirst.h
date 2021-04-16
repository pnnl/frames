///////////////////////////////////////////////////////////////////////////////
//          Code:  HWIRST.DLL
//          File:  HWIRST.H
//   Programmers:  Karl Castleton, Bonnnie Hooopes
//  Organization:  PNNL, Battelle
//       Created:  4/13/1998
//      Modified:
///////////////////////////////////////////////////////////////////////////////

#ifndef statH
#define statH

typedef struct // Digital Visual Fortran Descriptor
{
  long Base,Size,Offset,Defined,Dim,NumElem,Dist,Lower;
} Descriptor;

  // float version of stats
void TestTimes(int Num, float *Times);
void quickAdd(int Num1, float *Times1, float *Values1,
              int Num2, float *Times2, float *Values2,
              int *NumRes, float *TimesRes, float *ValuesRes);
int zeroStartsEnd(int Num1, float *Times1, float *Values1, float **Times, float **Values);
  void  Mult(int Num, float *Times, float *Values, float Scalar);
  void  Add(int Num1, float *Times1, float *Values1, int Num2, float *Times2, float *Values2, int *NumRes, float *TimesRes, float *ValuesRes);
  void  Max(int Num, float *Times, float *Values, float *MaxTime, float *MaxValue);
  void  Min(int Num, float *Times, float *Values, float *MinTime, float *MinValue);
  float Avg(int Num, float *Times, float *Values, float Start, float End);
  void  RunAvg(int Num, float *Times, float *Values, float Period, int *NumRes, float *TimesRes, float *ValuesRes);
  float Interp(int Num, float *Times, float *Values, float Time);

  // double version of stats
void dTestTimes(int Num, double *Times);
void dquickAdd(int Num1, double *Times1, double *Values1,
              int Num2, double *Times2, double *Values2,
              int *NumRes, double *TimesRes, double *ValuesRes);
int dzeroStartsEnd(int Num1, double *Times1, double *Values1, double **Times, double **Values);
  void   dMult(int Num, double *Times, double *Values, double Scalar);
  void   dAdd(int Num1, double *Times1, double *Values1, int Num2, double *Times2, double *Values2, int *NumRes, double *TimesRes, double *ValuesRes);
  void   dMax(int Num, double *Times, double *Values, double *MaxTime, double *MaxValue);
  void   dMin(int Num, double *Times, double *Values, double *MinTime, double *MinValue);
  double dAvg(int Num, double *Times, double *Values, double Start, double End);
  void   dRunAvg(int Num, double *Times, double *Values, double Period, int *NumRes, double *TimesRes, double *ValuesRes);
  double dInterp(int Num, double *Times, double *Values, double Time);


#endif
