///////////////////////////////////////////////////////////////////////////////
//          Code:  HWIRST.DLL
//          File:  HWIRST.H
//   Programmers:  Karl Castleton, Bonnnie Hooopes
//  Organization:  PNNL, Battelle
//       Created:  4/13/1998
//      Modified:
///////////////////////////////////////////////////////////////////////////////
#include <windows.h>


#ifdef __cplusplus
extern "C" {
#endif

int FAR PASCAL LibMain( HINSTANCE hInstance, WORD wDataSegment, WORD wHeapSize, LPSTR lpszCmdLine );

#ifdef BORLAND
  #define FPASS WINAPI _export
  #define DFPASS _cdecl _export
  #define CPASS _cdecl _export
#else
  #define FPASS _declspec(dllexport) WINAPI
  #define DFPASS _declspec(dllexport) _cdecl
  #define CPASS _declspec(dllexport) _cdecl   
#endif

typedef struct // Digital Visual Fortran Descriptor
{
	long Base,Size,Offset,Defined,Dim,NumElem,Dist,Lower;
} Descriptor;

void FPASS fMult(int Num,double *Times,double *Values,double Scalar);
void DFPASS dfMult(int Num,Descriptor *Times,Descriptor *Values,double Scalar);
//void DFPASS dfMult(int Num,double *Times,double *Values,double Scalar);
void CPASS Mult(int Num,double *Times,double *Values,double Scalar);

void FPASS fAdd(int Num1,double *Times1,double *Values1,
               int Num2,double *Times2,double *Values2,
               int *NumRes,double *TimesRes,double *ValuesRes);
void DFPASS dfAdd(int Num1 ,Descriptor *Times1  ,Descriptor *Values1,
               int Num2   ,Descriptor *Times2  ,Descriptor *Values2,
               int *NumRes,Descriptor *TimesRes,Descriptor *ValuesRes);
//void DFPASS dfAdd(int Num1,double *Times1,double *Values1,
//               int Num2,double *Times2,double *Values2,
//               int *NumRes,double *TimesRes,double *ValuesRes);
void CPASS Add(int Num1,double *Times1,double *Values1,
               int Num2,double *Times2,double *Values2,
               int *NumRes,double *TimesRes,double *ValuesRes);

void FPASS fMax(int Num,double *Times,double *Values,
               double *MaxTime,double *MaxValue);
void DFPASS dfMax(int Num,Descriptor *Times,Descriptor *Values,
               double *MaxTime,double *MaxValue);
//void DFPASS dfMax(int Num,double *Times,double *Values,
//                  double *MaxTime,double *MaxValue);
void CPASS Max(int Num,double *Times,double *Values,
               double *MaxTime,double *MaxValue);

void FPASS fMin(int Num,double *Times,double *Values,
               double *MinTime,double *MinValue);
void DFPASS dfMin(int Num,Descriptor *Times,Descriptor *Values,
                  double *MinTime,double *MinValue);
//void DFPASS dfMin(int Num,double *Times,double *Values,
//               double *MinTime,double *MinValue);
void CPASS Min(int Num,double *Times,double *Values,
               double *MinTime,double *MinValue);

double FPASS fAvg(int Num,double *Times,double *Values,
                 double Start,double End);
double DFPASS dfAvg(int Num,Descriptor *Times,Descriptor *Values,
                    double Start,double End);
//double DFPASS dfAvg(int Num,double *Times,double *Values,
//                 double Start,double End);
double CPASS Avg(int Num,double *Times,double *Values,
                 double Start,double End);

void FPASS fRunAvg(int Num,double *Times,double *Values,
               double Period,
               int *NumRes,double *TimesRes,double *ValuesRes);
void DFPASS dfRunAvg(int Num,Descriptor *Times,Descriptor *Values,
                     double Period,
                     int *NumRes,Descriptor *TimesRes,Descriptor *ValuesRes);
//void DFPASS dfRunAvg(int Num,double *Times,double *Values,
//                     double Period,
//                     int *NumRes,double *TimesRes,double *ValuesRes);
void CPASS RunAvg(int Num,double *Times,double *Values,
               double Period,
               int *NumRes,double *TimesRes,double *ValuesRes);

double FPASS fInterp(int Num,double *Times,double *Values,
                 double Time);
double DFPASS dfInterp(int Num,Descriptor *Times,Descriptor *Values,
                       double Time);
//double DFPASS dfInterp(int Num,double *Times,double *Values,
//               double Time);
double CPASS Interp(int Num,double *Times,double *Values,
                 double Time);

#ifdef __cplusplus
}
#endif
