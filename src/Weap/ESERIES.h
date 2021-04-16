#ifndef ESERIESH
#define ESERIESH

#include "cseries.h"

class EcoSeries
{
 public:
  int NumEco;
  char vary[LARGESTRING];
  char uncr[LARGESTRING];

  ConSeries *Eco;
  EcoSeries();
  ~EcoSeries();
  void InitConc(int numloc);
  int ReadConcBBF(GIDFILE *G, char *ConFile);
  int ReadConcWCF(GIDFILE *G, char *ConFile, char *modname);
  int ReadConcSCF(GIDFILE *G, char *ConFile, char *modname);
  int ReadIntakeTWI(GIDFILE *f);
  void Combine(ConSeries *CS, Series *Frequency);
};



#endif
