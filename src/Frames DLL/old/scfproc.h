#ifndef SCFPROC_H
#define SCFPROC_H
#include "series.h"
class wcfspec
{
  public:
  int numloc;
  scfspec();
  ~scfspec();
  void readloc(icsv *inf, char *Contam, Series **series);
  int read(char *fuiname, char *Source, char *Contam, Series **series);
};
#endif
