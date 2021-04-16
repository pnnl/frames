#ifndef WCFPROC_H
#define WCFPROC_H
#include "series.h"
class wcfspec
{
  public:
  int numloc;
  wcfspec();
  ~wcfspec();
  void readloc(icsv *inf, char *Contam, Series **series);
  int read(char *fuiname, char *Source, char *Contam, Series **series);
};
#endif

