/*========================================================================
  º                    DATE         TIME     º Written by Mitch Pelton   º
  º Written      : August,1996               º For Pacific Northwest     º
  º Received     :                           º     National Laboratories º
  º                                          º     Battelle Corporation  º
  ========================================================================*/
#ifndef DFL_H
#define DFL_H

#include "gid.h"
#include "robust.h"

typedef struct drec
{
  int iwdat;
  float val[5];
  char text[40];
}dflrec;

void Make_DFL(GIDFILE *pf, char *runname);

#endif
