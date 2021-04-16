/*========================================================================
  �                    DATE         TIME     � Written by Mitch Pelton   �
  � Written      : August,1996               � For Pacific Northwest     �
  � Received     :                           �     National Laboratories �
  �                                          �     Battelle Corporation  �
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
