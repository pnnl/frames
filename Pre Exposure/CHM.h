/*========================================================================
  �                    DATE         TIME     � Written by Mitch Pelton   �
  � Written      : August,1996               � For Pacific Northwest     �
  � Received     :                           �     National Laboratories �
  �                                          �     Battelle Corporation  �
  ========================================================================*/
#ifndef __CHM_H
#define __CHM_H

#include "gid.h"
#include "robust.h"

extern fcsv err;
extern fcsv wrn;
void Make_CHM(GIDFILE *gid,char *run,int SIdx);

#endif
