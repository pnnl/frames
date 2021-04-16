/*========================================================================
  º                    DATE         TIME     º Written by Mitch Pelton   º
  º Written      : August,1996               º For Pacific Northwest     º
  º Received     :                           º     National Laboratories º
  º                                          º     Battelle Corporation  º
  ========================================================================*/
#ifndef __CHM_H
#define __CHM_H

#include "gid.h"
#include "robust.h"

extern fcsv err;
extern fcsv wrn;
void Make_CHM(GIDFILE *gid,char *run,int SIdx);

#endif
