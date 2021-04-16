#include "p2jfd.h"
#include "newinfo.h"
#include "robust.h"

void makejfd(GIDFILE *pf,JFDFILE *jf,int Site,int Air)
  {
    int i,j,k;

    jf->j1.wil=6;
    strcpy(jf->j1.format,"(6F10.3)");
    for (i=0;i<7;i++)
      for (j=0;j<6;j++)
        for (k=0;k<16;k++)
          jf->j2.rf[i][j][k]=ratof(infol(pf,"Airajfdata",Site,Air,i+1,k+1,j+1));
    for (i=0;i<7;i++)
      jf->j3.calms[i]=ratof(infol(pf,"AIRajcalms",Site,Air,i+1,1));
  }

