#include "p2top.h"
#include "top2.h"
#include "robust.h"
#include "newinfo.h"
#include <string.h>

void maketop(GIDFILE *PF,TOPFILE *tf,int Site,int Air)
  {
    int i,j,k;
    float topbas;

    tf->top=!rstrcmpi(infol(PF,"AirARTOPTYP",Site,Air),"true");
    tf->chanl=!rstrcmpi(infol(PF,"AirARCHANL",Site,Air),"true");
    if (tf->top==1 && tf->chanl==1)
      {
        i= tconv(17);
        for (j=0;j<16;j++)
          tf->t[i][j]=0.0;
        tf->t[i][0]=6.0;
        tf->t[i][1]=ratof(infol(PF,"AirARCWIDTH",Site,Air));
        tf->t[i][2]=ratof(infol(PF,"AirARCLEN",Site,Air));
        tf->t[i][3]=ratof(infol(PF,"AirARCHGT",Site,Air));
        tf->t[i][4]=ratof(infol(PF,"AirARCDIR",Site,Air));
        tf->t[i][5]=ratof(infol(PF,"AirARCSLOPE",Site,Air));
      }
    if (tf->top)
      {
        i= tconv(20);
        for (j=2;j<16;j++)
          tf->t[i][j]=0.0;
        topbas=ratof(infol(PF,"AirARTOPBAS",Site,Air));
        tf->t[i][0]=topbas;
        tf->t[i][1]=1.0;

        i= tconv(21);
        for (j=2;j<16;j++)
          tf->t[i][j]=topbas;

        for (k=0;k<4;k++)
          {
            i= tconv(22+k);
            for (j=0;j<16;j++)
              tf->t[i][j]=ratof(infol(PF,"AIRartophts",Site,Air,k+1,j+1));
          }

        i= tconv(26);
        for (j=0;j<16;j++)
          tf->t[i][j]=ratof(infol(PF,"AIRartophts",Site,Air,4,j+1));
      }

    for (k=0;k<4;k++)
      {
        i= tconv(62+k);
        for (j=0;j<8;j++)
          {
            tf->t[i][j*2]=ratof(infol(PF,"AirARregSUR",Site,Air,k+1,j+1));
            tf->t[i][j*2+1]=0.0;
          }
      }
    i= tconv(99);
    for (j=0;j<16;j++)
      tf->t[i][j]=0.0;
  }
