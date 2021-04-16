#include "p2met.h"
#include "robust.h"
#include "newinfo.h"

char *fname(char *path)
  {
    char *p=&path[strlen(path)];
    while (p>path && *p!='\\')
      p--;
    if (*p=='\\') p++;   
    return p;
  }

int makemet(GIDFILE *pf,METFILE *m,int Site,int Air,char *runname)
  {
    int i;
    /* Make group 1 */
    strcpy(m->m1.sname,infol(pf,"AirAJSTATNM",Site,Air));
    /* Make group 2 */
    m->m2.hmstab= ratof(infol(pf,"AirACMIXAM",Site,Air));
    m->m2.hmunst= ratof(infol(pf,"AirACMIXPM",Site,Air));
    m->m2.dprecip=ratof(infol(pf,"AirACPRENUM",Site,Air));
    m->m2.tsnum=  ratof(infol(pf,"AirACNUMTS",Site,Air));
    m->m2.annrain=ratof(infol(pf,"AirACRAIN",Site,Air));
    /* Make group 3 */
    for (i=0;i<6;i++)
      m->m3.u[i]=ratof(infol(pf,"AirAJWINDS",Site,Air,i+1,1));
    /* Make group 4 */
    m->m4.anhgt=ratof(infol(pf,"AirAJANEMHT",Site,Air));
    m->m4.zos=ratof(infol(pf,"AirAJRLEN",Site,Air));
    /* Make group 5 */
    sprintf(m->m5.file,"     JFDATFILE=%s",fname(runname));
    return 5;
  }

