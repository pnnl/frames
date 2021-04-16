#include "met2.h"

METFILE *openmet(char *s,char *mo)
  {
    METFILE *m;
    m=(METFILE *)getmem(sizeof(METFILE));
    if (m==NULL) return NULL;
    m->f=fopen(s,mo);
    if (m->f==NULL)
      {
        putmem(m);
        return NULL;
      }
    return m;
  }

int readmet(METFILE *m)
  {
    char c,line[100];
    rewind(m->f);
    /* Read Group 1 */
    fscanf(m->f,"%[^\n]%c",m->m1.sname,&c);
    /* Read Group 2 */
    fgets(line,81,m->f);
    sscanf(line,"%f %f %f %f %f",
      m->m2.hmstab,
      m->m2.hmunst,
      m->m2.dprecip,
      m->m2.tsnum,
      m->m2.annrain);
    /* Read Group 3 */
    fgets(line,81,m->f);
    sscanf(line,"%f %f %f %f %f",
      m->m3.u[0],
      m->m3.u[1],
      m->m3.u[2],
      m->m3.u[3],
      m->m3.u[4],
      m->m3.u[5]);
    /* Read Group 4 */
    fgets(line,81,m->f);
    sscanf(line,"%f %f",m->m4.anhgt,m->m4.zos);
    /* Read Group 5 */
    fscanf(m->f,"%[^\n]%c",m->m5.file,&c);
    return 5;
  }

int writemet(METFILE *m)
  {
    rewind(m->f);
    /* Write Group 1 */
    fprintf(m->f,"%s\n",m->m1.sname);
    /* Write Group 2 */
    fprintf(m->f," %#9.3G %#9.3G %#9.3G %#9.3G %#9.3G\n",
      m->m2.hmstab,
      m->m2.hmunst,
      m->m2.dprecip,
      m->m2.tsnum,
      m->m2.annrain);
    /* Write Group 3 */
    fprintf(m->f," %#9.3G %#9.3G %#9.3G %#9.3G %#9.3G %#9.3G\n",
      m->m3.u[0],
      m->m3.u[1],
      m->m3.u[2],
      m->m3.u[3],
      m->m3.u[4],
      m->m3.u[5]);
    /* Write Group 4 */
    fprintf(m->f," %#4.3G %#4.3G\n",
      m->m4.anhgt,
      m->m4.zos);
    /* Write Group 5 */
    fprintf(m->f,"%s\n",m->m5.file);
    return 5;
  }

void closemet(METFILE *m)
  {
    fclose(m->f);
    putmem(m);
  }

void namemet(METFILE *m,char *s)
  {
    fclose(m->f);
    m->f=fopen(s,"r+");
  }
