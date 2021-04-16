#include "jfd2.h"

#include "memhand.h"
#include <math.h>
#include <string.h>
#include <stdlib.h>

JFDFILE *openjfd(char *s,char *m)
  {
    JFDFILE *j;
    j=(JFDFILE *)getmem(sizeof(JFDFILE));
    j->f=fopen(s,m);
    if (j->f==NULL)
      {
	putmem(j);
	return NULL;
      }
    return j;
  }

void closejfd(JFDFILE *j)
  {
    fclose(j->f);
    putmem(j);
  }

int readjfd(JFDFILE *jf)
  {
    int i,j,k,w;
    char temp[256],num[20],*ptr;
    rewind(jf->f);
    /* Read Group 1 */
    fgets(temp,256,jf->f);
    strncpy(num,temp,5);
    ptr=strchr(temp,'F');
    if (ptr==NULL)
      ptr=strchr(temp,'f');
    ptr++;
    w=atoi(ptr);
    jf->j1.wil=atoi(num);
    /* Read Group 2 */
    if (jf->j1.wil==16)
      {
	      strcpy(jf->j1.format,"(16F6.3)");
	      for (i=0;i<7;i++)
	        for (j=0;j<6;j++)
            {
              fgets(temp,256,jf->f);
	            for (k=15;k>=0;k--)
                {
		              jf->j2.rf[i][j][k]=atof(&temp[k*w]);
                  temp[k*w]='\0';
                }
            }
      }
    else
      {
	      strcpy(jf->j1.format,"(6F10.3)");
	      for (i=0;i<7;i++)
	        for (j=0;j<16;j++)
            {
              fgets(temp,256,jf->f);
	            for (k=5;k>=0;k--)
	              {
		              jf->j2.rf[i][k][j]=atof(&temp[k*w]);
                  temp[k*w]='\0';
	              }
            }
      }
    return 2;
  }

int writejfd(JFDFILE *jf)
  {
    int i,j,k;
    rewind(jf->f);
    /* Write Group 1 */
    fprintf(jf->f,"%5i%-30s\n",jf->j1.wil,jf->j1.format);
    /* Write Group 2 */
    if (jf->j1.wil==16)
      {
	      for (i=0;i<7;i++)
	        for (j=0;j<6;j++)
	          {
	            for (k=0;k<16;k++)
		            fprintf(jf->f,"%#6.3F",jf->j2.rf[i][j][k]);
	            fprintf(jf->f,"\n");
	          }
        for (i=0;i<6;i++)
          fprintf(jf->f,"%#6.3f",jf->j3.calms[i]);
        fprintf(jf->f,"\n%#6.3f\n",jf->j3.calms[i]);
      }
    else
      {
	      for (i=0;i<7;i++)
	        for (j=0;j<16;j++)
	          {
	            for (k=0;k<6;k++)
		            fprintf(jf->f,"%#10.3G",jf->j2.rf[i][k][j]);
	            fprintf(jf->f,"\n");
	          }
        for (i=0;i<6;i++)
          fprintf(jf->f,"%#10.3G",jf->j3.calms[i]);
        fprintf(jf->f,"\n%#10.3G\n",jf->j3.calms[i]);
      }
    return 2;
  }
