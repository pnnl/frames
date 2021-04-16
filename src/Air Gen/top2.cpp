#include "top2.h"
#include "memhand.h"
#include <stdlib.h>
#include <math.h>

int tconv(int v)
  {
    switch(v)
      {
        case 17:return 0;
        case 20:return 1;
        case 21:return 2;
        case 22:return 3;
        case 23:return 4;
        case 24:return 5;
        case 25:return 6;
        case 26:return 7;
        case 62:return 8;
        case 63:return 9;
        case 64:return 10;
        case 65:return 11;
        default:return 12;
      }
  }

TOPFILE *opentop(char *fn,char *m)
  {
    TOPFILE *tf;
    tf=(TOPFILE *)getmem(sizeof(TOPFILE));
    if (tf==NULL) return NULL;
    tf->f=fopen(fn,m);
    if (tf->f==NULL)
      {
        putmem(tf);
        return NULL;
      }
    return tf;
  }

void closetop(TOPFILE *tf)
  {
    fclose(tf->f);
    putmem(tf);
  }

void readline(TOPFILE *tf,int *code,float *values)
  {
    char line[200];
    int p,c;

    fgets(line,200,tf->f);
    *code=atoi(line);
    p=0;
    c=0;
    while (line[p]>='0' && line[p]<='9') p++;
    while ((line[p]==' ' || line[p]==',') && line[p]!='\0') p++;
    do
      {
        values[c]=atof(&line[p]);
        while (line[p]!=',')
          p++;
        while ((line[p]==' ' || line[p]==',') && line[p]!='\0')
          p++;
        c++;
      }
    while (c<16 && p<200 && line[p]!='\0');
    for (;c<16;c++)
      values[c]=0.0;
  }

void readtop(TOPFILE *tf)
  {
    int code,i;
    float temp[16];

    tf->top=0;
    tf->chanl=0;
    fseek(tf->f,0,SEEK_SET);
    do
      {
        readline(tf,&code,temp);
        if (code==17)
          {
            tf->chanl=1;
            tf->top=1;
          }
        if (code>=20 && code <=26)
          {
            tf->top=1;
          }
        for (i=0;i<16;i++)
          tf->t[tconv(code)][i]=temp[i];
      }
    while (code!=99);
  }

void printline(TOPFILE *tf,int code)
  {
    int i,p;
    fprintf(tf->f,"%d",code);
    p=tconv(code);
    for (i=0;i<16;i++)
      fprintf(tf->f,",%f",tf->t[p][i]);
    fprintf(tf->f,"\n");
  }

void writetop(TOPFILE *tf)
  {
    fseek(tf->f,0,SEEK_SET);
    if (tf->top==1 && tf->chanl==1)
      printline(tf,17);
    if (tf->top==1)
      {
        printline(tf,20);
        printline(tf,21);
        printline(tf,22);
        printline(tf,23);
        printline(tf,24);
        printline(tf,25);
        printline(tf,26);
      }
    printline(tf,62);
    printline(tf,63);
    printline(tf,64);
    printline(tf,65);
    printline(tf,99);
  }
