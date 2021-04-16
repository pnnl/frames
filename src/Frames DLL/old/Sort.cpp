#ifndef SORT_H
#define SORT_H

#include <math.h>

void sort(int n,int *idx,float *val,float *mean,float *min,float *max,float *median,float *sd)
  {
    float sum,sums;
    int i,j,temp;
    if (n==0) return;
    *min=val[0];
    *max=val[0];
    sum=val[0];
    sums=val[0]*val[0];
    for (i=1;i<n;i++)
      {
        if (val[i]<*min) *min=val[i];
        if (val[i]>*max) *max=val[i];
        sum+=val[i];
        sums+=val[i]*val[i];
      }
    *mean=sum/(float) n;
    if (n>2)
      *sd=sqrt((sums-sum*sum/(float)n)/((float)n-1.0));
    else
      *sd=-1.0;
    for (i=0;i<n-1;i++)
      for (j=i+1;j<n;j++)
        if (val[idx[i]]>=val[idx[j]])
          {
            temp=idx[i];
            idx[i]=idx[j];
            idx[j]=temp;
          }
    if (n % 2 !=0)
      *median=val[idx[n/2]];
    else
      *median=(val[idx[n/2-1]]+val[idx[n/2]])/2.0;
    return;
  }
#endif

