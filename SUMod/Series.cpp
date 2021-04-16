#include "hwirst.h"
#include "series.h"
#include <string.h>

//------------------------------------------------------------------------------
Series::Series(int n)
{
  int i;
  label = strdup("");
  this->value=new double[n];
  this->time=new double[n];
  for (i=0;i<n;i++)
  {
    this->value[i]=(double)i;
    this->time[i]=(double)i;
  }
  this->num=n;
}
//------------------------------------------------------------------------------
Series::Series(int n, char *lbl)
{
  int i;
  label = strdup(lbl);
  this->value=new double[n];
  this->time=new double[n];
  for (i=0;i<n;i++)
  {
    this->value[i]=(double)i;
    this->time[i]=(double)i;
  }
  this->num=n;
}
//------------------------------------------------------------------------------
Series::~Series()
{
  delete[] this->value;
  delete[] this->time;
}
//------------------------------------------------------------------------------
void Series::Add(int n, double t, double v)
{
  this->time[n]=t;
  this->value[n]=v;
}
//------------------------------------------------------------------------------
void Series::SetNum(int n)
{
  this->num=n;
}
//------------------------------------------------------------------------------
void Series::Peak(double *year, double *val)
{ // HWIRST
  Max(num,time,value,year,val);
  return;
}
//------------------------------------------------------------------------------
double Series::ValueAt(double year)
{
  // HWIRST
  return Interp(num,time,value,year);
}
//------------------------------------------------------------------------------
double Series::Average(double year1, double year2)
{
  // HWIRST
  return Avg(num,time,value,year1,year2);
}





