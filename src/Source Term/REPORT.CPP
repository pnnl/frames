/************************************************************************************************************
 * Source Term Release Module                   MODULE:  REPORT.CPP                           VERSION: 1.00 *
 *          Copyright 1996 by Battelle Pacific Northwest National Laboratory.  All Rights Reserved          *
 ************************************************************************************************************
 *******                                     PROGRAM:  STRM                                           *******
 *******  This code incorporates and expands upon the STC Source Term Mass Balance Code developed     *******
 *******  and written by James L. Stroh for RAAS Ver 1 PNL 1995                                       *******
 *******                                                                                              *******
 *                                                                                                          *
 *      Written by:  Keith D. Shields                                                                       *
 *                   Pacific Northwest National Laboratory                                                  *
 *                   P.O. Box 999                                                                           *
 *                   Richland, WA  99352                                                                    *
 *                                                                                                          *
 *         Created:  06/01/96                                                                               *
 *   Last Modified:  06/10/96 -- KDS                                                                        *
 ************************************************************************************************************
 *                                         MODULE:  REPORT.CPP                                              *
 * REPORT was prepared by Karl J. Castleton to create a reporting year array to give better resolution to   *
 * times when technologies or series parameter values change.                                               *
 ************************************************************************************************************
 *       MODULE ORGANIZATION                                                                                *
 * Routine:	Report::Clear(), Report::TagTime(), Report::TruncTime(), ShouldReport(), DoAVariable(),         *
 *				InitReport()                                                                                    *
 ************************************************************************************************************
 *       MODIFICATION HISTORY                                                                               *
 *		DATE	WHO			DESCRIPTION                                                                         *
 *	--------	---	----------------------------------------------------------------------------------------  *
 *	06/10/96	KDS	Changed read_f, read_i, and read_s subroutine calls to ParamFile::f_read,                 *
 *						ParamFile::i_read subroutines, to make data access consistant.                            *
 *	10/28/96	KDS	Changed InitReport() call to add loop to evaluate Kd changes for all contaminants, also   *
 *						added additional parameters to be evaluated in that subroutine.(AIRSPC, TOTPOR)           *
 *	11/1/96	KDS	Added Init_Act_Report() to establish reporting times for active (known flux) sources.     *
 ************************************************************************************************************
 */

#include "strm1.h"
#include "report.h"

class Report
  {
    public:
    char Data[MAX_REPORT_TIME];
    void Clear();
    void TagTime(int t);
    void TruncTime();
  };

static Report ReportData;

void Report::Clear()
  {
    int i;
    for (i=0;i<MAX_REPORT_TIME;i++)
	 Data[i]=0;
  }

void Report::TagTime(int t)
  {
    int s,c,i,v;
    s=1;
    c=0;
    do
	 {
	   v=0;
	   i=t;
	   while (i<t+s && v==0)
		{
		  if (Data[i]) v=1;
		  i++;
		}
	   if (v==0)
		Data[t]=1;
	   t+=s;
	   c++;
	   if (c==9)
		{
		  s*=10;
		  c=0;
		}
	 }
    while (t<MAX_REPORT_TIME);
  }

void Report::TruncTime()
  {
    int i,c,last;
    c=0;
    last=1;
    for (i=0;i<MAX_REPORT_TIME;i++)
	 {
	   if (c<100 && Data[i]>0)
		{
		  c++;
		  last=i;
		}
	   else
		Data[i]=0;
	 }
    if (c==100) Data[last]=3;
  }

int ShouldReport(float t)
  {
    return (int)ReportData.Data[(int)t];
  }

void DoAVariable(ParamFile &p,char *var,int c)
  {
    int i,n,t;
    char var1[30],var2[30];
    sprintf(var1,"ST%s_NUM",var);
    sprintf(var2,"ST%s_TIM",var);
	 n=p.i_read(var1,c,0,0,0,0,0);
    if (n>=2)
	 for (i=0;i<n-1;i++)
	   {
		t=p.i_read(var2,c,i+1,0,0,0,0);
		ReportData.TagTime(t-1);
	   }
  }

void InitReport(ParamFile &p, int numcontam)    // Call Modified 10/28/96
  {
	 int i, max;
    ReportData.Clear();
    ReportData.Data[0]=1;
    ReportData.TagTime(1);
//	 DoAVariable(p,"GLASS",0);
//	 DoAVariable(p,"SURFVOL",0);
//	 DoAVariable(p,"AIRSPC",0);     // Added 10/28/96
//	 DoAVariable(p,"TOTPOR",0);     // Added 10/28/96
	  DoAVariable(p,"WIND",0);
	  DoAVariable(p,"WATR",0);
    DoAVariable(p,"DARCY",0);
	  for(i=1;i<=numcontam;i++)
      DoAVariable(p,"KD",i);    // Modified 10/28/96

	  max=p.f_read("STMAXTIME",0,0,0,0,0,0);
    if (max>=MAX_REPORT_TIME)
	    max=MAX_REPORT_TIME-1;
    ReportData.Data[max]=2;
    ReportData.TruncTime();
  }

void Init_Act_Report(ParamFile &p, int numcontam)    // Subroutine Added 11/1/96
  {
	 int i, max;
    ReportData.Clear();
    ReportData.Data[0]=1;
    ReportData.TagTime(1);
	 for(i=0;i<numcontam;i++)
		{
		DoAVariable(p,"LEACH",i);
		DoAVariable(p,"OVL",i);
		DoAVariable(p,"SUSP",i);
		DoAVariable(p,"VOLAT",i);
		DoAVariable(p,"SOURCE",i);
		}
	 max=p.f_read("STMAXTIME",0,0,0,0,0,0);
    if (max>=MAX_REPORT_TIME)
	 max=MAX_REPORT_TIME-1;
    ReportData.Data[max]=2;
    ReportData.TruncTime();
  }
