/************************************************************************************************************
 * Source Term Release Module              MODULE:  PARAM3.CPP                                VERSION: 1.00 *
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
 *         Created:                                                                                         *
 *   Last Modified:  07/01/96  -- KDS                                                                       *
 *                                                                                                          *
 ************************************************************************************************************
 *                                         MODULE:  PARAM3.CPP                                              *
 *  PARAM2 was prepared by Karl J. Castleton to create the underlying structure to make access of the       *
 *  parameter file simpler.                                                                                 *
 ************************************************************************************************************
 *       MODULE ORGANIZATION                                                                                *
 *  Routine:  Param::Read(), Param::operator ==(), ParamFile::Get(), ParamFile::Open(), ParamFile::Close(), *
 *            ParamFile::Get(), ParamFile::Entries(), *ParamFile::Info()                                    *
 ************************************************************************************************************
 *       MODIFICATION HISTORY                                                                               *
 *		DATE	WHO			DESCRIPTION                                                                         *
 *	--------	---	----------------------------------------------------------------------------------------  *
 *	07/01/96	KDS	Conversion of STC to STRM in preparation of application of MEPAS QA program and           *
 *						procedures                                                                                *
 ************************************************************************************************************
 */

#include <string.h>
#include <stdlib.h>

#include "param3.h"
#include "fcsv.h"

#ifndef FRAME
int Param::Read(fcsv &f)
	{
	int j;

	if (f.eof()) return 0;

	f.read(Name);			// Read in parameter name
	for( j=0; j<6; j++)
		f.read(&Count[j]);	// Read in the six counters
	f.read(&Source);
	f.read(&Ref);
	f.read(&Status);
	f.read(Units);
	f.read(String);
	return 1;
	}
#endif

#ifdef FRAME
int Param::Read(fcsv &f)
	{
	int j;

	if (f.eof()) return 0;

	f.read(Name);			// Read in parameter name
	for( j=0; j<6; j++)
		f.read(&Count[j]);	// Read in the six counters
	f.read(&Ref);
	f.read(UseUnit);
	f.read(Units);
	f.read(String);
	return 1;
	}
#endif

int Param::operator ==(Param p)
	{
	float testval = !strcmpi(p.Name,Name);

	if ((testval) && p.Count[0]==Count[0] && p.Count[1]==Count[1] &&
		p.Count[2]==Count[2] && p.Count[3]==Count[3] && p.Count[4]==Count[4] &&
		p.Count[5]==Count[5])
		return 1;
	else
		return 0;
  }

Param temp;

int  ParamFile::Get(Param &p)
	{
	int found,neof;
	f.rewind();
//	fseek(f,0l,SEEK_SET);
	found=0;
	do
		{
		neof=temp.Read(f);
		if (neof)
		if (p==temp) found=1;  // p.operator ==(temp)
		f.readln();
		}
	while (!found && neof);
	if (found)
		{
		p=temp;
		return 1;
		}
	else
		{
		return 0;
		}
	}

int ParamFile::Open(char *s,char delim)
	{
	return(f.open(s,READ,delim));
//     return;
	}

void ParamFile::Close()
	{
	f.close();
	}

int  ParamFile::Get(int n,Param &p)
	{
	Param temp;
	f.rewind();
//	fseek(f,0l,SEEK_SET);
	while (temp.Read(f) && n>0)
		n--;
	if (n==0)
		{
		p=temp;
		return 1;
		}
	else
		return 0;
	}

int  ParamFile::Entries()
	{
//	char line[256];
	int num;
	f.rewind();
//	fseek(f,0l,SEEK_SET);
	num=0;
	while (!f.eof())
		{
		f.readln();
//		fgets(line,256,f);
		num++;
		}
	return num;
	}

Param RetVal;

char *ParamFile::Info(char *s,int c1,int c2,int c3,int c4,int c5,int c6)
	{
	strcpy(RetVal.Name,s);
	strcpy(RetVal.String,"");
	RetVal.Count[0]=c1;
	RetVal.Count[1]=c2;
	RetVal.Count[2]=c3;
	RetVal.Count[3]=c4;
	RetVal.Count[4]=c5;
	RetVal.Count[5]=c6;
	if (Get(RetVal))
		return RetVal.String;
	else
		return NULL;
	}
