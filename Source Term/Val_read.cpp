/************************************************************************************************************
 * Source Term Release Module              MODULE:  VAL_READ.CPP                              VERSION: 1.00 *
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
 *   Last Modified:  10/25/96  -- KDS                                                                       *
 ************************************************************************************************************
 *                                       MODULE:  VAL_READ.CPP                                              *
 *  VAL_READ is the module that contains the read statements called from other parts fo the module.  These  *
 *  functions read values from the parameter file for use in the module.                                    *
 ************************************************************************************************************
 *       MODULE ORGANIZATION                                                                                *
 *  Routines:  ParamFile::f_read(), ParamFile::i_read(), ParamFile::s_read(), Param_find_err(), remap()     *
 ************************************************************************************************************
 *       MODIFICATION HISTORY                                                                               *
 *		DATE	WHO			DESCRIPTION                                                                         *
 *	--------	---	----------------------------------------------------------------------------------------  *
 *	06/01/96	KDS	Conversion of STC to STRM in preparation of application of MEPAS QA program and           *
 *						procedures                                                                                *
 *	06/10/96	KDS	Changed read_f, read_i, and read_s subroutines to incorporate them at ParmaFile class     *
 *                subroutines, to make data access consistant.                                              *
 *	06/11/96	KDS	Added remap() so upper level structure would not change with implementation in the        *
 *                FRAMEWORK mode.                                                                           *
 *	10/25/96	KDS	Restructure *_read subroutines to incorporate the gid file loading and accessing          *
 *						methodology developed by MA Pelton.  Adds significant speed to code.  Also changed            *
 *						compile time #if configurations so there is only one version of each subroutine.              *
 *  10/02/03  BLH Added f_read_cl to return default values in the case where constituent properties do not  *
 *            exist in the database                                                                         *
 ************************************************************************************************************
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <dos.h>

#include "strm1.h"
#include "fcsv.h"
#include "fileprep.h"
#include "gid.h"

//===========================================================================================================
//	PROTOTYPES  ==============================================================================================
//===========================================================================================================
void param_find_err(char *s,int c1,int c2,int c3,int c4,int c5,int c6);
void WriteWarning(char *s);

#ifdef FRAME
//===========================================================================================================
//	SUBROUTINE remap  ========================================================================================
//	This subroutine remaps the call flags from the .prm style flags to .gid style.
//===========================================================================================================

int *remap(int c1, int c2, int c5, int c6)
	{
	int a;
	static int new_ind[7];
	for (a=0; a<7; a++) new_ind[a]=0;

	new_ind[0]=site.idx;
	new_ind[2]=c6;

	if ( c5 )	// Monthly Met Data
		{
		new_ind[1]=c5;
		return(new_ind);
		}
	else if ( c2 )
			if ( c1 )
				{
				new_ind[2]=c6;
				new_ind[3]=c2;
				new_ind[1]=c1;
				}
			else
				new_ind[1]=c2;

	else new_ind[1]=c1;

	if ( c6 && c1!=0 ) new_ind[2]=c6;

	return(new_ind);
	}

#endif

//===========================================================================================================
//	SUBROUTINE f_read  =======================================================================================
//	This subroutine reads and returns a float value for s from the parameter file, ParamFile.
//===========================================================================================================
float ParamFile::f_read(char *s,int c1,int c2,int c3,int c4,int c5,int c6)
	{
	char *ptr;
	float temp;
#ifdef FRAME       // Version for FRAMEWORK
	int *call_par;
	call_par=remap(c1,c2,c5,c6);
//	ptr = Info(s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
	ptr = info(g, s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
	if(ptr == NULL)
		param_find_err(s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
#else
	ptr = Info(s, c1, c2, c3, c4, c5, c6);
	if(ptr == NULL)
		param_find_err(s, c1, c2, c3, c4, c5, c6);
#endif
	temp = (float)atof(ptr);
	return(temp);
	}

float ParamFile::f_read_cl(char *s,int c1,int c2,int c3,int c4,int c5,int c6,float def)
{
	char *ptr;
	float temp;
	int *call_par;
	call_par=remap(c1,c2,c5,c6);
	ptr = info(g, s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
  if (ptr==NULL)
  {
    char buf[MAXPATH];
    sprintf(buf,"Missing value for %s (%d,%d,%d,%d,%d,%d) set to %f",
      s,call_par[0],call_par[1],call_par[2],call_par[3],call_par[4],call_par[5],def);
    WriteWarning(buf);
    temp = def;
  }
  else
	  temp = (float)atof(ptr);
	return(temp);
}

//===========================================================================================================
//	SUBROUTINE i_read  =======================================================================================
//	This subroutine reads and returns an integer value for s from the parameter file, ParamFile.
//===========================================================================================================
int ParamFile::i_read(char *s,int c1,int c2,int c3,int c4,int c5,int c6)
	{
	char *ptr;
	int temp;
#ifdef FRAME   // FRAMEWORK Version
	int *call_par;
	call_par=remap(c1,c2,c5,c6);
//	ptr = Info(s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
	ptr = info(g, s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
	if(ptr == NULL)
		param_find_err(s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
#else
	ptr = Info(s, c1, c2, c3, c4, c5, c6);
	if(ptr == NULL)
		param_find_err(s, c1, c2, c3, c4, c5, c6);
#endif
	temp = atoi(ptr);
	return(temp);
	}

//===========================================================================================================
//	SUBROUTINE s_read  =======================================================================================
//	This subroutine reads and returns a string value for s from the parameter file, ParamFile.
//===========================================================================================================
int ParamFile::s_read(char *buf,int size, char *s,int c1,int c2,int c3,int c4,int c5,int c6)
	{
	char *ptr;
#ifdef FRAME
	int *call_par;
	call_par=remap(c1,c2,c5,c6);
//	ptr = Info(s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
	ptr = info(g, s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
	if(ptr == NULL)
		param_find_err(s, call_par[0], call_par[1], call_par[2], call_par[3], call_par[4], call_par[5]);
#else
	ptr = Info(s, c1, c2, c3, c4, c5, c6) ;
	if(ptr == NULL)
		param_find_err(s, c1, c2, c3, c4, c5, c6);
#endif
	if( strlen(ptr) > size )
		ptr[size-1] = '\0';  // Place null in the last place to prevent overwrite
	strcpy(buf, ptr);
	return(strlen(ptr));
	}

//===========================================================================================================
//	SUBROUTINE param_find_err  ===============================================================================
//	This subroutine returns the error message to the screen and the error file when a parameter is not found
//===========================================================================================================
void param_find_err(char *s,int c1,int c2,int c3,int c4,int c5,int c6)
	{
	fprintf(stderr,"\nError reading %s  (%d, %d, %d, %d, %d, %d)\n", s, c1, c2, c3, c4, c5, c6);
	err.writeln();
	err.write("Error reading ");
	err.write(s);
	err.write("(");
	err.write(c1);
	err.write(c2);
	err.write(c3);
	err.write(c4);
	err.write(c5);
	err.write(c6);
	err.write(")");
	err.writeln();
	go_die(3,"dumm");
	return;
	}


