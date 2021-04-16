#ifndef __PARAM_H
#define __PARAM_H
#include <stdio.h>

#include"fcsv.h"
#include"gid.h"

#define   NAME_LENGTH     32
#define   UNITS_LENGTH    32
#define   STRING_LENGTH   64

class Param
	{
	public:
	char Name[NAME_LENGTH];
	int  Count[6];
	char Source;
	int  Ref;
	char Status;
	char UseUnit[UNITS_LENGTH];
	char Units[UNITS_LENGTH];
	char String[STRING_LENGTH];
	int Read(fcsv &f);
	int operator ==(Param p);
	};

class ParamFile
	{
	fcsv f;
	public:
	GIDFILE *g;
	int  Get(Param &p);
	int  Open(char *s, char delim=',');
	void Close();
	int  Get(int n,Param &p);
	int  Entries();

	char *Info(char *s,int c1,int c2,int c3,int c4,int c5,int c6);

	float f_read_cl(char *s,int c1,int c2,int c3,int c4,int c5,int c6,float def);
	float f_read(char *s,int c1,int c2,int c3,int c4,int c5,int c6);
	int i_read(char *s,int c1,int c2,int c3,int c4,int c5,int c6) ;
	int s_read(char *buf,int size, char *s,int c1,int c2,int c3,int c4,int c5,int c6);


	float water_sum(unsigned int water_type);
	float *water_balance( );

	float Sediment_trans();
	float Wind_Susp(float *norm1, float *norm2, float *norm3);
	};

void mem_out();

#endif
