//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "sls.h"
#include "strm1.h"
#include "fileprep.h"
#include "recopy2.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

extern Remediate	init_dry_layer, darcy_inf, water_eros, wind_eros, surf_vol, glass_series,
						air_space, total_porosity, sed_load, over_flow;


SLS::SLS(fcsv &err, char *run)
    {
    char buf[MAXPATH];
    sprintf(buf,"%s.sls",run);
    if((fp.open(buf,WRITE))== 0)
    	{
    	fprintf(stderr,"Cannot open logfile file %s\n", buf);
    	err.write("Cannot open logfile file");
    	err.write(buf);
    	go_die(3,buf);
    	}
    file_head(fp, buf);
    fp.writeln();
    fp.writeln();
    }

SLS::~SLS()
    {
    fp.close();
    }

void SLS::analysis()
  {
  	if (glass_series.get_value(0.0) > 0.0)
  		{
  		fp.write("Analysis is for VITRIFIED case."); fp.writeln(); fp.writeln();
  		}
  	else if (surf_vol.get_value(0.0) > 0.0)
  		{
  		fp.write("Analysis is for GROUT case."); fp.writeln(); fp.writeln();
  		}
  	else
  		{
  		fp.write("Analysis is for BASELINE case."); fp.writeln(); fp.writeln();
  		}

  	fp.write("The starting value for the wind erosion rate is:");
  	fp.write(wind_eros.get_value(0.0)); fp.write("(cm/y)");
  	fp.writeln();
  	fp.writeln();
  	fp.write("The starting value for the surface water erosion rate is:");
  	fp.write(water_eros.get_value(0.0)); fp.write("(cm/y)");
  	fp.writeln();
  	fp.writeln();
  	fp.write("The starting value for the infiltration rate is:");
  	fp.write(darcy_inf.get_value(0.0)); fp.write("(cm/y)");
  	fp.writeln();
  	fp.writeln();
  }

void SLS::contaminants()
  {
  	fp.write("Contaminants evaluated for this run");
  	fp.writeln();
    fp.write("NUM"); fp.write("CAS_ID"); fp.write("PARENT_ID");
  	fp.write("NAME"); fp.write("NUM PROGENY");
  	fp.writeln();

  	for(int i=0; i < site.numtotcon; i++)
  		{
  		fp.write(i);
  		fp.write(props[i].cas_id);
  		fp.write(props[i].p_id);
  		fp.write(props[i].name);
  		fp.write(props[i].progeny);
  		fp.writeln();
  		}
  	fp.writeln();
  }

void SLS::all_gone(int contam, float time)
{
  fp.write("All the mass for contaminant:");
  fp.write(contam);
  fp.write("lost at time:");
  fp.write(time);
  fp.writeln();
  lst_time[contam] = 1; //counter so message is only logged once
}

void SLS::BlaneyCriddle()
{
  fp.write("NO ADJUSTMENT TO BLANEY DATA DONE SINCE F WAS <2.0");
}


void SLS::writeln(char *desc, float val)
{
  fp.write(desc);
  fp.write(val);
  fp.writeln();
  fp.flush();
}

void SLS::writeln(char *desc)
{
  fp.write(desc);
  fp.writeln();
  fp.flush();
}

void SLS::write(char *desc)
{
  fp.write(desc);
}

void SLS::writeln()
{
  fp.writeln();
  fp.flush();
}

void SLS::write(int i)
{
  fp.write(i);
}

void SLS::write(float r)
{
  fp.write(r);
}

