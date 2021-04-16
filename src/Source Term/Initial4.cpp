/************************************************************************************************************
 * Source Term Release Module               MODULE:  INITIAL2.CPP                             VERSION: 1.00 *
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
 *   Last Modified:  11/29/96 -- KDS                                                                        *
 ************************************************************************************************************
 *                                          MODULE:  INITIAL.CPP                                            *
 *  INITIAL is the module that handles the data for the series parameters for the Source Term Module.       *
 ************************************************************************************************************
 *       MODULE ORGANIZATION                                                                                *
 *  Routines: Series_param::nonzero(), Series_param::Series_param(), Series_param::~Series_param(),        *
 *          Series_param::get_value(), Series_param::get_series(), Pathway::init_param(),                *
 *          Remediate::init_param()                                                                      *
 ************************************************************************************************************
 *       MODIFICATION HISTORY                                                                               *
 *    DATE  WHO     DESCRIPTION                                                                         *
 *  --------  --- ---------------------------------------------------------------------------------------   *
 *  07/01/96  KDS Conversion of STC to STRM in preparation of application of MEPAS QA program and           *
 *            procedures                                                                                *
 * 11/29/96 KDS Changed procedure Series_param::get_series() for new initial index for framework, start   *
 *            at 1 instead of 0.                                                                        *
 ************************************************************************************************************
 */
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include<new.h>
#include"strm1.h"
#include"rk_path.h"
#include"fcsv.h"
#include"fileprep.h"
#include"param3.h"


//===========================================================================================================
//     SUBROUTINE FLUX_CONV     =============================================================================
//     Function to check and see if fluxes must be converted to masses from activities
//===========================================================================================================
float flux_conv(float flux, char *param, int cont_num)
{
float new_flux;
int conv_flag=0;

if(!strcmpi("STLEACH",param) )                //  Pathway parameter names evaluated for conversion
  conv_flag=1;                              //  only rads converted, others spec_act = 1.0
else if(!strcmpi("STOVL",param))
  conv_flag=1;
else if(!strcmpi("STSUSP",param) )
  conv_flag=1;
else if(!strcmpi("STVOLAT",param) )
  conv_flag=1;
else if(!strcmpi("STSOURCE",param) )
  conv_flag=1;

if (conv_flag)
  new_flux = flux/props[cont_num].spec_act;    // Mass = Act/SpecAct
else
  new_flux = flux;

return (new_flux);
}

contaminant::contaminant()  // constructor
{
    air_vp_diff=0.0;
    adj_conc=0.0;
    aq_conc=0.0;
    vapor_conc=0.0;
    k_g=0.0;
    k_l=0.0;
    k_i=0.0;
    retard=0.0;
    glass=0.0;
    grout=0.0;
    gdecay=0.0;
    init_mass=0.0;
    adj_org_conc=0.0;
    mole_fract=0.0;
    henrys_const=0.0;
    mass=0.0;
    min_mass=0.0;
}


//===========================================================================================================
//     SUBROUTINE SERIES_PARAM::NONZERO     =================================================================
//     Function to check and see if any value in series is non-zero
//===========================================================================================================
unsigned int Series_param::nonzero(void)
{
int i=0, temp=0;

do {
  if( flux[i] > 0.0 )
    temp = 1;
  i++;
  }while ( i < num_values );

return(temp);
}

//===========================================================================================================
//     SUBROUTINE SERIES_PARAM::SERIES_PARAM     ============================================================
//     Constructor for Series_param class data
//===========================================================================================================
Series_param::Series_param()
{
count=0;        //  added 4/98 BLH
num_values=0;   //  added 4/98 BLH
time = NULL;
flux = NULL;
}

//===========================================================================================================
//     SUBROUTINE SERIES_PARAM::~SERIES_PARAM     ===========================================================
//     Destructor for Series_param class data
//===========================================================================================================
Series_param::~Series_param()
{
if ( time != NULL )
  delete[] time;
if ( flux != NULL )
  delete[] flux;
}

//===========================================================================================================
//     SUBROUTINE SERIES_PARAM::GET_VALUE     ===============================================================
//     Function returns current value of parameter in question
//===========================================================================================================
float Series_param::get_value(float elapsed_time)
{
int old_count;
float lastValue;
float r;

old_count = count;
lastValue = value;

count = 0;
value = 0.0;

  while((num_values > count) &&  (elapsed_time >= time[count]))
  count++;

  if (num_values==0 && flux!=NULL)
    value=flux[0];
  else  /* interpolate */
    {
    if (count>0 && time[count]>=elapsed_time)
      {
      r = (elapsed_time - time[count-1])/(time[count]-time[count-1]);
      value = flux[count-1]  + (r * (flux[count] - flux[count-1]));
      }
    }

if( count != old_count || lastValue != value )
  changed = 1;    // Value has changed, flag used to trigger calculations requiring this value
              // Must be reset to zero by the user
return(value);
}

//===========================================================================================================
//     SUBROUTINE SERIES_PARAM::GET_SERIES     ==============================================================
//     Function reads in time series data
//===========================================================================================================
void Series_param::get_series(ParamFile &data,char *buf1,int contam,int c3,int c4,int c5,int c6)
{
char buf2[32];
int i;//,size;
float tmp_flx;

//  size=max(num_values,1);
flux = new float[MAX_FLUX_CT];
time = new float[MAX_FLUX_CT];

  if( num_values >= 1 )
  {
    rstrcpy(buf2, buf1);
    strncat(buf2, "_tim",6);

    for(i=0; i<num_values; i++)
    {
      tmp_flx = data.f_read(buf1,contam,i+1,c3,c4,c5,c6);   // Read in flux values from paramfile
/* 5/98 BLH
    flux_conv is only applied to STLEACH, STSUSP, STOVL, STVOLAT and STSOURCE which
    are defined for parents only --> replace c4 with contam-1
    flux[i] = flux_conv(tmp_flx, buf1, c4);
*/
/* 11/02 BLH
   the above may have been correct at one time but certainly isn't now...
   pathways series are initialized for all contams, including progeny, therefore
   c4 is now correct, contam-1 isn't...  this was uncovered when debugging
   source_sink problems.  No way of telling how long this has been an issue.
                flux[i] = flux_conv(tmp_flx, buf1, contam-1);
*/
    flux[i] = flux_conv(tmp_flx, buf1, c4);
    time[i] = data.f_read(buf2,contam,i+1,c3,c4,c5,c6);   //  Use i-1 since we know start time = zero
    }
  }

  changed = 1;  //  set changed to non-zero (true) such that first check will force a calculation
  num_values--; //  Decrement the num_values so updates will occur correctly
}

//===========================================================================================================
//     SUBROUTINE SERIES_PARAM::GET_COUNT
//     Function returns number of values
//===========================================================================================================
unsigned int Series_param::get_count()
{
  return num_values;
}

//===========================================================================================================
//     SUBROUTINE SERIES_PARAM::OUTPUT_SERIES
//     Function reports time series
//===========================================================================================================
void Series_param::output_series(fcsv *wff)
{
  int i; float area;

  area = (site.width/100.)*(site.length/100.);


  if (num_values==0)
  {
    wff->write(num_values+2);
    wff->writeln();
    wff->write(0.0);
    wff->write((float)((flux[0]/100.)*area));  // Write WATER_FLUX values
    wff->writeln();
    wff->write(site.max_time);
    wff->write((float)((flux[0]/100.)*area));  // Write WATER_FLUX values
    wff->writeln();
  }
  else
  {
    wff->write(num_values+1);
     wff->writeln();
    for(i=0; i<=num_values; i++)
     {
        wff->write(time[i]);
       wff->write((float)((flux[i]/100.)*area));  // Write WATER_FLUX values
       wff->writeln();
    }
  }
}


//===========================================================================================================
//     SUBROUTINE PATHWAY::SET_SERIES     ===================================================================
//     Function sets pathways to be calculated or turned off
//===========================================================================================================
int Pathway::set_series(int set_type, float set_value)
{             //  Note: c2 is the index for the flux time pairs
flux = new float[MAX_FLUX_CT];
time = new float[MAX_FLUX_CT];

if (set_type == 0)
  {
  known_flux = 0; // Must be calculated
  changed = 1;    // set changed to non-zero (true) such that first check will force a calculation
   num_values=-1;
  return(0);
  }
else
  {
  count=0;
  known_flux = 1; // This is a known flux case
  flux[0] = set_value;
  time[0] = site.max_time;
  return(1);
  }
}

int Remediate::set_series(int set_type, float set_value)
{
int i;

/* flux = new float[1];   replaced 4/98 BLH */
/* time = new float[1];   replaced 4/98 BLH */
   flux = new float[MAX_FLUX_CT];
   time = new float[MAX_FLUX_CT];

count=0;
if (set_type==1)
  {
  flux[0] = set_value;
  time[0] = site.max_time;
  return(1);
  }
else
  {
  for (i=0;i< data.i_read("stwatr_num",0,0,0,0,0,0);i++)
    {         //      cm/yr       *                g/cm^3               *  cm^2     *  kg/g   == kg/yr
    flux[i] = (water_eros.flux[i])*(data.f_read("stzbulkd",0,0,0,0,0,0))*(site.area)*(1./1000);
    time[i] = water_eros.time[i];
    }
  return(1);
  }
}

//===========================================================================================================
//     SUBROUTINE PATHWAY::INIT_PARAM     ===================================================================
//     Function initializes know Pathway fluxes.
//===========================================================================================================
int  Pathway::init_param(ParamFile &data,char *buf1,int contam,int c3,int c4,int c5,int c6)
{             //  Note: c2 is the index for the flux time pairs
char buf2[32];
//  float flux[50];
strcpy(buf2, buf1);
strncat(buf2, "_num",5);
num_values = data.i_read(buf2,contam,0,c3,c4,c5,c6);

if( num_values < 1)   //  Do not read in any values
  {
  known_flux = 0; // Must be calculated
  changed = 1;    // set changed to non-zero (true) such that first check will force a calculation
  num_values--;   // Decrement the num_values so updates will occur correctly ( -1 represents no values)
  return(num_values+1); //  ( 0 represents one value )
  }
else
  {
  get_series(data, buf1, contam, c3, c4, c5, c6);
  count=0;
  known_flux = 1; // This is a known flux case
  }
return(num_values+1);
}

int Pathway::init_param(int contnum, Series *TS)
{
  if (flux != NULL) delete[] flux;
  if (time != NULL) delete[] time;
  lasttime=0.0;
  num_values = TS->count;
  flux = new float[num_values];
  time = new float[num_values];
  for (int i=0;i<TS->count;i++)
  {
    flux[i] = flux_conv(TS->yValues[i], "STSOURCE", contnum);
    time[i]= TS->xValues[i];
    if (flux[i]>0.0) lasttime=time[i];
  }
  count=0;
  known_flux=1;
  return 0;
}

//===========================================================================================================
//     SUBROUTINE REMEDIATE::INIT_PARAM     =================================================================
//     Function initializes Remediation Class variable Series
//===========================================================================================================
int Remediate::init_param(ParamFile &data, char *buf1,int c1,int c3,int c4,int c5,int c6,int flag)
{             //  Note: c2 is the index for the flux time pairs *
char buf2[32];
int my_return=0;

strcpy(buf2, buf1);
strncat(buf2, "_num",5);
num_values =data.i_read(buf2, c1, 0, c3, c4, c5, c6);

if( num_values < 1)   //  Do not read in any values
  {       //  allocate space for one value
  flux=new float[1];
  time=new float[1];
  switch(flag)
    {
    case 1:   //   Infiltration Rate
      flux[0] = data.water_sum(INFIL);
      time[0] = data.f_read("STMAXTIME",0,0,0,0,0,0);
      break;
    case 2:   //  Wind Suspension Rate
      flux[0] = data.Wind_Susp(&nfactor[0], &nfactor[1], &nfactor[2]);
      time[0] = data.f_read("STMAXTIME",0,0,0,0,0,0);
      break;
    case 3:   //  Water Erosion Rate
      flux[0] = data.Sediment_trans();
      time[0] = data.f_read("STMAXTIME",0,0,0,0,0,0);
      break;
    default:
      fprintf(stderr,  "Error reading %s, must have an initial value.\n",buf1);
      err.write("Error reading");
      err.write(buf1);
      err.write(" must have an initial value.");
      err.writeln();
      go_die(3,"dumm");
      my_return = -1;
      break;
    }
  changed = 1;       // set changed to non-zero (true) such that first check will force a calculation
  return(my_return);
  }
else
  get_series(data, buf1, c1, c3, c4, c5, c6);
return(num_values+1);
}
