/************************************************************************************************************
* Source Term Release Module                 MODULE:  SEDIMENT.CPP                           VERSION: 1.00 *
*          Copyright 1996 by Battelle Pacific Northwest National Laboratory.  All Rights Reserved          *
************************************************************************************************************
********                                    PROGRAM:  STRM                                           *******
********  This code incorporates and expands upon the STC Source Term Mass Balance Code developed    *******
********  and written by James L. Stroh for RAAS Ver 1 PNL 1995                                      *******
********                                                                                             *******
*                                                                                                          *
*      Written by:  Keith D. Shields                                                                       *
*                   Pacific Northwest National Laboratory                                                  *
*                   P.O. Box 999                                                                           *
*                   Richland, WA  99352                                                                    *
*                                                                                                          *
*         Created:  06/01/96                                                                               *
*   Last Modified:  12/10/96  -- KDS                                                                       *
************************************************************************************************************
*                                            MODULE:  SEDIMENT.CPP                                         *
*  SEDIMENT was written by James L. Stroh.   Function to calculate the soil lost to water erosion, returns *
*          soil erosion rate in (cm/y)                                                                     *
************************************************************************************************************
*       MODULE ORGANIZATION                                                                                *
*         Routine: Sediment_trans()                                                                        *
************************************************************************************************************
*       MODIFICATION HISTORY                                                                               *
*		DATE	WHO			DESCRIPTION                                                                         *
*	--------	---	---------------------------------------------------------------------------------------   *
*	07/01/96	KDS	Conversion of STC to STRM in preparation of application of MEPAS QA program and           *
*						procedures.                                                                               *
*                 1	storm_mlt[3] = {2.124, 3.624, 1.314} to storm_mlt[3] = {2.1289, 3.5717, 1.3121}        *
*                 2	Conversion from (metric tonnes)/(ha * yr) -> g/(m^2 * yr) changed to tons/ (acre * yr) *
*                    -> g/ (m^2 * yr)                                                                      *
*	12/10/96	KDS	Added initialization of sed_load.() to capture sediment mass flux from site.              *
************************************************************************************************************
*/

#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"param3.h"
#include"strm1.h"

float ParamFile::Sediment_trans()
{
  int storm_indx;					//	Storm index, {1,2,3}
  float sed_flx,						//	Mass flux of sediment to the overland path (kg/yr)
    soil_loss,						//	Average-annual soil loss, (t/ha)
    //runoff_vol,						//	runoff volume as calculated by water_balance()
    storm_exp[3] = {2.2, 2.17, 2.2},		//	Exponent used in calculating rain factor
    storm_mlt[3] = {2.1289, 3.5717, 1.3121},//	multiplier used in calculating rain factor
    storm_prcp,						//	2yr recurrence-interval, 6h duration precipitation event, (cm)
    rain_fact,						//	Rainfall erosivity factor, (J/ha)
    soil_fact,						//	Soil erodibility factor, (t/J)
    slope_fact,						//	Slope length and steepness factor, (dimensionless)
    slope,							//	The percent slope
    slope_exp,
    slope_param,
    slope_length,
    bulk_density,					//	bulk density of the soil, (g/cm^3)
    veg_fact,						//	Vegetative cover factor, (dimensionless)
    control_fact;					//	Erosion control practice factor

  printf("Computing overland water erosion rate\n\n");

#ifdef DEBUG
	 printf("Doing SEDIMENT\n");
#endif

   //runoff_vol = water_sum(WATER);
   //if( runoff_vol > 0.0 )
   //	{
   storm_indx   = i_read("ststormi",0,0,0,0,0,0);
   storm_prcp   = f_read("stprecip",0,0,0,0,0,0);
   soil_fact    = f_read("stkfactr",0,0,0,0,0,0);
   slope        = f_read("stslope", 0,0,0,0,0,0);
   veg_fact     = f_read("stcfactr",0,0,0,0,0,0);
   control_fact = f_read("stpfactr",0,0,0,0,0,0);
   slope_length = f_read("stslength",0,0,0,0,0,0);
   bulk_density = f_read("stsbulkd",0,0,0,0,0,0);
   
   storm_indx --; /* correct for indexing */
   rain_fact = storm_mlt[storm_indx]*pow(storm_prcp,storm_exp[storm_indx]);
   if( slope < 1.0)
     slope_exp = 0.2;
   else if( slope < 3.0 )
     slope_exp = 0.3;
   else if( slope < 4.5 )
     slope_exp = 0.4;
   else
     slope_exp = 0.5;
   
   slope_param = pow((slope*slope + 1.0e4), 0.5);
   slope_fact = ((slope/slope_param)*(65.41*slope/slope_param + 4.56) + 0.065)*
     pow(4.53e-4*slope_length, slope_exp);
   
   soil_loss = rain_fact*soil_fact*slope_fact*veg_fact*control_fact;
   //	Convert from (metric tonnes)/(hectare*yr) to g/(sq. meter * yr)
   //			soil_loss = 1.0e2*soil_loss;
   //	Convert from (tonnes)/(acre*yr) to g/(sq. meter * yr)
   soil_loss *= 224.1702;
   //	Initialize Sediment Flux series
   //	Convert from g/(m^2 y) soil loss to kg/y sediment flux
   sed_flx = soil_loss*(site.width/100)*(site.length/100)/1000;
   sed_load.set_series(1,sed_flx);
   //	Convert from g/(m^2 y) to cm/y
   soil_loss *= 1.0e-4*(1.0/bulk_density);
   //	}
   //else
   //	soil_loss = 0.0;
   
#ifdef DEBUG
   printf("Returning from SEDIMENT\n");
#endif
   
   return(soil_loss);
}

