/************************************************************************************************************
 * Source Term Release Module                MODULE:  WATERBAL2.CPP                           VERSION: 1.00 *
 *          Copyright 1996 by Battelle Pacific Northwest National Laboratory.  All Rights Reserved          *
 ************************************************************************************************************
 *******                                       PROGRAM:  STRM                                         *******
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
 *   Last Modified:  12/10/96 -- KDS                                                                        *
 ************************************************************************************************************
 *                                       MODULE:  WATERBAL.CPP                                              *
 *  WATER_BALANCE is the module used to calculate the hydrological balance for the source due to            *
 *  metoerological conditions which calculates infiltration and overland runoff Equation and table          *
 *  references are to document, Whelan, "MEPAS Water-Mass Budget and Precipitation-Generated Percolation",  *
 *  1993                                                                                                    *
 ************************************************************************************************************
 *  MODULE ORGANIZATION                                                                                     *
 *                                                                                                          *
 *  Routine: water_balance(), water_sum()                                                                   *
 ************************************************************************************************************
 *       MODIFICATION HISTORY                                                                               *
 *    DATE    WHO       DESCRIPTION                                                                         *
 *  --------  ---  ---------------------------------------------------------------------------------------- *
 *  06/01/96  KDS  Conversion of STC to STRM in preparation of application of MEPAS QA program and          *
 *  					 procedures.                                                                              *
 *                 1  Changed call for Blaney-Criddle method to match changes made to that module.          *
 *                 2  Removed some common calculations from penman.cpp and blaney2.cpp to waterbal.cpp      *
 * 						 including:   nratio, height_factor, and u_day.                                        *
 *                 3  Changed water_holding_capacity from a calculated parameter using strootd, and stwiltp *
 *							 to a read in value from stavailw.                                                     *
 *	12/10/96	KDS	Added initialization of over_flow.() to capture overland water flux.                      *
 *  08/14/97  BLH Added unit conversion on computed over_flow.() from cm^3 to m^3
 ************************************************************************************************************
 */

#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#include"strm1.h"
#include"fcsv.h"
#include"fileprep.h"
#include"param3.h"

//float *water_info;
static float water_info[25];
//===========================================================================================================
//     SUBROUTINE *PARAMFILE::WATER_BALANCE  ================================================================
//     Function performs the hydrologic water balance calculation
//===========================================================================================================
float *ParamFile::water_balance( )
{
int i,j, month, start;	// Various Counters
float num_days[12] = {31.0,28.25,31.0,30.0,31.0,30.0,31.0,31.0,30.0,31.0,30.0,31.0}, // days in month
		wind_height,		// Height above ground, of Local Wind measurements,(m)
		LCD_elev,			// Elevation of the Local Climatalogical Data(LCD) station,(m)
		site_elev,       	// Elevation of waste site,(m)
		lattitude,       	// Lattitude of the waste site, (degrees)
		curve_num,       	// SCS curve number, (unitless)
		snow_melt,       	// Average monthly snowmelt, (cm)
		snow_storage=0.0,	// Depth of snow storage accumulated during freezing months, (cm)
		clear_melt[4]={0.89,1.07,1.22,1.33}, // cm of Snow melt occuring in a half-day
								// (New Reference in STRM Formulations)
		unadj_temp,			// Temperature at the meteoralogical station, (deg C)
		temperature[12],	// Temperature adjusted for waste site elevation, (deg C)
		precip,				// Precipitation measured at the LCD for a particular month, (cm)
		mean_humid,			// Mean relative humidity, (%)
		max_humid,			// Maximum relative humidity, (%)
		min_humid,			// Minimum relative humidity, (%)
		mean_vp,				// Mean actual vapor pressure of the air, (mb)
		saturation_vp,		// Saturated vapor pressure at mean air temperature, (mb)
		mean_unadj_wind,	// The mean monthly wind speed, at the LCD, (m/s)
		cloud_fract,		// Degree of cloudiness (0 for clear weather and 1.0 for complete overcast)
		num_precip,			// Number of precip events occuring in a particular month
		adj_precip[12],	// Precipitation adjusted for snow melt
		runoff_volume[12],	// Runoff volume per area occuring in a particular month, (cm)
		pet_b,				// PET calculated using the Blaney-Criddle method, (mm/day)
		pet_p,				// PET calculated using the Penman method, (mm/day)
		pet_pc,				// PET calculated using the corrected Penman method, (mm/day)
		min_pet[12],		// Minimum PET, (cm/month)
		soil_moist[12],	// Volume of moisture per area contained in the root zone, (cm)
		soil_moist_prev,	// Volume of moisture per area contained in the root zone(previous), (cm)
		water_loss,			// Amount of water loss from evapotranspiration, (cm)
		water_holding_capacity,	// ( (field capacity) - (wilting point) ) * (root depth), (cm)
		diff,					// Difference measured in iterative method
		potential_perc[12],	// precipitation adjusted for snow melt, subtracting runoff and PET, (cm/month)
		infiltration[12],	// The net infiltration for the month, (cm/month)
		u_day,
		nratio,
		height_factor;

printf("Computing water balance\n\n");

#ifdef DEBUG
	printf("Doing WATER_BALANCE\n");
#endif

  sls->writeln("Doing WATER_BALANCE");

site_elev   = f_read("stelev",0,0,0,0,0,0);
LCD_elev    = f_read("stlcdelev",0,0,0,0,0,0);
lattitude   = f_read("stlat",0,0,0,0,0,0);
wind_height = f_read("stwindht",0,0,0,0,0,0);
curve_num   = f_read("stscscn",0,0,0,0,0,0);
water_holding_capacity = f_read("stavailw",0,0,0,0,0,0);

  sls->writeln("stelev",site_elev);
  sls->writeln("stlcdelev",LCD_elev);
  sls->writeln("stlat",lattitude);
  sls->writeln("stwindht",wind_height);
  sls->writeln("stscscn",curve_num);
  sls->writeln("water holding capacity (stavailw)",water_holding_capacity);

//water_info = new float[25];
for(i=0; i<25; i++)     // Initialize water_info
	water_info[i] = 0.0;

//	water_holding_capacity = (field_capacity - wilting_point)*root_depth; /* Fc  fraction */
if(water_holding_capacity < 2.5)
	{
	sls->writeln("Warning: Water Holding Capacity",water_holding_capacity);
	sls->writeln(" (cm) is below the minimum using 2.5 cm");
	water_holding_capacity = 2.5;
	}
else if(water_holding_capacity > 40.0)
	{
	sls->writeln("Warning: Water Holding Capacity",water_holding_capacity);
	sls->writeln(" (cm) exceeds the maximum using 40.5 cm");
	water_holding_capacity = 40.0;
	}

for(i=0; i<12; i++)     // Read in Monthly temperature data
	{
	unadj_temp = f_read("sttemp",0,0,0,0,i+1,0);
	temperature[i] = unadj_temp - 0.007*(site_elev - LCD_elev);  /* Equation 1, MEPAS: Water-Mass Budget */
	}

//	Find first month with temperature below freezing
start = 0;
for(i=0; i<12; i++)
	if( (temperature[(i+1)%12] <= 0.0) && (temperature[i] > 0.0) )
		start = (i+1)%12;

sls->write("month" );
sls->write("nratio" );
sls->write("wind_factor" );
sls->write("temp_factor" );
sls->write("et_rad" );
sls->write("short_rad" );
sls->write("long_rad" );
sls->write("day_night_factor" );
sls->writeln();

for(i=start; i<(start+12); i++)     /* Calculate monthly runoff volumes */
	{
	month = i%12;
	precip =          f_read("stmprecip",0,0,0,0,month+1,0);
	mean_unadj_wind = f_read("stwindv",0,0,0,0,month+1,0);
	cloud_fract =     f_read("stcloud",0,0,0,0,month+1,0);
	num_precip =      f_read("stmnumpre",0,0,0,0,month+1,0);
	min_humid =       f_read("strhmin",0,0,0,0,month+1,0);
	max_humid =       f_read("strhmax",0,0,0,0,month+1,0);
	mean_humid = (max_humid + min_humid) / 2.0;

	if( temperature[month] > 0.0)
		{                                    /* Equation 7, MEPAS: Water-Mass Budget */
		saturation_vp = 33.864*( pow(7.38e-3*temperature[month]+0.8072, 8.0) -
                                      1.9e-5*fabs(1.8*temperature[month] + 48.0) + 1.316e-3);
		mean_vp = mean_humid * saturation_vp/100.0;

		if(snow_storage > 0.0)    /* Only calculate snow melt if we have snow stored on the surface */
			{
			if(month < 3)   /* Since we only have clear melt days for March-June we must adjust months */
				j = 0;        /* if it occurs in a month before March, use the March data*/
			else if(month < 6) /* if it occurs between March and June, adjust the index such that March is now 0 */
				j = month - 2;
			else
				j = 3;       /* if it occurs in a month after June, use the June data */

			snow_melt = melt_snow(precip, mean_vp, mean_unadj_wind, cloud_fract,
                                     temperature[month], num_days[month], clear_melt[j], wind_height, site_elev);
			if(snow_storage < snow_melt)
				{                            /* check to make sure we don't melt more snow than we have */
				snow_melt = snow_storage;
				snow_storage = 0.0;
				}
			else
				snow_storage = snow_storage - snow_melt;
			}
		else
			snow_melt = 0.0;

		adj_precip[month] = precip + snow_melt;

#ifdef DEBUG
		printf("Month is %d,\n",month);
#endif

		runoff_volume[month] = runoff(adj_precip[month], num_precip, temperature[month], curve_num);
// Calculate Potential Evapotranspiration (PET)
//			Convert cloud fraction to percent of possible sun (n/N) */
		nratio = 1.0 - 0.016*cloud_fract - 0.0084*cloud_fract*cloud_fract;   /* Equation ref.? JLS */
		sls->write(month+1);
		sls->write(nratio);

		if(wind_height>2.0)
			height_factor = pow(2.0/wind_height, 0.17);     /* Equation 9, MEPAS: Water-Mass Budget */
		else
			height_factor = pow(2.0/wind_height, 0.22);     /* Equation 10, MEPAS: Water-Mass Budget */
		u_day = 1.33 * height_factor * mean_unadj_wind;

//		pet_b = Blaney_Criddle(site_elev, lattitude, temperature[month], month, cloud_fract, LCD_elev, mean_unadj_wind);
		pet_b = Blaney_Criddle(lattitude, month+1, temperature[month], min_humid, nratio, u_day);

// Call the Penman equation with correction factor
		pet_pc = Penman(saturation_vp, mean_vp, max_humid, nratio, height_factor,
                          temperature[month], mean_unadj_wind, site_elev, lattitude, month,'Y');
// Call the Penman equation without correction factor
		pet_p = Penman(saturation_vp, mean_vp, max_humid, nratio, height_factor,
                         temperature[month], mean_unadj_wind, site_elev, lattitude, month,'N');
		min_pet[month] =  min( pet_b, min(pet_p, pet_pc) );   /* Find minimum value for most conservative estimate */
		min_pet[month] = min_pet[month]*num_days[month]/10.0; /* Convert from mm/day to cm/month */

		potential_perc[month] = adj_precip[month] - runoff_volume[month] - min_pet[month];
		}
	else
		{
		adj_precip[month] = 0.0;
		runoff_volume[month] = 0.0;
		snow_storage += precip;
		min_pet[month] = 0.0;
		potential_perc[month] = 0.0;
		}
	}

diff = 1.0;
j = 0;
soil_moist[0] = water_holding_capacity; /* initialize January soil moisture */
if (soil_moist[0]<0.1) // soil moist must be >= 0.1 cm
	soil_moist[0] = 0.1;
water_loss = 0.0;
soil_moist_prev = soil_moist[0];

while( (diff>0.01) && (j<MAX_ITER) )  // loop until soil moisture converges, or MAX_ITER is achieved
	{
	for (i=(1); i<(13); i++) /* loop through all 12 months starting with February*/
		{
		month = i%12;
		if(potential_perc[month] < 0.0)        // Compute Soil Moisture Retention for the dry months
			{
			water_loss -= potential_perc[month];
			soil_moist[month] = soil_moist_table(water_loss, water_holding_capacity);
			if (soil_moist[month]<0.1) // soil moist must be >= 0.1 cm
				soil_moist[month] = 0.1;
			infiltration[month] = 0.0;
			}
		else                                   // Compute Soil Moisture Retention for the wet months
			{
			soil_moist[month] = soil_moist[(month+11)%12] + potential_perc[month];// plus 11 and mod 12 is equivalent to
			if (soil_moist[month]<0.1) // soil moist must be >= 0.1 cm               subtracting one from the month, but
				soil_moist[month] = 0.1;                                         // prevents using mod on a negative number
			if( soil_moist[month] > water_holding_capacity)
				{
				infiltration[month] = soil_moist[month] - water_holding_capacity;
				soil_moist[month] = water_holding_capacity;
				water_loss = 0.0;
				}
			else
				{   /* In case there are more than one wet and dry seasons, reset water_loss */
				water_loss = water_loss_table(soil_moist[month], water_holding_capacity);
				infiltration[month] = 0.0;
				}
			}
		diff = fabs(soil_moist[month] - soil_moist_prev)/soil_moist_prev;
		soil_moist_prev = soil_moist[month];
		j++;
		}
	}
sls->writeln();
sls->write(" ");
sls->write("Jan");
sls->write("Feb");
sls->write("Mar");
sls->write("Apr");
sls->write("May");
sls->write("Jun");
sls->write("Jul");
sls->write("Aug");
sls->write("Sep");
sls->write("Oct");
sls->write("Nov");
sls->write("Dec");
sls->writeln();
sls->write("Temp (C):");
for(i=0;i<12;i++)
	sls->write(temperature[i]);
sls->writeln();
sls->write("Precip (cm):");
for(i=0;i<12;i++)
	sls->write(adj_precip[i]);
sls->writeln();
sls->write("PET (cm):");
for(i=0;i<12;i++)
	sls->write(min_pet[i]);
sls->writeln();
sls->write("Soil Moist (cm):");
for(i=0;i<12;i++)
	sls->write(soil_moist[i]);
sls->writeln();
sls->write("Pot Perc (cm):");
for(i=0;i<12;i++)
	sls->write(potential_perc[i]);
sls->writeln();
sls->write("Infilt (cm):");
for(i=0;i<12;i++)
	sls->write(infiltration[i]);
sls->writeln();
sls->write("Runoff (cm):");
for(i=0;i<12;i++)
	sls->write(runoff_volume[i]);
sls->writeln();

//	if( (water_info = (float *)malloc(2*12*sizeof(float))) == NULL)
//		return(NULL);

for(i=0; i<12; i++)
	{
	water_info[i] = runoff_volume[i];
	water_info[i+12] = infiltration[i];
	}

#ifdef DEBUG
  printf("Returning from WATERBAL\n");
#endif

return(water_info);
}

//===========================================================================================================
//     SUBROUTINE PARAMFILE::WATER_SUM    ===================================================================
//     Function to sum all the infiltration or overland runoff values
//===========================================================================================================
float ParamFile::water_sum(unsigned int water_type)
{
//	Modified by BLH to enable re-initialization when running in DLL
int i;
//	shift=0;
float temp;
static int flag=0;
static float runoff=0.0;
static float infilt=0.0;

float *pwater_info; /* pointer to the monthly infiltration and overland runoff */

#ifdef DEBUG
	printf("Doing WATER_SUM\n");
#endif

if( water_type == REINITIALIZE )
	{
	flag = 0;
	temp = 0.0;
	}
else
	{
	if(flag == 0) /* water_balance() has not been called, call function */
		{
		pwater_info = water_balance( );
		if(pwater_info == NULL)
			{
			fprintf(stderr,  "Out of memory on returning from water_balance function\n");
			err.write("Out of memory on returning from water_balance function");
			err.writeln();
			go_die(3,"dumm");
			}
		flag = 1; /* Set flag to indicate water_balance() has been called */
		runoff = 0.0;
		infilt = 0.0;
		for (i=0; i<12; i++)
			{
			runoff += water_info[i];
			infilt += water_info[i+12];
			}
		}


//		if( water_type == INFIL )
//			shift = 12;
//		for(i=0; i<12; i++)
//			temp += water_info[i+shift];
	if (water_type == INFIL)
		temp = infilt;
	else
		temp = runoff;
	}
#ifdef DEBUG
  printf("Doing WATER_SUM\n");
#endif
// Added 12/10/96 to capture overland water flux rate
// Corrected unit conversion (from cm^3 to m^3) on 8/14/97
over_flow.set_series(1,(runoff*site.length*site.width*1.0e-6));

//delete[] water_info;
return(temp);
}
