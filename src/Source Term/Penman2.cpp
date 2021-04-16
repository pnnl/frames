/************************************************************************************************************
 * Source Term Release Module                  MODULE:  PENMAN2.CPP                           VERSION: 1.00 *
 *          Copyright 1996 by Battelle Pacific Northwest National Laboratory.  All Rights Reserved          *
 ************************************************************************************************************
 *******                                    PROGRAM:  STRM                                            *******
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
 *   Last Modified:  07/17/96 -- KDS                                                                        *
 ************************************************************************************************************
 *                                             MODULE:  PENMAN.CPP                                          *
 *  PENMAN was written by James L. Stroh.   Function to calculate the Potential Evapotranspiration(PET)     *
 *  using the Penman Method and the Penman Method with Correction Factor.  This code can be found in        *
 *  Doorenbos and Pruit 1977 as FORTRAN code.  The FORTRAN was modified to C++ for this application.        *
 ************************************************************************************************************
 *       MODULE ORGANIZATION                                                                                *
 *                                                                                                          *
 *         Routines: Penman()                                                                               *
 ************************************************************************************************************
 *       MODIFICATION HISTORY                                                                               *
 *    DATE    WHO       DESCRIPTION                                                                         *
 *  --------  ---  ---------------------------------------------------------------------------------------- *
 *  07/17/96  KDS  Conversion of STC to STRM in preparation of application of MEPAS QA program and          *
 *                 procedures                                                                               *
 ************************************************************************************************************
 */

#include<stdio.h>
#include<math.h>     
#include"strm1.h"
#include"fcsv.h"

float Penman(float saturation_vp, float mean_vp, float max_humid, float nratio, float height_factor,
				 float temperature, float mean_wind_speed, float elevation, float lattitude, int month,
				 char corrected)
{
float	pet,				//	Potential Evapotranspiration (PET), (mm/day)
		day_wind,		//	Mean wind speed corrected to mean daytime windspeeds, (m/s)
		wind_factor,	//	Wind-related function
		et_rad,			//	Extraterrestrial Radiation expressed in equivalent evaportation, (mm/day)
		solar_rad,		//	ET Radiation corrected for actual to maximum possible sunshine hours
							//	expressed in equivalent evaportation, (mm/day)
		short_rad,     //	Short Radiation expressed in equivalent evaportation, (mm/day)
		long_rad,		//	Long Radiation expressed in equivalent evaportation, (mm/day)
		net_rad,			//	Net Radiation expressed in equivalent evaportation, (mm/day)
		tmp1,				//	Temporary variable used in interpolating between table values
		tmp2,				//	Temporary variable used in interpolating between table values
		temp_factor,	//	Temperature-related weighting factor
		day_night_factor,	//	Adjustment factor to compensate for the effect of day and night weather conditions
//	Data from Table 5.4, DRAFT--STRM Formulations Document
temp_data[6][20] =
	{{0.43,0.46,0.49,0.52,0.55,0.58,0.61,0.64,0.66,0.69,0.71,0.73,0.75,0.77,0.78,0.8 ,0.82,0.83,0.84,0.85},
	{0.44,0.48,0.51,0.54,0.57,0.6 ,0.62,0.65,0.67,0.7 ,0.72,0.74,0.76,0.78,0.79,0.81,0.82,0.84,0.85,0.86},
	{0.46,0.49,0.52,0.55,0.58,0.61,0.64,0.66,0.69,0.71,0.73,0.75,0.77,0.79,0.8 ,0.82,0.83,0.85,0.86,0.87},
	{0.49,0.52,0.55,0.58,0.61,0.64,0.66,0.69,0.71,0.73,0.75,0.77,0.79,0.81,0.82,0.84,0.85,0.86,0.87,0.88},
	{0.52,0.55,0.58,0.61,0.64,0.66,0.69,0.71,0.73,0.75,0.77,0.79,0.81,0.82,0.84,0.85,0.86,0.87,0.88,0.89},
	{0.54,0.58,0.61,0.64,0.66,0.69,0.71,0.73,0.75,0.77,0.79,0.81,0.82,0.84,0.85,0.86,0.87,0.89,0.9 ,0.9 }},
//	Data from Table 5.5, DRAFT--STRM Formulations Document
rad_data[26][12] = {{3.8,6.1,9.4,12.7,15.8,17.1,16.4,14.1,10.9,7.4,4.5,3.2},
	{4.3,6.6,9.8,13,15.9,17.2,16.5,14.3,11.2,7.8,5,3.7},
	{4.9,7.1,10.2,13.3,16,17.2,16.6,14.5,11.5,8.3,5.5,4.3},
	{5.3,7.6,10.6,13.7,16.1,17.2,16.6,14.7,11.9,8.7,6,4.7},
	{5.9,8.1,11,14,16.2,17.3,16.7,15,12.2,9.1,6.5,5.2},
	{6.4,8.6,11.4,14.3,16.4,17.3,16.7,15.2,12.5,9.6,7,5.7},
	{6.9,9,11.8,14.5,16.4,17.2,16.7,15.3,12.8,10,7.5,6.1},
	{7.4,9.4,12.1,14.7,16.4,17.2,16.7,15.4,13.1,10.6,8,6.6},
	{7.9,9.8,12.4,14.8,16.5,17.1,16.8,15.5,13.4,10.8,8.5,7.2},
	{8.3,10.2,12.8,15,16.5,17,16.8,15.6,13.6,11.2,9,7.8},
	{8.8,10.7,13.1,15.2,16.5,17,16.8,15.7,13.9,11.6,9.5,8.3},
	{9.3,11.1,13.4,15.3,16.5,16.8,16.7,15.7,14.1,12,9.9,8.8},
	{9.8,11.5,13.7,15.3,16.4,16.7,16.6,15.7,14.3,12.3,10.3,9.3},
	{10.2,11.9,13.9,15.4,16.4,16.6,16.5,15.8,14.5,12.6,10.7,9.7},
	{10.7,12.3,14.2,15.5,16.3,16.4,16.4,15.8,14.6,13,11.1,10.2},
	{11.2,12.7,14.4,15.6,16.3,16.4,16.3,15.9,14.8,13.3,11.6,10.7},
	{11.6,13,14.6,15.6,16.1,16.1,16.1,15.8,14.9,13.6,12,11.1},
	{12,13.3,14.7,15.6,16,15.9,15.9,15.7,15,13.9,12.4,11.6},
	{12.4,13.6,14.9,15.7,15.8,15.7,15.7,15.7,15.1,14.1,12.8,12},
	{12.8,13.9,15.1,15.7,15.7,15.5,15.5,15.6,15.2,14.4,13.3,12.5},
	{13.2,14.2,15.3,15.7,15.5,15.3,15.3,15.5,15.3,14.7,13.6,12.9},
	{13.6,14.5,15.3,15.6,15.3,15,15.1,15.4,15.3,14.8,13.9,13.3},
	{13.9,14.8,15.4,15.4,15.1,14.7,14.9,15.2,15.3,15,14.2,13.7},
	{14.3,15,15.5,15.5,14.9,14.4,14.6,15.1,15.3,15.1,14.5,14.1},
	{14.7,15.3,15.6,15.3,14.6,14.2,14.3,14.9,15.3,15.3,14.8,14.4},
	{15,15.5,15.7,15.3,14.4,13.9,14.1,14.8,15.3,15.4,15.1,14.8}},
//	Data from Table 5.6, DRAFT--STRM Formulations Document
day_night_data[3][4][4]=
//	Rhmax = 30%
	{{{0.86,0.9,1,1}, {0.69,0.76,0.85,0.92}, {0.53,0.61,0.74,0.84}, {0.37,0.48,0.65,0.76}},
//	Rhmax = 60%
	{{0.96,0.98,1.05,1.05}, {0.83,0.91,0.99,1.05}, {0.7,0.8,0.94,1.02}, {0.59,0.7,0.84,0.95}},
//	Rhmax = 90%
	{{1.02,1.06,1.1,1.1}, {0.89,0.98,1.1,1.14}, {0.79,0.92,1.05,1.12}, {0.71,0.81,0.96,1.06}}};

int i,j,k,
	corr_temp[20] = {2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40},
	corr_alt[6]   = {0,500,1000,2000,3000,4000},
	corr_lat[26] = {50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,18,16,14,12,10,8,6,4,2,0},
	corr_solar_rad[4] = {3,6,9,12},
	corr_wind[4] = {0,3,6,9};

#ifdef DEBUG
	printf("Doing PENMAN\n");
#endif

  sls->writeln("Doing PENMAN");

if(temperature > 0.0)
	{
	wind_factor = 0.27*(1.0 + height_factor*mean_wind_speed*0.01);
	i=1;
	while( ((float)corr_alt[i] < elevation) && (i < 5) )  /* There are 0 - 5 entries for corr_alt[]*/
		i++;

	j=1;
	while( ((float)corr_temp[j] < temperature) && (j < 19) )  /* There are 0 - 19 entries for corr_temp[]*/
		j++;

	tmp1 = temp_data[i-1][j-1] + (temperature-corr_temp[j-1])*
											(temp_data[i-1][j] - temp_data[i-1][j-1])/(corr_temp[j] - corr_temp[j-1]);
	tmp2 = temp_data[i][j-1]   + (temperature-corr_temp[j-1])*
											(temp_data[i][j]   - temp_data[i][j-1]  )/(corr_temp[j] - corr_temp[j-1]);
	temp_factor = tmp1 + (elevation-corr_alt[i-1])*(tmp2 - tmp1)/(corr_alt[i] - corr_alt[i-1]);

	i=1;
	while( ((float)corr_lat[i] > lattitude) && (i < 25) )    /* There are 0 - 25 entries for corr_lat[]*/
		i++;
	et_rad = rad_data[i-1][month] + (lattitude - corr_lat[i-1])*
				  (rad_data[i][month] - rad_data[i-1][month])/(corr_lat[i] - corr_lat[i-1]);

//	Equation 5.12, DRAFT--STRM Formulations Document
	solar_rad = (0.25 + 0.5*nratio)*et_rad;
//	Equation 5.13, DRAFT--STRM Formulations Document
	short_rad = (1.0 - 0.25)*solar_rad;

//	Equation 5.14, DRAFT--STRM Formulations Document
	long_rad = 2.0e-9*pow(temperature+273.15, 4.0)*(0.34 - 0.044*pow(mean_vp, 0.5) )*(0.1 + 0.9*nratio);

//	Equation 5.15, DRAFT--STRM Formulations Document
	net_rad = short_rad - long_rad;

	if(( corrected == 'Y') || ( corrected == 'y' ))   /* If corrected Penman equation */
		{
		if(max_humid < 45.0)    /* Find Maximum humidity range */
			k=0;
		else
			{
			if(max_humid < 75.0)
				k=1;
			else
				k=2;
			}

		day_wind = mean_wind_speed*1.33;  // Equation 5.6, DRAFT--STRM Formulations Document
		i=1;
		while( ((float)corr_wind[i] < day_wind) && (i < 3) ) /* There are 0-3 entries for corr_wind[]*/
			i++;

		j=1;              /* Find solar radiation value for table 5, MEPAS: Water-Mass Budget */
		while( ((float)corr_solar_rad[j] < solar_rad) && (j < 3) )  /* There are 0-3 entries for corr_solar_rad[]*/
			j++;
//	Data from Table 5.6, DRAFT--STRM Formulations Document
		tmp1 = day_night_data[k][i-1][j-1] + (solar_rad - corr_solar_rad[j-1])*
					 (day_night_data[k][i-1][j] - day_night_data[k][i-1][j-1])/(corr_solar_rad[j] - corr_solar_rad[j-1]);
		tmp2 = day_night_data[k][i][j-1]   + (solar_rad - corr_solar_rad[j-1])*
					 (day_night_data[k][i][j]   - day_night_data[k][i][j-1]  )/(corr_solar_rad[j] - corr_solar_rad[j-1]);
		day_night_factor = tmp1 + (day_wind - corr_wind[i-1])*(tmp2 - tmp1)/(corr_wind[i] - corr_wind[i-1]);
		}
	else                // If uncorrected Penman equation
		day_night_factor = 1.0;

// Equation 5.8, DRAFT--STRM Formulations Document
	pet = day_night_factor*(temp_factor*net_rad + (1.0 - temp_factor)*wind_factor*(saturation_vp - mean_vp));
	if(pet<0.0)   // Make certain PET is never negative (possible in Northern lattitudes with low temps
		pet = 0.0;
	if(( corrected == 'N') || ( corrected == 'n' ))   // If corrected Penman equation
    {
		sls->writeln("wind factor",wind_factor );
		sls->writeln("temp factor", temp_factor );
		sls->writeln("et rad", et_rad );
		sls->writeln("short rad", short_rad );
		sls->writeln("long rad", long_rad );
		sls->writeln("day night factor", day_night_factor );
    }
	}
else
	pet = 0.0;

#ifdef DEBUG
	 printf("Returning from PENMAN\n");
#endif

return(pet);
}
