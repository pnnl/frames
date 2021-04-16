/************************************************************************************************************
 * Source Term Release Module                MODULE:  WATER.CPP                               VERSION: 1.00 *
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
 *   Last Modified:  06/01/96  -- KDS                                                                       *
 ************************************************************************************************************
 *                                           MODULE:  WATER.CPP                                             *
 *  WATER was written by James L. Stroh.  This module performs a number of the underlying water balance     *
 *  calculations.                                                                                           *
 ************************************************************************************************************
 *       MODULE ORGANIZATION                                                                                *
 *  Routine: melt_snow(), runoff(), soil_moist_table(), water_loss_table()                                  *
 ************************************************************************************************************
 *       MODIFICATION HISTORY                                                                               *
 *		DATE	WHO			DESCRIPTION                                                                         *
 *	--------	---	---------------------------------------------------------------------------------------   *
 *	06/01/96	KDS	Conversion of STC to STRM in preparation of application of MEPAS QA program and           *
 *						procedures.                                                                               *
 ************************************************************************************************************
 */

#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include"param3.h"
#include"strm1.h"

//===========================================================================================================
//     SUBROUTINE MELT_SNOW     =============================================================================
//     This subroutine calculates the precipitation adjusted for snowmelt.
//===========================================================================================================

float melt_snow(float precip, float mean_vp, float mean_unadj_wind, float cloud_percnt, float temperature,
					 float num_days, float clear_melt, float wind_height, float site_elev)
{                        /* JLS  Using saturated_vp instead of mean_vp, double check!!*/
float snowmelt,
		mean_wind,
		vapor_melt,
		heat_coef,
		convection_melt,
		rad_melt,
		rain_melt;

#ifdef DEBUG
	printf("Doing MELT_SNOW\n");
#endif

//	All equations from "MEPAS:  Water-Mass Budget and Precip-Generated Percolation"
if( temperature > 0.0 )
	{
	mean_wind = 1.59*pow(wind_height,-0.17)*mean_unadj_wind;  /* Equation 16 */
	vapor_melt = 0.102*num_days*mean_wind*(mean_vp - 6.11);   /* Equation 17 */
	if(vapor_melt < 0.0)

		vapor_melt = 0.0;

		heat_coef = pow( 10.0, -1.725 - 5.12e-5*site_elev );      /* Equation 19 */

		convection_melt = 4.0*num_days*heat_coef*mean_wind*temperature; /* Equation 20 */

		rad_melt = num_days*clear_melt*(1.0 - 0.75*cloud_percnt); /* Equation 22 */

		rain_melt = 0.032*precip*temperature;                     /* Equation 24 */

		snowmelt = vapor_melt + convection_melt + rad_melt + rain_melt; /* Equation 25 */
	}
else
	snowmelt = 0.0;

#ifdef DEBUG
	 printf("Returning from MELT_SNOW\n");
#endif

return(snowmelt);
}

//===========================================================================================================
//     SUBROUTINE RUNOFF     ================================================================================
//     This subroutine calculates runoff.
//===========================================================================================================

float runoff(float total_precip, float num_precip, float temperature, float curve_num)
{
float monthly_runoff,  /* Total runoff, in cm/month */
		temp1,           /* temporary variable */
		temp2,           /* temporary variable */
		in_to_cm = 2.54; /* Converstion factor for inches to centimeters */

#ifdef DEBUG
	 printf("Doing RUNOFF\n");
#endif

temp1 = total_precip*curve_num;
temp2 = in_to_cm*num_precip*(1000. - 10.*curve_num);

if( (temperature > 0.0) && (temp1 > 0.2*temp2))
	{                 /* Equation 27, MEPAS: Water-Mass Budget */
	monthly_runoff = pow(temp1 - 0.2*temp2, 2.0) / (curve_num*(temp1 + 0.8*temp2));
	}
else               /* Equation 28, MEPAS: Water-Mass Budget*/
	monthly_runoff = 0.0;

#ifdef DEBUG
	 printf("Returning from RUNOFF\n");
#endif

return(monthly_runoff);
}

//===========================================================================================================
//     SUBROUTINE SOIL_MOIST_TABLE     ======================================================================
//     This subroutine calculateS Soil Moisture Retention from formulations based on the tables of
//     Thornwaite and Mather 1957.
//===========================================================================================================
float soil_moist_table(float water_loss, float water_holding_capacity)
{
int num_table;
float table[11] = {2.5, 5.0, 7.5, 10.0, 12.5, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0}, // Available water, (cm)
		slope[11] = {-4.9889E-02, -2.3511E-02, -1.4580E-02, -1.0855E-02, -8.5939E-03, -7.1000E-03,
						-5.2015E-03,-4.2005E-03, -3.4411E-03,-2.9372E-03,-2.5798E-03},
		y_intercpt[11] = {3.262058, 3.971088, 4.346333, 4.641729, 4.859457, 5.041892,
						5.314720, 5.548105, 5.716835, 5.870702, 6.008591},
		sm1, sm2, soil_moist_ret;

#ifdef DEBUG
	 printf("Doing SOIL_MOIST_TABLE\n");
#endif

num_table = 1;              /* Find correct table to use */
while( (num_table < 10) && (water_holding_capacity > table[num_table]) )
	num_table++;

water_loss = water_loss * 10.0; /* Convert from cm to mm */
sm1 = exp( water_loss*slope[num_table-1] + y_intercpt[num_table-1] );
sm2 = exp( water_loss*slope[num_table]   + y_intercpt[num_table] );
soil_moist_ret = (sm2 - sm1)*(water_holding_capacity - table[num_table-1])
							 /(table[num_table] - table[num_table-1]) + sm1;
soil_moist_ret = soil_moist_ret/10.0; /* Convert from mm to cm */

if( soil_moist_ret > water_holding_capacity)
	soil_moist_ret = water_holding_capacity;  // Don't let soil_moist exceed the water holding capacity

#ifdef DEBUG
	 printf("Returning from SOIL_MOIST_TABLE\n");
#endif

return(soil_moist_ret);
}

//===========================================================================================================
//     SUBROUTINE WATER_LOSS_TABLE     ======================================================================
//		This function calculates the Potential Water Loss from formulations based on the tables of
//		Thornwaite and Mather 1957.
//===========================================================================================================

float water_loss_table(float soil_moist_ret, float water_holding_capacity)
{
int num_table;
float table[11] = {2.5, 5.0, 7.5, 10.0, 12.5, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0}, // Available water, (cm)
		slope[11] = {-4.9889E-02, -2.3511E-02, -1.4580E-02, -1.0855E-02, -8.5939E-03, -7.1000E-03,
						-5.2015E-03,-4.2005E-03, -3.4411E-03,-2.9372E-03,-2.5798E-03},
		y_intercpt[11] = {3.262058, 3.971088, 4.346333, 4.641729, 4.859457, 5.041892,
						5.314720, 5.548105, 5.716835, 5.870702, 6.008591},
		wl1, wl2, water_loss;

#ifdef DEBUG
	 printf("Doing WATER_LOSS_TABLE\n");
#endif

num_table = 1;              /* Find correct table to use */
while( (num_table < 10) && (water_holding_capacity > table[num_table]) )
	num_table++;

if( soil_moist_ret < water_holding_capacity) // soil_moist_ret <= water_holding_cap
	{
	soil_moist_ret = soil_moist_ret * 10.0; /* Convert from cm to mm */
	wl1 = ( log(soil_moist_ret) - y_intercpt[num_table-1] ) / slope[num_table-1];
	wl2 = ( log(soil_moist_ret) - y_intercpt[num_table]   ) / slope[num_table];
	water_loss = (wl2 - wl1)*(water_holding_capacity - table[num_table-1]) /
					  ( table[num_table] - table[num_table-1] ) + wl1;
	water_loss = water_loss / 10.0; /* Convert from mm to cm */
	}
else
	water_loss = 0.0;

#ifdef DEBUG
	 printf("Returning from WATER_LOSS_TABLE\n");
#endif

return(water_loss);
}
