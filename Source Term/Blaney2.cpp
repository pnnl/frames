/************************************************************************************************************
 * Source Term Release Code Module          MODULE:  BLANEY2.CPP                              VERSION: 1.00 *
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
 *   Last Modified:  06/11/96  -- KDS                                                                       *
 ************************************************************************************************************
 *                                       MODULE:  BLANEY2.CPP                                               *
 * BLANEY2 was taken from MEPAS RADCON.   Function to calculate the Potential Evapotranspiration(PET)       *
 * using the Blaney-Criddle method This code can be found in Doorenbos and Pruit 1977 as FORTRAN code. The  *
 * FORTRAN was modified to C++ for this application.                                                        *
 ************************************************************************************************************
 *       MODULE ORGANIZATION                                                                                *
 *         Routines:  Blaney_Criddle()                                                                      *
 ************************************************************************************************************
 *       MODIFICATION HISTORY                                                                               *
 *		DATE	WHO			DESCRIPTION                                                                         *
 *	--------	---	----------------------------------------------------------------------------------------  *
 *	07/01/96	KDS	Conversion of STC to STRM in preparation of application of MEPAS QA program and           *
 *						procedures.  Module completely rewritten to match current MEPAS calculation methodology.  *
 *						This is the numerical methodology described in Doorenbos and Pruitt 1977, Appendix II     *
 *						and FORTRAN coded in Appendix III                                                         *
 ************************************************************************************************************
 */

#include<stdio.h>
#include"strm1.h"
#include"fcsv.h"

//***********************************************************************************************************
//	THIS SECTION INTERPOLATES 'F'.

float PP[12][11] = {{.267,.264,.261,.257,.252,.246,.239,.231,.220,.209,.195},
				{.269,.268,.266,.264,.261,.257,.253,.248,.243,.236,.228},
                    {.269,.269,.269,.269,.269,.269,.268,.268,.268,.267,.266},
                    {.269,.270,.272,.275,.278,.282,.286,.291,.297,.303,.310},
                    {.271,.273,.276,.281,.287,.294,.303,.312,.322,.334,.346},
                    {.274,.280,.285,.291,.298,.307,.316,.328,.341,.355,.371},
                    {.275,.281,.287,.293,.299,.305,.313,.321,.330,.341,.354},
                    {.274,.278,.282,.287,.291,.295,.300,.304,.309,.315,.322},
                    {.271,.277,.280,.281,.281,.281,.281,.281,.281,.281,.281},
                    {.270,.269,.268,.267,.264,.261,.258,.254,.250,.245,.240},
                    {.269,.267,.264,.260,.254,.247,.240,.231,.222,.211,.200},
                    {.268,.266,.262,.257,.250,.242,.232,.221,.209,.195,.180}},
// Order of counters changed for PP due to difference in C++ storage methodology

//***********************************************************************************************************
//	THIS SECTION INTERPOLATES ET1 USING A, B, AND F.

	 BB[6][6][6] = {{{0.84, 0.80, 0.74, 0.64, 0.52, 0.38},
                      {1.03, 0.95, 0.87, 0.76, 0.63, 0.48},
                      {1.22, 1.10, 1.01, 0.88, 0.74, 0.57},
                      {1.38, 1.24, 1.13, 0.99, 0.85, 0.66},
                      {1.54, 1.37, 1.25, 1.09, 0.94, 0.75},
                      {1.68, 1.50, 1.36, 1.18, 1.04, 0.84}},
                     {{0.97, 0.90, 0.81, 0.68, 0.54, 0.40},
                      {1.19, 1.08, 0.96, 0.84, 0.66, 0.50},
                      {1.41, 1.26, 1.11, 0.97, 0.77, 0.60},
                      {1.60, 1.42, 1.25, 1.09, 0.89, 0.70},
                      {1.79, 1.59, 1.39, 1.21, 1.01, 0.79},
                      {1.98, 1.74, 1.52, 1.31, 1.11, 0.89}},
                     {{1.08, 0.98, 0.87, 0.72, 0.56, 0.42},
                      {1.33, 1.18, 1.03, 0.87, 0.69, 0.52},
                      {1.56, 1.38, 1.19, 1.02, 0.82, 0.62},
                      {1.78, 1.56, 1.34, 1.15, 0.94, 0.73},
                      {2.00, 1.74, 1.50, 1.28, 1.05, 0.83},
                      {2.19, 1.90, 1.64, 1.39, 1.16, 0.92}},
                     {{1.18, 1.06, 0.92, 0.74, 0.58, 0.43},
                      {1.44, 1.27, 1.10, 0.91, 0.72, 0.54},
                      {1.70, 1.48, 1.27, 1.06, 0.85, 0.64},
                      {1.94, 1.67, 1.44, 1.21, 0.97, 0.75},
                      {2.18, 1.86, 1.59, 1.34, 1.09, 0.85},
                      {2.39, 2.03, 1.74, 1.46, 1.20, 0.95}},
                     {{1.26, 1.11, 0.96, 0.76, 0.60, 0.44},
                      {1.52, 1.34, 1.14, 0.93, 0.74, 0.55},
                      {1.79, 1.56, 1.32, 1.10, 0.87, 0.66},
                      {2.05, 1.76, 1.49, 1.25, 1.00, 0.77},
                      {2.30, 1.96, 1.66, 1.39, 1.12, 0.87},
                      {2.54, 2.14, 1.82, 1.52, 1.24, 0.98}},
                     {{1.29, 1.15, 0.98, 0.78, 0.61, 0.45},
                      {1.58, 1.38, 1.17, 0.96, 0.75, 0.56},
                      {1.86, 1.61, 1.36, 1.13, 0.89, 0.68},
                      {2.13, 1.83, 1.54, 1.28, 1.03, 0.79},
                      {2.39, 2.03, 1.71, 1.43, 1.15, 0.89},
                      {2.63, 2.22, 1.86, 1.56, 1.27, 1.00}}};


float Blaney_Criddle(float lattitude, int month, float temperature, float rhmin, float nratio, float u_day)
     {
//	VALUES USED IN THIS FUNCTION
//	lattitude   lattitude of location being evaluated,			(degrees)
//	month       index, indicating month of the year,				(Unitless)
//	temperature average monthly temperature,					(deg C)
//	rhmin,      minimum relative humidity,						(%)
//	nratio      ratio of actual to maximum possible sunshine hours,	(Unitless)
//	u_day       average monthly daytime wind velocity,			(m/s)


	static unsigned int I1, I2, J1, J2, K1, K2, L1, L2, LL, X1, X2, Z1, Z2;
	static float AP, BP, ET1, F, FAC1, FACX, FACY, FACZ, P, P1, P2, Y1, Y2,
		C[2][2] = {{0.0,0.0},{0.0,0.0}},
		D[2] = {0.0,0.0};
#ifdef DEBUG
     printf("Doing BLANEY_CRIDDLE\n");
#endif

	LL=int(lattitude / 5) * 5;
	if (lattitude > 50)
		LL = 50;
	L1 = LL / 5 + 1;
	L2 = L1 + 1;
	if (L2 > 11)
		L2 = 11;
	FAC1 = (lattitude - LL) / 5. ;
//	MONTH1=MONTH
//	MONTH2=MONTH
//***********************************************************************************************************
//	INTERMEDIATE CALCULATIONS PRIOR TO USING EQUATION 3.2
//	changed month1 and month2 to month in next two lines Order of counters changed and decremented by 1 for
//	PP due to difference in C++ storage methodology versus FORTRAN
	P1 = PP[month-1][L1-1] + FAC1 * (PP[month-1][L2-1] - PP[month-1][L1-1]) ;
	P2 = PP[month-1][L1-1] + FAC1 * (PP[month-1][L2-1] - PP[month-1][L1-1]) ;
	P = P1 + 0.5 * (P2 - P1);
	F = P * (0.46 * temperature + 8.13) ;

//***********************************************************************************************************
//	THIS SECTION INTERPOLATES ET1 USING A, B, AND F.

	I1 = int(rhmin / 20.) + 1;
	I2 = I1 + 1;
	if (I2 > 6)
		I2 = 6;
	J1 = int(nratio / 0.2) + 1;
	J2 = J1 + 1;
	if (J2 > 6)
		J2 = 6;
	K1 = int(u_day / 2) + 1;
	K2 = K1 + 1;
	if(K2 > 6)
		K2 = 6;
	if(K1 > 6)
		K1 = 6;
	X1 = (I1 - 1) * 20;
	X2 = (I2 - 1) * 20;
	Y1 = (J1 - 1) * 0.2;
	Y2 = (J2 - 1) * 0.2;
	Z1 = (K1 - 1) * 2;
	Z2 = (K2 - 1) * 2;

	FACX = 0.0;
	FACY = 0.0;
	FACZ = 0.0;

#ifdef DEBUG
     printf("Middle of Blaney\n");
#endif

	if (K1 != K2)
		FACZ=(u_day - Z1) / (Z2 - Z1);

//	Counters decremented by 1 due to difference in C++ storage methodology versus FORTRAN
	C[0][0] = BB[I1-1][J1-1][K1-1] + FACZ * (BB[I1-1][J1-1][K2-1] - BB[I1-1][J1-1][K1-1]);
	C[0][1] = BB[I1-1][J2-1][K1-1] + FACZ * (BB[I1-1][J2-1][K2-1] - BB[I1-1][J2-1][K1-1]);
	C[1][0] = BB[I2-1][J1-1][K1-1] + FACZ * (BB[I2-1][J1-1][K2-1] - BB[I2-1][J1-1][K1-1]);
	C[1][1] = BB[I2-1][J2-1][K1-1] + FACZ * (BB[I2-1][J2-1][K2-1] - BB[I2-1][J2-1][K1-1]);

	if (J1 != J2)
		FACY = (nratio - Y1) / (Y2 - Y1);
	if (I1 != I2)
		FACX = (rhmin - X1) / (X2 - X1);
//	Counters decremented by 1 due to difference in C++ storage methodology versus FORTRAN
	D[0] = C[0][0] + FACY * (C[0][1] - C[0][0]);
	D[1] = C[1][0] + FACY * (C[1][1] - C[1][0]);
	BP = D[0] + FACX * (D[1] - D[0]);
	AP = 0.0043 * rhmin - nratio - 1.41;
//***********************************************************************************************************
//	CALCULATION OF POTENTIAL EVAPOTRANSIRATION, BASED ON EQUATION 3.2 FROM WHELAN ET AL. 1987

	ET1 = AP + BP * F;

//***********************************************************************************************************
//	FAO BLANEY CURVES FOR ADJUSTMENTS ARE FOR F>2.0

	if (F >= 2.0)    // Changed from 3.0 in BLANEY.FOR
		{
#ifdef DEBUG
     printf("Returning from BLANEY_CRIDDLE\n");
#endif
		return (ET1);
		}
	else
		{
      sls->BlaneyCriddle();
#ifdef DEBUG
     printf("Returning from BLANEY_CRIDDLE\n");
#endif
		return(F);    //  ET1=F
		}
	}
