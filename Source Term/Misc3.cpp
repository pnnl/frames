/************************************************************************************************************
* Source Term Release Module                MODULE:  MISC3.CPP                               VERSION: 1.00 *
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
*   Last Modified:  06/01/96 -- KDS                                                                        *
************************************************************************************************************
*                                           MODULE:  MISC3.CPP                                             *
*     This module performs a number of miscellaneous calculations.                                         *
************************************************************************************************************
*       MODULE ORGANIZATION                                                                                *
*  Routine: dateoutst(), timeoutst(), calc_vapor_cons(), batemn(), chain_mass()                            *
************************************************************************************************************
*       MODIFICATION HISTORY                                                                               *
*		DATE	WHO			DESCRIPTION                                                                         *
*	--------	---	----------------------------------------------------------------------------------------  *
*	06/01/96	KDS	Conversion of STC to STRM in preparation of application of MEPAS QA program and           *
*						procedures.                                                                               *
*	11/04/96	KDS	Added subrouting NAPL_state() to do the NAPL phase existence calculation.                 *
************************************************************************************************************
*/
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include<dos.h>
#include"strm1.h"
#include"misc3.h"

static char retval[40];

//===========================================================================================================
//	SUBROUTINE dateoutst()  ==================================================================================
//	This function to returns the date for use in file headers.
//===========================================================================================================
char *dateoutst()
{
  struct date d;
  getdate(&d);
  sprintf(retval,"%d/%d/%d", (int)d.da_mon,(int)d.da_day, d.da_year);
  return(retval);
}

//===========================================================================================================
//	SUBROUTINE timeoutst()  ==================================================================================
//	This function to returns the time for use in file headers.
//===========================================================================================================
char *timeoutst()
{
  struct time t;
  gettime(&t);
  sprintf(retval,"%d:%d:%d",(int)t.ti_hour,(int)t.ti_min,(int)t.ti_sec);
  return(retval);
}

//===========================================================================================================
//	SUBROUTINE calc_vapor_cons()  ============================================================================
//	This function calculates the vapor concentration using either Henry's or Raoult's Law.
//===========================================================================================================
/*  replaced by JIM STROH version 6/98 BLH
float calc_vapor_cons(float sat_conc, float thick, float elap_time, int count,float wz_bulkd)
{   // TEMPORARY VERSION UNTIL THE DATABASE IS UPDATED WITH LIQUID DENSITIES
float temp;

  if (flg.napl)
  temp = sat_conc * contam[count].mole_fract * props[count].vol_ratio;
  //	ref. - in WEYER, Subsurface Contamination by immiscible fluids
  else
  temp = props[count].vol_ratio * (contam[count].henrys_const*contam[count].mass)
  / (site.area*thick*(site.theta+(air_space.get_value(elap_time)*contam[count].henrys_const)
  + (wz_bulkd*k_d[count].get_value(elap_time))));
  return(temp);
  }
*/

/*		JIM STROH VERSION  --  TEMPORARILY NOT USED DUE TO THE INABILITY TO CREATE A ROOT FINDING SOLUTION
BECAUSE THERE IS NO LIQUID DENSITY FOR CONTAMINANTS IN THE DATABASE YET
-- restored 6/98 BLH
*/
float calc_vapor_cons(float henrys_const, float sat_vapor_cons, float mole_fract, float alpha)
{
  //	Alpha converts Mole fraction to Concentration in organic liquid
  float transition,	//	Trasition point where Henry's law (trans) is equal to Raoult's law of (1-trans)
        vapor_cons;	//	Vapor concentration in (g/cm^3)
    //	Henry's Law(small mole fraction):   vapor_con = henrys_const*conc_org_liq
    //	Raoult's Law(large mole fraction):  vapor_con = mole_fract*sat_vapor_cons
    //	Use Henry's Law for small mole fraction and Raoult's Law for large mole fraction
    transition = sat_vapor_cons / (henrys_const*alpha +  sat_vapor_cons);
  if( transition < 0.5 )
		{
    if( mole_fract < transition )						//	Use Henry's Law
      vapor_cons = henrys_const * (alpha * mole_fract);	//	conc_org_liq = (alpha * mole_fract)
    else if ( mole_fract < (1.0 - transition) )			//	Use the point where they are equivalent
      vapor_cons = sat_vapor_cons * (1.0 - transition);
    else											//	Use Raoult's Law
      vapor_cons = sat_vapor_cons * mole_fract;
		}
  else
		{
    if( mole_fract < 0.5 )							//	Use Henry's Law
      vapor_cons = henrys_const * (alpha * mole_fract);	//	conc_org_liq = (alpha * mole_fract)
    else											//	Use Raoult's Law
      vapor_cons = sat_vapor_cons * mole_fract;
		}
  return(vapor_cons);
}

//===========================================================================================================
//	SUBROUTINE batemn()  =====================================================================================
//	This function calculates the activity of a decay product using the Bateman Equation.
//===========================================================================================================
float batemn(int parent,int child, float elapsed_time)
{
  static float prod1, prod2, tot;
  static int j, r;
  tot=0.0;
  for (j=parent;j<=child;j++)
		{
    prod1 = 1.0;
    for (r=parent;r<=child;r++)
    {
      if (r!=j)
      {
        prod1 *= props[r].lambda-props[j].lambda;
      }
    }
    if (prod1>0.0)
      tot += exp(-props[j].lambda * elapsed_time)/prod1;
		}
  prod2 = 1.0;
  
  for (j=parent;j<child;j++)
    prod2 *= props[j].lambda;
  
  return( tot * prod2);
}

//===========================================================================================================
//	SUBROUTINE chain_mass()  =================================================================================
//	This function searches the prior members of a decay chain to determine whether or not a decay product
//	will be produced.
//===========================================================================================================
int chain_mass(int parent, int progprod, float elapsed_time)
{
  //return 1;  // always true;
  if (source_sink[parent].lasttime>=elapsed_time)
    return 1;
  float ckmass=0.0;
  int i;
  if (parent == progprod )
    return (0);
  else
		{
    for (i=parent; i<progprod; i++)
      ckmass += contam[i].mass;
    if (ckmass > 0.0 )
      return (1);
    else
      return (0);
		}
}


//===========================================================================================================
//	SUBROUTINE NAPL_state()  =================================================================================
//	This function searches the prior members of a decay chain to determine whether or not a decay product
//	will be produced.
//===========================================================================================================
int NAPL_state(float org_moles, float gamma, int e_time)
{
  int i;
  float val; //, v1, v2;
  
  for (i=0;i<site.numtotcon; i++) // BLH 7/99
    contam[i].napl=0;
  
  for (i=0; i < site.numtotcon; i++ )
  {
    if(( props[i].typeflag == 1) && ( org_moles > 0.0 ))				// BLH skip div by zero
    {
      if (flg.pond == 0 && e_time > 0 ) //check to see if pond has been remediated
        if ((total_porosity.get_value(((e_time-1)*site.delta_t)) == 1.0 )
          && air_space.get_value(((e_time-1)*site.delta_t)) == 0.0)
          contam[i].mass = 0.0;  //contaminant removed with pond so no more mass to remove
        //		Pond and Aquifer case check
        //  if ( flg.pond || site.theta == total_porosity.get_value(e_time*site.delta_t))
        if ( flg.pond || 0==strcmpi(site.media_type,"aquifer"))    // blh
          //	For aquifer theta == P_t  and abc == waste_area * waste_thick == waste volume
          if ( ((contam[i].mass/
            (site.theta*contam[i].retard*site.area*waste_thick[i]))-(aqueous_sol[i]*1e-6)) > 0.0)
          {
            contam[i].napl=1; // BLH 7/99
            return(1);
          }
          if ( (waste_thick[i]-gamma*(e_time*site.delta_t)) > 0.0 )
          {
            if ( depth_dry_layer[i] <= 0.0 )     // Contaminated Vadose Zone w\o clean layer
            {
              //val = (site.theta*contam[i].retard*site.area*(waste_thick[i]-gamma*(e_time*site.delta_t)));
              //        v1 = (contam[i].mass / val);
              //        v2 = v1 - (aqueous_sol[i]*1e-6);
              val = (contam[i].mass/
                (site.theta*contam[i].retard*site.area*(waste_thick[i]-gamma*(e_time*site.delta_t))))
                -(aqueous_sol[i]*1e-6);
              //				if (((contam[i].mass/
              //								(site.theta*contam[i].retard*site.area*(waste_thick[i]-gamma*(e_time*site.delta_t))))
              //								-(aqueous_sol[i]*1e-6)) > 0.0 )
              if (val>0.0)
              {
                contam[i].napl=1; // BLH 7/99
                return(1);
              }
            }
            else //( depth_dry_layer[i] > 0.0 )  // Contaminated Vadose Zone w\ clean layer
              if ( ((contam[i].mass/
                (site.theta*contam[i].retard*site.area*waste_thick[i]))-(aqueous_sol[i]*1e-6)) > 0.0)
              {
                contam[i].napl=1; // BLH 7/99
                return(1);
              }
          }
    }
  }
  return(0);
}
