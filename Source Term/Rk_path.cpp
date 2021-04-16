/************************************************************************************************************
* Source Term Release Module               MODULE:  RK_PATH.CPP                              VERSION: 1.00 *
*          Copyright 1996 by Battelle Pacific Northwest National Laboratory.  All Rights Reserved          *
************************************************************************************************************
*******                                      PROGRAM:  STRM                                          *******
*******  This code incorporates and expands upon the STC Source Term Mass Balance Code developed and *******
*******  written by James L. Stroh for RAAS Ver 1 PNL 1995                                           *******
*******                                                                                              *******
*                                                                                                          *
*      Written by:  Keith D. Shields                                                                       *
*                   Pacific Northwest National Laboratory                                                  *
*                   P.O. Box 999                                                                           *
*                   Richland, WA  99352                                                                    *
*                                                                                                          *
*         Created:  06/01/96                                                                               *
*   Last Modified:  10/28/96  -- KDS                                                                       *
************************************************************************************************************
*                                         MODULE:  RK_PATH.CPP                                             *
*  RK_PATH is the module incorporating the Runge-Kutte (RK) calculation methodology.  This model           *
*  calculates the losses to each pathway, and combines them using the RK methodology as described in the   *
*  MEPAS Source Term Release Formulations Document.  The module returns a pointer to the array of pathway  *
*  fluxes which are calculated for the individual pathways.                                                *
************************************************************************************************************
*       MODULE ORGANIZATION                                                                                *
*                                                                                                          *
*  Routines:  rk_calc(), rk_step(), leach[].dm_dt(), overland[].dm_dt(), suspend[].dm_dt, volate[].dm_dt   *
************************************************************************************************************
*       MODIFICATION HISTORY                                                                               *
*    DATE    WHO       DESCRIPTION                                                                         *
*  --------  ---  ---------------------------------------------------------------------------------------  *
*  05/31/96  KDS  Conversion of STC to STRM in preparation of application of MEPAS QA program and          *
*                 procedures                                                                               *
*  10/28/96  KDS  Created rk_step() and modified rk_calc() to make module more effective.                  *
*  09/05/97  BLH  use site.pond_out only if known discharge from pond                                      *
*  02/20/04  BLH  corrected application of source_sink term (a number of redundant expressions removed)    *
*                 problems with graphical views of source_sink cases prompted closer look at how the term  *
*                 was being applied which revealed several statements that had no effect and were removed  *
*                   see apply_dmdt and very end of rk_calc                                                 *
*  04/05/04  BLH  resolving lingering issues with decay and the source/sink term, which is now always and  *
*                   only applied to remaining mass at end of a time step.  It is no longer added to mass   *
*                   and partitioned to loss routes in the special case where initial mass goes to zero.    *
************************************************************************************************************
*/
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include<dos.h>

#include"strm1.h"
#include"rk_path.h"
#include"misc3.h"
#include "con_init.h"

//===========================================================================================================
//	SUBROUTINE RK_CALC()  ====================================================================================
//	This subroutine calculates the loss to the leaching pathway
//===========================================================================================================
void rk_calc(float *rk_inst, float *rk_path, float *dzdt, int num, Remediate *darcy_inf,
             float thickness, float new_soil_1,
             float dry_layer, float delta_dry, float start_dry,
             float elap_time, Remediate *wind_erosion,
             Remediate *water_erosion, float soil_lost, float gamma, float gamma_t)
{
  int i;
  float new_soil_23, new_time_23, new_soil_4, new_time_4, new_mass, k1[6], k2[6], k3[6], k4[6];
  float dzdt1, dzdt2, dzdt3, dzdt4, dzdt_mass;
  
  //===========================================================================================================
  //  STEP 1  --  RUNGE-KUTTA CALCULATION STEP 1 OF 4
  //===========================================================================================================
  rk_step(k1, &dzdt1, contam[num].mass, num, contam[num].mass, darcy_inf, thickness, new_soil_1,
    dry_layer, delta_dry, start_dry, elap_time,
    wind_erosion, water_erosion);
  
  rk_inst[5]=0.0;
  for (i=0;i<5;i++) {
    rk_inst[i]=k1[i];        // preserve instantaneous values 6/98 BLH
    rk_inst[5]+=rk_inst[i];
  }
  //===========================================================================================================
  //  STEP 2  --  RUNGE-KUTTA CALCULATION STEP 2 OF 4
  //===========================================================================================================
  new_mass = contam[num].mass-0.5*k1[5];
  if(new_mass < 0.0) new_mass = 0.0;
  dzdt_mass = contam[num].mass-0.5*dzdt1;
  if(dzdt_mass < 0.0) dzdt_mass = 0.0;
  
  new_time_23 = elap_time+(0.5*site.delta_t);
  
  if ( flg.no_erode || flg.pond )
    new_soil_23 = 0.0;
  else
    new_soil_23 = (soil_lost + gamma*(new_time_23 - gamma_t));
  
  rk_step(k2, &dzdt2, dzdt_mass, num, new_mass, darcy_inf, thickness, new_soil_23,
    dry_layer, delta_dry, start_dry, new_time_23,
    wind_erosion, water_erosion);
  
  //===========================================================================================================
  //  STEP 3  --  RUNGE-KUTTA CALCULATION STEP 3 OF 4
  //===========================================================================================================
  new_mass = contam[num].mass-0.5*k2[5];
  if(new_mass < 0.0)	new_mass = 0.0;
  dzdt_mass = contam[num].mass-0.5*dzdt2;
  if(dzdt_mass < 0.0)	dzdt_mass = 0.0;
  
  rk_step(k3, &dzdt3, dzdt_mass, num, new_mass, darcy_inf, thickness, new_soil_23,
    dry_layer, delta_dry, start_dry,
    new_time_23, wind_erosion, water_erosion);
  
  //===========================================================================================================
  //  STEP 4  --  RUNGE-KUTTA CALCULATION STEP 4 OF 4
  //===========================================================================================================
  new_mass = contam[num].mass - k3[5];
  if(new_mass < 0.0)	new_mass = 0.0;
  dzdt_mass = contam[num].mass-dzdt3;
  if(dzdt_mass < 0.0)	dzdt_mass = 0.0;
  
  new_time_4 = elap_time + site.delta_t;
  
  if ( flg.no_erode || flg.pond)
    new_soil_4 = 0.0;
  else
    new_soil_4 = (soil_lost + gamma*(new_time_4 - gamma_t));
  
  rk_step(k4, &dzdt4, dzdt_mass, num, new_mass, darcy_inf, thickness, new_soil_4,
    dry_layer, delta_dry, start_dry,
    new_time_4, wind_erosion, water_erosion);
  
  //===========================================================================================================
  //  STEP 5  --  CALCULATE RUNGE-KUTTA FOR EACH PATHWAY
  //===========================================================================================================
  for(i=0; i<5; i++)
    rk_path[i] = (1.0/6.0)*(k1[i] + 2.0*k2[i] + 2.0*k3[i] + k4[i]);
  
  //===========================================================================================================
  //  STEP 6  --  CALCULATE TOTAL MASS LOST, INCLUDING SOURCE/SINK TERM
  //===========================================================================================================
  rk_path[5] = (1.0/6.0)*(k1[5] + 2.0*k2[5] + 2.0*k3[5] + k4[5]) ;
  
  *dzdt =(1.0/6.0)*(dzdt1 + 2.0*dzdt2 + 2.0*dzdt3 + dzdt4);
}


//===========================================================================================================
//	SUBROUTINE RK_STEP()  ====================================================================================
//	This subroutine calculates each step for the RK Calculation methodology
//===========================================================================================================
void rk_step(float *step, float *dzdt, float dzdt_mass,
             int cont_num, float mass,
             Remediate *darcy_inf, float thick, float new_soil,
             float dry_layer, float delta_dry, float start_dry,
             float elap_time, Remediate *wind, Remediate *water)
{
  
  step[0] = site.delta_t * mass * props[cont_num].lambda;
  
  step[1] = site.delta_t * leach[cont_num].dm_dt(darcy_inf, cont_num, mass, thick, new_soil-dry_layer,
    delta_dry, start_dry, elap_time);
  
  step[2] = site.delta_t * mass * suspend[cont_num].dm_dt(wind, thick, new_soil-dry_layer, new_soil-start_dry, elap_time);
  
  step[3] = site.delta_t * mass * overland[cont_num].dm_dt(water, thick, new_soil-dry_layer, new_soil-start_dry, elap_time);
  
  //step[4] = volate[cont_num].dm_dt(mass, cont_num, elap_time, -new_soil-dry_layer, thick, dry_layer);
  step[4] = volate[cont_num].dm_dt(dzdt, dzdt_mass, mass, cont_num,
    elap_time, new_soil, thick, dry_layer, start_dry); // 4/98 BLH added function arguments
  
  // 6/98 BLH -- seems that dm/dt(vol) should also be multiplied by site.delta_t
  //                as should dzdt -- confirmed this with Gary Streile 6/2/98
  step[4] *= site.delta_t;
  *dzdt *= site.delta_t;
  
  //  Adjustment for known wind & water mass flux rates with no erosion
  if (suspend[cont_num].known_flux==1)
    step[2]=suspend[cont_num].get_value(elap_time);
  if (overland[cont_num].known_flux==1)
    step[3]=overland[cont_num].get_value(elap_time);
  
  //  Adjustment for known flux cases and waste form
  // BLH 09/97 known contaminant flux to overland from pond is now active
  if ( flg.no_erode ) // Glass, Grout, Pond, Aquifer
  {
    step[2] = 0.0;
    if (!flg.pond) step[3] = 0.0;     // BLH 09/97
  }
  //  BLH 09/97 add additional condition ' && ovl.flg == 1
  if ( flg.pond && (flg.ovl == 1) ) // known discharge
    step[3] = site.pond_out * contam[cont_num].mass;
  
  if ( flg.grt )  // No volatization from grout
    step[4] = 0.0;
  
  step[5] = step[0] + step[1] + step[2] + step[3] + step[4];
}

//===========================================================================================================
//	SUBROUTINE leach[].dm_dt()  ==============================================================================
//	This subroutine calculates the loss to the leaching pathway
//===========================================================================================================
float Pathway::dm_dt(Remediate *darcy_inf, int contnum, float mass, float thick, float net_soil_lost,
                     float delta_dry_layer, float start_dry, float elapsed_time)
{
  float limit=0.0,              // NOTE: conc_liq = site.area*(conc_liq/octanol_h2o)
    darcy,                      // NOTE: grout = sqrt( grout_diff/ M_PI)
    temp=0.0,
    remediate=0.0, c;
  // Known Flux Case, retrieves values and returns to main
  if( num_values >= 0 )
  {
    temp = leach[contnum].get_value(elapsed_time);
    return(temp);
  }
  
  
  // Glass or Grout remediated
  if(contam[contnum].grout > 0.0)
  {
    if( elapsed_time <= 0.0 )      // If the time is less than zero, add a small epsilon, to prevent
      elapsed_time = 0.03503;     // divide by zero error.  This value works nicely for a delta time
    c = sqrt(elapsed_time);        // of one year.
    remediate = contam[contnum].init_mass * contam[contnum].gdecay * contam[contnum].grout  / c;
  }
  else if(contam[contnum].glass > 0.0)
    remediate=contam[contnum].glass*contam[contnum].init_mass*exp(-props[contnum].lambda*elapsed_time);
  // glass= 1.44e-7(1/y) =5.76e-7 g/(cm2 y) * (0.6 (1/cm)) * (1/(2.40 g/cm3)) -No longer true ???
  
  darcy = darcy_inf->get_value(elapsed_time);
  
  if (net_soil_lost > (start_dry + delta_dry_layer))
    thick = (thick + start_dry)-net_soil_lost;
  else
    thick = (thick + start_dry)-delta_dry_layer;
  
  if ( props[contnum].typeflag == 1 )  // Identify organic case
  {
    if ( flg.napl )
      // Formulations Eq. 5.60   - note: area is a factor in aq_conc
      temp = darcy * contam[contnum].aq_conc;
    else      // Pond, Aquifer, Contaminated Soil with no clean layer
    {
      float div= site.theta * contam[contnum].retard * thick;
      if (div>0.0)
        temp = ((darcy * mass) / (site.theta * contam[contnum].retard * thick));
      else
        temp=0.0;
    }
  }
  else   // Inorganics and rads -- STC Document eq. 9
  {
    float div= site.theta * contam[contnum].retard * thick;
    if (div>0.0)
      temp = (mass * darcy) / (site.theta * contam[contnum].retard * thick);
    else
      temp=0.0;
  }
  
  //  Check for solubility limitations to leaching values
  limit = darcy * props[contnum].adj_aqu_sol;
  temp = min(limit, temp);     // limits leach to solubility case
  
  if(remediate > 0.0)
    temp = min(remediate, temp); //Remediated case may not give worse results than the natural system
  
  temp = max(temp,(float)0.0); // prevents return of negative values
  return(temp);
  
}

//===========================================================================================================
//	SUBROUTINE overland[].dm_dt() and suspend[].dm_dt()  =====================================================
//	This subroutine calculates both erosion loss pathways, using the separate erosion rates for each pathway
//===========================================================================================================
float Pathway::dm_dt(Remediate *w_eros, float init_waste_thick, float net_soil_lost, float soil_loss, float elapsed_time)
{ // net_soil_lost = soil_lost - depth_dry_layer
  // soil_loss = start_dry_layer - soil_lost
  float temp;
  
  if( num_values >= 0 )  // Find known flux value
    temp = w_eros->get_value(elapsed_time); // get pathway known flux
  else
  {
    if( net_soil_lost < 0.0 ) // Old spill, waste below the surface.  No mass lost to surface erosion
      temp = 0.0;
    else if ( init_waste_thick == net_soil_lost )
      temp = 1.0;
    else
      temp = w_eros->get_value(elapsed_time) / (init_waste_thick - soil_loss);
  }
  temp = max(temp,(float)0.0); // prevents return of negative values
  return(temp);
}

//===========================================================================================================
//	SUBROUTINE volate[].dm_dt()  =============================================================================
//	This subroutine calculates the mass flux due to Volatilization
//===========================================================================================================
float Pathway::dm_dt(float *dzdt, float dzdt_mass, float mass, int contnum, float elapsed_time,
                     float net_soil_loss, float thick,
                     float dry_layer, float start_dry)
{
  // dry_layer = depth_dry_layer + delta_dry_layer
  //  net_soil_loss = cumulative loss to erosion
  
  /*
  zb-z        = (thick + start_dry) - dry_layer
  z-(S+E)t    = dry_layer - net_soil_loss   = clean_layer
  */
  
  float temp = 0.0, init_conc, old_spill = 0.0, new_spill;
  float clean_layer;
  float dzdt_limit, zb_minus_z;
  float divisor;
  
  *dzdt=0.0;
  
  // Known Flux Case, retrieves values and returns to main
  if( num_values >= 0 )
  {
    temp = volate[contnum].get_value(elapsed_time);
    return(temp);
  }
  
  if ((net_soil_loss>(thick+start_dry)) || (dry_layer>(thick+start_dry)))
    return(0.0);
  
  if (net_soil_loss>dry_layer)
    // loss to erosion greater than volatilization of dry layer
    clean_layer=0.0;
  else
    // volatilization of dry layer greater than loss to erosion
    clean_layer=dry_layer-net_soil_loss;
  
  
  if ( flg.pond )
    init_conc = contam[contnum].aq_conc;
  else
    init_conc = mass / (site.area * thick);
  
  // Pond Volatization Case
  if ( flg.pond )
  {
    temp = mass/(site.theta * contam[contnum].retard * thick);
    temp = temp * contam[contnum].k_i* 3.156e7;
    temp = temp * exp(-((contam[contnum].k_i* 3.156e7)/(site.theta*thick)));// t=1
                                                                            /*  substituted with above 4/99 BLH pending review
                                                                            // Atmospheric Formulations Document Eq. 30  w/ T = 1 yr = 31557600 s
                                                                            temp = init_conc * ( 1.0 - exp((-contam[contnum].k_i * 3.156e7 /
                                                                            (thick*site.theta_pond) ))) * site.area * thick * site.theta_pond;
    */
    return(temp);
  }
  
  
  // Calculated volatization rate case
  if ( contam[contnum].vapor_conc > 0.0 )  // Contaminant is a volatile
  {
    // STRM Formulation Document Eq. 5.84 as upper limiting case
    new_spill =  (2.0*site.area*init_conc) *
      sqrt((props[contnum].vapor_diff_coef * contam[contnum].henrys_const)
      /(site.theta * contam[contnum].retard * M_PI * site.delta_t));
    
    // apply volatilization ratio for no clean layer (and bounding value) 4/98 BLH
    new_spill *= props[contnum].vol_ratio;
    
    // STRM Formulation Document Eq. 5.87
    // the rate of recession of the top boundary of the source zone ...
    //  (when a bounding value for volatilization is used)
    dzdt_limit = 2.0 * sqrt( (props[contnum].vapor_diff_coef * contam[contnum].henrys_const)
      / (site.theta * contam[contnum].retard * M_PI * site.delta_t) );
    
    // apply volatilization ratio for no clean layer case 4/98 BLH
    dzdt_limit *= props[contnum].vol_ratio;
    
    if( flg.napl )    // If NAPL present calculate old-spill for NAPL
    {
      if( clean_layer > 0.0 )
      {
        // STRM Formulation Document  Eq. 5.99
        old_spill = site.area * props[contnum].vapor_diff_coef * contam[contnum].vapor_conc
          / ( clean_layer );
        
        zb_minus_z = (thick + start_dry) - dry_layer;
        // STRM Formulation Document Eq. 5.79/5.101  -- clean layer, napl
        divisor=dzdt_mass * clean_layer;
        if (divisor>0.0)
          *dzdt = (site.area * props[contnum].vapor_diff_coef * contam[contnum].vapor_conc * zb_minus_z)
          / divisor ;
      }
      else
      {
        old_spill = new_spill;
        *dzdt = dzdt_limit;
      }
      /*  do this at the end
      temp = min(old_spill, new_spill);  // Compares the new flux to old flux returning the smaller
      temp = max(temp,(float)0.0);       // prevents return of negative values
      return(temp);
      */
    }
    else if ( props[contnum].vapor_diff_coef > 0.0 )    // if no NAPL present use modified calculations
    {                                              // for cap case turn off volatization
      if( clean_layer > 0.0 )
      {
        divisor=(site.theta * contam[contnum].retard
          * ((thick+start_dry)-dry_layer) * clean_layer);
        if (divisor>0.0)
          // STRM Formulation Document Eq. 5.100
          old_spill =  (contam[contnum].henrys_const * props[contnum].vapor_diff_coef * mass)
          //                             / (site.theta * contam[contnum].retard * thick * dry_layer);
          / divisor;
        else
          old_spill=0.0;
        
        // STRM Formulation Document Eq. 5.82/5.102  -- clean layer, no napl
        divisor=( site.theta * contam[contnum].retard * clean_layer );
        if (divisor>0.0)
          *dzdt = ( props[contnum].vapor_diff_coef * contam[contnum].henrys_const)
          / divisor;
      }
      
      else
      {
        // apply volatilization ratio for no clean layer case 4/98 BLH
        old_spill = new_spill;
        *dzdt = dzdt_limit;
      }
    }
    //  temp = min(old_spill, new_spill);  // Compares the new flux to old flux returning the smaller
    if (old_spill<new_spill)
      temp = old_spill;
    else
    {
      temp = new_spill;
      *dzdt = dzdt_limit;
    }
    temp = max(temp,(float)0.0);       // prevents return of negative values
    *dzdt = max(*dzdt,(float)0.0);
    return(temp);
  }
  // Nonvolatile contaminants
  return(0.0);
}
//===========================================================================================================
//	SUBROUTINE RECEDING()  ===================================================================================
//	This subroutine calculates the change in depth rate of the dry layer above the organic liquid
//===========================================================================================================
float recedingV2(int contnum, float waste_thick, float depth_dry_layer, float delta_dry_layer, float net_soil_loss, float mass)
{
  float temp=0.0, clean_layer;
  
  //  net_soil_loss = accumulative loss to erosion
  //  waste_thick = initial waste thick + start_dry_layer
  //  depth_dry_layer = includes start_dry_layer + sum(delta_dry_layer)*
  //            *does not include delta_dry_layer for this time step
  //  delta_dry_layer = dz_dt this time step
  
  /* change to arguments 4/98 BLH */
  if (net_soil_loss>(delta_dry_layer+depth_dry_layer))
    // loss to erosion greater than volatilization of dry layer
    clean_layer=0.0;
  else
    // volatilization of dry layer greater than loss to erosion
    clean_layer=(delta_dry_layer+depth_dry_layer)-net_soil_loss;
  
  if (! (contam[contnum].vapor_conc>0.0 && mass>0.0)) return (temp);
  
  if (clean_layer>0.0)
  {
    if (flg.napl)
    {
      waste_thick -= depth_dry_layer;
      // STRM Formulation Document Eq. 5.79
      temp = (site.area * props[contnum].vapor_diff_coef * contam[contnum].vapor_conc * waste_thick)
        / ( mass * clean_layer ) ;
    }
    else
    {
      // STRM Formulation Document Eq. 5.82  -- clean layer, no napl
      temp = ( props[contnum].vapor_diff_coef * contam[contnum].henrys_const)
        / ( site.theta * contam[contnum].retard * clean_layer );
    }
  }
  else
  {
    //  if (flg.napl)
    // Eq. 5.80
    //    temp=0.0;
    //  else
    // STRM Formulation Document Eq. 5.87
    // the rate of recession of the top boundary of the source zone ...
    //  (when a bounding value for volatilization is used)
  		temp = 2.0 * sqrt( (props[contnum].vapor_diff_coef * contam[contnum].henrys_const)
        / (site.theta * contam[contnum].retard * M_PI * site.delta_t) );
      
      // apply volatilization ratio for no clean layer case 4/98 BLH
      temp *= props[contnum].vol_ratio;
  }
  
  return(temp);
}

//===========================================================================================================
//	SUBROUTINE RECEDING()  ===================================================================================
//	This subroutine calculates the change in depth rate of the dry layer above the organic liquid
//===========================================================================================================
float recedingV1(int contnum, float waste_thick, float net_dry_layer, float mass)
{
  float temp;
  if( contam[contnum].vapor_conc > 0.0 )
  {
    if( net_dry_layer > 0.0 )
    {
      if ( flg.napl )
      {
        if ( mass > 0.0 )
          // STRM Formulation Document (dft) Eq. 5.79
          temp = (site.area * props[contnum].vapor_diff_coef * contam[contnum].vapor_conc * waste_thick)
          / ( mass * net_dry_layer ) ;
        else
          temp = 0.0;
      }
      else
        // STRM Formulation Document (dft) Eq. 5.82
        temp = ( props[contnum].vapor_diff_coef * contam[contnum].henrys_const )
        / ( site.theta * contam[contnum].retard * net_dry_layer );
    }
    else
      // STRM Formulation Document Eq. 5.87
      // the rate of recession of the top boundary of the source zone ...
      //  (when a bounding value for volatilization is used)
      temp = 2.0 * sqrt( (props[contnum].vapor_diff_coef * contam[contnum].henrys_const)
      / (site.theta * contam[contnum].retard * M_PI * site.delta_t) );
  }
  else
    temp = 0.0;

  return(temp);
}
