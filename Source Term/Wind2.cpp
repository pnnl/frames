/************************************************************************************************************
* Source Term Release Module             MODULE:  WIND2.CPP                                  VERSION: 2.00 *
*          Copyright 1996, 2003 by Battelle, Pacific Northwest National Laboratory.  All Rights Reserved   *
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
*   Last Modified:                                                                                         *
************************************************************************************************************
*                                             MODULE:  WIND.CPP                                            *
*          Function to calculate Wind Suspension Rate. Original written by James L. Stroh                  *
************************************************************************************************************
*       MODULE ORGANIZATION                                                                                *
*                                                                                                          *
*         Routine: Wind_Susp()                                                                             *
************************************************************************************************************
*       MODIFICATION HISTORY                                                                               *
*          DATE     WHO               DESCRIPTION                                                          *
*     --------     ---     ------------------------------------------------------------------------------  *
*     06/01/96     KDS     Conversion of STC to STRM in preparation of application of MEPAS QA program and *
*                              procedures.                                                                 *
*     09/11/03     JGD     Version 2
*                          Major update of all sections with new models and particle size info
*     09/12/03     JGD     Added metric conversion for paved road                                                                                                     *
*
*     01/29/04     JGD
*
*       BUG:  I have a test case where "height above ground of wind measurement (STWINDHT)" is equal to
*       "surface roughness length (STLOCSUR)" which results in division by zero when computing
*       the ean wind speed (m/s) adjusted to reference height:
*	      mean_wind_speed = mean_wind_speed * (log(ref_height/surf_rough)/log(wind_height/surf_rough));
*
*       FIX: The test case is likely using a roughness length greater than the wind height.  The more general
*       form of this equation will solve the problem -- by assuming a surface displacement height equal to the
*       roughness length.  Although I cannot think of any situation where one would want to compute at
*       wind heights that are smaller than the roughness length, this formula will allow roughness lengths
*       greater than the measurement height by assuming the measurement height is defined over the top of the
*       roughness elements.
*       Use:
*	        mean_wind_speed = mean_wind_speed * (log((ref_height+surf_rough)/surf_rough)/log((wind_height+surf_rough)/surf_rough));
*       To be consistent, we also need to change:
*         crt_wind_speed = (1.0/0.4) * threshold_frict * log( (ref_height+surf_rough)/surf_rough);
*         max_wind = max_wind * ((log(ref_height+surf_rough)/surf_rough)/log((wind_height+surf_rough)/surf_rough));
*       The physics is that we currently assume that wind speed goes to zero at the surface,
*       and the new formulation assumes the wind speed goes to zero at the height of one roughness length over the surface…..

  ************************************************************************************************************
*/

#include<stdio.h>
#include<string.h>
#include<math.h>
#include"strm1.h"
#include"fcsv.h"

float ParamFile::Wind_Susp(float *norm1, float *norm2, float *norm3 )
{
  float waste_area,          // Area of the waste zone, (cm^2)
    bulk_density,        // Bulk Density, (g/cm^3)
    threshold_frict,     // Threshold friction velocity, (m/s)
    correction,          // Non-erodible correction factor
    size_dist,           // Aggregate size distribution (STSDISTB), (mm)
//    sand_percent,        // Percent of sand in surface soil, (%)
    crt_wind_speed,      // Critical wind speed at reference height, (m/s)
    ref_height=7.0,      // Reference height above the surface, (m)
    wind_height,         // Height of the anemometer above the ground, (m)
    surf_rough,          // Surface roughness length, (m)
    freq_disturb,        // Frequency of mechanical disturbances, number/month)
    max_wind,            // Maximum wind speed at the site, (m/s)
    eros_pot,            // Erosion potential, (g/m^2)
    veg_cover_fract,     // Vegetation coverage on surface, (fraction)
    crust_fract,         // Fraction of area covered with a crust, (fraction)
    limited_fract,       // Fraction of area with limited emissions
    mean_wind_speed,     // Mean annual wind speed, (m/s)
    precip_evap_ind,     // Thornthwaite's Precipitation-Evaporation (PE) Index, (dimensionless)
    silt_percent,        // Percent of silt in road surface, (percent)
    silt_massload,       // Mass loading of silt on paved road surface, (g/m2)
    vehicle_speed,       // Mean vehicle speed, (km/h)
    vehicle_weight,      // Mean vehicle weight,      (Metric ton=10^6g)
    num_rain_days,       // Number of days with at least 0.254mm of precip. per year, (days)
    dist_travel,         // Distance of travel over contaminated surface, (km)
    num_vehicle_day,     // Average number of vehicles traveling over the contaminated surface per day
    unlim_C01,           // Class 1 emission rate for unlimited wind erosion, (g/m^2/h)
    unlim_C02,           // Class 2 emission rate for unlimited wind erosion, (g/m^2/h)
    unlim_C03,           // Class 3 emission rate for unlimited wind erosion, (g/m^2/h)
    lim_C01,             // Class 1 emission rate for limited wind erosion, (g/m^2/h)
    lim_C02,             // Class 2 emission rate for limited wind erosion, (g/m^2/h)
    lim_C03,             // Class 3 emission rate for limited wind erosion, (g/m^2/h)
    paved_C01_veh,       // Class 1 emission factor for a paved road, (g/m^2/h)
    paved_C02_veh,       // Class 2 emission factor for a paved road, (g/m^2/h)
    paved_C03_veh,       // Class 3 emission factor for a paved road, (g/m^2/h)
    unpav_C01_veh,       // Class 1 emission factor for an unpaved road, (g/m^2/h)
    unpav_C02_veh,       // Class 2 emission factor for an unpaved road, (g/m^2/h)
    unpav_C03_veh,       // Class 3 emission factor for an unpaved road, (g/m^2/h)
    totl_C01,            // Total emission for class 1, (g/m^2/h)
    totl_C02,            // Total emission for class 2, (g/m^2/h)
    totl_C03,            // Total emission for class 3, (g/m^2/h)
    totl_pm10,           // Total emission for PM10, (g/m^2/h)
    norm_C01,            // Class 1 Emission normalized to PMP10 emission rate
    norm_C02,            // Class 2 Emission normalized to PMP10 emission rate
    norm_C03,            // Class 3 Emission normalized to PMP10 emission rate
    emis_rate,           // Total emission rate for site,      (cm/yr)
    unlim_rate,          // Emission rate for unlimited wind erosion, (g/m^2/h)
    lim_rate,            // Emission rate for limited wind erosion, (g/m^2/h)
    paved_veh_rate,      // Emission factor for a paved road, (g/m^2/h)
    unpav_veh_rate,      // Emission factor for an unpaved road, (g/m^2/h)
    unpav_vel_fac,       // Factor for low wind speeds over unpaved roads
    unpav_dry_moist,     // Dry surface moisture content (STDRYSCO), (%)
    integ_funct,         // Integration function F(x), from equation 6
    x_tmp;               // X value from equation 6
  char  road_flag[8];        // Flag to designate road travel,("None", "Unpav", "Paved" , "Both"  Def="None")
  
#ifdef DEBUG
  printf("Doing WIND_SUSP\n");
#endif
  
  printf("Computing wind erosion rate\n\n");
  sls->write("Computing wind erosion rate: Wind2:ver09/12/03 ");
  
  // Read in parameters
  waste_area        = site.length*site.width;//f_read("starea",0,0,0,0,0,0);
  bulk_density      = f_read("stsbulkd",   0,0,0,0,0,0);
  correction        = f_read("stcorrsc",0,0,0,0,0,0);
  wind_height       = f_read("stwindht",0,0,0,0,0,0);
  surf_rough        = f_read("stlocsur",0,0,0,0,0,0);
  veg_cover_fract   = f_read("stvegfr",0,0,0,0,0,0);
  crust_fract       = f_read("stcrust",0,0,0,0,0,0);
  size_dist         = f_read("stsdistb",0,0,0,0,0,0);
  
  sls->writeln("waste_area",waste_area);
  sls->writeln("stsbulkd",bulk_density);
  sls->writeln("stcorrsc",correction);
  sls->writeln("stwindht",wind_height);
  sls->writeln("stlocsur",surf_rough);
  sls->writeln("stvegfr",veg_cover_fract);
  sls->writeln("stcrust",crust_fract);
  sls->writeln("stsdistb",size_dist);
  s_read(road_flag,8,"stroads",0,0,0,0,0,0);
  
  // size_dist = 0.0106*sand_percent + 0.05;  Used in former version
  
  // Equation 1, "MEPAS: Atmospheric Pathway".  Gillette et al. (1980)
  threshold_frict = correction * exp(0.4118428*log(size_dist) + 4.167173) / 100.0;
  sls->writeln("Threshold friction velocity (m/s) is: ", threshold_frict);
  
  // Equation 2, "MEPAS: Atmospheric Pathway".  Cowherd et al. (1984)
  crt_wind_speed = (1.0/0.4) * threshold_frict * log( (ref_height+surf_rough)/surf_rough);
  sls->writeln("Reference wind speed (m/s) is: ", crt_wind_speed);
  
  // define fraction of total area that has limited erosion 
  if( threshold_frict >= 0.75 )
  {
    limited_fract = 1.0;
  }
  else
  {
    limited_fract = crust_fract;
  }
  sls->writeln("Limited area fraction is: ", limited_fract); //JGD
  sls->writeln("Unlimited area fraction is: ", (1-limited_fract)); //JGD
  
  if( threshold_frict < 0.75 ) // Unlimited erosion potential
  {
    mean_wind_speed   = f_read("stavwindv",0,0,0,0,0,0);
    // Adjust the wind speed to the 7m reference height
    
    mean_wind_speed = mean_wind_speed * (log((ref_height+surf_rough)/surf_rough)/log((wind_height+surf_rough)/surf_rough));
    sls->writeln("Mean wind speed (m/s) adjusted to reference height is: ", mean_wind_speed);
    
    // Equation 6, MEPAS: Atmospheric Pathway
    x_tmp = 0.886*(crt_wind_speed/mean_wind_speed);
    if( x_tmp < 0.0 )
      integ_funct = 0.0;
    else if( x_tmp < 0.5)
      integ_funct = 1.91;
    else if( x_tmp < 1.0 )
      integ_funct = 1.9 - (x_tmp - 0.5)*0.6;
    else if (x_tmp < 2.0 )
      //          integ_funct = 1.6 - (x_tmp - 1.0)*1.3;   jgd 10/03
      integ_funct = 1.2 - (x_tmp - 1.0)*1.3;
    else  // Cowherd et. al. 1984:  Appendix B
      integ_funct = 0.18*x_tmp*(8.0*x_tmp*x_tmp + 12.0)*exp(-x_tmp*x_tmp);
    sls->writeln("Integration function is: ", integ_funct);
    
    // Equation 5, "MEPAS: Atmospheric Pathway"
    unlim_rate = (0.036*(1.0-veg_cover_fract)
      *pow(mean_wind_speed/crt_wind_speed,3.0)*integ_funct)*(1.0-crust_fract);
    unlim_C01 = 0.4* unlim_rate;
    unlim_C02 = 0.6* unlim_rate;
    unlim_C03 = 1.0* unlim_rate;
  }
  else
  {
    unlim_rate = 0.0;
    unlim_C01 = 0.0;
    unlim_C02 = 0.0;
    unlim_C03 = 0.0;
  }
  sls->writeln("PM-10 unlimited emission rate (g/(m^2 h)) is:", unlim_rate);
  sls->writeln("Class 1 emission rate (g/(m^2 h)) for unlimited erosion is:", unlim_C01);
  sls->writeln("Class 2 emission rate (g/(m^2 h)) for unlimited erosion is:", unlim_C02);
  sls->writeln("Class 3 emission rate (g/(m^2 h)) for unlimited erosion is:", unlim_C03);
  
  if (limited_fract > 0.0) // Limited Erosion Potential
  {
    freq_disturb      = f_read("stnumdis",0,0,0,0,0,0);
    max_wind          = f_read("stmaxwind", 0,0,0,0,0,0);
    precip_evap_ind   = f_read("stpei",0,0,0,0,0,0);
    // Adjust the maximum wind speed to the reference height
    max_wind = max_wind * ((log(ref_height+surf_rough)/surf_rough)/log((wind_height+surf_rough)/surf_rough));
    
    sls->writeln("stnumdis",freq_disturb);
    sls->writeln("stmaxwind",max_wind);
    sls->writeln("stpei",precip_evap_ind);
    sls->writeln("Maximum wind speed (m/s) adjusted to reference height is:",max_wind);
    
    // Equation 4, "MEPAS: Atmospheric Pathway"
    if ( max_wind > crt_wind_speed )   //JGD logic correction
    {
      eros_pot = 6.7 * (max_wind - crt_wind_speed);
    }
    else
    {
      eros_pot = 0.0;
    }
    sls->writeln("Erosion potental is: ", eros_pot);
    //Equation 3, "MEPAS: Atmospheric Pathway" based on(Cowherd 1985)
    lim_rate = ((8.3e-4)*(freq_disturb*eros_pot
      *(1.0-veg_cover_fract))/pow(precip_evap_ind/50.0,2.0))*limited_fract;
    lim_C01 = lim_rate *0.4;
    lim_C02 = lim_rate *0.6;
    lim_C03 = lim_rate *1.0;
  }
  else
  {
    lim_rate = 0.0;
    lim_C01 =  0.0;
    lim_C02 =  0.0;
    lim_C03 =  0.0;
  }
  sls->writeln("Limited PM-10 Emission rate   (g/m^2 h) is:",lim_rate);
  sls->writeln("Limited class 1 emission rate (g/m^2 h) is:",lim_C01);
  sls->writeln("Limited class 2 emission rate (g/m^2 h) is:",lim_C02);
  sls->writeln("Limited class 3 emission rate (g/m^2 h) is:",lim_C03);
  
  // Roadway emissions
  
  if( strcmpi(road_flag,"Unpav") == 0 || strcmpi(road_flag,"both") == 0)
  {
    sls->writeln("Unpav roads");
    
    silt_percent      = f_read("stsilt",1,0,0,0,0,0);
    vehicle_speed     = f_read("stvspeed",1,0,0,0,0,0);
    vehicle_weight    = f_read("stvweigh",1,0,0,0,0,0);
    num_rain_days     = f_read("stnumprcp",0,0,0,0,0,0);  // BLH 8/97 counter=0
    dist_travel       = f_read("strtdist",1,0,0,0,0,0);
    num_vehicle_day   = f_read("strtnum",1,0,0,0,0,0);
    unpav_dry_moist   = f_read("stdrysco",1,0,0,0,0,0);
    
    vehicle_weight *= 0.9071847; // convert ton to metric ton
    
    sls->writeln("stsilt, %",silt_percent);
    sls->writeln("stvspeed, km/hr",vehicle_speed); //jgd
    sls->writeln("stvweigh, metric tons",vehicle_weight); //jgd
    sls->writeln("stnumprcp",num_rain_days);
    sls->writeln("strtdist, km",dist_travel);      //jgd
    sls->writeln("strtnum",num_vehicle_day);
    sls->writeln("stdrysco",unpav_dry_moist);
    
    if (vehicle_speed < 24.14)
    {
      unpav_vel_fac= vehicle_speed/24.14;
    }
    else
    {
      unpav_vel_fac= 1.0;
    }
    sls->writeln("velocity factor",unpav_vel_fac);
    //   Equation 2, US EPA AP-42 9/98 rev, p 13.2.2-5 plus low speed guidance p 13.2.2-4
    //   with factors included to give g/m2 h
    unpav_veh_rate = 732.9*pow(silt_percent/12.,.8)*pow(vehicle_weight/2.72,.4)
      *pow(unpav_dry_moist/0.2,.3)*((365.25-num_rain_days)/365.25) * unpav_vel_fac
      *dist_travel * num_vehicle_day/24./waste_area*10000;
    unpav_C01_veh = unpav_veh_rate * 107.1 / 732.9;
    unpav_C02_veh = unpav_veh_rate * 625.8 / 732.9;
    unpav_C03_veh = 2086.*pow(silt_percent/12.,.8)*pow(vehicle_weight/2.72,.5)
      *pow(unpav_dry_moist/0.2,.4)*((365.25-num_rain_days)/365.25)*unpav_vel_fac
      *dist_travel * num_vehicle_day/24./waste_area*10000;
  }
  else
  {
    unpav_veh_rate = 0.0;
    unpav_C01_veh = 0.0;
    unpav_C02_veh = 0.0;
    unpav_C03_veh = 0.0;
  }
  sls->writeln("Unpaved road emission rate    (g/m^2/h) is:",unpav_veh_rate);
  sls->writeln("Unpaved road Class 1 emission (g/m^2/h) is:",unpav_C01_veh);
  sls->writeln("Unpaved road Class 2 emission (g/m^2/h) is:",unpav_C02_veh);
  sls->writeln("Unpaved road Class 3 emission (g/m^2/h) is:",unpav_C03_veh);
  
  if( strcmpi(road_flag,"paved") == 0 || strcmpi(road_flag,"both") == 0)
  {
    sls->writeln("paved roads");
    
    silt_massload     = f_read("stsiltl",2,0,0,0,0,0);
    vehicle_weight    = f_read("stvweigh",2,0,0,0,0,0);
    num_rain_days     = f_read("stnumprcp",0,0,0,0,0,0); // BLH 8/97 counter=0
    dist_travel       = f_read("strtdist",2,0,0,0,0,0);
    num_vehicle_day   = f_read("strtnum",2,0,0,0,0,0);
    
    vehicle_weight *= 0.9071847; // convert ton to metric ton
    
    sls->writeln("stsiltl",silt_massload);
    sls->writeln("stvweigh, metric tons",vehicle_weight);
    sls->writeln("stnumprcp",num_rain_days);
    sls->writeln("strtdist",dist_travel);
    sls->writeln("strtnum",num_vehicle_day);
    
    // Equation 2 from US EPA AP-42 10/02 rev: page 13.21-5
    paved_veh_rate = 4.6 /24. * dist_travel*num_vehicle_day/waste_area*10000.
      * pow(silt_massload/2.0,0.65)* pow(vehicle_weight/2.72,1.5)
      * ((365.-0.25*num_rain_days)/365.);  // Factor for PM-10, 4.5
    paved_C01_veh = 1.1 /4.6 * paved_veh_rate;  // Cumulative PM-2.5 factor
    paved_C02_veh = 3.5 /4.6 * paved_veh_rate;  // Cumulative PM-10 factor
    paved_C03_veh =19.4 /4.6 * paved_veh_rate;  // Cumulative PM-30 factor
  }
  else
  {
    paved_veh_rate = 0.0;
    paved_C01_veh = 0.0;
    paved_C02_veh = 0.0;
    paved_C03_veh = 0.0;
  }
  
  sls->writeln("Paved PM-10 emission   (g/m^2 h) is:", paved_veh_rate);
  sls->writeln("Paved class 1 emission (g/m^2 h) is:", paved_C01_veh);
  sls->writeln("Paved class 2 emission (g/m^2 h) is:", paved_C02_veh);
  sls->writeln("Paved class 3 emission (g/m^2 h) is:", paved_C03_veh);
  
  totl_C01 = unlim_C01+lim_C01+paved_C01_veh+unpav_C01_veh;
  totl_C02 = unlim_C02+lim_C02+paved_C02_veh+unpav_C02_veh;
  totl_C03 = unlim_C03+lim_C03+paved_C03_veh+unpav_C03_veh;
  totl_pm10 = unlim_rate+lim_rate+paved_veh_rate+unpav_veh_rate;
  norm_C01 = totl_C01/ totl_pm10;
  norm_C02 = totl_C02/ totl_pm10;
  norm_C03 = totl_C03/ totl_pm10;
  
  sls->writeln("Total class 1 emission rate (g/m2 h) is:", totl_C01);
  sls->writeln("Total class 2 emission rate (g/m2 h) is:", totl_C02);
  sls->writeln("Total class 3 emission rate (g/m2 h) is:", totl_C03);
  sls->writeln("Total PM10 emission rate    (g/m2 h) is:", totl_pm10);
  sls->writeln("Ratio class 1 / PM10 emission rates is:", norm_C01);
  sls->writeln("Ratio class 2 / PM10 emission rates is:", norm_C02);
  sls->writeln("Ratio class 3 / PM10 emission rates is:", norm_C03);
  
  // Compute total emissions from area
  // Add all rates that have been converted to g/m^2/h
  emis_rate=(paved_veh_rate+unpav_veh_rate+lim_rate+unlim_rate);
  // Convert to cm/year
  emis_rate *= (24*365.25)/(bulk_density * 10000);
  
  sls->writeln("Emission rate (cm/yr)",emis_rate);
  
#ifdef DEBUG
  printf("Returning from WIND_SUSP\n");
#endif
  
  // return( emis_rate );       /* Finally in cm/year */
  // need to return normilized rates
  //return( emis_rate,norm_C01,norm_C02,norm_C03);
  *norm1=norm_C01;
  *norm2=norm_C02;
  *norm3=norm_C03;
  return emis_rate;
}
// Implementation notes - jgd Sept 2003
// PM10 emissions are used to compute the mass lost from the area
// Larger particles are assumed to fall back on the area and not be lost
// The PM10 is the sum of classes 1 and 2
// The "norm_c0#" parameters are the factors for computation of the emissions
//     in the three classes from the PM10 emission rate
//     (for class 3 that factor will normally be greater than 1)
// That is,
// Class 1 emission rate = norm_c01 * "total emission rate"
// Class 2 emission rate = norm_c02 * "total emission rate"
// Class 3 emission rate = norm_c03 * "total emission rate"

