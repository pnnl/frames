
#pragma hdrstop
#include <condefs.h>

/* Version 1.0.1  - increased MAX_FLUX_CT from 5 to 10 - BLH Aug 29/2000
*
*
*
*  02/20/04  BLH  corrected the first comparison of loss and mass by accounting
*                 for [correctly] the contribution of the source_sink term
*/


//---------------------------------------------------------------------------
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\csv.cpp");
USEUNIT("..\Common Files\fcsv.cpp");
USEUNIT("..\Common Files\gid.cpp");
USEUNIT("..\COMMON FILES\series.cpp");
USEUNIT("..\Common Files\conClass.cpp");
USEUNIT("Rk_path.cpp");
USEUNIT("Con_init.cpp");
USEUNIT("Fileprep.cpp");
USEUNIT("Initial4.cpp");
USEUNIT("Misc3.cpp");
USEUNIT("Param3.cpp");
USEUNIT("Penman2.cpp");
USEUNIT("Report.cpp");
USEUNIT("Blaney2.cpp");
USEUNIT("Sediment.cpp");
USEUNIT("Val_read.cpp");
USEUNIT("Water.cpp");
USEUNIT("Wind2.cpp");
USEUNIT("Wtr-bal2.cpp");
USEUNIT("sls.cpp");
USEUNIT("Recopy4.cpp");
USERES("STRM1.res");
//---------------------------------------------------------------------------
#pragma argsused

/************************************************************************************************************
* Source Term Release Module                MODULE:  STRM1.CPP                                VERSION: 1.00 *
*           Copyright 1996 by Battelle Pacific Northwest National Laboratory.  All Rights Reserved          *
*************************************************************************************************************
*                                            PROGRAM:  STRM                                                 *
* STRM Source Term Releas Calculation Module for computation of releases from a contaminated source zone.   *
* This model addresses mass balance in the contaminated source calculating releases to the environment via  *
* numerous pathways including:  decay, leaching, volatization, overland runoff, and wind suspension. Fluxes *
* are provided to the .SRC file for use by the transport codes, or to the .WFF, .ARR, and .SCF files for    *
* use in the Multi-Media Framework currently under development.                                             *
*                                                                                                           *
*      Written by:  Keith D. Shields                                                                        *
*                   Pacific Northwest National Laboratory                                                   *
*                   P.O. Box 999                                                                            *
*                   Richland, WA  99352                                                                     *
*                                                                                                           *
*         Created:  06/01/96                                                                                *
*   Last Modified:  10/31/96  -- KDS                                                                        *
*                   03/--/97  -- BLH
*                                                                                                           *
*************************************************************************************************************
*******  This code incorporates and expands upon the STC Source Term Mass Balance Code developed      *******
*******  and written by James L. Stroh for RAAS Ver 1 PNL 1995                                        *******
*************************************************************************************************************
*                                           MODULE:  STRM1.CPP                                              *
* STRM1 is the main module of the program STRM!.exe.  The overall control of the program is found in this   *
* module.  This version was developed for the Multi-Media Modeling Framework project.  This version was     *
* developed to be compatible with the STC.exe code.  Compile time definitions will create the different     *
* versions of the codes.                                                                                    *
*************************************************************************************************************
*       MODULE ORGANIZATION                                                                                 *
*  Routine:  Main()                                                                                         *
*************************************************************************************************************
*       MODIFICATION HISTORY                                                                                *
*    DATE	WHO			DESCRIPTION                                                                         *
*	--------	---	----------------------------------------------------------------------------------------  *
*	06/01/96	KDS	Conversion of STC to STRM in preparation of application of MEPAS QA program and           *
*						procedures.                                                                               *
*	06/13/96	KDS	Conversion of cllambda from a read in value to calculated value for the framework mode.   *
*                 Conversion of clspecact from a read in value to calculated value for the framework mode.  *
*                 Incorporation of Site Information Data.  Moved file preparation activities to separate    *
*						subroutines in another module (fileprep.cpp).                                             *
*	10/28/96	KDS	Movement of contaminant initialization activities to new module (con_init.cpp).           *
* 02/  /98  BLH corrected conditional computation of retardation factor based on media
* 04/  /99  BLH use Henrys Const(org) to calc_vapor_cons
*************************************************************************************************************
*/

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include<new.h>

#include"strm1.h"
#include"fcsv.h"
#include"rk_path.h"
#include"fileprep.h"
#include"recopy2.h"
#include"report.h"
#include"misc3.h"
#include"gid.h"
#include"con_init.h"
#include "sls.h"

#define RK_DEC 0
#define RK_INF 1
#define RK_SUS 2
#define RK_OVL 3
#define RK_VOL 4
#define RK_SNK 5

void write_sdl(int delta_t, int count);

int	count=0,
e_time=0,
i=0,
j=0,
rec=0,
t_steps=0,
*child,
*lst_time,
*parent;

long int records=0;

float gamma=0.0,
gamma_t=0.0,
min_mass=0.0,
min_waste_fract=0.0,
new_soil_1=0.0,
new_soil_23=0.0,
new_soil_4=0.0,
org_thick=0.0,
org_thick_new=0.0,
prt_time=0.0,
soil_lost=0.0,
start_dry_layer=0.0,
susp_sol=0.0,
temp_coef=0.0,
temp_new1=0.0,
temp_new2=0.0,
vol_delta=0.0,
wet_bulkd=0.0,
gammaWind=0.0,
gammaWater=0.0,
srcsnk=0.0;

unsigned int aveper=0,
count_contam=0,
count_iter=0,
decay_count=0,
ef_open=0,
ewnum=0,
masschain=0,
max_decay=0,
print_count=0,
progname=0,
remed_state=0,
state=0,
vol_count=0;
float k0[6]={0.0,0.0,0.0,0.0,0.0,0.0},
kr[5]={0.0,0.0,0.0,0.0,0.0},
org_moles=0.0,
rk_average[6]={0.0,0.0,0.0,0.0,0.0,0.0},
*aqueous_sol,
*delta_dry_layer,
*depth_dry_layer,
*org_liq_conc,
*sat_vapor_conc,
*waste_thick,
net_soil_loss=0.0;

Pathway *k_d,
*leach,
*overland,
*source_sink,
*suspend,
*volate;

contaminant *contam;

properties *props;

static char buf[MAXPATH];

Remediate air_space,
darcy_inf,
glass_series,
init_dry_layer,
surf_vol,
total_porosity,
water_eros,
wind_eros,
sed_load,
over_flow;

fcsv	sdl, wrn, err;
SLS *sls;

ParamFile data, *data_file;

SiteDef site;

ContFlags flg;

int main(int argc, char **argv)
{
  printf("\nSource Term Release Module");   // Print out version
  printf("Pacific Northwest National Laboratory\n");
  
  //===========================================================================================================
  //-----------------------------------------------------------------------------------------------------------
  //  STEP A -- OPEN PARAMETER FILE AND CREATE OUTPUT FILE
  //-----------------------------------------------------------------------------------------------------------
  //===========================================================================================================
  
  fileprep(argc, argv);
  
  //===========================================================================================================
  //-----------------------------------------------------------------------------------------------------------
  //  STEP B -- LOAD THE INITIAL DATA FOR THIS RUN
  //-----------------------------------------------------------------------------------------------------------
  //===========================================================================================================
  
  printf("\nPerforming analysis on run: %s %s\n\n", site.gid, site.name);
  data_file=&data;
  
  //site = new SiteDef;
  //flg = new ContFlags;
  
  // READ IN CONFIGURATION FLAGS
  flg.inf  = data.i_read("stinf_op",0,0,0,0,0,0);
  flg.ovl  = data.i_read("stovl_op",0,0,0,0,0,0);
  flg.susp = data.i_read("stsus_op",0,0,0,0,0,0);
  flg.vol  = data.i_read("stvol_op",0,0,0,0,0,0);
  flg.src  = data.i_read("stsrc_op",0,0,0,0,0,0);
  // decay now determined by STGHALF contaminant parameter 7/99 BLH
  flg.decay = 1;  // flg.decay = data.i_read("stdec_op",0,0,0,0,0,0);
  flg.annual = 1; // flg.annual = data.i_read("stannual",0,0,0,0,0,0);
  
  // SCENARIO DATA
  site.num_contam      = data.i_read("numcon",0,0,0,0,0,0);
  site.numtotcon = site.num_contam;
  for(i=0; i < site.num_contam; i++)
  {
    site.numtotcon += data.i_read("NDS",i+1,0,0,0,0,0);
  }
  site.max_time			= data.f_read("stmaxtime",0,0,0,0,0,0);
  site.delta_t		= data.f_read("stdelta_t",0,0,0,0,0,0);
  if (MAX_REPORT_TIME > (site.max_time/site.delta_t))
    t_steps = site.max_time/site.delta_t;
  else
    t_steps = MAX_REPORT_TIME-1;
  
  site.temp = data.f_read("stavtemp",0,0,0,0,0,0);
  site.thick = data.f_read("stthick",0,0,0,0,0,0);
  
  data.s_read(site.media_type, BUF_SIZE, "stmedia", 0,0,0,0,0,0);
  if (strcmpi(site.media_type,"surface water"))    // 3/97 BLH
    site.wz_bulkd = data.f_read("stzbulkd",0,0,0,0,0,0);
  
  // 12-10-2004 This formula always generates 1.0 which is the default
  // value for site.theta_pond
  //   site.theta_pond = 1.0 - (susp_sol/site.wz_bulkd); // 4/99 BLH
  
  min_waste_fract = data.f_read("stminwst",0,0,0,0,0,0);
  
  //Waste Zone Data
  site.length		= data.f_read("stlength", 0,0,0,0,0,0);
  site.width		= data.f_read("stwidth",  0,0,0,0,0,0);
  site.num_media      = 1;
  site.area = site.length * site.width;
  
  // correct for orientation of aquifer dimensions
  site.width		= data.f_read("stwidth",  0,0,0,0,0,0);
  if (0==strcmpi(site.media_type,"aquifer"))
  {
    site.length		= data.f_read("stthick", 0,0,0,0,0,0);
    site.thick = data.f_read("stlength",0,0,0,0,0,0);
    site.theta = data.f_read("steffpor",0,0,0,0,0,0);
  }
  else
  {
    site.length		= data.f_read("stlength", 0,0,0,0,0,0);
    site.thick = data.f_read("stthick",0,0,0,0,0,0);
    if (0==strcmpi(site.media_type,"surface water"))    // 3/97 BLH
    {
      site.theta = 1.0;
      susp_sol = data.f_read("stssol",0,0,0,0,0,0);
      // 12-10-2004 KJC site.theta in this case is based on total weight of sediment
      // over total volume of liquid sample (stsol).  The 2.68 g/cm^3 is the particle
      // density that is typically used by "Sedimentation Engineers" ref Vanoni 1975
      site.theta_pond = 1.0 - (susp_sol/2.68);
      // 4/99 BLH
      site.theta = site.theta_pond; // 4/99 BLH
                                    /*  9/97 BLH - per discussions with JPM/KJC
                                    if (flg.ovl != 0)
                                    site.pond_out = data.f_read("stovlflx",0,0,0,0,0,0);
                                    else
      */
      if (flg.ovl == KNOWN_REL) // known discharge
        site.pond_out=data.f_read("stovlflx",0,0,0,0,0,0);
      else
        site.pond_out=0.0;
    }
    else
      site.theta = data.f_read("stmoistc",0,0,0,0,0,0);
  }
  site.area = site.length * site.width;
  
  data.s_read(site.arel_type,BUF_SIZE,"statype",0,0,0,0,0,0);
  if (!strcmpi("area",site.arel_type))
  {
    site.rel_hght=0.;
    site.struc_hght=0.;
    site.exit_vel=0.;
    site.exit_temp=site.temp;
  }
  else
  {
    site.rel_hght = data.f_read("strelht",0,0,0,0,0,0);
    site.struc_hght = data.f_read("ststrucht",0,0,0,0,0,0);
    site.exit_vel = data.f_read("stexvel",0,0,0,0,0,0);
    site.exit_temp = data.f_read("stextemp",0,0,0,0,0,0);
  }
  
  //sls = new SLS(err, site.run);  done in fileprep
  
  //	INITIALIZATION OF ARRAYED VARIABLES
  contam = new contaminant[site.numtotcon+1];
  props = new properties[site.numtotcon+1];
  
  parent = new int[site.numtotcon+1];
  child = new int[site.numtotcon+1];
  lst_time = new int[site.numtotcon+1];
  
  org_liq_conc = new float[site.numtotcon+1];
  sat_vapor_conc = new float[site.numtotcon+1];
  waste_thick = new float[site.numtotcon+1];
  depth_dry_layer = new float[site.numtotcon+1];
  delta_dry_layer = new float[site.numtotcon+1];
  aqueous_sol = new float[site.numtotcon+1];
  
  leach = new Pathway[site.numtotcon+1];
  overland = new Pathway[site.numtotcon+1];
  suspend = new Pathway[site.numtotcon+1];
  volate = new Pathway[site.numtotcon+1];
  k_d = new Pathway[site.numtotcon+1];
  source_sink = new Pathway[site.numtotcon+1];
  
  //	Read in any Known Fluxes, Initialize or Calculated Parameters
  darcy_inf.init_param(data,"stdarcy",0,0,0,0,0,INFIL);
  wind_eros.init_param(data,"stwind",0,0,0,0,0,WIND);
  water_eros.init_param(data,"stwatr",0,0,0,0,0,WATER);
  
  if (0==strcmpi(site.media_type,"surface water"))
  {
    total_porosity.set_series(1,1.0);
    air_space.set_series(1,0.0);
  }
  else
  {
   	total_porosity.set_series(1,data.f_read("sttotpor",1,0,0,0,0,0));
    if (0==strcmpi(site.media_type,"aquifer"))
      air_space.set_series(1,0.0); // BLH 7/99
    else
      air_space.set_series(1,data.f_read("stairspc",1,0,0,0,0,0));
  }
  if (flg.ovl == KNOWN_REL)     // known discharge/erosion
  {
    sed_load.set_series(2,0.0);
    over_flow.set_series(1,data.f_read("stovlflo",1,0,0,0,0,0));
  }
  else if (flg.ovl == KNOWN_FLX) // known flux
  {
    sed_load.set_series(1,data.f_read("stsedlod",1,0,0,0,0,0));
    over_flow.set_series(1,data.f_read("stovlflo",1,0,0,0,0,0));
  }
  init_dry_layer.set_series(1,data.f_read("stclean",1,0,0,0,0,0));
  glass_series.set_series(1,0.0);
  surf_vol.set_series(1,0.0);

  site.fin_flux=darcy_inf.get_value(site.max_time);
  site.ovl_flux=over_flow.get_value(site.max_time);

  //	===========================================================================================================
  //	STEP B.2  --  WRITE INITIAL CONDITIONS OF RUN TO THE LOG FILE
  //	===========================================================================================================

  sls->analysis();

  sdl.write("Contam");
  sdl.write("Parent");
  sdl.write("Time");
  sdl.write("Waste");
  sdl.write("Infiltration");
  sdl.write("Sediment");
  sdl.write("Overland");
  sdl.write("Mass in waste unit(g)");
  sdl.write("Mass lost to pathway (g)");
  sdl.writeln();
  sdl.write(" ");
  sdl.write(" ");
  sdl.write("(y)");
  sdl.write("Thick(cm)");
  sdl.write("(cm/y)");
  sdl.write("(kg/yr)");
  sdl.write("(m^3/yr)");
  sdl.write("Mass remaining");
  sdl.write("Decay (inst)");
  sdl.write("Leaching (inst)");
  sdl.write("Wind (inst)");
  sdl.write("Water (inst)");
  sdl.write("Volatilize (inst)");
  sdl.write("Known Source/Sink");
  sdl.write("state");
  // for debugging
  sdl.write("napl");
  sdl.write("%mass lost");
  sdl.write("erosion");
  sdl.write("(thick+start_dry)-erosion");
  sdl.write("depth_dry_layer");
  sdl.write("delta_dry_layer");
  sdl.write("vol_delta");
  sdl.write("dz/dt");
  sdl.write("Decay (avg)");
  sdl.write("Leaching (avg)");
  sdl.write("Wind (avg)");
  sdl.write("Water (avg)");
  sdl.write("Volatilize (avg)");
  
  sdl.writeln();
  
  max_decay = 0;
  org_moles = 0.0;
  site.org_moles_new = 0.0;
  //	==========================================================================================================
  //	----------------------------------------------------------------------------------------------------------
  //	STEP C -- INITIALIZE CONTAMINANT DATA, ORGANICS, INORGANICS, AND RADIONUCLIDES
  //	----------------------------------------------------------------------------------------------------------
  //	==========================================================================================================
  
  con_init(site.num_contam);
  
  sls->contaminants();
  
  InitReport(*data_file, site.num_contam);
  //	==========================================================================================================
  //	STEP D  --  PRINT CONTAMINANT LIST TO THE LOG FILE
  //	==========================================================================================================
  
  
  //	==========================================================================================================
  //	----------------------------------------------------------------------------------------------------------
  //	STEP E  --	CONTAMINANT ANALYSIS LOOP, PERFORMS CALCULATIONS ON ALL CONTAMINANTS FOR EACH
  //					TIME STEP THEN PROCEEDS TO THE NEXT TIME STEP
  //	----------------------------------------------------------------------------------------------------------
  //	==========================================================================================================
  //	==========================================================================================================
  //	STEP E.1  --  INITIALIZE PARAMETERS FOR ELAPSED TIME=0 CALCULATIONS
  //	==========================================================================================================
  print_count = 0;
  count_iter=1;
  
  soil_lost= 0.0;
  gamma_t  = 0.0;
  temp_coef = 0.0;
  
  start_dry_layer = init_dry_layer.get_value(0.0);
#ifdef DEBUG
  printf("Initializing release properties\n");
#endif
  for (i=0; i < site.numtotcon; i++ )   // initialize variables
  {
    if (0==strcmpi(site.media_type,"surface water"))    // 3/97 BLH
      init_relprop(i,0.0,susp_sol);
    else
      init_relprop(i,0.0,site.wz_bulkd);
    
    delta_dry_layer[i]=0.0;
    depth_dry_layer[i] = start_dry_layer;
    lst_time[i]=0;
    contam[i].min_mass = contam[i].init_mass * min_waste_fract;
    if (contam[i].min_mass < props[i].molecule)
      contam[i].min_mass = props[i].molecule;    // 4/98 BLH
  }
  
  //	==========================================================================================================
  //	STEP E.2  --  PREPARING COLUMN HEADERS FOR LOG FILE OF ALL DATA, TEMP OR VERBOSE LOG FILE
  //	==========================================================================================================
  records = 2;
#ifdef DEBUG
  printf("Starting Analysis\n");
#endif
  for (e_time=0; e_time < t_steps; e_time++)
  {
    //		printf("depth dry layer,  %10.3e\n",depth_dry_layer[1]);
    org_thick = org_thick_new;
    
    if (e_time>90)
      org_thick = org_thick_new;  // debug break point
    
    //		=======================================================================================================
    //		STEP E.3  --  UPDATE MOLE FRACTION, VAPOR CONCENTRATION, AND ORGANIC SOLUBILITY CALCULATIONS
    //		=======================================================================================================
    //		SET CASE FLAGS FOR POND, AQUIFER, GLASS, OR GROUT
    //		Set ponded case flag
    if ( (total_porosity.get_value((e_time*site.delta_t)) == 1.0 )
      && air_space.get_value((e_time*site.delta_t)) == 0.0  )
      flg.pond = 1; // always the case for "surface water"
    else
      flg.pond = 0;
    //		Set aquifer case flag
    if (0==strcmpi(site.media_type,"aquifer"))   // blh
      flg.aquifer = 1;
    else
      flg.aquifer = 0;
    //		Set glass case flag
    if (glass_series.get_value(e_time*site.delta_t) > 0.0)
      flg.vit = 1;
    else
      flg.vit = 0;
    //		Set grout case flag
    if (surf_vol.get_value(e_time*site.delta_t) > 0.0 )
      flg.grt = 1;
    else
      flg.grt = 0;
    //		Set no erosion case flag
    if ( flg.vit || flg.grt || flg.aquifer || flg.pond )
      flg.no_erode = 1;
    else
      flg.no_erode = 0;
    
    org_moles = site.org_moles_new;
    site.org_moles_new = 0.0;
#ifdef DEBUG
    printf("Prepping Organic Data-- %d -- %10.3e \n",site.numtotcon,org_moles);
    getc(stdin);
#endif
    
    for (i=0; i < site.numtotcon; i++ )
    {
#ifdef DEBUG
      printf("Doing Contam -- %d -- %d \n",i,props[i].typeflag);
      getc(stdin);
#endif
      if(( props[i].typeflag == 1) && ( org_moles > 0.0 ))				// BLH skip div by zero
      {
#ifdef DEBUG
        printf("Contaminant --%d\n",i);
        getc(stdin);
#endif
        /*      moved out of this loop - contains a contaminant loop itself 6/98 BLH
        flg.napl = NAPL_state(org_moles, gamma, e_time);
        */
        
        depth_dry_layer[i] = start_dry_layer + delta_dry_layer[i];
        
        temp_coef = pow(air_space.get_value(e_time*site.delta_t),(10.0/3.0))
										/ (total_porosity.get_value(e_time*site.delta_t)
                    *total_porosity.get_value(e_time*site.delta_t));
        props[i].vapor_diff_coef = contam[i].air_vp_diff*temp_coef;
        contam[i].mole_fract = (contam[i].mass/props[i].mole_wt)/org_moles;
      }
    }
#ifdef DEBUG
    printf("Starting Contaminants\n");
    getc(stdin);
#endif
    
    if (e_time==0)
    {
      new_soil_4=0.0;
      state=0;
      for (count=site.numtotcon-1;count >=0; count--)
      {
        for (i=0;i<5;i++)
          contam[count].rk_pathway[i]=0.0;
        for (i=0;i<5;i++)
          contam[count].rk_instant[i]=0.0;
        contam[count].dzdt=0.0;
        write_sdl(0,count); // write time 0 record 4/98 BLH
      }
    }
    
    flg.napl = NAPL_state(org_moles, gamma, e_time);
    
    for (count=site.numtotcon-1; count >=0; count--)
    {
      state = 0;
      vol_delta = 0.0;
      min_mass = contam[count].min_mass;
      
      srcsnk = source_sink[count].get_value(e_time*site.delta_t);

      //			====================================================================================================
      //			STEP E.4  --  VERIFY IF ANY CONTAMINANT REMAINS AT THE START OF THE TIME STEP
      //			====================================================================================================
#ifdef DEBUG
      printf("Step E.4\n");
#endif
      
      if ((contam[count].mass > min_mass)
        && ((waste_thick[count]+start_dry_layer)>soil_lost)
        && ((waste_thick[count]+start_dry_layer)>depth_dry_layer[count]))
      {
        if ( flg.no_erode )
        {
          gamma = 0.0;
          new_soil_1 = 0.0;
        }
        else
        {
          //	Initialize  temporary variable
          gammaWind = wind_eros.get_value((e_time*site.delta_t));
          gammaWater = water_eros.get_value(e_time*site.delta_t);
          gamma = gammaWind + gammaWater;
          new_soil_1 = (soil_lost + gamma*((e_time*site.delta_t) - gamma_t));
        }
        
        //	  	init_relprop(count,(e_time*site.delta_t),site.wz_bulkd);
        if (flg.pond)    // 4/99 BLH
          init_relprop(count,(e_time*site.delta_t),susp_sol);
        else
          init_relprop(count,(e_time*site.delta_t),site.wz_bulkd);
        
        if (contam[count].mole_fract > 0.0)     // when all mass is gone divide by 0 check
        {
          if (sat_vapor_conc[count]>0.0)
            contam[count].vapor_conc = calc_vapor_cons(contam[count].henrys_org, // 4/99 BLH
            sat_vapor_conc[count], contam[count].mole_fract,
            contam[count].liq_dens);
          else
            contam[count].vapor_conc=0.0;
          
          if (flg.napl)
            // equ 2.16
            contam[count].aq_conc =  props[count].adj_aqu_sol * contam[count].mole_fract;
          else
            // equ 4.9
            contam[count].aq_conc = contam[count].mass
            /(site.theta*contam[count].retard*site.area*waste_thick[count]);
        }
        else
        {
          contam[count].vapor_conc = 0.0;
          contam[count].aq_conc = props[count].adj_aqu_sol;
        }
        
        prt_time = (e_time*site.delta_t)+site.delta_t;
        wet_bulkd = site.wz_bulkd + site.theta;
        
        //				=================================================================================================
        //				STEP E.5  --  CHECK TO SEE IF ALL CONTAMINANT GOES AWAY DURING THIS TIME STEP
        //				=================================================================================================
#ifdef DEBUG
        printf("Step E.5\n");
#endif
        // compute loss per route: decay, leach, susp, ovl, vol, k0[5] = sum of all routes
        rk_step(k0, &contam[count].dzdt, contam[count].mass, count, contam[count].mass, &darcy_inf, waste_thick[count], new_soil_1,
          depth_dry_layer[count], delta_dry_layer[count], start_dry_layer,
          (e_time*site.delta_t), &wind_eros, &water_eros) ;
        
        //				=================================================================================================
        //				STEP E.6  --	IF ALL CONTAMINANT DOES NOT GO AWAY DURING THIS TIME STEP, PERFORM RUNGE-KUTTA
        //									METHODOLOGY, IF ALL MASS DOES GO AWAY PARTITION THE MASS TO THE
        //									PATHWAYS FOR PRINTING AND SET THE REMAINING MASS TO ZERO
        //				=================================================================================================
        // determine (apriori) whether all possible mass will be lost this time step
        //              if src_snk is >0 then it contributes to the mass
        //              if src_snk is <0 then it contributes to the loss
        if (0.0 <= ((contam[count].mass-k0[5])+srcsnk))
        {
          //					==============================================================================================
          //					STEP E.6.a  --  UPDATE VARIABLES FOR USE IN RUNGE-KUTTA CALCULATIONS
          //					==============================================================================================
#ifdef DEBUG
          printf("Step E.6.a\n");
#endif
          temp_new1 = gammaWater = water_eros.get_value(e_time*site.delta_t);
          temp_new2  = gammaWind = wind_eros.get_value(e_time*site.delta_t);
          if( water_eros.changed || wind_eros.changed )
          {
            soil_lost += gamma * (((e_time*site.delta_t) - site.delta_t) - gamma_t);
            gamma_t = ((e_time*site.delta_t) - site.delta_t);
            gamma = temp_new1 + temp_new2;  /* Recalculate temporary variable */
            water_eros.changed = 0;
            wind_eros.changed  = 0;
          }
          
          // this condition is suspect !!! 4/98 BLH but doesn't really matter  ????
          if( (soil_lost + gamma*((e_time*site.delta_t)+site.delta_t - gamma_t)) >=
            (waste_thick[count] + depth_dry_layer[count]) )
          {
            if ( flg.no_erode || flg.pond )
              state = 0;
          }
          
          if( contam[count].vapor_conc > 0.0 )     // If parent is a volatile
          {
            temp_new1 = air_space.get_value(e_time*site.delta_t);
            temp_new2 = total_porosity.get_value(e_time*site.delta_t);
            if( air_space.changed || total_porosity.changed )
            {         //calculate vapor_diff_coef only if values have changed
              temp_coef = pow(temp_new1,(10.0/3.0)) / (temp_new2*temp_new2);
              air_space.changed = 0;
              total_porosity.changed = 0;
            }
          }
          //	STRM Formulation Document (dft) Eq. 2.5
          //    BLH 2/98 compute retardation based on media
          if (0==strcmpi(site.media_type,"aquifer"))
            temp_new2 = total_porosity.get_value(e_time*site.delta_t) / site.theta;
          else
            temp_new2 = 1.0;
          
          if (0==strcmpi(site.media_type,"surface water"))    // 4/99 BLH
            contam[count].retard = temp_new2 + ( (air_space.get_value(e_time*site.delta_t)
            *contam[count].henrys_const) +
  						    (susp_sol * k_d[count].get_value(e_time*site.delta_t)) ) / site.theta ;
          else
            contam[count].retard = temp_new2 + ( (air_space.get_value(e_time*site.delta_t)
            *contam[count].henrys_const) +
  						    (site.wz_bulkd * k_d[count].get_value(e_time*site.delta_t)) ) / site.theta ;
          
          props[count].vapor_diff_coef = contam[count].air_vp_diff*temp_coef;
          contam[count].grout = props[count].grout_coef * surf_vol.get_value(e_time*site.delta_t);
          
          // rk_instant does not (ever) include source_sink contribution
          // rk_calc adds contribution of source_sink term after executing the 4-stage RK method
          // for all other routes, returning the result in rk_pathway
          rk_calc(contam[count].rk_instant, contam[count].rk_pathway, &contam[count].dzdt, count, &darcy_inf, waste_thick[count], new_soil_1,
            depth_dry_layer[count], delta_dry_layer[count], start_dry_layer,
            (e_time*site.delta_t), &wind_eros, &water_eros,
            soil_lost, gamma, gamma_t);
          
          contam[count].vol_mass = contam[count].mass;
          
          // apply rk_pathway to mass  - rk_pathway does NOT include source_sink contribution
          //  note however that the first statement in apply_dmdt recalculates and eliminates source_sink
          //  adding it back in at the end...
          
          // NOTE: 8/19/2004 BLH -- only the instaneous solution is being written to the system output -
          // Given that fact and problems with secondary source inflow it is clear that the instaneous
          // solution should be the one that is applied to the mass (which is figured in the function apply_dmdt).
          // This may explain why the outputs often contain a small inexplicable variance between losses and mass as
          // output to sdl and wff, etc.
          // On further review, because the function apply_dmdt is only called here, I have elected to insert
          // the logic here for clarity and simplification.  The function will be removed from rk_path.cpp.
          
          // ============= apply_dmdt ==================================================================================
          // apply_dmdt(&state, count, contam[count].rk_pathway, parent, child, lst_time, e_time);
          //                 void apply_dmdt(unsigned int *state, int count, float *rk_pathway,int *parent, int *child, int *lst_time, int e_time)
          //                 {
          // Check to see whether total loss exceeds existing mass - if so apply ratio of loss routes to mass
          if (contam[count].rk_instant[5] >= contam[count].mass )
            for (i=0;i<5;i++)
              contam[count].rk_instant[i] = contam[count].mass * (contam[count].rk_instant[i]/contam[count].rk_instant[5]);
            if (contam[count].rk_pathway[5] >= contam[count].mass )
              for (i=0;i<5;i++)
                contam[count].rk_pathway[i] = contam[count].mass * (contam[count].rk_pathway[i]/contam[count].rk_pathway[5]);
              float *pathway_losses=contam[count].rk_instant; // instantaneous rather than average is passed through the system
              if (pathway_losses[5]>=contam[count].mass)
              {
                int masschain;
                contam[count].mass = 0.0;
                masschain = chain_mass(parent[count],count,e_time*site.delta_t);
                if (masschain == 0 )
                  state = 2;
                if(lst_time[count] == 0)
                  if ( masschain == 0 )
                    sls->all_gone(count,((e_time*site.delta_t)+site.delta_t));
              }
              else
                contam[count].mass -= pathway_losses[5];
              if (child[count] != count )
                contam[child[count]].mass += pathway_losses[0]; // decay
              //                 }
              // ============= apply_dmdt ==================================================================================
              
              //					==============================================================================================
              //					STEP F  --  RUNGE-KUTTA SOLUTION FOR RECEDING OF TOP OF WASTE LAYER
              //					=============================================================================================
              
#ifdef DEBUG
              printf("Step F\n");
#endif
              if ( flg.no_erode || flg.pond)
              {
                new_soil_23 = 0.0;
                new_soil_4 = 0.0;
              }
              else
              {
                new_soil_23 =  (soil_lost + gamma*((e_time*site.delta_t)+(0.5*site.delta_t) - gamma_t));
                new_soil_4 = (soil_lost + gamma*((e_time*site.delta_t)+site.delta_t - gamma_t));
              }
              
              if( contam[count].vapor_conc > 0.0 && flg.vol!=0 ) //  && suspend[count].known_flux == 0 ) BLH 4/98
              {
#ifdef DEBUG
                printf("Doing Delta Z\n");
#endif
                if ( flg.pond == 0 && flg.aquifer == 0 )
                {
                /*
                moved the calculation of dz_dt to dm_dt(vol) to allow for
                considering the bounding case
                  */
                  if (soil_lost>(depth_dry_layer[count]+contam[count].dzdt))
                    contam[count].dzdt=0.0;
                  delta_dry_layer[count] += contam[count].dzdt;
                  
                  // STRM Formulation Document Eq. 5.80
                  vol_delta += contam[count].mole_fract * delta_dry_layer[count];
                }
              }
              
        }
        else
          //					==============================================================================================
          //   				STEP E.7  --	ALL CONTAMINANT MASS GOES AWAY DURING THIS TIME STEP.  THE MASS IS PARTITIONED
          //                				TO THE DIFFERENT PATHWAYS.
          //					==============================================================================================
        {
#ifdef DEBUG
          printf("Step E.7\n");
#endif
          for (j=0;j<5;j++)   // 0=decay, 1=leaching, 2=suspension, 3=overland , 4=volatization,
          {
            // remove division by site.delta_t - already accounted for in dm_dt   4/98 BLH
    				    contam[count].rk_pathway[j] = ((k0[j]/k0[5])*contam[count].mass);  //  mass loss
                contam[count].rk_instant[j] = contam[count].rk_pathway[j];
          }
          contam[count].mass=0.0;
          masschain = chain_mass(parent[count],count,e_time*site.delta_t);
          if (child[count] != count )
            contam[child[count]].mass += contam[count].rk_pathway[0];
          else if (child[count] == count)
          {
            if ( masschain == 0 )
            {
              state = 2;  // sets the end flag for parent or non-rads
              if(lst_time[count] == 0)
                sls->all_gone(count,((e_time*site.delta_t)+site.delta_t));
            }
          }
          if ( masschain == 0)
          { // sets end flag for progeny if the previous members of decay chain also have 0 mass
            state = 2;
            if(lst_time[count] == 0)
              sls->all_gone(count, ((e_time*site.delta_t)+site.delta_t));
          }
        }
      }
      //			====================================================================================================
      //  		STEP E.8  --  IF THERE IS [INSUFFICIENT] CONTAMINANT MASS FOR THIS TIME STEP, SET ALL PATHWAYS TO ZERO AND
      //                SET THE MASS FOR THE NEXT TIME STEP TO ZERO
      //			====================================================================================================
      else
      {
#ifdef DEBUG
        printf("Step E.8\n");
#endif
        masschain = chain_mass(parent[count],count,e_time*site.delta_t);
        contam[count].mass = 0.0;
        for(j=0;j<5;j++)         // Pathway fluxes set to zero because no remaining mass.
        {
          contam[count].rk_instant[j] = 0.0;  // Unless daughter product
          contam[count].rk_pathway[j] = 0.0;  // Unless daughter product
        }
        if ( masschain == 0 )   // Sets terminator
        {
          state = 2;
          if(lst_time[count] == 0)
            sls->all_gone(count, ((e_time*site.delta_t)+site.delta_t));
        }
      }
    }
    
    //     =========================================================================
    //     Write to SDL output
    //     =========================================================================
    if( flg.annual  || ( (ShouldReport((e_time*site.delta_t)+site.delta_t))))
    		for (count=site.numtotcon-1; count >=0; count--)
        {
          if (lst_time[count]!=2)
          {
            if ( ((e_time*site.delta_t)+site.delta_t) >= site.max_time )
          				state = 1;
            write_sdl(1,count);
          }
          if (lst_time[count]==1)
          {
            lst_time[count]=2;
            for (i=0;i<5;i++)
            {
              contam[count].rk_pathway[i]=0.0;
              contam[count].rk_instant[i]=0.0;
            }
            // write final record so that interpolation works correctly
            //    for last (possible only) time step
            write_sdl(2,count);
          }
          flushall();
        }
        
        for (i=0;i<site.numtotcon;i++)
          if (props[i].typeflag == 1)
            delta_dry_layer[i] = vol_delta;
          vol_delta = 0.0;
          vol_count = 1;
          
          if (ShouldReport((e_time*site.delta_t)+site.delta_t))
            printf("Elapsed Time:  %8.1f\r",((e_time*site.delta_t)+site.delta_t));
          
          for (count=site.numtotcon-1; count >=0; count--)
          {
            //       =========================================================================
            //       Modify MASS by source/sink term for next time step
            //       =========================================================================
            srcsnk = source_sink[count].get_value(e_time*site.delta_t);
            contam[count].mass = max (0.0, contam[count].mass + srcsnk);
            //       ===========================================================================
            //       STEP D.10  --  UPDATE THE MASS OF THE ORGANIC FOR MOLE FRACTION CALCULATION
            //       ===========================================================================
            if ( (props[count].typeflag == 1) && (e_time+1 <= t_steps) )
              site.org_moles_new += (contam[count].mass/ props[count].mole_wt);
          }
          
  } // for (e_time=0; e_time < t_steps; e_time++)
  
  data_file->Close();
  sdl.close();
  delete sls;
  
  
  delete[] leach;
  delete[] overland;
  delete[] suspend;
  delete[] volate;
  delete[] source_sink;
  
  recopy(site.run, records);     //  site.name added to cover for transaction handling for FUI
  
  //delete[] k0;
  //delete[] rk_pathway;
  
  delete[] contam;
  delete[] parent;
  delete[] child;
  delete[] lst_time;
  
  delete[] org_liq_conc;
  delete[] sat_vapor_conc;
  delete[] waste_thick;
  delete[] depth_dry_layer;
  delete[] delta_dry_layer;
  delete[] aqueous_sol;
  
  if (flg.bigverb == 0)
  {
    sprintf(buf,"%s.sdl",site.run);
    //remove( dstr );
  }
  
  err.close();
  
  sprintf(buf,"%s.err",site.run);
  remove(buf); // unlink
  
  fcloseall();
  
  return(0);
}

void write_sdl(int delta_t,int count)
{
  sdl.write(props[count].cas_id);
  sdl.write(props[count].p_id);
  
  if (delta_t==0)
    /* time zero */
    sdl.write(e_time);
  else if (delta_t==1)
    sdl.write((e_time*site.delta_t)+site.delta_t);//(prt_time));
  else
    /* last time */
    sdl.write(site.delta_t+(e_time*site.delta_t)+site.delta_t);//(prt_time));
                                                               /*
                                                               if ( new_soil_4 > 0.0)
                                                               {
                                                               if ( (waste_thick[count]-new_soil_4) < 0.0 )
                                                               waste_thick[count] = new_soil_4;
                                                               sdl.write(waste_thick[count]-new_soil_4);
                                                               }
  */
  
  if ( new_soil_4 > depth_dry_layer[count])  // substitution 4/98 BLH
  {
    if ( ((waste_thick[count]+start_dry_layer)-new_soil_4) < 0.0 )
      sdl.write(0.0);
    else
      sdl.write((waste_thick[count]+start_dry_layer)-new_soil_4);
  }
  else
  {
    if ( new_soil_4 > start_dry_layer )
      sdl.write((waste_thick[count]+start_dry_layer)-new_soil_4);
    else
      sdl.write(waste_thick[count]);
  }
  sdl.write(darcy_inf.get_value((e_time*site.delta_t)+site.delta_t));
  sdl.write(sed_load.get_value((e_time*site.delta_t)+site.delta_t));
  sdl.write(over_flow.get_value((e_time*site.delta_t)+site.delta_t));
  sdl.write((double)contam[count].mass);
  /*
 	sdl.write(rk_pathway[0]); // average
 	sdl.write(rk_pathway[1]);
 	sdl.write(rk_pathway[2]);
 	sdl.write(rk_pathway[3]);
 	sdl.write(rk_pathway[4]);
  */
 	sdl.write(contam[count].rk_instant[0]); // instantaneous
 	sdl.write(contam[count].rk_instant[1]);
 	sdl.write(contam[count].rk_instant[2]);
 	sdl.write(contam[count].rk_instant[3]);
 	sdl.write(contam[count].rk_instant[4]);
  
 	sdl.write(source_sink[count].get_value(e_time*site.delta_t));
  if (delta_t>1)
    sdl.write(9);
  else
    sdl.write((int)state);
  //  sdl.write((int)lst_time[count]);
  //  sdl.write(flg.napl);
  sdl.write(contam[count].napl);  // BLH 7/99
  if (contam[count].init_mass>0.0 && contam[count].mass>0.0)
    sdl.write(100.0*(contam[count].init_mass-contam[count].mass)/contam[count].init_mass);  // % lost
  else
    sdl.write(0.0);
  sdl.write(new_soil_4);
  sdl.write((waste_thick[count]+start_dry_layer)-new_soil_4);
  sdl.write(depth_dry_layer[count]);
  sdl.write(delta_dry_layer[count]);
  sdl.write(vol_delta); //(vol_delta);
  sdl.write(contam[count].dzdt);
  sdl.write(contam[count].rk_pathway[0]); // average
  sdl.write(contam[count].rk_pathway[1]);
  sdl.write(contam[count].rk_pathway[2]);
  sdl.write(contam[count].rk_pathway[3]);
  sdl.write(contam[count].rk_pathway[4]);
  sdl.writeln();
  
 	state=0;
  records +=1;
}
