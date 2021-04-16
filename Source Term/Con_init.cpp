#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include<new.h>

#include"strm1.h"
#include"fcsv.h"
#include"gid.h"
#include"misc3.h"
#include "conClass.h"

/*
*************************************************************************************************************
*       MODIFICATION HISTORY                                                                                *
*    DATE  WHO      DESCRIPTION                                                                         *
*  --------  ---  ----------------------------------------------------------------------------------------  *
* 04/  /99  added organic Henrys constant as contam[index].henrys_org (for Formulations Eq. 2.12)
* 08/  /02  accomodate secondary source input to source/sink series
*************************************************************************************************************
*/
void InitSecondarySource();

void con_init(int numcontam)
{
  int num, newnum=0, prognum;
  float grout_diff, sat_vapor=0., octanol_h2o=0.;

  printf("Initializing contaminant data\n\n");

  sls->writeln("Initializing contaminant data");

  for(num=0; num < numcontam; num++)
  {
//  ==========================================================================================================
//  STEP 1  --  DETERMINE WHETHER CONTAMINANT HAS ANY PROGENY TO DETERMINE INITIALIZATION METHODOLOGY
//  ==========================================================================================================
  props[newnum].progeny = data.i_read("NDS",num+1,0,0,0,0,0);

  props[newnum].typeflag=3;  //default for inorganics, changed for organics and rads
  for (prognum=0; prognum<=props[newnum].progeny; prognum++)
    {
#ifdef DEBUG
  printf("initializing contaminant P-%d,  Prog-%d\n",num,prognum);
#endif
//    =======================================================================================================
//    STEP 2  --  INITIALIZE DATA COMMON TO ALL CONTAMINANT TYPES
//    =======================================================================================================
    data.s_read(props[newnum+prognum].name,  BUF_SIZE, "fscname", num+1,0,0,0,0,prognum);
    data.s_read(props[newnum+prognum].cas_id, BUF_SIZE, "fscasid", num+1,0,0,0,0,prognum);


//  Finds the mass of one molecule to use a min limit
    props[newnum+prognum].mole_wt     = data.f_read_cl("clwm", num+1,0,0,0,0,prognum,1.0);
    props[newnum+prognum].molecule = props[newnum+prognum].mole_wt / 6.0221367e23 ;
    aqueous_sol[newnum+prognum]        = data.f_read("stsol",   num+1,0,0,0,0,prognum);

//  Converts mg/L from param file to g/cc for STC, adjusted for concentration/unit depth
    props[newnum+prognum].adj_aqu_sol = site.area * (aqueous_sol[newnum+prognum]*1e-6);

    props[newnum+prognum].type        = data.i_read("clktype", num+1,0,0,0,0,prognum);

    if (prognum == 0)
      {
      contam[newnum+prognum].mass = data.f_read("stinven",num+1,0,0,0,0,0);
      contam[newnum+prognum].init_mass = contam[newnum+prognum].mass;
      }
    else
      {
      contam[newnum+prognum].mass = 0.0;
      contam[newnum+prognum].init_mass = 0.0;
      }

//      ====================================================================================================
//     STEP 3  --  READ IN APPROPRIATE CONTAMINANT DATA FOR VERSION (RAAS/MEPAS - FRAMEWORK)
//      ====================================================================================================
    // allow decay of organics for MEPAS4.1 - 3/99 BLH
    props[newnum+prognum].t_half = data.f_read("stghalf", num+1,0,0,0,0,prognum);  // days
    props[newnum+prognum].lambda = 0.0;
    props[newnum+prognum].spec_act = 1.0;
    if (props[newnum+prognum].t_half > 0.0)
      {
      props[newnum+prognum].lambda = log(2.0)/ (props[newnum+prognum].t_half / 365.25);  // t_half in years
      if (props[newnum+prognum].type==1)
        {
        props[newnum+prognum].spec_act = 1.306E+8
                    / ( props[newnum+prognum].t_half * props[newnum+prognum].mole_wt);   // t_half is days
                                                    //  Convert Activity -> Mass for rads
        contam[newnum+prognum].mass /= props[newnum+prognum].spec_act;  //  Mass = Act/SpecAct
        props[newnum+prognum].typeflag=2;
        }
      }

#ifdef DEBUG
  printf("Initializing Kd\n");
#endif
    k_d[newnum+prognum].init_param(data,"stkd",num+1,0,0,0,prognum);

    waste_thick[newnum+prognum] = site.thick;
    contam[newnum+prognum].mole_fract = 1.0;
    props[newnum+prognum].vapor_diff_coef = 0.0;
    contam[newnum+prognum].vapor_conc = 0.0;
    sat_vapor_conc[newnum+prognum] = 0.0;
    contam[newnum+prognum].henrys_const = 0.0;
    contam[newnum+prognum].henrys_org = 0.0;
    contam[newnum+prognum].adj_org_conc = 0.0;
    contam[newnum+prognum].k_g = 0.0;
    contam[newnum+prognum].k_l = 0.0;
    contam[newnum+prognum].k_i = 0.0;
    contam[newnum+prognum].liq_dens=0.0;
    contam[newnum+prognum].napl=0;  // BLH 7/99

     if( surf_vol.nonzero() ) //
      {
      grout_diff = data.f_read("stdifco",num+1,0,0,0,0,prognum);
      grout_diff *= 3.15576e7; // Convert from cm^2/s to cm^2/yr
      props[newnum+prognum].grout_coef = sqrt( grout_diff/ M_PI);   // Used for Grout leach calc.
      }
    else
      props[newnum+prognum].grout_coef = 0.0;

    props[newnum+prognum].unmod_henry = data.f_read_cl("clhlc",    num+1,0,0,0,0,prognum,0.0);

    if( props[newnum+prognum].unmod_henry > 1.0e-7 ) // Treat as a volatile
//    =======================================================================================================
//    STEP 4  --  INITIALIZE VOLATILE CONTAMINANT SPECIFIC DATA
//    =======================================================================================================
      {
      /*    4/98 BLH - volatile organics must also have KOC>0
          this prevents problems from Mercury, cyanides, etc. */
      float koc=0.0;
      koc = data.f_read_cl("clkoc",num+1,0,0,0,0,prognum,0.0);
      if (koc > 0.0) // data.f_read("clkoc",num+1,0,0,0,0,prognum))
      {
        octanol_h2o = data.f_read_cl("clkow",num+1,0,0,0,0,prognum,0.0);
//      ====================================================================================================
//      STEP 5  --  INITIALIZE ORGANIC CONTAMINANT SPECIFIC DATA
//      ====================================================================================================
        /* 6/98 BLH - per item#2 5/31/98 memo from Gary Streile
            ...designate a contaminant as a NAPL-category contaminant
              if HLC>0.0 and KOC>0.0 and KOW>0.0
        */
        if (octanol_h2o > 0.0 )
          {
#ifdef DEBUG
  printf("Volatization\n");
#endif
          contam[newnum+prognum].liq_dens=data.f_read_cl("clpcden",num+1,0,0,0,0,prognum,0.0);

//  Read in data for volatile calculations
            sat_vapor = data.f_read_cl("clvap",num+1,0,0,0,0,prognum,0.0);
            sat_vapor = sat_vapor / 760.0;  // Convert: mmHg -> atm
            sat_vapor_conc[newnum+prognum] =  props[newnum+prognum].mole_wt * sat_vapor
                 / (GAS_CONST * (site.temp + 273.0));
            sat_vapor_conc[newnum+prognum] = sat_vapor_conc[newnum+prognum]/1.0e6; // Convert: g/m^3 -> g/cm^3
//  O'Connor and Muller 1980, Converted from (cm^2/s) to (cm^2/y)
            contam[newnum+prognum].air_vp_diff = (1.9*pow(props[newnum+prognum].mole_wt,-2.0/3.0))* 3.15576e7;
            contam[newnum+prognum].henrys_const = props[newnum+prognum].unmod_henry
                               / (GAS_CONST * (site.temp + 273.0));

            props[newnum+prognum].typeflag=1 ;
//          org_liq_conc[newnum+prognum] = data.f_read("storgcon",num+1,0,0,0,0,prognum);
            org_liq_conc[newnum+prognum] = 0.0;
            contam[newnum+prognum].adj_org_conc = site.area * org_liq_conc[newnum+prognum];
            if (props[newnum+prognum].mole_wt > 0.0)  // BLH skip div by zero
              {
//  Calculate the total number of moles in the organic liquid phase
                site.org_moles_new += (contam[newnum+prognum].mass/props[newnum+prognum].mole_wt);
#ifdef DEBUG
                printf("%d --",newnum+prognum);
                printf("%10.3e --",contam[newnum+prognum].mass);
                printf("%10.3e --\n",props[newnum+prognum].mole_wt);
                printf("Organic moles -- %10.3e \n",site.org_moles_new);
//              getc(stdin);
#endif
              }
            }
//  MEPAS Atmospheric Formulations Eq 33    ---   where 1.4 replaces 1e-3 as k_g_H2O
          contam[newnum+prognum].k_g = (pow((18.0/props[newnum+prognum].mole_wt),0.335) *
                pow(((site.temp + 273.0)/298.0),1.005) * (1.4));
//  MEPAS Atmospheric Formulations Eq 32    ---   where 2.2e-3 replaces 3.01 as k_l_O2
          contam[newnum+prognum].k_l = (pow((32.0/props[newnum+prognum].mole_wt),0.50)*
                ((site.temp + 273.0)/298.0)*(2.2e-3));

          if( octanol_h2o > 0.0 )        // Check against divide by zero
        {
//  MEPAS Atmospheric Formulations Eq 31
          contam[newnum+prognum].k_i =1/((1/contam[newnum+prognum].k_l)+
              (1/(contam[newnum+prognum].henrys_const*contam[newnum+prognum].k_g)))*7.4e-4;

//      contam[newnum+prognum].adj_org_conc /= octanol_h2o;
//      contam[newnum+prognum].henrys_const /= octanol_h2o;
        contam[newnum+prognum].henrys_org =
          contam[newnum+prognum].henrys_const / octanol_h2o; // Formulations Eq. 2.12 4/99 BLH
        }
      }
    }
//    =======================================================================================================
//    STEP 6  --  INITIALIZE DATA FOR PARENT RADIONUCLIDES
//    =======================================================================================================
    if (prognum==0)
      {
#ifdef DEBUG
  printf("Parent\n");
#endif
//  Added capability to adjust volatization rates for individual contaminants
      props[newnum+prognum].vol_ratio   = data.f_read("stvolrat", num+1,0,0,0,0,0);
      contam[newnum+prognum].init_mass = contam[newnum+prognum].mass;

      leach[newnum].init_param(data,"stleach",num+1,0,newnum,0,0);
      suspend[newnum].init_param(data,"stsusp",num+1,0,newnum,0,0);
      overland[newnum].init_param(data,"stovl",num+1,0,newnum,0,0);
      volate[newnum].init_param(data,"stvolat",num+1,0,newnum,0,0);
      source_sink[newnum].init_param(data,"stsource",num+1,0,newnum,0,0);
      rstrcpy(props[newnum].p_name,props[newnum].name);
      rstrcpy(props[newnum].p_id,props[newnum].cas_id);
      parent[newnum] = newnum;
      child[newnum] = newnum;   // sets decay chain indicator to self to indicate no decay chain
      }
    else
      {
//      ====================================================================================================
//      STEP 7  --  INITIALIZE PROGENY SPECIFIC DATA
//      ====================================================================================================
#ifdef DEBUG
  printf("Progeny\n");
#endif
//      Volatization rates are not adjustable for progeny
      props[newnum+prognum].vol_ratio = 1.0;

//      Initialize all pathway series to "Calculate"
      leach[newnum+prognum].set_series(0,0.0);
      suspend[newnum+prognum].set_series(0,0.0);
      overland[newnum+prognum].set_series(0,0.0);
      volate[newnum+prognum].set_series(0,0.0);
//      There is no Source/Sink Term for progeny
      source_sink[newnum+prognum].set_series(1,0.0);
      rstrcpy(props[newnum+prognum].p_name,props[newnum].name) ;
      rstrcpy(props[newnum+prognum].p_id,props[newnum].cas_id) ;
      parent[newnum+prognum] = newnum;

      // already populated above -
      //   just plain wrong for chemical degradation products
      //       props[newnum+prognum].typeflag = 2;
      //       props[newnum+prognum].type = 1;

//  Links the previous member of the decay chain to the current
      child[newnum+prognum-1] = newnum + prognum;
      if ( prognum == props[newnum].progeny )     //  Sets the child indicator to self to terminate decay chain
        child[newnum+prognum] = newnum + prognum;
      }
    }
  newnum +=prognum;
  }

  if (flg.src==SECOND_SRC) InitSecondarySource();  // BLH 8/2002
}

void init_relprop(int count, float time, float bulkd)
{
contam[count].grout = props[count].grout_coef * surf_vol.get_value(time);
contam[count].gdecay = batemn(parent[count],count,time);
contam[count].glass = glass_series.get_value(time);
contam[count].retard = 1.0 + ( ( (air_space.get_value(time)*contam[count].henrys_const) +
                       (bulkd*k_d[count].get_value(time)) ) / site.theta );
}

void InitSecondarySource()
{
  int numcon, nhdr;
  fcsv *fin;

#ifdef DEBUG
  printf("Begin InitSecondarySource\n");
#endif

  char filename[MAXPATH];
  char drive[MAXDRIVE];
  char dir[MAXDIR];
  char file[MAXFILE];
  char ext[MAXEXT];
  char modid[64];

  fnsplit(site.gid,drive,dir,file,ext);
  rstrcpy(filename,AddExtension(site.run,"dat"));
  try
  {      fin = new fcsv;    }
  catch(...)
  {    err.write("Can't allocate memory for input file"); return;}
  if (!fin->open(filename,READ))
  {    err.write("Can't open input file!"); return; }

  fin->read(&nhdr); fin->readln();
  for (int i=0;i<nhdr;i++) fin->readln();

  fin->read(modid); fin->read(&numcon); fin->readln();
  if (strcmpi(modid, site.name)) return;

  for (int ncon=1;ncon<=numcon;ncon++)
  {
    CON *con = new CON();
    con->Read(fin);

    for (int num=0; num<site.numtotcon; num++)
      if (!strcmpi(props[num].p_id,con->cas) && !strcmpi(props[num].cas_id,con->cas))
      {
        source_sink[num].init_param(num,con->TS);
        break;
      }
    for (int nprog=0; nprog<con->numProgeny; nprog++)
    {
      for (int num=0; num<site.numtotcon; num++)
        if ((!strcmpi(props[num].p_id,con->cas)) && (!strcmpi(props[num].cas_id,con->prog[nprog]->cas)))
        {
          source_sink[num].init_param(num,con->prog[nprog]->TS);
          break;
        }
    }
    delete con;
  }
  fin->close();

#ifdef DEBUG
  printf("Exit InitSecondarySource\n");
#endif

}
