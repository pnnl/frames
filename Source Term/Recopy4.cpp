/************************************************************************************************************
 * Source Term Release Module                   MODULE:  RECOPY.CPP                           VERSION: 1.00 *
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
 *   Last Modified:  10/28/96 -- KDS                                                                        *
 ************************************************************************************************************
 *                                         MODULE:  RECOPY.CPP                                              *
 *  RECOPY is the module that prepares the output files for the results from the Source Term Module.  For   *
 *  MEPAS and RAAS this module creates the .SRC file.  When run for the proposed Multimedia Modeling        *
 *  Framework this module additionally produces the .WFF, .AFF, AND .SCF files.                             *
 ************************************************************************************************************
 *       MODULE ORGANIZATION                                                                                *
 *  Routine:  recopy(), src_make(), wff_make(), aff_make(), scf_make(), file_head(), flux_count()           *
 ************************************************************************************************************
 *       MODIFICATION HISTORY                                                                               *
 *    DATE  WHO      DESCRIPTION                                                                         *
 *  --------  ---  ----------------------------------------------------------------------------------------  *
 *  06/01/96  KDS  Conversion of STC to STRM in preparation of application of MEPAS QA program and           *
 *            procedures.                                                                               *
 *  10/28/96  KDS  Changes for FRAMEWORK STRM.  Fixed source length, width, and thickness as well as the     *
 *            number of progeny for the WFF, AFF and SCF Files.                                         *
 *  12/2/96  KDS  Added sff_make().  Edited wff_make(), aff_make(), scf_make(), and src_make() to allow     *
 *            for the addition of SEDIMENT FLUX and OVERLAND FLUX to the .sdl file.                     *
 *  05/12/97  BLH Add overland and leaching for .WFF
 *  07/16/97  BLH Write overland and leaching in .WFF as separate media types
 *  04/  /99  turn off SCF output
 *  06/  /02  BLH Restore SCF output - modify to report 'surface concentration'
 *              report 0.0 if overburden is non-zero; else report computed concentration,
 *              taking into account loss of waste form thickness due to erosion (= residualthickness)
 *  11/  /02  BLH correct dataset name and medium type for SCF, WFF and AFF per specifications
 *  12/  /03  BLH per Randal Taira:
 *      Please make the following correction to the MEPAS Source in Aquifer module:
 *      1. Current Formulation:
 *         Concentration = total mass (g) /source volume (cm^3)
 *           where source volume = L x W x D (cm^3)
 *      2. Change Formulation to:
 *         Concentration = total mass (g) / water volume (cm^3)
 *           where water volume = source volume x total porosity (cm^3)
 *           where source volume = L x W x D (cm^3)
 *      This will leave us with mass of contaminant/volume of water (g/mL).
 *      Note that the units are just there as an example. They are not necessarily the units you should use.
 *      Note also that this change is only for Source in Aquifer.
 *      The WCF computation for Source in Standing Surface Water is correct.
 *      The SCR computation for Source in Soil is also correct as is.
 *  12/  /03 BLH per Randal Taira:
 *      Gene provided me with an equation to use for the WCF file.  See below:
 *      Ct = Total contaminant mass/Total source volume
 *      Water Concentration = Ct / (total porosity + (bulk density)(Kd))
 ************************************************************************************************************
 */

#include<stdio.h>
#include<stdlib.h>
#include<dir.h>
#include<string.h>
#include<math.h>

#include"strm1.h"
#include"fcsv.h"
#include"misc3.h"
#include"rk_path.h"
#include"recopy2.h"

//===========================================================================================================
// PROTOTYPES  ==============================================================================================
//===========================================================================================================
extern fcsv sdl;
fcsv src, wff, aff, scf, sff, wcf;

//#ifndef FRAME
  void src_make(char *run_name, long int records);
//#endif
#ifdef TESTING
  void src_make(char *run_name, long int records);
#endif

#ifdef FRAME
  void wff_make(char *run_name, long int records);
  void aff_make(char *run_name, long int records);
  void scf_make(char *run_name, long int records );
  void wcf_make(char *run_name, long int records );
  void sff_make(char *run_name, long int records );
  long int flux_count(int count, long int records);
  void wff_make_type(int infiltration, long int count, long int records);
#endif

//===========================================================================================================
// This subroutine controls the creation of output files.  It makes the .src file and for the framework,
// creates the PDCFs for fluxes, .wff, .aff, .scf, and .sff files.
//===========================================================================================================
void recopy(char *run_name, long int records)
{
  char buf[MAXPATH];

  rstrcpy(buf,run_name);
  strcat(buf,".sdl");
  sdl.open(buf,READ);

#ifdef FRAME
  if (strcmpi(site.media_type,"aquifer"))
      aff_make(run_name, records);
    wff_make(run_name, records);
    if (0==strcmpi(site.media_type,"soil"))
      scf_make(run_name, records);    // restored June 2002 - BLH
    else
      wcf_make(run_name, records);    // Nov 2002 - BLH
#elif
  src_make(run_name, records);
#endif

#ifdef TESTING
  src_make(run_name, records);
#endif

  sdl.close();
  return;
}
//#ifndef FRAME

//===========================================================================================================
//  This subroutine creates the .src output file for RAAS and MEPAS.  It takes lines of output values from
//  the verbose data file (.sdl) and copies them to the .src file.  This maintains file format continuity
//  from prior versions of the code.
//===========================================================================================================
void src_make(char *run_name, long int records)
{
  char buf[MAXPATH], cstr[32];
  int i, num, ct, load[100], state=0;
  float cflt=0.0, start_mass, org_layer[100];
  long int rec;

//  STEP 1  --  OPEN AND PREPARE WORK FILES
  rstrcpy(buf, run_name);
  strcat(buf, ".src");
  src.open(buf, WRITE);
  src.rewind();
  printf("\n\nWriting contaminant data to .SRC file\n\n");
  for (i=0; i<100; i++)
  {
    load[i]=0;
    org_layer[i]=0.0;
  }

//  STEP 2  --  WRITE HEADER TEXT FOR FILE
  src.write(11);
  src.writeln();
  file_head(src, buf);
  src.write("Total Number of Contaminants");
  src.write((int)site.numtotcon);
  src.writeln();
  src.write("Number of Parent Contaminants");
  src.write((int)site.num_contam);
  src.writeln();
  src.write("Number of Progeny Contaminants");
  src.write((int)(site.numtotcon-site.num_contam));
  src.writeln();
  src.write("Waste area(cm^2)");
  src.write((float)site.area);
  src.writeln();

//  STEP 3  --  WRITE HEADER TEXT FOR EACH CONTAMINANT
  for (num=0; num < site.numtotcon; )
  {
    ct=0;
    src.writeln();
    src.write("Contaminant");
    src.write(props[num].name);
    src.write(props[num].cas_id);
    src.write(props[num].progeny);
    src.write(props[num].type);
//    if (props[num].typeflag == 2)
      start_mass = contam[num].init_mass * props[num].spec_act;
//    else
//      start_mass = contam[num].init_mass;
    src.write(start_mass);
//    src.write((float)init_mass[num]);
    src.writeln();
    src.write("Cont");
    src.write("Parent");
    src.write("Time");
    src.write("Waste");
    src.write("Infiltration");
    if (props[num].typeflag == 2)
    {
      src.write("Activity in waste unit(Ci)");
      src.write("Activity lost to pathway (Ci)");
    }
    else
    {
      src.write("Mass in waste unit(g)");
      src.write("Mass lost to pathway (g)");
    }
    src.writeln();
    src.write(" ");
    src.write(" ");
    src.write("(yr)");
    src.write("Thick(cm)");
    src.write("(cm/yr)");
    src.write("Mass remaining");
    src.write("Decay");
    src.write("Leaching");
    src.write("Wind");
    src.write("Water");
    src.write("Volatilize");
    src.write("Known Source/Sink");
    src.writeln();
    sdl.rewind();
    sdl.readln();
    sdl.readln();

//  STEP 4  --  READ DATA FROM VERBOSE DATA FILE AND COPY TO OUTPUT FILE
    for (rec=3; rec<=records; rec++)
    {
      sdl.read(cstr);
      if (strcmpi(cstr,props[num].cas_id)==0)
      {
        src.write(cstr);          //  write contaminant name
        sdl.read(cstr);
        src.write(cstr);          //  write parent name
        sdl.read(&cflt);
        src.write(cflt);          //  Write TIME
        sdl.read(&cflt);
        if ( props[num].typeflag == 1 )    //  Common organic thickness section
          if ( load[ct] == 0 )
          {
            org_layer[ct] = cflt;    //  load organic thickness
            load[ct]=1;
          }
          else
            cflt = org_layer[ct];
        src.write(cflt);          //  Write THICKNESS
        sdl.read(&cflt);
        src.write(cflt);          //  Write INFILTRATION RATE
        sdl.read(&cflt);              // Read past SEDIMENT LOAD    --  12/2/96
        sdl.read(&cflt);              // Read past OVERLAND FLUX    --  12/2/96
        for (i=0; i<7; i++)
        {
          sdl.read(&cflt);        //  Write Mass/Activity
//          if ( props[num].typeflag == 2 )    //  Convert Mass -> Activity for rads
            cflt *= props[num].spec_act;  //  Act = Mass*SpecAct
          src.write(cflt);        //  REMAIN,DECAY,LEACH,WIND,WATER,VOL,KNOWN
        }
        sdl.read(&state);
        if (state > 0)
        {
          rec = records ;

//  STEP 5  --  WRITE FOOTER TEXT FOR END OF EACH CONTAMINANT RUN

          src.writeln();
          src.write("End");
          if(state == 1)
            {
            src.write(" Exceeded maximum run time");
            }
          else if(state == 2)
            {
            src.write(" Below minimum mass fraction for run");
            }
          else if(state == 3)
            {
            src.write(" Run truncated to 100 output values");
            }
          src.writeln();
          sdl.rewind();
          num++;
        }
        else
        {
          src.writeln();
          sdl.readln();
        }
        ct++;
        }
      else
        sdl.readln();
      }

    }
  src.close();
  return ;
}
//#endif

//===========================================================================================================
// This subroutine prints out the standard header for Source Term Module Output Files
//===========================================================================================================
void file_head(fcsv &out, char *outfile)
{
  out.write("================================================================================");
  out.writeln();
  out.write("  MEPAS Source Term Release Module");
  out.writeln();
#ifndef FRAME
  out.writeln();
#endif
  out.write("  Case File:");
  out.write(site.gid);
  out.writeln();
#ifdef FRAME
  out.write("  Module Id:");
  out.write(site.name);
  out.writeln();
#endif
  out.write("  Run Performed:");
  out.write(dateoutst());
  out.write(timeoutst());
  out.writeln();
#ifndef FRAME
  out.writeln();
#endif
  out.write("  Output File:");
  out.write(outfile);
  out.writeln();
  out.write("================================================================================");
  out.writeln();
  return;
  }


#ifdef FRAME
//===========================================================================================================
// SUBROUTINE WFF_MAKE  =====================================================================================
//  This subroutine creates the .wff output file for the multimedia framework.  It takes lines of output
//  values from the verbose data file (.sdl) and copies them to the .wff file.
//===========================================================================================================

void wff_make(char *run_name, long int records)
  {
  char buf[MAXPATH];
  int i=0;
  long int count;

//***********************************************************************************************************
//  STEP 1  --  OPEN AND PREPARE WORK FILES
  rstrcpy(buf,run_name);
//  rstrcpy(buf,site.name);  //  Added to cover for transaction handling in FUI
  strcat(buf, ".wff");
  wff.open(buf,WRITE);
  wff.rewind();
  sdl.rewind();

  printf("Writing contaminant data to .WFF file\n");

//***********************************************************************************************************
//  STEP 2  --  WRITE HEADER TEXT FOR FILE
  wff.write(7);
  wff.writeln();
  file_head(wff, buf);

//wff.write(site.num_media);
  count = (flg.inf>0) ? 1 : 0;
  count += (flg.ovl>0) ? 1 : 0;

  wff.write(count);
  wff.writeln();

  if (count>0) count = flux_count(i,records);

  if (flg.inf>0) wff_make_type(1, count, records); // infiltration - surface water, vadose, aquifer
  if (flg.ovl>0) wff_make_type(0, count, records); // overland - surface water, vadose, NO Aquifer

//***********************************************************************************************************
//  STEP 6  --  CLOSE OUTPUT FILE
  wff.close();
  return;
}

//===========================================================================================================
//  SUBROUTINE SCF_MAKE  =====================================================================================
//  This subroutine creates the .scf output file for the multimedia framework.  It takes lines of output
//  values from the verbose data file (.sdl) and copies them to the Source Concentration File.
//===========================================================================================================
void scf_make(char *run_name, long int records)
  {
  char buf[MAXPATH], cstr[32];
  float cflt=0.0, conc=0.0, mass=0.0;
  float thick=0.0, erosion=0.0;
  int count, i, k, done, state=0;
  fpos_t pos1, pos2;
  long int j;

  float start_dry_layer = init_dry_layer.get_value(0.0);

//***********************************************************************************************************
//  STEP 1  --  OPEN AND PREPARE WORK FILES
  rstrcpy(buf,run_name);
  strcat(buf, ".scf");
  scf.open(buf,WRITE);
  scf.rewind();
  sdl.rewind();
  printf("Writing contaminant data to .SCF file\n");

//***********************************************************************************************************
//  STEP 2  --  WRITE HEADER TEXT FOR FILE
  scf.write(7);
  scf.writeln();
  file_head(scf, buf);
  scf.write(1); // If data set name is "All" then "Number of Data Sets" must be equal to one
  scf.writeln();

  scf.write("All");
  scf.write(site.media_type);
  scf.write((float)(site.length/100.));
  scf.write("m");
  scf.write((float)(site.width/100.));
  scf.write("m");
  scf.write((float)(site.thick/100.));
  scf.write("m");
  scf.write(site.num_contam);
  scf.writeln();


//***********************************************************************************************************
//  STEP 3  --  WRITE HEADER TEXT FOR EACH CONTAMINANT
  for (i=0; i<site.numtotcon; i++)
    {

    printf("\t%d of %d: %s (%s)\n", i+1, site.numtotcon, props[i].name,props[i].cas_id);

    scf.write(props[i].name);
    scf.write(props[i].cas_id);
    scf.write("yr");
    if (props[i].typeflag == 2)
      scf.write("pCi/Kg");
    else
      scf.write("mg/Kg");

    // BLH 2/98 eliminate double pass, count records on output
    pos1 = scf.getfpos()+1; //  count=flux_count(i,records);
    scf.write("       ",false);    //  scf.write(count);

    if (strcmpi(props[i].p_id,props[i].cas_id)==0)
      scf.write(props[i].progeny);
    else
      {
      scf.write(props[i].p_name);
      scf.write(props[i].p_id);
      }
    scf.writeln();
    sdl.readln();
    sdl.readln();

//***********************************************************************************************************
//  STEP 4  --  READ DATA FROM VERBOSE DATA FILE AND COPY TO OUTPUT FILE
    count=0; done=0;
    for ( j=0; j<records; j++)
      {
      sdl.read(cstr);                       // constituent cas
      if (strcmpi(cstr,props[i].cas_id)==0)
        {
        sdl.read(cstr);                     // parent cas
        if (strcmpi(cstr,props[i].p_id)==0)
          {
          sdl.read(&cflt);          // time(yr)
          scf.write(cflt);          // Write out TIME
          sdl.read(&thick);         // waste form thickness (cm)
                                    //    = site.thick - erosion (if any)
          for (k=0;k<4;k++)
            sdl.read(&mass);        // remaining mass in waste form (g)
          sdl.read(&cflt);          // decay rk[0]
          sdl.read(&cflt);          // leaching   rk[1]
          sdl.read(&cflt);          // wind       rk[2]
          sdl.read(&cflt);          // water      rk[3]
          sdl.read(&cflt);          // vol        rk[4]
          sdl.read(&cflt);          // src/sink
          sdl.read(&state);         // state
          for (k=0;k<3;k++)
            sdl.read(&erosion);     // erosion (cm)

//        conc = mass / (site.length*site.width*site.thick);  //  site concentration in g/mL
          if ((erosion < start_dry_layer) || (0.0 >= thick))
            scf.write(0.0);         // remaining overburden is clean: surface conc=0.0
          else
          {                         // no overburden: report conc
                                                              //  mass in g or Ci
            conc = mass / (site.length*site.width*thick);     //  residual concentration (g/mL == g/cm^3 or Ci/cm^3)
            if (strcmpi("soil",site.media_type)==0)
            {
              // the next 2 equations are not appropriate for for rads
              // cflt = conc / (site.wz_bulkd/1000.);          //  converts cflt from g/mL to g/Kg
              // cflt *= 1000.;                                //  g/Kg --> mg/Kg

              // substitution for above
              cflt = conc / site.wz_bulkd;  // g/g or Ci/g
              if (props[i].typeflag!=2)
                cflt *= 1e6;                // g/g to mg/kg
              else
                cflt *= 1e3;                // Ci/g to Ci/kg
            }
            else
              cflt = min(conc, props[i].adj_aqu_sol);         //  Limits concentration to solubility
            if ( props[i].typeflag == 2 )
                cflt *= props[i].spec_act*1e+12;              //  Converts to pCi for rads
            scf.write(cflt);
          }
          scf.writeln();

          count++;
         if (state==9 || state==1)
               done=1;
          }
        }
      if (done==1) break;
      sdl.readln();
      }

    // BLH 2/98 use file position to insert record count
    pos2 = scf.getfpos();
    scf.write(pos1,count);
    scf.setfpos(pos2);
    scf.newline = true;

    sdl.rewind();
    }

//***********************************************************************************************************
//  STEP 5  --  CLOSE OUTPUT FILE
  scf.close();
  return;
  }

//===========================================================================================================
//  SUBROUTINE WCF_MAKE  =====================================================================================
//  This subroutine creates the .wcf output file for the multimedia framework.  It takes lines of output
//  values from the verbose data file (.sdl) and copies them to the Source Concentration File.
//===========================================================================================================
void wcf_make(char *run_name, long int records)
  {
  char buf[MAXPATH], cstr[32];
  float cflt=0.0, conc=0.0, mass=0.0;
  float thick=0.0, erosion=0.0;
  float sourceVolume;
  int count, i, k, done, state=0, e_time=0;
  fpos_t pos1, pos2;
  long int j;

  float start_dry_layer = init_dry_layer.get_value(0.0);

//***********************************************************************************************************
//  STEP 1  --  OPEN AND PREPARE WORK FILES
  rstrcpy(buf,run_name);
  strcat(buf, ".wcf");
  wcf.open(buf,WRITE);
  wcf.rewind();
  sdl.rewind();
  printf("Writing contaminant data to .SCF file\n");

//***********************************************************************************************************
//  STEP 2  --  WRITE HEADER TEXT FOR FILE
  wcf.write(7);
  wcf.writeln();
  file_head(wcf, buf);
  wcf.write(1); // If data set name is "All" then "Number of Data Sets" must be equal to one
  wcf.writeln();

  wcf.write("All");
  wcf.write(site.media_type);
  wcf.write(site.num_contam);
  wcf.writeln();


//***********************************************************************************************************
//  STEP 3  --  WRITE HEADER TEXT FOR EACH CONTAMINANT
  for (i=0; i<site.numtotcon; i++)
    {

    printf("\t%d of %d: %s (%s)\n", i+1, site.numtotcon, props[i].name,props[i].cas_id);

    wcf.write(props[i].name);
    wcf.write(props[i].cas_id);
    wcf.write("yr");
    if (props[i].typeflag == 2)
      wcf.write("pCi/mL");    // per WCF specifications 11/2002
    else
      wcf.write("g/mL");      // per WCF specifications 11/2002

    // BLH 2/98 eliminate double pass, count records on output
    pos1 = wcf.getfpos()+1; //  count=flux_count(i,records);
    wcf.write("       ",false);    //  wcf.write(count);

    if (strcmpi(props[i].p_id,props[i].cas_id)==0)
      wcf.write(props[i].progeny);
    else
      {
      wcf.write(props[i].p_name);
      wcf.write(props[i].p_id);
      }
    wcf.writeln();
    sdl.readln();
    sdl.readln();

//***********************************************************************************************************
//  STEP 4  --  READ DATA FROM VERBOSE DATA FILE AND COPY TO OUTPUT FILE
    count=0; done=0;
    for ( j=0; j<records; j++)
      {
      sdl.read(cstr);                       // constituent cas
      if (strcmpi(cstr,props[i].cas_id)==0)
        {
        sdl.read(cstr);                     // parent cas
        if (strcmpi(cstr,props[i].p_id)==0)
          {
          sdl.read(&e_time);          // time(yr)
          wcf.write(e_time);          // Write out TIME
          sdl.read(&thick);         // waste form thickness (cm)
                                    //    = site.thick - erosion (if any)
          for (k=0;k<4;k++)
            sdl.read(&mass);        // remaining mass in waste form (g)
          sdl.read(&cflt);          // decay rk[0]
          sdl.read(&cflt);          // leaching   rk[1]
          sdl.read(&cflt);          // wind       rk[2]
          sdl.read(&cflt);          // water      rk[3]
          sdl.read(&cflt);          // vol        rk[4]
          sdl.read(&cflt);          // src/sink
          sdl.read(&state);         // state
          for (k=0;k<3;k++)
            sdl.read(&erosion);     // erosion (cm)

//        conc = mass / (site.length*site.width*site.thick);  //  site concentration in g/mL
          if (erosion < start_dry_layer)
            wcf.write(0.0);         // remaining overburden is clean: surface conc=0.0
          else
          {
                                    // no overburden: report conc
            sourceVolume=site.length*site.width*thick;
            conc = mass / sourceVolume;                      //  residual concentration (g/mL)
            if (0==strcmpi(site.media_type,"aquifer"))
              conc /= (total_porosity.get_value(e_time*site.delta_t)+
                (site.wz_bulkd * k_d[i].get_value(e_time*site.delta_t)));
            cflt = min(conc, props[i].adj_aqu_sol);            //  Limits concentration to solubility
            if ( props[i].typeflag == 2 )
              cflt *= props[i].spec_act*1e+12;                //  Converts to pCi for rads
            wcf.write(cflt);
          }
          wcf.writeln();

          count++;
          if (state==9 || state==1) done=1;
          }
        }
      if (done==1) break;
      sdl.readln();
      }

    // BLH 2/98 use file position to insert record count
    pos2 = wcf.getfpos();
    wcf.write(pos1,count);
    wcf.setfpos(pos2);
    wcf.newline = true;


    sdl.rewind();
    }
//  STEP 5  --  CLOSE OUTPUT FILE
  wcf.close();
  return;
}

//===========================================================================================================
//  This subroutine creates the .aff output file for the multimedia framework.  It takes lines of output
//  values from the verbose data file (.sdl) and copies them to the Air Flux File.
//===========================================================================================================
void aff_make(char *run_name, long  int records)
{
  char buf[MAXPATH], cstr[32];
  int count, i, k, m, done, state=0, numflux=2;
  float susp=0.0, vol=0.0, cflt=0.0, air_rate[4]={0.0,0.0,0.0,0.0};
  fpos_t pos1, pos2;
  long int j;
//  STEP 1  --  OPEN AND PREPARE WORK FILES
  rstrcpy(buf, run_name);
  strcat(buf, ".aff");
  aff.open(buf, WRITE);
  aff.rewind();
  sdl.rewind();
  printf("Writing contaminant data to .AFF file\n");
//  STEP 2  --  WRITE HEADER TEXT FOR FILE
  aff.write(7);
  aff.writeln();
  file_head(aff, buf);
  aff.write(1); aff.writeln();                                    // number of sets
  aff.write("All"); aff.writeln();                                // name of set
  aff.write(site.arel_type); aff.writeln();                       // point or area release
  aff.write((site.area/10000)); aff.write("m^2"); aff.writeln();  // Exit Area
  aff.write(site.rel_hght); aff.write("m"); aff.writeln();        // Exit Height
  aff.write(site.struc_hght); aff.write("m"); aff.writeln();      // Structure Height
  aff.write(site.exit_vel); aff.write("m/s"); aff.writeln();      // Exit Velocity
  aff.write(site.exit_temp); aff.write("C"); aff.writeln();       // Exit Temperature
  aff.write(site.temp); aff.write("C"); aff.writeln();            // Ambient Temperature
//  Flux Type Loops
//aff.write(2);      // Temporary Hard Code Fix  GAS1==VOLATE  PARTICLE1=7.5 um  PASSIVE SITE
  aff.write((!strcmpi("area",site.arel_type)) ? 4 : 2);
  aff.writeln();                                      //       PARTICLE1=0.3 um  ACTIVE SITE

//aff.write("Gas 1");  aff.write((float)0.0);  aff.write("um");
//aff.write((float)0.0);  aff.write("g/cm^3");  aff.writeln() ; // density

//change in aff specifications - 02/2000 BLH
  aff.write("Gas 1");
  aff.write((float)0.0);  aff.write("fraction");                  // Reactive Gas Fraction
  aff.write((float)0.0);  aff.write("g/cm^3");                    // density
  aff.writeln();

  aff.write("Particle 1"); aff.write((float)0.3); aff.write("um");
  aff.write((float)1.5);  aff.write("g/cm^3");  aff.writeln() ;   // density

  if (!strcmpi("area",site.arel_type)) numflux=4;
  if (numflux==4)
  {
      aff.write("Particle 2"); aff.write((float)3.0); aff.write("um");
      aff.write((float)1.5);  aff.write("g/cm^3");  aff.writeln() ;   // density
      aff.write("Particle 3"); aff.write((float)7.5); aff.write("um");
      aff.write((float)1.5);  aff.write("g/cm^3");  aff.writeln() ;   // density
  }
  aff.write(site.num_contam);                                     // Number of Contaminants
  aff.writeln();
//  STEP 3  --  WRITE HEADER TEXT FOR EACH CONTAMINANT
  for (i=0; i<site.numtotcon; i++)
  {
    printf("\t%d of %d: %s (%s)\n", i+1, site.numtotcon, props[i].name,props[i].cas_id);
    aff.write(props[i].name);
    aff.write(props[i].cas_id);
    aff.write("yr");
    if (props[i].typeflag == 2)
      aff.write("pCi/yr");
    else
      aff.write("g/yr");
    pos1=aff.getfpos()+1;
    aff.write("       ",false);    // write space for record count
    if (strcmpi(props[i].p_id,props[i].cas_id)==0)
      aff.write(props[i].progeny);
    else
    {
      aff.write(props[i].p_name);
      aff.write(props[i].p_id);
    }
    aff.writeln();
    sdl.readln();
    sdl.readln();
//  STEP 4  --  READ DATA FROM VERBOSE DATA FILE AND COPY TO OUTPUT FILE
    count=0; done=0;
    for ( j=0; j<records; j++)
      {
      sdl.read(cstr);
      if (strcmpi(cstr,props[i].cas_id)==0)
      {
        sdl.read(cstr);
        if (strcmpi(cstr,props[i].p_id)==0)
        {
          sdl.read(&cflt);
          aff.write(cflt);           //  Write out TIME
          for (k=0;k<8;k++)
            sdl.read(&susp);         //  Read in suspension rate
          sdl.read(&cflt);
          sdl.read(&vol);            //  Read in volatization rate
          sdl.read(&cflt);           // src/sink
          sdl.read(&state);          // state
          air_rate[0]=vol;
          air_rate[1]=susp;
          if (numflux==4)
          {
          air_rate[1]=susp * wind_eros.nfactor[0];
          air_rate[2]=susp * wind_eros.nfactor[1];
          air_rate[3]=susp * wind_eros.nfactor[2];
          }
          for ( m=0;m<numflux;m++)         //  Multiple flux types write out loop
          {
            if (props[i].typeflag == 2)
              air_rate[m] *= props[i].spec_act*1e+12;   // Converts to pCi for rads
            aff.write(air_rate[m]);
          }
          aff.writeln();
          count++;
//        cflt = air_rate[0]+air_rate[1];
          if (state==9 || state==1)
            done=1;
          for (m=0; m<numflux; m++) air_rate[m] = 0.0;        // Reset array
        }
      }
      sdl.readln();
      if (done==1) break;
    }
    pos2 = aff.getfpos();
    aff.write(pos1,count);
    aff.setfpos(pos2);
    aff.newline = true;
    sdl.rewind();
  }
//  STEP 5  --  CLOSE OUTPUT FILE
  aff.close();
  return;
}

//===========================================================================================================
//  This subroutine creates the .wff output file for the multimedia framework.  It takes lines of output
//  values from the verbose data file (.sdl) and copies them to the .wff file.
//===========================================================================================================

void sff_make(char *run_name, long int records)
  {
  char buf[MAXPATH], cstr[32];
  float cflt=0.0; //, cPrev=0.0;
  int i=0; //,done;
  fpos_t pos1=0, pos2;
  long int count, j, k;

//***********************************************************************************************************
//  STEP 1  --  OPEN AND PREPARE WORK FILES
  rstrcpy(buf,run_name);
//  rstrcpy(buf,site.name);  //  Added to cover for transaction handling in FUI
  strcat(buf, ".sff");
  sff.open(buf,WRITE);
  sff.rewind();
  sdl.rewind();

  printf("Writing contaminant data to .SFF file\n\n");

//***********************************************************************************************************
//  STEP 2  --  WRITE HEADER TEXT FOR FILE
  sff.write(7);
  sff.writeln();
  file_head(sff, buf);
  count = flux_count(i,records);

  sff.write(site.num_media);
  sff.writeln();
  sff.write(site.name);
  sff.write(site.media_type);
  sff.write((float)(site.width/100.));
  sff.write("m");
  sff.write(site.num_contam);
  sff.writeln();
  sff.write("yr");
  sff.write("kg/yr");
  sff.write("m^3/yr");
  sff.write(count);
  sff.writeln();

//***********************************************************************************************************
//  STEP 3  --  READ DATA FROM VERBOSE DATA FILE AND COPY TO OUTPUT FILE FOR WATER FLUX DATA
  count=0;
//  done=0;
  for ( j=0; j<records; j++)
    {
    sdl.read(cstr);
    if (strcmpi(cstr,props[i].cas_id)==0)
      {
      sdl.read(&cflt);
      sdl.read(&cflt);
      sff.write(cflt);      // Write TIME value
      for (k=0;k<3;k++)
        sdl.read(&cflt);    //  Read in SEDIMENT FLUX
      sff.write(cflt);      // Write SEDIMENT FLUX values
      sdl.read(&cflt);
      sff.write(cflt);      // Write WATER FLUX values
      sff.writeln();
      count++;
//      if (count>=2)
//        if (cflt<cPrev && cflt<props[i].molecule) done=1;
//      cPrev = cflt;
      }
    sdl.readln();
    }
  pos2 = sff.getfpos();
  sff.write(pos1,(long)0);
  sff.setfpos(pos2);
  sff.newline = true;
  sdl.rewind();

//***********************************************************************************************************
//  STEP 4  --  WRITE HEADER TEXT FOR EACH CONTAMINANT
  for (i=0; i<site.numtotcon; i++)
    {

    printf("\t%d of %d: %s (%s)\n", i+1, site.numtotcon, props[i].name,props[i].cas_id);

    sff.write(props[i].name);
    sff.write(props[i].cas_id);
    sff.write("yr");
    if (props[i].typeflag == 2)
      sff.write("pCi/yr");
    else
      sff.write("g/yr");
    sff.write(count);
    if (strcmpi(props[i].p_id,props[i].cas_id)==0)
      sff.write(props[i].progeny);
    else
      {
      sff.write(props[i].p_name);
      sff.write(props[i].p_id);
      }
    sff.writeln();
    sdl.readln();
    sdl.readln();

//***********************************************************************************************************
//  STEP 5  --  READ DATA FROM VERBOSE DATA FILE AND COPY TO OUTPUT FILE
    for ( j=0; j<records; j++)
      {
      sdl.read(cstr);
      if (strcmpi(cstr,props[i].cas_id)==0)
        {
        sdl.read(&cflt);
        sdl.read(&cflt);
        sff.write(cflt);       // Write out TIME
        for (k=0;k<9;k++)
          sdl.read(&cflt);
        if ( props[i].typeflag == 2 )
          cflt *= props[i].spec_act*1e+12;   // Converts to pCi for rads
        sff.write(cflt);
        sff.writeln();
        }
      sdl.readln();
      }

    pos2 = sff.getfpos();
    sff.write(pos1,count);
    sff.setfpos(pos2);
    sdl.rewind();
    }
//  STEP 6  --  CLOSE OUTPUT FILE
  sff.close();
  return;
}


//===========================================================================================================
// Return the number of time/flux pairs for contaminant 'number'.
//===========================================================================================================
long int flux_count(int number, long int records)
{
  char cstr[32];
  int  done=0, i, state=0;
  float cflt=0.0;
  long int count=0, rec;

//  STEP 1  --  PASSES HEADER TEXT IN .SDL FILE
  sdl.readln();
  sdl.readln();
//  STEP 2  --  PERFORM SINGLE PASS OF DATA
  for (rec=3; rec<=records; rec++)
  {
    sdl.read(cstr);
    if (strcmpi(cstr,props[number].cas_id)==0)
    {
      sdl.read(cstr);
      if (strcmpi(cstr,props[number].p_id)==0)
      {
//  STEP 3  --  CHECK STATE STATUS FLAG
        for (i=0; i<11; i++)
          sdl.read(&cflt);
        sdl.read(&state);
//  STEP 4  --  INCREMENT COUNT IF NOT DONE
        if ( done == 0 )
          count++;
//  STEP 5  --  SET THE DONE FLAG
        if (state==9 || state==1)  // fixed 02/2001 BLH
        {
          done = 1;
          rec=records;
        }
      }
    }
    sdl.readln();
  }
  sdl.rewind();
  return(count);
}
#endif


void wff_make_type(int infiltration, long int count, long int records)
{
  int i,j;
  int done, state=0;
  int numFluxTypes=1;
  float cLeach=0.0, cWater=0.0, cflt=0.0;
  char cstr[32];

  fpos_t pos1, pos2;

  wff.write("All");      // dataset name
  if (infiltration)
  {
    if (0==strcmpi(site.media_type,"aquifer"))
      wff.write("Aquifer");
    else
      wff.write("Vadose");
  }
  else
  {
    wff.write("Surface Water");
    numFluxTypes=2;
  }

  wff.write((float)(site.width/100.));  // width
  wff.write("m");
  wff.write((float)(site.length/100.)); // length
  wff.write("m");
  wff.write((float)0.0);//(site.thick/100.)); JPM changes to WFF file 10/18/96
  wff.write("m");
  if (infiltration)
    wff.write((float)(site.fin_flux/100.));
  else  // overland
    wff.write((float)(site.ovl_flux/100.));
  wff.write("m/yr");
  wff.write(site.num_contam);
  wff.writeln();
  wff.write("yr");
  wff.write("m^3/yr");
//  wff.write(count);     incorporated into output_series() below  4/98 BLH

//***********************************************************************************************************
//  STEP 3  --  READ DATA FROM VERBOSE DATA FILE AND COPY TO OUTPUT FILE FOR WATER FLUX DATA
/// 4/98 BLH substituted output_series because contaminants no longer have same number of records in SDL file

  if (infiltration)
    darcy_inf.output_series(&wff);
  else
    over_flow.output_series(&wff);

  /*
  for ( j=0; j<records; j++)
    {
    sdl.read(cstr);     // contam
  if (strcmpi(cstr,props[0].cas_id)==0)
    {
      sdl.read(&cflt);    // parent
      sdl.read(&cflt);    // time
      wff.write(cflt);
      sdl.read(&cflt);    // thick
      sdl.read(&cInf);    // infiltration   cm/y
      sdl.read(&cflt);    // sediment       kg/y
      sdl.read(&cOvl);    // overland       m^3/yr
      if (infiltration)
        wff.write((float)((cInf/100.)*((site.width/100.)*(site.length/100.))));  // Write WATER_FLUX values
      else
        wff.write(cOvl);
      wff.writeln();
    }
    sdl.readln();
    }
  */
  sdl.rewind();


//***********************************************************************************************************
//  STEP 4  --  WRITE HEADER TEXT FOR EACH CONTAMINANT
  for (i=0; i<site.numtotcon; i++)
  {
    printf("\t%d of %d: %s (%s)\n", i+1, site.numtotcon, props[i].name,props[i].cas_id);
    wff.write(props[i].name);
    wff.write(props[i].cas_id);
    wff.write("yr");
    if (props[i].typeflag == 2)
      wff.write("pCi/yr");
    else
      wff.write("g/yr");
    pos1 = wff.getfpos()+1;
    wff.write("       ",false);        // write space for count
    wff.write(numFluxTypes);           // number of flux types
    if (strcmpi(props[i].p_id,props[i].cas_id)==0)
      wff.write(props[i].progeny);
    else
    {
      wff.write(props[i].p_name);
      wff.write(props[i].p_id);
    }
    wff.writeln();
    sdl.readln();
    sdl.readln();

//***********************************************************************************************************
//  STEP 5  --  READ DATA FROM VERBOSE DATA FILE AND COPY TO OUTPUT FILE FOR CONTAMINANT FLUXES
    done=0;
    count=0;
    for ( j=0; j<records; j++)
    {
      sdl.read(cstr);         // contam
      if (strcmpi(cstr,props[i].cas_id)==0)
      {
        sdl.read(cstr);       // parent
        if (strcmpi(cstr,props[i].p_id)==0)
        {
          sdl.read(&cflt);    // time
          wff.write(cflt);
          sdl.read(&cflt);    // thick
          sdl.read(&cflt);    // infiltration
          sdl.read(&cflt);    // sediment
          sdl.read(&cflt);    // overland
          sdl.read(&cflt);    // mass
          sdl.read(&cflt);    // decay rk[0]
          sdl.read(&cLeach);  // leaching   rk[1]
          sdl.read(&cflt);    // wind       rk[2]
          sdl.read(&cWater);  // water      rk[3]
          sdl.read(&cflt);    // vol        rk[4]
          sdl.read(&cflt);    // src/sink
          sdl.read(&state);   // state
//        cflt = cLeach+cWater;
          if (infiltration)              cflt = cLeach;
          else                           cflt = cWater;
          if (props[i].typeflag == 2)    cflt *= props[i].spec_act*1e+12;   // Converts to pCi for rads
          wff.write(cflt);
          if (numFluxTypes == 2)
            wff.write(0.0);
          wff.writeln();
          count++;
          if (state==9 || state==1)
            done=1;
        }
      }
      if (done==1) break;
      sdl.readln();
    }
    pos2 = wff.getfpos();
    wff.write(pos1,count);
    wff.setfpos(pos2);
    wff.newline = true;
    sdl.rewind();
  }
}
