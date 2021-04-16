/*______________________________________________________________________________

   Date:       February 2001
   Programmer: Bonnie Hoopes
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "atoSSrc.h"
#include "frames.h"

#define PI 3.14159
#define LOC_PT1    0
#define LOC_PT2    1
#define LOC_PT3    2
#define LOC_DIST   0
#define LOC_X      1
#define LOC_Y      2
#define LOC_VAL    3

atoSSrc::~atoSSrc(){}

atoSSrc::atoSSrc(GIDFILE *g, int idx, void (*func)(char *casid, char *pcas, Series *TS))
{
  G = g;
  siteidx = idx;
  AddSeries = func;
}

void atoSSrc::getSeries(char *fname, char *srcid, char *atoid)
{
  TS = new Series();

  length = ratof(info(G,"stlength",siteidx)) / 100.0; // cm -> m
  width = ratof(info(G,"stwidth",siteidx)) / 100.0;  // cm -> m
  area = length * width;

  modid = Get_Element(G,"ModID");

  srcidx = findidx(modid,srcid,2);
  atoidx = findidx(modid,atoid,2);

  srcLocX = ratof(info(G,"ModLocX",siteidx,srcidx)); // km
  srcLocY = ratof(info(G,"ModLocY",siteidx,srcidx)); // km

  srcLocX *= 1000.0; // km --> m
  srcLocY *= 1000.0; // km --> m

  atoLocX = ratof(info(G,"ModLocX",siteidx,atoidx)); // km
  atoLocY = ratof(info(G,"ModLocY",siteidx,atoidx)); // km
  atoLocY *= 1000.0; // km -> m
  atoLocX *= 1000.0; // km -> m

  dist = 0.0;
  dist = pow(srcLocX-atoLocX,2) + pow(srcLocY-atoLocY,2);
  if (dist > 0.0)
    dist = sqrt(dist);

  atoOpen(fname, atoid);
  int numds = atoGetDatasetCount();
  for (int ds=0; ds<numds; ds++)
  {
    char release[64];
    char coord[64];
    char spatial[64];

    atoGetReleaseType(ds, release);
    atoGetCoordinateType(ds, coord);
    atoGetSpatialType(ds, spatial);
    int numCon = atoGetConstituentCount(ds);
    for (int ncon=0; ncon<numCon; ncon++)
    {
      char pname[64];
      char pcas[64];

      atoGetChemName(ds,ncon,-1,pname);
      atoGetCasID(ds,ncon,-1,pcas);

      int numProg = atoGetProgenyCount(ds, ncon);
      for (int nprog=-1; nprog<numProg; nprog++)
      {
        char cname[64];
        char casid[64];
        int numTimes;
        atoGetChemName(ds, ncon, nprog, cname);
        atoGetCasID(ds, ncon, nprog, casid);
        numTimes = atoGetTimePeriodCount(ds, ncon, nprog);

        TS->Init(numTimes);

        for (int ntime=0; ntime<numTimes; ntime++)
        {
          char dataunit[32];
          char timeunit[32];  // acute=hr, chronic=yr
          double timeperiod = atoGetTimePeriodTime(ds, ncon, nprog, ntime);
          atoGetTimePeriodUnit(ds,ncon,nprog,ntime,timeunit);
          int numOutput = atoGetTimePeriodOutputCount(ds, ncon, nprog, ntime);

          for (int i=LOC_PT1;i<=LOC_PT3;i++)
          {
            for (int j=LOC_DIST;j<LOC_VAL;j++) loc[i][j]=-1.0;
            loc[i][LOC_VAL]=0.0;
          }

          if (0==strcmpi(spatial,"points"))
           this->getThreeClosestPoints(ds, ncon, nprog, ntime, 0);

          if (0==strcmpi(spatial,"grid"))
            this->getThreeClosestGridPoints(coord, ds, ncon, nprog, ntime, 0);

          for (int nout=0; nout<numOutput; nout++)
          {
            char gridtype[64];
            atoGetGridOutputType(ds, ncon, nprog, ntime, nout, gridtype);
            if (0==strcmpi(gridtype,"Deposition Rate"))
            {
              atoGetGridDataUnit(ds,ncon,nprog,ntime,nout,dataunit);
              for (int i=LOC_PT1;i<=LOC_PT3;i++)
              {
                int ix = loc[i][LOC_X];
                int iy = loc[i][LOC_Y];
                if (ix>=0 && iy>=0)
                {
                  float val = atoGetGridValue(ds,ncon,nprog,ntime,nout,ix,iy);
                  loc[i][LOC_VAL]+=val;
                }
              }
            }
          }

          float ato=0.0;
          for (int i=LOC_PT1;i<=LOC_PT3;i++)
           if (loc[i][LOC_VAL]>ato && loc[i][LOC_VAL]>0) ato=loc[i][LOC_VAL];

          if (ato>0.0)
          {
            ato = ato * area;  // // unit/m^2/time --> unit/time
            if (0==strncmpi(dataunit,"Bq",2)) ato = ato * 2.7e-11; // Bq --> Ci
            if (0==strncmpi(dataunit,"Kg",2)) ato = ato * 1000.0;  // kg --> g
            if ((0==strcmpi(release,"acute")) && (0==strcmpi(timeunit,"hr")))
            { // yr = hr * 1.14e-4
              timeperiod = timeperiod * 1.14e-4;
              ato = ato * 1.14e-4;
            }
          }
          TS->Replace(ntime, timeperiod, ato);
        }
        AddSeries(casid, pcas, TS);
      }
    }
  }
  delete TS;
}

void atoSSrc::getThreeClosestGridPoints(char *coord, int ds, int ncon, int nprog, int ntime, int nout)
{
  float y, x, locx=0.0, locy=0.0;
  int xcount = atoGetGridAxis1Count(ds, ncon, nprog, ntime, nout);
  int ycount = atoGetGridAxis2Count(ds, ncon, nprog, ntime, nout);
  // locate three grid points closes to center of sec.source
  for (int iy=0; iy<ycount; iy++)
  { // polar: directions, cartesian: y coordinate
    // y = 0 = North, 90=E, 180=S, 270=W
    y = atoGetGridAxis2Value(ds,ncon,nprog,ntime,nout,iy);
    y = (360.0-y)-270.0;
    if (y<0) y+=360.0;    // polar angle (deg)
    y = (PI/180.0) * y;      // convert degrees to radians
    for (int ix=0; ix<xcount; ix++)
    { // polar: radial distances, cartesian: x coordinate
      x = atoGetGridAxis1Value(ds,ncon,nprog,ntime,nout,ix);
      if (0==strcmpi(coord,"polar"))
      {
        locx = atoLocX + (x * cos(y));
        locy = atoLocY + (x * sin(y));

/* =============================================================================
fVECT is the WIND VECTOR AZIMUTH, i.e. the direction TOWARDS which the wind is blowing.
It increases clockwise from North when viewed from above.
Terms such as northward, eastward etc. imply wind vector azimuths.

fMET is the METEOROLOGICAL WIND DIRECTION, i.e. the direction FROM which the wind is blowing.
It also increases clockwise from North when viewed from above.
Terms such as northerly, easterly etc. imply meteorological wind directions.

These directions are typically expressed in units of degrees, f(deg), but can either be in
the interval -180° to +180° or 0° to 360°. The wind vector azimuth and meteorological
convention direction are related by:
             fMET(deg) = fVECT(deg)+180

subtracting 360° where appropriate in order to keep the values within the desired range.
Note that when writing a computer program to convert between speed/direction and
orthogonal component conventions, the use of trigonometric functions assumes that
angles are expressed in units of radians, f(rad), rather than degrees (pocket calculators
 can typically perform trigonometric functions on angles expressed in either units).
 Directions are converted from units of degrees to radians through the relationship:

        rad = (PI/180) * deg

Moreover, the familiar expressions relating the x component of a vector to the cosine
of its angle and the y component to its sine imply use of:
        fPOLAR which is the WIND VECTOR POLAR ANGLE in two-dimensions.
        It increases ANTICLOCKWISE FROM the +ve x-axis, i.e. from EAST;
        this in the opposite sense to the wind vector azimuth and
        the meteorological wind direction, and from a different origin.
==============================================================================*/
      }
      if (0==strcmpi(coord,"cartesian"))
      {
        locx = atoLocX + x;
        locy = atoLocY + y;
      }
      dist = sqrt(pow(srcLocX-locx,2) + pow(srcLocY-locy,2));
      for (int i=LOC_PT1;i<=LOC_PT3;i++)
      {
        if ((dist<loc[i][LOC_DIST] || loc[i][LOC_DIST]<0.0))
        {
          for (int j=LOC_PT3;j>i;j--)
          {
            for (int k=LOC_DIST;k<LOC_Y;k++)
            {
              loc[j][k]=loc[j-1][k];
            }
          }
          loc[i][LOC_DIST] = dist;
          loc[i][LOC_X] = ix;
          loc[i][LOC_Y] = iy;
          break;
        }
      }
    }
  }
}

void atoSSrc::getThreeClosestPoints(int ds, int ncon, int nprog, int ntime, int nout)
{
  float y, x;
  int xcount = atoGetGridAxis1Count(ds, ncon, nprog, ntime, nout);
  int ycount = atoGetGridAxis2Count(ds, ncon, nprog, ntime, nout);
  for (int iy=0; iy<ycount; iy++)
  {
    y = atoGetGridAxis2Value(ds,ncon,nprog,ntime,nout,iy);
    y *= 1000.0; // km --> m
    for (int ix=0; ix<xcount; ix++)
    {
      x = atoGetGridAxis1Value(ds,ncon,nprog,ntime,nout,ix);
      x *= 1000.0; // km --> m

      dist = sqrt(pow(srcLocX-x,2) + pow(srcLocY-y,2));
      for (int i=LOC_PT1;i<LOC_PT2;i++)
      {
        if ((dist<loc[i][LOC_DIST] || loc[i][LOC_DIST]<0.0))
        {
          for (int j=LOC_PT3;j>i;j--)
            for (int k=LOC_DIST;k<LOC_Y;k++)
              loc[j][k]=loc[j-1][k];
          loc[i][LOC_DIST] = dist;
          loc[i][LOC_X] = ix;
          loc[i][LOC_Y] = iy;
          break;
        }
      }
    }
  }
}


