"aqu3",0000000204
****************************VERSION 06/18/1999****************************


RADCON -- WATERBORNE TRANSPORT COMPONENT for the Multimedia
Environmental Pollutant Assessment System (MEPAS):
Models movement of radionuclides and other chemicals
in groundwater, surface water, and overland pathways.

Battelle, Pacific Northwest Laboratory (PNL)
P.O. Box 999, Richland, WA 99352

Developed for the U.S. Department of Energy


Input file for this run is MEPAS_AQ.WIN
******************************************************************************
Modeling Parent Contaminant Number   1 for Scenario Number   1
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  1
Usage Location Medium =  1
Usage Location Name   = riv4

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

AMERICIU         1.000E+04 0.000E+00      1.000E+04
NEPTUNIU         1.000E+04 0.000E+00      1.000E+04
PLUTONIU         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          AMERICIU            2
2          NEPTUNIU            2
3          PLUTONIU            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  3

Source-Term Configuration (ISCONF)        =  6
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  4

Length of Release Unit (AL1 OR CLEN)                =   3.00E+03 cm
Width of Release Unit (B1)                          =   3.00E+03 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   2.05E+01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

AMERICIU     AM243    7.39E+03   2.22E+04  1.00E+00     1.00E+23    2.01E+13
NEPTUNIU     NP239    6.46E-03  -9.99E+01  0.00E+00     1.00E+23    1.01E-30
PLUTONIU     PU239    2.41E+04  -9.99E+01  0.00E+00     1.00E+23    1.01E-30


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+12  0.000E+00        1    2     1.00E+12  2.000E+01
2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
3    1     0.00E+00  0.000E+00        3    2     0.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+12  0.000E+00        1    2     1.00E+12  2.000E+01
2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
3    1     0.00E+00  0.000E+00        3    2     0.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       AMERICIU              2.000E+01
2       NEPTUNIU              1.000E+00
3       PLUTONIU              1.000E+00
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  AMERICIU   7.390E+03   1.000E+00   7.478E+00     1.598E+01     1.766E+01
2  NEPTUNIU   6.460E-03   0.000E+00   1.000E+00     2.311E-01     2.361E+00
3  PLUTONIU   2.410E+04   0.000E+00   1.000E+00     2.137E+00     2.361E+00

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.50E+02 cm
Z-Direction Dispersivity (A3)                   =   1.90E+00 cm
Bulk Density (R2)                               =   1.49E+00 g/cm**3
Total Porosity (AN4)                            =   4.37E-01 (fraction)
Effective Porosity (AN5)                        =   2.30E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   7.50E+03 cm
Perpendicular Distance Off Flow Centerline (Y2) =   0.00E+00 cm
Angle Between Flow Direction and Receptor
Direction (ANGLE)                               =   0.00E+00 deg
Depth Below Watertable of Calculated
Concentration at Receptor (Z1)                  =   0.00E+00 cm
Length of Burial Area (AL1)                     =   3.00E+03 cm
Width of Burial Area (B1)                       =   3.00E+03 cm
Depth of Release Unit Below Water Table (HW)    =   0.00E+00 cm

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

AMERICIU     AM243    7.39E+03   2.22E+04  1.00E+00     1.00E+23    2.01E+13
NEPTUNIU     NP239    6.46E-03   1.00E+30  0.00E+00     1.00E+23    1.01E-30
PLUTONIU     PU239    2.41E+04   1.00E+30  0.00E+00     1.00E+23    1.01E-30



AMERICIU (AM243   ): DECAY CONSTANT =  9.380E-05 1/YR
NEPTUNIU (NP239   ): DECAY CONSTANT =  1.073E+02 1/YR
PLUTONIU (PU239   ): DECAY CONSTANT =  2.876E-05 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.942E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.75E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

AMERICIU  AM243     7.988E-01  NUMERICAL    2.000E+13     1.996E+13 (  **  )
NEPTUNIU  NP239     2.311E-01  NUMERICAL    0.000E+00     0.000E+00 (  **  )
PLUTONIU  PU239     2.137E+00  NUMERICAL    0.000E+00     0.000E+00 (  **  )

**  Decaying constituent

(a) RRATIO= 3.000E+01.  RATIO is the estimated time to peak divided by the
duration of time over which the contaminant enters this medium.  When
RATIO >= RRATIO, the equation for an instantaneous release is used,
otherwise the numerical solution for a time varying release is used.



******************************************************************************
Times are given as years since the start of risk calculations (WS-TRISK)

Maximum Constituent Fluxes:


Maximum
Flux
Name       CASID    Type(a)   (g/yr or pCi/yr)      Time (Years)
--------    --------  -------   ---------------   --------------------

AMERICIU    AM243      PEAK        7.951E+11          28.5
NEPTUNIU    NP239      PEAK        7.951E+11          28.5
PLUTONIU    PU239      PEAK        4.038E+08          28.5

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
