"aqu2",0000000194
****************************VERSION 08/08/2000****************************


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
Usage Location Name   = exp3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Benzene          1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Benzene            41
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

Source-Term Configuration (ISCONF)        =  6
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  3

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   2.74E-01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Benzene      71432    7.50E+14   5.65E-06  1.40E-01     1.75E-03    1.09E+03


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  0.000E+00        1    2     5.65E+01  1.000E+00
1    3     5.34E+01  2.000E+00        1    4     5.04E+01  3.000E+00
1    5     4.77E+01  4.000E+00        1    6     4.51E+01  5.000E+00
1    7     4.26E+01  6.000E+00        1    8     4.02E+01  7.000E+00
1    9     3.80E+01  8.000E+00        1   10     3.59E+01  9.000E+00
1   11     3.40E+01  1.000E+01        1   12     3.21E+01  1.100E+01
1   13     3.03E+01  1.200E+01        1   14     2.87E+01  1.300E+01
1   15     2.71E+01  1.400E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  0.000E+00        1    2     5.65E+01  1.000E+00
1    3     5.34E+01  2.000E+00        1    4     5.04E+01  3.000E+00
1    5     4.77E+01  4.000E+00        1    6     4.51E+01  5.000E+00
1    7     4.26E+01  6.000E+00        1    8     4.02E+01  7.000E+00
1    9     3.80E+01  8.000E+00        1   10     3.59E+01  9.000E+00
1   11     3.40E+01  1.000E+01        1   12     3.21E+01  1.100E+01
1   13     3.03E+01  1.200E+01        1   14     2.87E+01  1.300E+01
1   15     2.71E+01  1.400E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Benzene               8.400E+01
Modeling Medium #             1
Medium Type (MED)          =  3
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A WELL




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Benzene    7.500E+14   1.400E-01   2.738E+00     4.071E-02     4.498E-02

Darcy Velocity (DARCYV)                         =   1.00E+02 cm/day
Pore-water Velocity (U)                         =   8.33E+02 cm/day
Aquifer Thickness (H1)                          =   1.00E+03 cm
X-Direction Dispersivity (A1)                   =   5.00E+02 cm
Y-Direction Dispersivity (A2)                   =   1.65E+02 cm
Z-Direction Dispersivity (A3)                   =   1.25E+00 cm
Bulk Density (R2)                               =   1.49E+00 g/cm**3
Total Porosity (AN4)                            =   4.37E-01 (fraction)
Effective Porosity (AN5)                        =   1.20E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   5.00E+03 cm
Perpendicular Distance Off Flow Centerline (Y2) =   0.00E+00 cm
Angle Between Flow Direction and Receptor
Direction (ANGLE)                               =   0.00E+00 deg
Depth Below Watertable of Calculated
Concentration at Receptor (Z1)                  =   0.00E+00 cm
Length of Burial Area (AL1)                     =   1.00E+03 cm
Width of Burial Area (B1)                       =   1.00E+03 cm
Depth of Release Unit Below Water Table (HW)    =   0.00E+00 cm

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Benzene      71432    7.50E+14   5.65E-06  1.40E-01     1.75E-03    1.09E+03



Benzene  (71432   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  1.522E+08 cm^2/yr
Lateral Dispersion Coefficient =       5.022E+07 cm^2/yr
Vertical Dispersion Coefficient =      3.807E+05 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

Benzene   71432     4.846E-04  NUMERICAL    1.077E+03

(a) RRATIO= 3.000E+01.  RATIO is the estimated time to peak divided by the
duration of time over which the contaminant enters this medium.  When
RATIO >= RRATIO, the equation for an instantaneous release is used,
otherwise the numerical solution for a time varying release is used.


******************************************************************************
Times are given as years since the start of risk calculations (WS-TRISK)

Concentration for Benzene  when risk calculations begin (0.0 yrs) is:
0.000E+00 (g/ml or pCi/ml)


******************************************************************************
Times are given as years since the start of risk calculations (WS-TRISK)

Maximum Constituent Concentrations:


Maximum
Concentration
Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
--------    --------  -------   ---------------   --------------------

Benzene     71432      PEAK        3.143E-09           2.1

(a)
PEAK: The maximum concentration occurs as a peak concentration
at the given time.


******************************************************************************
