"aqu3",0000000345
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
Usage Location Name   = fcm6

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Acetone          1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Acetone             2
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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+01 cm
Width of Release Unit (B1)                          =   1.50E+02 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   7.10E-02 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    7.50E+14   1.43E-03  5.08E-04     1.00E+00    1.31E+04


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+02  0.000E+00        1    2     1.00E+02  1.300E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+02  0.000E+00        1    2     1.00E+02  1.300E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Acetone               1.300E+02
Modeling Medium #             1
Medium Type (MED)          =  3
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A WELL




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Acetone    7.500E+14   5.080E-04   1.008E+00     1.122E+01     1.240E+01

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   7.35E+04 cm
Y-Direction Dispersivity (A2)                   =   2.43E+04 cm
Z-Direction Dispersivity (A3)                   =   1.84E+02 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   7.35E+05 cm
Perpendicular Distance Off Flow Centerline (Y2) =   0.00E+00 cm
Angle Between Flow Direction and Receptor
Direction (ANGLE)                               =   0.00E+00 deg
Depth Below Watertable of Calculated
Concentration at Receptor (Z1)                  =   0.00E+00 cm
Length of Burial Area (AL1)                     =   5.00E+01 cm
Width of Burial Area (B1)                       =   1.50E+02 cm
Depth of Release Unit Below Water Table (HW)    =   0.00E+00 cm

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    7.50E+14   1.43E-03  5.08E-04     1.00E+00    1.31E+04



Acetone  (67641   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  4.393E+09 cm^2/yr
Lateral Dispersion Coefficient =       1.452E+09 cm^2/yr
Vertical Dispersion Coefficient =      1.100E+07 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

Acetone   67641     8.630E-02  NUMERICAL    1.300E+04

(a) RRATIO= 3.000E+01.  RATIO is the estimated time to peak divided by the
duration of time over which the contaminant enters this medium.  When
RATIO >= RRATIO, the equation for an instantaneous release is used,
otherwise the numerical solution for a time varying release is used.


******************************************************************************
Times are given as years since the start of risk calculations (WS-TRISK)

Concentration for Acetone  when risk calculations begin (0.0 yrs) is:
0.000E+00 (g/ml or pCi/ml)


******************************************************************************
Times are given as years since the start of risk calculations (WS-TRISK)

Maximum Constituent Concentrations:


Maximum
Concentration
Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
--------    --------  -------   ---------------   --------------------

Acetone     67641      PEAK        9.995E-11          95.4

(a)
PEAK: The maximum concentration occurs as a peak concentration
at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   2 for Scenario Number   1
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  1
Usage Location Medium =  1
Usage Location Name   = fcm6

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

TRITIUM          1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          TRITIUM             2
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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+01 cm
Width of Release Unit (B1)                          =   1.50E+02 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   7.10E-02 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

TRITIUM      H3       1.23E+01   1.43E+05  0.00E+00     1.00E+23    1.31E+12


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+10  0.000E+00        1    2     1.00E+10  1.300E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+10  0.000E+00        1    2     1.00E+10  1.300E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       TRITIUM               1.300E+02
Modeling Medium #             1
Medium Type (MED)          =  3
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A WELL




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  TRITIUM    1.230E+01   0.000E+00   1.000E+00     9.961E+00     1.230E+01

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   7.35E+04 cm
Y-Direction Dispersivity (A2)                   =   2.43E+04 cm
Z-Direction Dispersivity (A3)                   =   1.84E+02 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   7.35E+05 cm
Perpendicular Distance Off Flow Centerline (Y2) =   0.00E+00 cm
Angle Between Flow Direction and Receptor
Direction (ANGLE)                               =   0.00E+00 deg
Depth Below Watertable of Calculated
Concentration at Receptor (Z1)                  =   0.00E+00 cm
Length of Burial Area (AL1)                     =   5.00E+01 cm
Width of Burial Area (B1)                       =   1.50E+02 cm
Depth of Release Unit Below Water Table (HW)    =   0.00E+00 cm

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

TRITIUM      H3       1.23E+01   1.43E+05  0.00E+00     1.00E+23    1.31E+12



TRITIUM  (H3      ): DECAY CONSTANT =  5.635E-02 1/YR

Longitudinal Dispersion Coefficient =  4.393E+09 cm^2/yr
Lateral Dispersion Coefficient =       1.452E+09 cm^2/yr
Vertical Dispersion Coefficient =      1.100E+07 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

TRITIUM   H3        7.662E-02  NUMERICAL    1.300E+12

(a) RRATIO= 3.000E+01.  RATIO is the estimated time to peak divided by the
duration of time over which the contaminant enters this medium.  When
RATIO >= RRATIO, the equation for an instantaneous release is used,
otherwise the numerical solution for a time varying release is used.


******************************************************************************
Times are given as years since the start of risk calculations (WS-TRISK)

Concentration for TRITIUM  when risk calculations begin (0.0 yrs) is:
0.000E+00 (g/ml or pCi/ml)


******************************************************************************
Times are given as years since the start of risk calculations (WS-TRISK)

Maximum Constituent Concentrations:


Maximum
Concentration
Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
--------    --------  -------   ---------------   --------------------

TRITIUM     H3         PEAK        4.917E-03          52.1

(a)
PEAK: The maximum concentration occurs as a peak concentration
at the given time.


******************************************************************************
