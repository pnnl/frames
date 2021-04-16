"aqu3",0000002005
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

1  Acetone    7.500E+14   5.080E-04   1.008E+00     4.763E-01     5.263E-01

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   3.12E+03 cm
Y-Direction Dispersivity (A2)                   =   1.04E+03 cm
Z-Direction Dispersivity (A3)                   =   8.00E+00 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   3.12E+04 cm
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

Longitudinal Dispersion Coefficient =  1.865E+08 cm^2/yr
Lateral Dispersion Coefficient =       6.216E+07 cm^2/yr
Vertical Dispersion Coefficient =      4.783E+05 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

Acetone   67641     3.664E-03  NUMERICAL    1.300E+04

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

Acetone     67641      SS          2.345E-09           3.4 to    127.2

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


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

1  TRITIUM    1.230E+01   0.000E+00   1.000E+00     4.699E-01     5.220E-01

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   3.12E+03 cm
Y-Direction Dispersivity (A2)                   =   1.04E+03 cm
Z-Direction Dispersivity (A3)                   =   8.00E+00 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   3.12E+04 cm
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

Longitudinal Dispersion Coefficient =  1.865E+08 cm^2/yr
Lateral Dispersion Coefficient =       6.216E+07 cm^2/yr
Vertical Dispersion Coefficient =      4.783E+05 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

TRITIUM   H3        3.615E-03  NUMERICAL    1.300E+12

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

TRITIUM     H3         SS          2.270E-01           3.4 to    127.2

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   1 for Scenario Number   2
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  2
Usage Location Medium =  1
Usage Location Name   = fcm7

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

1  Acetone    7.500E+14   5.080E-04   1.008E+00     9.540E-01     1.054E+00

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   6.25E+03 cm
Y-Direction Dispersivity (A2)                   =   2.06E+03 cm
Z-Direction Dispersivity (A3)                   =   1.60E+01 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   6.25E+04 cm
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

Longitudinal Dispersion Coefficient =  3.736E+08 cm^2/yr
Lateral Dispersion Coefficient =       1.231E+08 cm^2/yr
Vertical Dispersion Coefficient =      9.565E+05 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

Acetone   67641     7.339E-03  NUMERICAL    1.300E+04

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

Acetone     67641      SS          1.177E-09           7.0 to    127.6

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   2 for Scenario Number   2
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  2
Usage Location Medium =  1
Usage Location Name   = fcm7

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

1  TRITIUM    1.230E+01   0.000E+00   1.000E+00     9.365E-01     1.046E+00

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   6.25E+03 cm
Y-Direction Dispersivity (A2)                   =   2.06E+03 cm
Z-Direction Dispersivity (A3)                   =   1.60E+01 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   6.25E+04 cm
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

Longitudinal Dispersion Coefficient =  3.736E+08 cm^2/yr
Lateral Dispersion Coefficient =       1.231E+08 cm^2/yr
Vertical Dispersion Coefficient =      9.565E+05 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

TRITIUM   H3        7.204E-03  NUMERICAL    1.300E+12

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

TRITIUM     H3         SS          1.104E-01           7.0 to    127.3

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   1 for Scenario Number   3
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  3
Usage Location Medium =  1
Usage Location Name   = fcm8

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

1  Acetone    7.500E+14   5.080E-04   1.008E+00     1.908E+00     2.108E+00

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   1.25E+04 cm
Y-Direction Dispersivity (A2)                   =   4.13E+03 cm
Z-Direction Dispersivity (A3)                   =   3.10E+01 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   1.25E+05 cm
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

Longitudinal Dispersion Coefficient =  7.471E+08 cm^2/yr
Lateral Dispersion Coefficient =       2.468E+08 cm^2/yr
Vertical Dispersion Coefficient =      1.853E+06 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

Acetone   67641     1.468E-02  NUMERICAL    1.300E+04

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

Acetone     67641      SS          5.878E-10          11.1 to    127.6

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   2 for Scenario Number   3
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  3
Usage Location Medium =  1
Usage Location Name   = fcm8

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

1  TRITIUM    1.230E+01   0.000E+00   1.000E+00     1.854E+00     2.091E+00

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   1.25E+04 cm
Y-Direction Dispersivity (A2)                   =   4.13E+03 cm
Z-Direction Dispersivity (A3)                   =   3.10E+01 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   1.25E+05 cm
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

Longitudinal Dispersion Coefficient =  7.471E+08 cm^2/yr
Lateral Dispersion Coefficient =       2.468E+08 cm^2/yr
Vertical Dispersion Coefficient =      1.853E+06 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

TRITIUM   H3        1.426E-02  NUMERICAL    1.300E+12

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

TRITIUM     H3         SS          5.174E-02          11.1 to    126.9

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   1 for Scenario Number   4
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  4
Usage Location Medium =  1
Usage Location Name   = fcm9

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

1  Acetone    7.500E+14   5.080E-04   1.008E+00     3.816E+00     4.217E+00

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   2.50E+04 cm
Y-Direction Dispersivity (A2)                   =   8.25E+03 cm
Z-Direction Dispersivity (A3)                   =   6.30E+01 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   2.50E+05 cm
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

Longitudinal Dispersion Coefficient =  1.494E+09 cm^2/yr
Lateral Dispersion Coefficient =       4.931E+08 cm^2/yr
Vertical Dispersion Coefficient =      3.766E+06 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

Acetone   67641     2.936E-02  NUMERICAL    1.300E+04

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

Acetone     67641      SS          2.941E-10          20.6 to    129.3

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   2 for Scenario Number   4
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  4
Usage Location Medium =  1
Usage Location Name   = fcm9

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

1  TRITIUM    1.230E+01   0.000E+00   1.000E+00     3.635E+00     4.183E+00

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   2.50E+04 cm
Y-Direction Dispersivity (A2)                   =   8.25E+03 cm
Z-Direction Dispersivity (A3)                   =   6.30E+01 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   2.50E+05 cm
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

Longitudinal Dispersion Coefficient =  1.494E+09 cm^2/yr
Lateral Dispersion Coefficient =       4.931E+08 cm^2/yr
Vertical Dispersion Coefficient =      3.766E+06 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

TRITIUM   H3        2.796E-02  NUMERICAL    1.300E+12

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

TRITIUM     H3         SS          2.286E-02          20.3 to    127.3

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   1 for Scenario Number   5
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  5
Usage Location Medium =  1
Usage Location Name   = fcm10

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

1  Acetone    7.500E+14   5.080E-04   1.008E+00     7.632E+00     8.434E+00

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   5.00E+04 cm
Y-Direction Dispersivity (A2)                   =   1.65E+04 cm
Z-Direction Dispersivity (A3)                   =   1.25E+02 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   5.00E+05 cm
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

Longitudinal Dispersion Coefficient =  2.988E+09 cm^2/yr
Lateral Dispersion Coefficient =       9.862E+08 cm^2/yr
Vertical Dispersion Coefficient =      7.471E+06 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

Acetone   67641     5.871E-02  NUMERICAL    1.300E+04

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

Acetone     67641      SS          1.470E-10          49.0 to     68.2

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   2 for Scenario Number   5
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  5
Usage Location Medium =  1
Usage Location Name   = fcm10

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

1  TRITIUM    1.230E+01   0.000E+00   1.000E+00     7.002E+00     8.366E+00

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   5.00E+04 cm
Y-Direction Dispersivity (A2)                   =   1.65E+04 cm
Z-Direction Dispersivity (A3)                   =   1.25E+02 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   5.00E+05 cm
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

Longitudinal Dispersion Coefficient =  2.988E+09 cm^2/yr
Lateral Dispersion Coefficient =       9.862E+08 cm^2/yr
Vertical Dispersion Coefficient =      7.471E+06 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

TRITIUM   H3        5.386E-02  NUMERICAL    1.300E+12

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

TRITIUM     H3         SS          8.986E-03          55.8 to    124.5

(a)
SS: Concentrations vary by less than 1/100th of one percent
of the maximum concentration over the given time range
(steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   1 for Scenario Number   6
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  6
Usage Location Medium =  1
Usage Location Name   = fcm11

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

1  Acetone    7.500E+14   5.080E-04   1.008E+00     1.526E+01     1.687E+01

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   1.00E+05 cm
Y-Direction Dispersivity (A2)                   =   3.30E+04 cm
Z-Direction Dispersivity (A3)                   =   2.50E+02 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   1.00E+06 cm
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

Longitudinal Dispersion Coefficient =  5.977E+09 cm^2/yr
Lateral Dispersion Coefficient =       1.972E+09 cm^2/yr
Vertical Dispersion Coefficient =      1.494E+07 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

Acetone   67641     1.174E-01  NUMERICAL    1.300E+04

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

Acetone     67641      PEAK        7.353E-11          90.8

(a)
PEAK: The maximum concentration occurs as a peak concentration
at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   2 for Scenario Number   6
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  6
Usage Location Medium =  1
Usage Location Name   = fcm11

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

1  TRITIUM    1.230E+01   0.000E+00   1.000E+00     1.309E+01     1.673E+01

Darcy Velocity (DARCYV)                         =   1.80E+01 cm/day
Pore-water Velocity (U)                         =   1.64E+02 cm/day
Aquifer Thickness (H1)                          =   3.14E+02 cm
X-Direction Dispersivity (A1)                   =   1.00E+05 cm
Y-Direction Dispersivity (A2)                   =   3.30E+04 cm
Z-Direction Dispersivity (A3)                   =   2.50E+02 cm
Bulk Density (R2)                               =   1.76E+00 g/cm**3
Total Porosity (AN4)                            =   3.60E-01 (fraction)
Effective Porosity (AN5)                        =   1.10E-01 (fraction)
Centerline Downgradient Distance from
Center of Source (X2)                           =   1.00E+06 cm
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

Longitudinal Dispersion Coefficient =  5.977E+09 cm^2/yr
Lateral Dispersion Coefficient =       1.972E+09 cm^2/yr
Vertical Dispersion Coefficient =      1.494E+07 cm^2/yr


Mass Released
Solution   From Source
Name     CASID    RATIO(a)    Method     (g or pCi)
--------  --------  ---------  --------  -------------

TRITIUM   H3        1.007E-01  NUMERICAL    1.300E+12

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

TRITIUM     H3         PEAK        2.860E-03         124.6

(a)
PEAK: The maximum concentration occurs as a peak concentration
at the given time.


******************************************************************************
