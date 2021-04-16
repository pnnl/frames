"aqu3",0000003619
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

1,2 DICH         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          1,2 DICH            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

1,2 DICH     156605   7.50E+14   2.22E-08  1.30E-02     6.30E-03    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       1,2 DICH              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  1,2 DICH   7.500E+14   1.300E-02   1.084E+00     2.317E+00     2.560E+00

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

1,2 DICH     156605   7.50E+14   2.22E-08  1.30E-02     6.30E-03    1.01E+02



1,2 DICH (156605  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

1,2 DICH  156605    2.317E-02  NUMERICAL    1.000E+02     1.002E+02 (  **  )

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

1,2 DICH    156605     SS          1.000E+00          12.2 to     98.8

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


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
Usage Location Name   = riv4

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

Acetone      67641    7.50E+14   2.22E-08  7.60E-04     1.00E+00    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Acetone               1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Acetone    7.500E+14   7.600E-04   1.005E+00     2.148E+00     2.373E+00

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Acetone      67641    7.50E+14   2.22E-08  7.60E-04     1.00E+00    1.01E+02



Acetone  (67641   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Acetone   67641     2.148E-02  NUMERICAL    1.000E+02     1.001E+02 (  **  )

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

Acetone     67641      SS          1.000E+00          12.0 to     97.6

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   3 for Scenario Number   1
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

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          AMERICIU            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

AMERICIU     AM241    4.33E+02   2.22E-08  8.20E+01     1.00E+23    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       AMERICIU              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  AMERICIU   4.330E+02   8.200E+01   5.322E+02     8.684E+02     1.257E+03

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

AMERICIU     AM241    4.33E+02   2.22E-08  8.20E+01     1.00E+23    1.01E+02



AMERICIU (AM241   ): DECAY CONSTANT =  1.601E-03 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

AMERICIU  AM241     8.684E+00  NUMERICAL    1.000E+02     1.597E+01 (  **  )

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

AMERICIU    AM241      PEAK        1.851E-02         819.5

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   4 for Scenario Number   1
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

Aroclor          1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Aroclor             2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Aroclor      11104282 7.50E+14   2.22E-08  7.67E+00     4.00E-05    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Aroclor               1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Aroclor    7.500E+14   7.670E+00   5.069E+01     1.083E+02     1.197E+02

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Aroclor      11104282 7.50E+14   2.22E-08  7.67E+00     4.00E-05    1.01E+02



Aroclor  (11104282): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Aroclor   11104282  1.083E+00  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Aroclor     11104282   PEAK        6.670E-01         161.7

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   5 for Scenario Number   1
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

Benzo(a)         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Benzo(a)            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Benzo(a)     50328    7.50E+14   2.22E-08  7.27E+03     1.60E-09    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Benzo(a)              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Benzo(a)   7.500E+14   7.270E+03   4.710E+04     1.006E+05     1.112E+05

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Benzo(a)     50328    7.50E+14   2.22E-08  7.27E+03     1.60E-09    1.01E+02



Benzo(a) (50328   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Benzo(a)  50328     1.006E+03  INSTANT      1.000E+02     3.930E-07 (  **  )

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

Benzo(a)    50328      PEAK        8.083E-10       10101.0

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   6 for Scenario Number   1
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

CESIUM-1         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          CESIUM-1            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

CESIUM-1     CS137    3.01E+01   2.22E-08  5.10E+01     1.00E+23    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       CESIUM-1              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  CESIUM-1   3.010E+01   5.100E+01   3.314E+02     2.638E+02     7.825E+02

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

CESIUM-1     CS137    3.01E+01   2.22E-08  5.10E+01     1.00E+23    1.01E+02



CESIUM-1 (CS137   ): DECAY CONSTANT =  2.303E-02 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

CESIUM-1  CS137     2.638E+00  NUMERICAL    1.000E+02     1.016E-02 (  **  )

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

CESIUM-1    CS137      PEAK        5.366E-05         287.5

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   7 for Scenario Number   1
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

Chlorobe         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Chlorobe            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Chlorobe     108907   7.50E+14   2.22E-08  4.36E-01     4.66E-04    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Chlorobe              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chlorobe   7.500E+14   4.360E-01   3.825E+00     8.173E+00     9.031E+00

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Chlorobe     108907   7.50E+14   2.22E-08  4.36E-01     4.66E-04    1.01E+02



Chlorobe (108907  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chlorobe  108907    8.173E-02  NUMERICAL    1.000E+02     9.999E+01 (  **  )

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

Chlorobe    108907     SS          1.000E+00          43.0 to     97.9

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   8 for Scenario Number   1
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

Chromium         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Chromium            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Chromium     7440473  7.50E+14   2.22E-08  1.68E+01     1.00E+17    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Chromium              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chromium   7.500E+14   1.680E+01   1.098E+02     2.347E+02     2.594E+02

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Chromium     7440473  7.50E+14   2.22E-08  1.68E+01     1.00E+17    1.01E+02



Chromium (7440473 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chromium  7440473   2.347E+00  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Chromium    7440473    PEAK        3.489E-01         280.9

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number   9 for Scenario Number   1
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

Ethylene         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Ethylene            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Ethylene     107211   7.50E+14   2.22E-08  3.56E-05     5.82E-01    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Ethylene              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Ethylene   7.500E+14   3.560E-05   1.000E+00     2.138E+00     2.362E+00

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Ethylene     107211   7.50E+14   2.22E-08  3.56E-05     5.82E-01    1.01E+02



Ethylene (107211  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Ethylene  107211    2.138E-02  NUMERICAL    1.000E+02     1.001E+02 (  **  )

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

Ethylene    107211     SS          1.000E+00          12.0 to     97.6

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  10 for Scenario Number   1
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

Hexachlo         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Hexachlo            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Hexachlo     118741   7.50E+14   2.22E-08  5.16E+00     6.00E-09    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Hexachlo              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Hexachlo   7.500E+14   5.160E+00   3.443E+01     7.357E+01     8.130E+01

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Hexachlo     118741   7.50E+14   2.22E-08  5.16E+00     6.00E-09    1.01E+02



Hexachlo (118741  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Hexachlo  118741    7.357E-01  NUMERICAL    1.000E+02     9.999E+01 (  **  )

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

Hexachlo    118741     PEAK        8.355E-01         132.7

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  11 for Scenario Number   1
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

IODINE-1         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          IODINE-1            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

IODINE-1     I129     1.57E+07   2.22E-08  0.00E+00     1.00E+23    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       IODINE-1              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  IODINE-1   1.570E+07   0.000E+00   1.000E+00     2.137E+00     2.361E+00

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

IODINE-1     I129     1.57E+07   2.22E-08  0.00E+00     1.00E+23    1.01E+02



IODINE-1 (I129    ): DECAY CONSTANT =  4.415E-08 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

IODINE-1  I129      2.137E-02  NUMERICAL    1.000E+02     1.001E+02 (  **  )

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

IODINE-1    I129       SS          1.000E+00          12.0 to     97.6

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  12 for Scenario Number   1
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

Mercury          1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Mercury             2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Mercury      7439976  7.50E+14   2.22E-08  3.22E+02     3.00E-08    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Mercury               1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Mercury    7.500E+14   3.220E+02   2.087E+03     4.460E+03     4.928E+03

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Mercury      7439976  7.50E+14   2.22E-08  3.22E+02     3.00E-08    1.01E+02



Mercury  (7439976 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Mercury   7439976   4.460E+01  INSTANT      1.000E+02     9.466E+01 (  **  )

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

Mercury     7439976    PEAK        1.931E-02        4034.9

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  13 for Scenario Number   1
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

Naphthal         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Naphthal            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Naphthal     91203    7.50E+14   2.22E-08  1.24E+00     3.20E-05    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Naphthal              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Naphthal   7.500E+14   1.240E+00   9.033E+00     1.930E+01     2.133E+01

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Naphthal     91203    7.50E+14   2.22E-08  1.24E+00     3.20E-05    1.01E+02



Naphthal (91203   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Naphthal  91203     1.930E-01  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Naphthal    91203      PEAK        9.999E-01         100.3

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  14 for Scenario Number   1
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

PLUTONIU         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          PLUTONIU            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

PLUTONIU     PU240    6.54E+03   2.22E-08  1.00E+01     1.00E+23    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       PLUTONIU              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  PLUTONIU   6.540E+03   1.000E+01   6.578E+01     1.402E+02     1.553E+02

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

PLUTONIU     PU240    6.54E+03   2.22E-08  1.00E+01     1.00E+23    1.01E+02



PLUTONIU (PU240   ): DECAY CONSTANT =  1.060E-04 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

PLUTONIU  PU240     1.402E+00  NUMERICAL    1.000E+02     9.821E+01 (  **  )

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

PLUTONIU    PU240      PEAK        5.314E-01         172.4

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  15 for Scenario Number   1
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

Pyrene           1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Pyrene              2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Pyrene       129000   7.50E+14   2.22E-08  5.02E+01     1.35E-07    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Pyrene                1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Pyrene     7.500E+14   5.020E+01   3.262E+02     6.971E+02     7.703E+02

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Pyrene       129000   7.50E+14   2.22E-08  5.02E+01     1.35E-07    1.01E+02



Pyrene   (129000  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Pyrene    129000    6.971E+00  NUMERICAL    1.000E+02     9.999E+01 (  **  )

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

Pyrene      129000     PEAK        1.228E-01         660.0

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  16 for Scenario Number   1
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

Selenium         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Selenium            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Selenium     7782492  7.50E+14   2.22E-08  5.91E+00     1.00E+17    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Selenium              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Selenium   7.500E+14   5.910E+00   3.929E+01     8.396E+01     9.277E+01

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Selenium     7782492  7.50E+14   2.22E-08  5.91E+00     1.00E+17    1.01E+02



Selenium (7782492 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Selenium  7782492   8.396E-01  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Selenium    7782492    PEAK        7.670E-01         148.9

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  17 for Scenario Number   1
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

TECHNETI         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          TECHNETI            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

TECHNETI     TC99     2.13E+05   2.22E-08  3.00E+00     1.00E+23    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       TECHNETI              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  TECHNETI   2.130E+05   3.000E+00   2.043E+01     4.367E+01     4.825E+01

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

TECHNETI     TC99     2.13E+05   2.22E-08  3.00E+00     1.00E+23    1.01E+02



TECHNETI (TC99    ): DECAY CONSTANT =  3.254E-06 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

TECHNETI  TC99      4.367E-01  NUMERICAL    1.000E+02     9.997E+01 (  **  )

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

TECHNETI    TC99       PEAK        9.672E-01         109.3

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  18 for Scenario Number   1
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

Toluene          1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Toluene             2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Toluene      108883   7.50E+14   2.22E-08  3.97E-01     5.35E-04    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Toluene               1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Toluene    7.500E+14   3.970E-01   3.572E+00     7.633E+00     8.435E+00

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Toluene      108883   7.50E+14   2.22E-08  3.97E-01     5.35E-04    1.01E+02



Toluene  (108883  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Toluene   108883    7.633E-02  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Toluene     108883     SS          1.000E+00          41.8 to     99.3

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  19 for Scenario Number   1
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

URANIUM-         1.000E+04 0.000E+00      1.000E+04
THORIUM-         1.000E+04 0.000E+00      1.000E+04
URANIUM-         1.000E+04 0.000E+00      1.000E+04
THORIUM-         1.000E+04 0.000E+00      1.000E+04
RADIUM-2         1.000E+04 0.000E+00      1.000E+04
RADON-22         1.000E+04 0.000E+00      1.000E+04
LEAD-210         1.000E+04 0.000E+00      1.000E+04
BISMUTH-         1.000E+04 0.000E+00      1.000E+04
POLONIUM         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          URANIUM-            2
2          THORIUM-            2
3          URANIUM-            2
4          THORIUM-            2
5          RADIUM-2            2
6          RADON-22            2
7          LEAD-210            2
8          BISMUTH-            2
9          POLONIUM            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  9

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

URANIUM-     U238     4.46E+09   2.22E-08  0.00E+00     1.00E+23    1.01E+02
THORIUM-     TH234    6.60E-02  -9.99E+01  1.00E+02     1.00E+23    1.01E-30
URANIUM-     U234     2.44E+05  -9.99E+01  0.00E+00     1.00E+23    1.01E-30
THORIUM-     TH230    7.69E+04  -9.99E+01  1.00E+02     1.00E+23    1.01E-30
RADIUM-2     RA226    1.60E+03  -9.99E+01  2.43E+01     1.00E+23    1.01E-30
RADON-22     RN222    1.05E-02  -9.99E+01  0.00E+00     1.00E+23    1.01E-30
LEAD-210     PB210    2.23E+01  -9.99E+01  2.34E+02     1.00E+23    1.01E-30
BISMUTH-     BI210    1.37E-02  -9.99E+01  0.00E+00     1.00E+23    1.01E-30
POLONIUM     PO210    3.78E-01  -9.99E+01  5.90E+00     1.00E+23    1.01E-30


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02
2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
3    1     0.00E+00  0.000E+00        3    2     0.00E+00  1.000E+00
4    1     0.00E+00  0.000E+00        4    2     0.00E+00  1.000E+00
5    1     0.00E+00  0.000E+00        5    2     0.00E+00  1.000E+00
6    1     0.00E+00  0.000E+00        6    2     0.00E+00  1.000E+00
7    1     0.00E+00  0.000E+00        7    2     0.00E+00  1.000E+00
8    1     0.00E+00  0.000E+00        8    2     0.00E+00  1.000E+00
9    1     0.00E+00  0.000E+00        9    2     0.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02
2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
3    1     0.00E+00  0.000E+00        3    2     0.00E+00  1.000E+00
4    1     0.00E+00  0.000E+00        4    2     0.00E+00  1.000E+00
5    1     0.00E+00  0.000E+00        5    2     0.00E+00  1.000E+00
6    1     0.00E+00  0.000E+00        6    2     0.00E+00  1.000E+00
7    1     0.00E+00  0.000E+00        7    2     0.00E+00  1.000E+00
8    1     0.00E+00  0.000E+00        8    2     0.00E+00  1.000E+00
9    1     0.00E+00  0.000E+00        9    2     0.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       URANIUM-              1.000E+02
2       THORIUM-              1.000E+00
3       URANIUM-              1.000E+00
4       THORIUM-              1.000E+00
5       RADIUM-2              1.000E+00
6       RADON-22              1.000E+00
7       LEAD-210              1.000E+00
8       BISMUTH-              1.000E+00
9       POLONIUM              1.000E+00
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  URANIUM-   4.460E+09   0.000E+00   1.000E+00     2.137E+00     2.361E+00
2  THORIUM-   6.600E-02   1.000E+02   6.488E+02     1.907E+01     1.532E+03
3  URANIUM-   2.440E+05   0.000E+00   1.000E+00     2.137E+00     2.361E+00
4  THORIUM-   7.690E+04   1.000E+02   6.488E+02     1.383E+03     1.532E+03
5  RADIUM-2   1.600E+03   2.430E+01   1.584E+02     3.291E+02     3.741E+02
6  RADON-22   1.050E-02   0.000E+00   1.000E+00     2.930E-01     2.361E+00
7  LEAD-210   2.230E+01   2.340E+02   1.517E+03     5.230E+02     3.582E+03
8  BISMUTH-   1.370E-02   0.000E+00   1.000E+00     3.333E-01     2.361E+00
9  POLONIUM   3.780E-01   5.900E+00   3.922E+01     1.102E+01     9.262E+01

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

URANIUM-     U238     4.46E+09   2.22E-08  0.00E+00     1.00E+23    1.01E+02
THORIUM-     TH234    6.60E-02   1.00E+30  1.00E+02     1.00E+23    1.01E-30
URANIUM-     U234     2.44E+05   1.00E+30  0.00E+00     1.00E+23    1.01E-30
THORIUM-     TH230    7.69E+04   1.00E+30  1.00E+02     1.00E+23    1.01E-30
RADIUM-2     RA226    1.60E+03   1.00E+30  2.43E+01     1.00E+23    1.01E-30
RADON-22     RN222    1.05E-02   1.00E+30  0.00E+00     1.00E+23    1.01E-30
LEAD-210     PB210    2.23E+01   1.00E+30  2.34E+02     1.00E+23    1.01E-30
BISMUTH-     BI210    1.37E-02   1.00E+30  0.00E+00     1.00E+23    1.01E-30
POLONIUM     PO210    3.78E-01   1.00E+30  5.90E+00     1.00E+23    1.01E-30



URANIUM- (U238    ): DECAY CONSTANT =  1.554E-10 1/YR
THORIUM- (TH234   ): DECAY CONSTANT =  1.050E+01 1/YR
URANIUM- (U234    ): DECAY CONSTANT =  2.841E-06 1/YR
THORIUM- (TH230   ): DECAY CONSTANT =  9.014E-06 1/YR
RADIUM-2 (RA226   ): DECAY CONSTANT =  4.332E-04 1/YR
RADON-22 (RN222   ): DECAY CONSTANT =  6.601E+01 1/YR
LEAD-210 (PB210   ): DECAY CONSTANT =  3.108E-02 1/YR
BISMUTH- (BI210   ): DECAY CONSTANT =  5.059E+01 1/YR
POLONIUM (PO210   ): DECAY CONSTANT =  1.834E+00 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

URANIUM-  U238      2.137E-02  NUMERICAL    1.000E+02     1.001E+02 (  **  )
THORIUM-  TH234     1.907E+01  NUMERICAL    0.000E+00     0.000E+00 (  **  )
URANIUM-  U234      2.137E+00  NUMERICAL    0.000E+00     0.000E+00 (  **  )
THORIUM-  TH230     1.383E+03  INSTANT      0.000E+00     0.000E+00 (  **  )
RADIUM-2  RA226     3.291E+02  INSTANT      0.000E+00     0.000E+00 (  **  )
RADON-22  RN222     2.930E-01  NUMERICAL    0.000E+00     0.000E+00 (  **  )
LEAD-210  PB210     5.230E+02  INSTANT      0.000E+00     0.000E+00 (  **  )
BISMUTH-  BI210     3.333E-01  NUMERICAL    0.000E+00     0.000E+00 (  **  )
POLONIUM  PO210     1.102E+01  NUMERICAL    0.000E+00     0.000E+00 (  **  )

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

URANIUM-    U238       SS          1.000E+00          12.0 to     97.6
THORIUM-    TH234      SS          1.000E+00          12.0 to     97.6
URANIUM-    U234       SS          6.438E-06          12.0 to     97.6
THORIUM-    TH230      SS          6.587E-11          12.0 to     97.6
RADIUM-2    RA226      SS          2.157E-14          12.0 to     97.6
RADON-22    RN222      SS          2.116E-14          12.0 to     97.6
LEAD-210    PB210      SS          3.452E-16          12.0 to     97.6
BISMUTH-    BI210      SS          2.301E-16          12.0 to     97.6
POLONIUM    PO210      SS          2.031E-16          12.0 to     97.6

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  20 for Scenario Number   1
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

Vinyl ch         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Vinyl ch            2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Vinyl ch     75014    7.50E+14   2.22E-08  7.50E-02     2.67E-03    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Vinyl ch              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Vinyl ch   7.500E+14   7.500E-02   1.486E+00     3.175E+00     3.509E+00

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Vinyl ch     75014    7.50E+14   2.22E-08  7.50E-02     2.67E-03    1.01E+02



Vinyl ch (75014   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Vinyl ch  75014     3.175E-02  NUMERICAL    1.000E+02     1.002E+02 (  **  )

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

Vinyl ch    75014      SS          1.000E+00          16.1 to     98.5

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


******************************************************************************
******************************************************************************
Modeling Parent Contaminant Number  21 for Scenario Number   1
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

Xylene           1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

#           Constituent         NUM

1          Xylene              2
Source-Term Type (ISTYPE)                     =  2
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

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

Xylene       1330207  7.50E+14   2.22E-08  3.17E-01     1.98E-04    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Xylene                1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  4
Source Configuration       =  6
Source Term Discharge Type =  2
SATURATED ZONE AT A RIVER




CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Xylene     7.500E+14   3.170E-01   3.054E+00     6.526E+00     7.211E+00

Darcy Velocity (DARCYV)                         =   2.00E+00 cm/day
Pore-water Velocity (U)                         =   8.70E+00 cm/day
Aquifer Thickness (H1)                          =   1.14E+04 cm
X-Direction Dispersivity (A1)                   =   7.50E+02 cm
Y-Direction Dispersivity (A2)                   =   2.48E+02 cm
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

Xylene       1330207  7.50E+14   2.22E-08  3.17E-01     1.98E-04    1.01E+02



Xylene   (1330207 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.382E+06 cm^2/yr
Lateral Dispersion Coefficient =       7.879E+05 cm^2/yr
Vertical Dispersion Coefficient =      6.225E+03 cm^2/yr

SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

Width of Plume (ALEN)                      =   4.74E+03 cm
Depth to top of Plume (ZTOP)               =   0.00E+00 cm
Depth to bottom of Plume (ZBOT)            =   2.41E+02 cm



Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Xylene    1330207   6.526E-02  NUMERICAL    1.000E+02     9.999E+01 (  **  )

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

Xylene      1330207    SS          1.000E+00          35.5 to     97.5

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


******************************************************************************
