"vad3",0000000339
****************************VERSION 06/18/1999****************************


RADCON -- WATERBORNE TRANSPORT COMPONENT for the Multimedia
Environmental Pollutant Assessment System (MEPAS):
Models movement of radionuclides and other chemicals
in groundwater, surface water, and overland pathways.

Battelle, Pacific Northwest Laboratory (PNL)
P.O. Box 999, Richland, WA 99352

Developed for the U.S. Department of Energy


Input file for this run is MEPAS_VZ.WIN
******************************************************************************
Modeling Parent Contaminant Number   1 for Scenario Number   1
******************************************************************************

Name of Facility      = Facility Name
Name of Waste Unit    = Ranking Unit Name
Transport Scenario    = Transport Scenario
Exposure Scenario     = Exposure Scenario
Usage Location Number =  1
Usage Location Medium =  1
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

CURIUM-2         1.000E+04 0.000E+00      1.000E+04
PLUTONIU         1.000E+04 0.000E+00      1.000E+04
AMERICIU         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          CURIUM-2            2
2          PLUTONIU            2
3          AMERICIU            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.920E-02 cm/day

Source-Term Type (ISTYPE)                     =  1
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  3

Source-Term Configuration (ISCONF)        =  4
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  1

Length of Release Unit (AL1 OR CLEN)                =   2.50E+03 cm
Width of Release Unit (B1)                          =   2.50E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

CURIUM-2     CM245    8.49E+03   9.14E+02  8.20E+00     1.00E+23    1.01E+12
PLUTONIU     PU241    1.44E+01  -9.99E+01  1.00E+01     1.00E+23    1.01E-30
AMERICIU     AM241    4.33E+02  -9.99E+01  8.20E+00     1.00E+23    1.01E-30


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     4.00E+10  0.000E+00        1    2     4.00E+10  2.500E+01
2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
3    1     0.00E+00  0.000E+00        3    2     0.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     4.00E+10  0.000E+00        1    2     4.00E+10  2.500E+01
2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
3    1     0.00E+00  0.000E+00        3    2     0.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       CURIUM-2              2.500E+01
2       PLUTONIU              1.000E+00
3       AMERICIU              1.000E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   9.50E+02 cm
X-Direction Dispersivity (A1)       =   9.50E+00 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   2.80E-01 (fraction)
Permeability (X2)                   =   1.12E+01 cm/day
Darcy Velocity (VLEACH)             =   1.92E-02 cm/day
Soil-Type Coefficient (A2)          =   5.30E+00



Moisture Content (AN5)   =  28.00 %
Pore water velocity (U)  =   6.86E-02 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  CURIUM-2   8.490E+03   8.200E+00   4.434E+01     1.644E+03     1.682E+03
2  PLUTONIU   1.440E+01   1.000E+01   5.386E+01     7.144E+02     2.043E+03
3  AMERICIU   4.330E+02   8.200E+00   4.434E+01     1.514E+03     1.682E+03

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

CURIUM-2     CM245    8.49E+03   9.13E+02  8.20E+00     1.00E+23    1.01E+12
PLUTONIU     PU241    1.44E+01   1.00E+30  1.00E+01     1.00E+23    1.01E-30
AMERICIU     AM241    4.33E+02   1.00E+30  8.20E+00     1.00E+23    1.01E-30



CURIUM-2 (CM245   ): DECAY CONSTANT =  8.164E-05 1/YR
PLUTONIU (PU241   ): DECAY CONSTANT =  4.814E-02 1/YR
AMERICIU (AM241   ): DECAY CONSTANT =  1.601E-03 1/YR

Longitudinal Dispersion Coefficient =  4.279E+02 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

CURIUM-2  CM245     6.576E+01  INSTANT      1.000E+12     8.698E+11 (  **  )
PLUTONIU  PU241     7.144E+02  INSTANT      0.000E+00     0.000E+00 (  **  )
AMERICIU  AM241     1.514E+03  INSTANT      0.000E+00     0.000E+00 (  **  )

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

CURIUM-2    CM245      PEAK        1.109E+09        1579.3
PLUTONIU    PU241      PEAK        1.110E+09        1579.3
AMERICIU    AM241      PEAK        1.060E+09        1579.3

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Trichlor         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Trichlor            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.920E-02 cm/day

Source-Term Type (ISTYPE)                     =  1
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  1

Source-Term Configuration (ISCONF)        =  4
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  1

Length of Release Unit (AL1 OR CLEN)                =   2.50E+03 cm
Width of Release Unit (B1)                          =   2.50E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    7.50E+14   1.18E-04  2.02E+00     1.10E-03    5.16E+04


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     5.15E+03  0.000E+00        1    2     5.15E+03  1.000E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     5.15E+03  0.000E+00        1    2     5.15E+03  1.000E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Trichlor              1.000E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   9.50E+02 cm
X-Direction Dispersivity (A1)       =   9.50E+00 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   2.80E-01 (fraction)
Permeability (X2)                   =   1.12E+01 cm/day
Darcy Velocity (VLEACH)             =   1.92E-02 cm/day
Soil-Type Coefficient (A2)          =   5.30E+00



Moisture Content (AN5)   =  28.00 %
Pore water velocity (U)  =   6.86E-02 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Trichlor   7.500E+14   2.020E+00   1.168E+01     4.350E+02     4.429E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    7.50E+14   1.18E-04  2.02E+00     1.10E-03    5.16E+04



Trichlor (79016   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  4.279E+02 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Trichlor  79016     4.350E+01  INSTANT      5.150E+04     5.150E+04 (  **  )

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

Trichlor    79016      PEAK        2.478E+02         419.2

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
