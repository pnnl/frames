"vad6",0000000319
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
Usage Location Name   = ad6

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Acetone          1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Acetone             2

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

Length of Release Unit (AL1 OR CLEN)                =   2.00E+03 cm
Width of Release Unit (B1)                          =   2.50E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    7.50E+14   2.86E-05  1.00E-02     1.00E+00    1.31E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.300E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.300E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Acetone               1.300E+02
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   3.17E+02 cm
X-Direction Dispersivity (A1)       =   3.20E+00 cm
Bulk Density (R2)                   =   1.42E+00 g/cm**3
Total Porosity (AN4)                =   4.66E-01 (fraction)
Field Capacity (AN5)                =   2.35E-01 (fraction)
Permeability (X2)                   =   3.20E+01 cm/day
Darcy Velocity (VLEACH)             =   1.92E-02 cm/day
Soil-Type Coefficient (A2)          =   5.39E+00



Moisture Content (AN5)   =  27.20 %
Pore water velocity (U)  =   7.06E-02 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Acetone    7.500E+14   1.000E-02   1.052E+00     1.251E+01     1.294E+01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    7.50E+14   2.85E-05  1.00E-02     1.00E+00    1.31E+05



Acetone  (67641   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  2.725E+02 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Acetone   67641     9.626E-02  NUMERICAL    1.300E+05     1.300E+05 (  **  )

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

Acetone     67641      SS          1.000E+03          33.8 to    134.5

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
Usage Location Name   = ad6

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

TRITIUM          1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          TRITIUM             2

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

Length of Release Unit (AL1 OR CLEN)                =   2.00E+03 cm
Width of Release Unit (B1)                          =   2.50E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

TRITIUM      H3       1.23E+01   2.86E+02  0.00E+00     1.00E+23    1.31E+12


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   3.17E+02 cm
X-Direction Dispersivity (A1)       =   3.20E+00 cm
Bulk Density (R2)                   =   1.42E+00 g/cm**3
Total Porosity (AN4)                =   4.66E-01 (fraction)
Field Capacity (AN5)                =   2.35E-01 (fraction)
Permeability (X2)                   =   3.20E+01 cm/day
Darcy Velocity (VLEACH)             =   1.92E-02 cm/day
Soil-Type Coefficient (A2)          =   5.39E+00



Moisture Content (AN5)   =  27.20 %
Pore water velocity (U)  =   7.06E-02 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  TRITIUM    1.230E+01   0.000E+00   1.000E+00     1.139E+01     1.230E+01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

TRITIUM      H3       1.23E+01   2.85E+02  0.00E+00     1.00E+23    1.31E+12



TRITIUM  (H3      ): DECAY CONSTANT =  5.635E-02 1/YR

Longitudinal Dispersion Coefficient =  2.725E+02 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

TRITIUM   H3        8.765E-02  NUMERICAL    1.300E+12     6.459E+11 (  **  )

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

TRITIUM     H3         SS          4.969E+09          32.7 to    134.6

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


******************************************************************************
