"vad3",0000000317
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

Acetone          1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Acetone             2

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.940E+00 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   7.50E+03 cm
Width of Release Unit (B1)                          =   7.50E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    7.50E+14   2.07E-07  1.10E-02     1.00E+00    1.26E+04


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.25E+04  0.000E+00        1    2     1.25E+04  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.25E+04  0.000E+00        1    2     1.25E+04  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Acetone               1.000E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.25E+04 cm
X-Direction Dispersivity (A1)       =   1.25E+02 cm
Bulk Density (R2)                   =   1.51E+00 g/cm**3
Total Porosity (AN4)                =   4.30E-01 (fraction)
Field Capacity (AN5)                =   3.20E-01 (fraction)
Permeability (X2)                   =   2.94E+00 cm/day
Darcy Velocity (VLEACH)             =   2.94E+00 cm/day
Soil-Type Coefficient (A2)          =   1.04E+01



Moisture Content (AN5)   =  43.00 %
Pore water velocity (U)  =   6.84E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Acetone    7.500E+14   1.100E-02   1.039E+00     5.147E+00     5.199E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    7.50E+14   2.07E-07  1.10E-02     1.00E+00    1.26E+04



Acetone  (67641   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.124E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Acetone   67641     5.147E+00  NUMERICAL    1.250E+04     1.250E+04 (  **  )

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

Acetone     67641      PEAK        6.301E+03           5.5

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

TRITIUM          1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          TRITIUM             2

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.940E+00 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   7.50E+03 cm
Width of Release Unit (B1)                          =   7.50E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

TRITIUM      H3       1.23E+01   2.44E+01  0.00E+00     1.00E+23    1.48E+12


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.47E+12  0.000E+00        1    2     1.47E+12  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.47E+12  0.000E+00        1    2     1.47E+12  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       TRITIUM               1.000E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.25E+04 cm
X-Direction Dispersivity (A1)       =   1.25E+02 cm
Bulk Density (R2)                   =   1.51E+00 g/cm**3
Total Porosity (AN4)                =   4.30E-01 (fraction)
Field Capacity (AN5)                =   3.20E-01 (fraction)
Permeability (X2)                   =   2.94E+00 cm/day
Darcy Velocity (VLEACH)             =   2.94E+00 cm/day
Soil-Type Coefficient (A2)          =   1.04E+01



Moisture Content (AN5)   =  43.00 %
Pore water velocity (U)  =   6.84E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  TRITIUM    1.230E+01   0.000E+00   1.000E+00     4.928E+00     5.005E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

TRITIUM      H3       1.23E+01   2.44E+01  0.00E+00     1.00E+23    1.48E+12



TRITIUM  (H3      ): DECAY CONSTANT =  5.635E-02 1/YR

Longitudinal Dispersion Coefficient =  3.124E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

TRITIUM   H3        4.928E+00  NUMERICAL    1.470E+12     1.106E+12 (  **  )

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

TRITIUM     H3         PEAK        5.787E+11           5.3

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
