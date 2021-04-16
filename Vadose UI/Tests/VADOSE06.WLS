"vad3",620
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

Chromium         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Chromium            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.650E+04 cm/day

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

Chromium     16065831 7.50E+14   1.65E-09  0.00E+00     1.00E+17    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+05  0.000E+00        1    2     1.00E+05  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+05  0.000E+00        1    2     1.00E+05  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Chromium              1.000E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.22E+03 cm
X-Direction Dispersivity (A1)       =   1.20E+01 cm
Bulk Density (R2)                   =   1.39E+00 g/cm**3
Total Porosity (AN4)                =   4.77E-01 (fraction)
Field Capacity (AN5)                =   3.40E-01 (fraction)
Permeability (X2)                   =   1.50E+01 cm/day
Darcy Velocity (VLEACH)             =   2.65E+04 cm/day
Soil-Type Coefficient (A2)          =   8.52E+00



Moisture Content (AN5)   =  47.70 %
Pore water velocity (U)  =   3.14E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chromium   7.500E+14   0.000E+00   1.000E+00     1.052E-01     1.062E-01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chromium     16065831 7.50E+14   2.92E-06  0.00E+00     1.00E+17    1.01E+05



Chromium (16065831): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  1.380E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chromium  16065831  1.052E-01  NUMERICAL    1.000E+05     1.000E+05 (  **  )

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

Chromium    16065831   SS          1.000E+05            .2 to      1.1

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Cobalt           1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Cobalt              2

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.650E+04 cm/day

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

Cobalt       7440484  7.50E+14   1.65E-09  5.00E+00     1.00E+17    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+05  0.000E+00        1    2     1.00E+05  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+05  0.000E+00        1    2     1.00E+05  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Cobalt                1.000E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.22E+03 cm
X-Direction Dispersivity (A1)       =   1.20E+01 cm
Bulk Density (R2)                   =   1.39E+00 g/cm**3
Total Porosity (AN4)                =   4.77E-01 (fraction)
Field Capacity (AN5)                =   3.40E-01 (fraction)
Permeability (X2)                   =   1.50E+01 cm/day
Darcy Velocity (VLEACH)             =   2.65E+04 cm/day
Soil-Type Coefficient (A2)          =   8.52E+00



Moisture Content (AN5)   =  47.70 %
Pore water velocity (U)  =   3.14E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Cobalt     7.500E+14   5.000E+00   1.557E+01     1.638E+00     1.654E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Cobalt       7440484  7.50E+14   2.92E-06  5.00E+00     1.00E+17    1.01E+05



Cobalt   (7440484 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  1.380E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Cobalt    7440484   1.638E+00  NUMERICAL    1.000E+05     9.999E+04 (  **  )

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

Cobalt      7440484    PEAK        9.671E+04           2.2

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Lead             1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Lead                2

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.650E+04 cm/day

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

Lead         7439921  7.50E+14   1.65E-09  3.00E+01     1.00E+17    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+05  0.000E+00        1    2     1.00E+05  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+05  0.000E+00        1    2     1.00E+05  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Lead                  1.000E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.22E+03 cm
X-Direction Dispersivity (A1)       =   1.20E+01 cm
Bulk Density (R2)                   =   1.39E+00 g/cm**3
Total Porosity (AN4)                =   4.77E-01 (fraction)
Field Capacity (AN5)                =   3.40E-01 (fraction)
Permeability (X2)                   =   1.50E+01 cm/day
Darcy Velocity (VLEACH)             =   2.65E+04 cm/day
Soil-Type Coefficient (A2)          =   8.52E+00



Moisture Content (AN5)   =  47.70 %
Pore water velocity (U)  =   3.14E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Lead       7.500E+14   3.000E+01   8.842E+01     9.300E+00     9.392E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Lead         7439921  7.50E+14   2.92E-06  3.00E+01     1.00E+17    1.01E+05



Lead     (7439921 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  1.380E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Lead      7439921   9.300E+00  NUMERICAL    1.000E+05     1.000E+05 (  **  )

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

Lead        7439921    PEAK        2.982E+04           9.7

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Manganes         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Manganes            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.650E+04 cm/day

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

Manganes     7439965  7.50E+14   1.65E-09  1.50E+01     1.00E+17    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+05  0.000E+00        1    2     1.00E+05  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+05  0.000E+00        1    2     1.00E+05  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Manganes              1.000E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.22E+03 cm
X-Direction Dispersivity (A1)       =   1.20E+01 cm
Bulk Density (R2)                   =   1.39E+00 g/cm**3
Total Porosity (AN4)                =   4.77E-01 (fraction)
Field Capacity (AN5)                =   3.40E-01 (fraction)
Permeability (X2)                   =   1.50E+01 cm/day
Darcy Velocity (VLEACH)             =   2.65E+04 cm/day
Soil-Type Coefficient (A2)          =   8.52E+00



Moisture Content (AN5)   =  47.70 %
Pore water velocity (U)  =   3.14E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Manganes   7.500E+14   1.500E+01   4.471E+01     4.703E+00     4.749E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Manganes     7439965  7.50E+14   2.92E-06  1.50E+01     1.00E+17    1.01E+05



Manganes (7439965 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  1.380E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Manganes  7439965   4.703E+00  NUMERICAL    1.000E+05     1.000E+05 (  **  )

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

Manganes    7439965    PEAK        5.442E+04           5.1

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
"vad4",676
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
Usage Location Name   = ad4

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Chromium         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Chromium           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Chromium     16065831 7.50E+14   2.92E-06  0.00E+00     1.00E+17    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  4.790E-02        1    2     1.17E+03  7.740E-02
1    3     5.17E+04  1.070E-01        1    4     9.63E+04  1.360E-01
1    5     9.99E+04  1.660E-01        1    6     1.00E+05  1.950E-01
1    7     1.00E+05  2.250E-01        1    8     1.00E+05  2.540E-01
1    9     1.00E+05  2.840E-01        1   10     1.00E+05  3.130E-01
1   11     1.00E+05  3.430E-01        1   12     1.00E+05  3.720E-01
1   13     1.00E+05  4.020E-01        1   14     1.00E+05  4.310E-01
1   15     1.00E+05  4.610E-01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  4.790E-02        1    2     1.17E+03  7.740E-02
1    3     5.17E+04  1.070E-01        1    4     9.63E+04  1.360E-01
1    5     9.99E+04  1.660E-01        1    6     1.00E+05  1.950E-01
1    7     1.00E+05  2.250E-01        1    8     1.00E+05  2.540E-01
1    9     1.00E+05  2.840E-01        1   10     1.00E+05  3.130E-01
1   11     1.00E+05  3.430E-01        1   12     1.00E+05  3.720E-01
1   13     1.00E+05  4.020E-01        1   14     1.00E+05  4.310E-01
1   15     1.00E+05  4.610E-01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Chromium              1.182E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   6.10E+02 cm
X-Direction Dispersivity (A1)       =   6.00E+00 cm
Bulk Density (R2)                   =   1.60E+00 g/cm**3
Total Porosity (AN4)                =   3.98E-01 (fraction)
Field Capacity (AN5)                =   2.40E-01 (fraction)
Permeability (X2)                   =   5.00E+01 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   7.12E+00



Moisture Content (AN5)   =  37.12 %
Pore water velocity (U)  =   4.04E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chromium   7.500E+14   0.000E+00   1.000E+00     4.092E-02     4.132E-02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chromium     16065831 7.50E+14   2.92E-06  0.00E+00     1.00E+17    1.01E+05



Chromium (16065831): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  8.876E+04 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chromium  16065831  3.462E-02  NUMERICAL    1.001E+05     1.001E+05 (  **  )

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

Chromium    16065831   SS          1.000E+05            .3 to      1.1

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
Usage Location Name   = ad4

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Cobalt           1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Cobalt             41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Cobalt       7440484  7.50E+14   2.82E-06  5.00E+00     1.00E+17    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  7.460E-01        1    2     4.38E-02  8.400E-01
1    3     1.92E+00  9.350E-01        1    4     3.31E+01  1.030E+00
1    5     2.87E+02  1.120E+00        1    6     1.47E+03  1.220E+00
1    7     5.06E+03  1.310E+00        1    8     1.27E+04  1.410E+00
1    9     2.50E+04  1.500E+00        1   10     4.05E+04  1.600E+00
1   11     5.68E+04  1.690E+00        1   12     7.12E+04  1.790E+00
1   13     8.23E+04  1.880E+00        1   14     8.99E+04  1.980E+00
1   15     9.46E+04  2.070E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  7.460E-01        1    2     4.38E-02  8.400E-01
1    3     1.92E+00  9.350E-01        1    4     3.31E+01  1.030E+00
1    5     2.87E+02  1.120E+00        1    6     1.47E+03  1.220E+00
1    7     5.06E+03  1.310E+00        1    8     1.27E+04  1.410E+00
1    9     2.50E+04  1.500E+00        1   10     4.05E+04  1.600E+00
1   11     5.68E+04  1.690E+00        1   12     7.12E+04  1.790E+00
1   13     8.23E+04  1.880E+00        1   14     8.99E+04  1.980E+00
1   15     9.46E+04  2.070E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Cobalt                2.535E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   6.10E+02 cm
X-Direction Dispersivity (A1)       =   6.00E+00 cm
Bulk Density (R2)                   =   1.60E+00 g/cm**3
Total Porosity (AN4)                =   3.98E-01 (fraction)
Field Capacity (AN5)                =   2.40E-01 (fraction)
Permeability (X2)                   =   5.00E+01 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   7.12E+00



Moisture Content (AN5)   =  37.12 %
Pore water velocity (U)  =   4.04E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Cobalt     7.500E+14   5.000E+00   2.255E+01     9.229E-01     9.320E-01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Cobalt       7440484  7.50E+14   2.83E-06  5.00E+00     1.00E+17    1.00E+05



Cobalt   (7440484 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  8.876E+04 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Cobalt    7440484   3.640E-01  NUMERICAL    1.000E+05     1.000E+05 (  **  )

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

Cobalt      7440484    PEAK        9.356E+04           3.1

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad4

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Lead             1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Lead               41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Lead         7439921  7.50E+14   8.71E-07  3.00E+01     1.00E+17    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  4.230E+00        1    2     1.64E-02  4.660E+00
1    3     4.25E-01  5.080E+00        1    4     5.67E+00  5.500E+00
1    5     4.54E+01  5.920E+00        1    6     2.41E+02  6.340E+00
1    7     9.18E+02  6.760E+00        1    8     2.65E+03  7.190E+00
1    9     6.04E+03  7.610E+00        1   10     1.13E+04  8.030E+00
1   11     1.77E+04  8.450E+00        1   12     2.39E+04  8.870E+00
1   13     2.82E+04  9.290E+00        1   14     2.98E+04  9.720E+00
1   15     2.84E+04  1.010E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  4.230E+00        1    2     1.64E-02  4.660E+00
1    3     4.25E-01  5.080E+00        1    4     5.67E+00  5.500E+00
1    5     4.54E+01  5.920E+00        1    6     2.41E+02  6.340E+00
1    7     9.18E+02  6.760E+00        1    8     2.65E+03  7.190E+00
1    9     6.04E+03  7.610E+00        1   10     1.13E+04  8.030E+00
1   11     1.77E+04  8.450E+00        1   12     2.39E+04  8.870E+00
1   13     2.82E+04  9.290E+00        1   14     2.98E+04  9.720E+00
1   15     2.84E+04  1.010E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Lead                  1.199E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   6.10E+02 cm
X-Direction Dispersivity (A1)       =   6.00E+00 cm
Bulk Density (R2)                   =   1.60E+00 g/cm**3
Total Porosity (AN4)                =   3.98E-01 (fraction)
Field Capacity (AN5)                =   2.40E-01 (fraction)
Permeability (X2)                   =   5.00E+01 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   7.12E+00



Moisture Content (AN5)   =  37.12 %
Pore water velocity (U)  =   4.04E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Lead       7.500E+14   3.000E+01   1.303E+02     5.333E+00     5.386E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Lead         7439921  7.50E+14   8.71E-07  3.00E+01     1.00E+17    1.00E+05



Lead     (7439921 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  8.876E+04 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Lead      7439921   4.449E-01  NUMERICAL    1.000E+05     9.998E+04 (  **  )

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

Lead        7439921    PEAK        2.560E+04          15.1

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
Usage Location Name   = ad4

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Manganes         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Manganes           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Manganes     7439965  7.50E+14   1.59E-06  1.50E+01     1.00E+17    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  2.140E+00        1    2     2.02E-02  2.370E+00
1    3     5.92E-01  2.590E+00        1    4     8.42E+00  2.820E+00
1    5     6.94E+01  3.040E+00        1    6     3.71E+02  3.270E+00
1    7     1.41E+03  3.490E+00        1    8     4.05E+03  3.720E+00
1    9     9.27E+03  3.950E+00        1   10     1.76E+04  4.170E+00
1   11     2.83E+04  4.400E+00        1   12     3.97E+04  4.620E+00
1   13     4.92E+04  4.850E+00        1   14     5.44E+04  5.070E+00
1   15     5.44E+04  5.300E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  2.140E+00        1    2     2.02E-02  2.370E+00
1    3     5.92E-01  2.590E+00        1    4     8.42E+00  2.820E+00
1    5     6.94E+01  3.040E+00        1    6     3.71E+02  3.270E+00
1    7     1.41E+03  3.490E+00        1    8     4.05E+03  3.720E+00
1    9     9.27E+03  3.950E+00        1   10     1.76E+04  4.170E+00
1   11     2.83E+04  4.400E+00        1   12     3.97E+04  4.620E+00
1   13     4.92E+04  4.850E+00        1   14     5.44E+04  5.070E+00
1   15     5.44E+04  5.300E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Manganes              9.060E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   6.10E+02 cm
X-Direction Dispersivity (A1)       =   6.00E+00 cm
Bulk Density (R2)                   =   1.60E+00 g/cm**3
Total Porosity (AN4)                =   3.98E-01 (fraction)
Field Capacity (AN5)                =   2.40E-01 (fraction)
Permeability (X2)                   =   5.00E+01 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   7.12E+00



Moisture Content (AN5)   =  37.12 %
Pore water velocity (U)  =   4.04E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Manganes   7.500E+14   1.500E+01   6.566E+01     2.687E+00     2.713E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Manganes     7439965  7.50E+14   1.59E-06  1.50E+01     1.00E+17    1.00E+05



Manganes (7439965 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  8.876E+04 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Manganes  7439965   2.966E-01  NUMERICAL    9.997E+04     9.997E+04 (  **  )

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

Manganes    7439965    PEAK        4.762E+04           7.8

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
"vad5",668
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
Usage Location Name   = ad5

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Chromium         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Chromium           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Chromium     16065831 2.74E+17   2.92E-06  0.00E+00     1.00E+20    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  6.650E-02        1    2     3.37E+02  9.780E-02
1    3     1.85E+04  1.290E-01        1    4     6.97E+04  1.600E-01
1    5     9.79E+04  1.920E-01        1    6     9.99E+04  2.230E-01
1    7     1.00E+05  2.540E-01        1    8     1.00E+05  2.860E-01
1    9     1.00E+05  3.170E-01        1   10     1.00E+05  3.480E-01
1   11     1.00E+05  3.800E-01        1   12     1.00E+05  4.110E-01
1   13     1.00E+05  4.420E-01        1   14     1.00E+05  4.730E-01
1   15     1.00E+05  5.050E-01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  6.650E-02        1    2     3.37E+02  9.780E-02
1    3     1.85E+04  1.290E-01        1    4     6.97E+04  1.600E-01
1    5     9.79E+04  1.920E-01        1    6     9.99E+04  2.230E-01
1    7     1.00E+05  2.540E-01        1    8     1.00E+05  2.860E-01
1    9     1.00E+05  3.170E-01        1   10     1.00E+05  3.480E-01
1   11     1.00E+05  3.800E-01        1   12     1.00E+05  4.110E-01
1   13     1.00E+05  4.420E-01        1   14     1.00E+05  4.730E-01
1   15     1.00E+05  5.050E-01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Chromium              1.254E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.35E+03 cm
X-Direction Dispersivity (A1)       =   5.30E+01 cm
Bulk Density (R2)                   =   1.49E+00 g/cm**3
Total Porosity (AN4)                =   4.37E-01 (fraction)
Field Capacity (AN5)                =   1.20E-01 (fraction)
Permeability (X2)                   =   9.81E+03 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   4.38E+00



Moisture Content (AN5)   =  25.18 %
Pore water velocity (U)  =   5.96E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chromium   2.740E+17   0.000E+00   1.000E+00     2.435E-01     2.459E-01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chromium     16065831 2.74E+17   2.92E-06  0.00E+00     1.00E+20    1.01E+05



Chromium (16065831): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  1.153E+06 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chromium  16065831  1.942E-01  NUMERICAL    9.993E+04     9.993E+04 (100.0%)

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

Chromium    16065831   SS          1.000E+05            .6 to      1.2

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
Usage Location Name   = ad5

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Cobalt           1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Cobalt             41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Cobalt       7440484  2.74E+17   2.73E-06  5.00E+00     1.00E+20    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  1.170E+00        1    2     1.03E-07  1.270E+00
1    3     3.16E-05  1.370E+00        1    4     2.32E-03  1.470E+00
1    5     7.57E-02  1.580E+00        1    6     1.36E+00  1.680E+00
1    7     1.50E+01  1.780E+00        1    8     1.06E+02  1.890E+00
1    9     5.20E+02  1.990E+00        1   10     1.89E+03  2.090E+00
1   11     5.31E+03  2.190E+00        1   12     1.20E+04  2.300E+00
1   13     2.25E+04  2.400E+00        1   14     3.62E+04  2.500E+00
1   15     5.14E+04  2.600E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  1.170E+00        1    2     1.03E-07  1.270E+00
1    3     3.16E-05  1.370E+00        1    4     2.32E-03  1.470E+00
1    5     7.57E-02  1.580E+00        1    6     1.36E+00  1.680E+00
1    7     1.50E+01  1.780E+00        1    8     1.06E+02  1.890E+00
1    9     5.20E+02  1.990E+00        1   10     1.89E+03  2.090E+00
1   11     5.31E+03  2.190E+00        1   12     1.20E+04  2.300E+00
1   13     2.25E+04  2.400E+00        1   14     3.62E+04  2.500E+00
1   15     5.14E+04  2.600E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Cobalt                4.110E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.35E+03 cm
X-Direction Dispersivity (A1)       =   5.30E+01 cm
Bulk Density (R2)                   =   1.49E+00 g/cm**3
Total Porosity (AN4)                =   4.37E-01 (fraction)
Field Capacity (AN5)                =   1.20E-01 (fraction)
Permeability (X2)                   =   9.81E+03 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   4.38E+00



Moisture Content (AN5)   =  25.18 %
Pore water velocity (U)  =   5.96E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Cobalt     2.740E+17   5.000E+00   3.059E+01     7.447E+00     7.521E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Cobalt       7440484  2.74E+17   2.74E-06  5.00E+00     1.00E+20    1.01E+05



Cobalt   (7440484 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  1.153E+06 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Cobalt    7440484   1.812E+00  NUMERICAL    1.001E+05     1.001E+05 (100.0%)

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

Cobalt      7440484    PEAK        3.550E+04          10.5

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad5

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Lead             1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Lead               41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Lead         7439921  2.74E+17   7.48E-07  3.00E+01     1.00E+20    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  6.660E+00        1    2     2.23E-08  7.180E+00
1    3     5.92E-06  7.710E+00        1    4     4.11E-04  8.240E+00
1    5     1.34E-02  8.770E+00        1    6     2.47E-01  9.290E+00
1    7     2.86E+00  9.820E+00        1    8     2.23E+01  1.030E+01
1    9     1.22E+02  1.090E+01        1   10     4.97E+02  1.140E+01
1   11     1.56E+03  1.190E+01        1   12     3.88E+03  1.250E+01
1   13     7.87E+03  1.300E+01        1   14     1.33E+04  1.350E+01
1   15     1.91E+04  1.400E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  6.660E+00        1    2     2.23E-08  7.180E+00
1    3     5.92E-06  7.710E+00        1    4     4.11E-04  8.240E+00
1    5     1.34E-02  8.770E+00        1    6     2.47E-01  9.290E+00
1    7     2.86E+00  9.820E+00        1    8     2.23E+01  1.030E+01
1    9     1.22E+02  1.090E+01        1   10     4.97E+02  1.140E+01
1   11     1.56E+03  1.190E+01        1   12     3.88E+03  1.250E+01
1   13     7.87E+03  1.300E+01        1   14     1.33E+04  1.350E+01
1   15     1.91E+04  1.400E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Lead                  2.104E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.35E+03 cm
X-Direction Dispersivity (A1)       =   5.30E+01 cm
Bulk Density (R2)                   =   1.49E+00 g/cm**3
Total Porosity (AN4)                =   4.37E-01 (fraction)
Field Capacity (AN5)                =   1.20E-01 (fraction)
Permeability (X2)                   =   9.81E+03 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   4.38E+00



Moisture Content (AN5)   =  25.18 %
Pore water velocity (U)  =   5.96E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Lead       2.740E+17   3.000E+01   1.785E+02     4.346E+01     4.390E+01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Lead         7439921  2.74E+17   7.48E-07  3.00E+01     1.00E+20    1.00E+05



Lead     (7439921 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  1.153E+06 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Lead      7439921   2.066E+00  NUMERICAL    9.988E+04     9.990E+04 (100.0%)

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

Lead        7439921    PEAK        6.202E+03          57.4

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
Usage Location Name   = ad5

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Manganes         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Manganes           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Manganes     7439965  2.74E+17   1.39E-06  1.50E+01     1.00E+20    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  3.360E+00        1    2     1.52E-07  3.700E+00
1    3     8.07E-05  4.050E+00        1    4     8.86E-03  4.390E+00
1    5     3.67E-01  4.730E+00        1    6     7.25E+00  5.070E+00
1    7     7.84E+01  5.410E+00        1    8     5.14E+02  5.750E+00
1    9     2.23E+03  6.090E+00        1   10     6.87E+03  6.430E+00
1   11     1.58E+04  6.770E+00        1   12     2.83E+04  7.120E+00
1   13     4.06E+04  7.460E+00        1   14     4.76E+04  7.800E+00
1   15     4.67E+04  8.140E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  3.360E+00        1    2     1.52E-07  3.700E+00
1    3     8.07E-05  4.050E+00        1    4     8.86E-03  4.390E+00
1    5     3.67E-01  4.730E+00        1    6     7.25E+00  5.070E+00
1    7     7.84E+01  5.410E+00        1    8     5.14E+02  5.750E+00
1    9     2.23E+03  6.090E+00        1   10     6.87E+03  6.430E+00
1   11     1.58E+04  6.770E+00        1   12     2.83E+04  7.120E+00
1   13     4.06E+04  7.460E+00        1   14     4.76E+04  7.800E+00
1   15     4.67E+04  8.140E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Manganes              1.364E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.35E+03 cm
X-Direction Dispersivity (A1)       =   5.30E+01 cm
Bulk Density (R2)                   =   1.49E+00 g/cm**3
Total Porosity (AN4)                =   4.37E-01 (fraction)
Field Capacity (AN5)                =   1.20E-01 (fraction)
Permeability (X2)                   =   9.81E+03 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   4.38E+00



Moisture Content (AN5)   =  25.18 %
Pore water velocity (U)  =   5.96E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Manganes   2.740E+17   1.500E+01   8.976E+01     2.185E+01     2.207E+01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Manganes     7439965  2.74E+17   1.39E-06  1.50E+01     1.00E+20    1.00E+05



Manganes (7439965 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  1.153E+06 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Manganes  7439965   1.602E+00  NUMERICAL    9.985E+04     9.984E+04 (100.0%)

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

Manganes    7439965    PEAK        1.246E+04          29.9

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
"vad6",668
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

Chromium         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Chromium           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Chromium     16065831 2.74E+17   2.92E-06  0.00E+00     1.00E+20    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  1.770E-01        1    2     1.67E-02  2.190E-01
1    3     1.09E+01  2.610E-01        1    4     8.28E+02  3.020E-01
1    5     1.04E+04  3.440E-01        1    6     4.10E+04  3.860E-01
1    7     7.67E+04  4.280E-01        1    8     9.47E+04  4.690E-01
1    9     9.93E+04  5.110E-01        1   10     9.99E+04  5.530E-01
1   11     1.00E+05  5.950E-01        1   12     1.00E+05  6.360E-01
1   13     1.00E+05  6.780E-01        1   14     1.00E+05  7.200E-01
1   15     1.00E+05  7.620E-01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  1.770E-01        1    2     1.67E-02  2.190E-01
1    3     1.09E+01  2.610E-01        1    4     8.28E+02  3.020E-01
1    5     1.04E+04  3.440E-01        1    6     4.10E+04  3.860E-01
1    7     7.67E+04  4.280E-01        1    8     9.47E+04  4.690E-01
1    9     9.93E+04  5.110E-01        1   10     9.99E+04  5.530E-01
1   11     1.00E+05  5.950E-01        1   12     1.00E+05  6.360E-01
1   13     1.00E+05  6.780E-01        1   14     1.00E+05  7.200E-01
1   15     1.00E+05  7.620E-01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Chromium              1.329E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.52E+03 cm
X-Direction Dispersivity (A1)       =   1.50E+01 cm
Bulk Density (R2)                   =   1.49E+00 g/cm**3
Total Porosity (AN4)                =   4.37E-01 (fraction)
Field Capacity (AN5)                =   1.20E-01 (fraction)
Permeability (X2)                   =   9.81E+03 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   4.38E+00



Moisture Content (AN5)   =  25.18 %
Pore water velocity (U)  =   5.96E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chromium   2.740E+17   0.000E+00   1.000E+00     6.917E-02     6.986E-02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chromium     16065831 2.74E+17   2.92E-06  0.00E+00     1.00E+20    1.00E+05



Chromium (16065831): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  3.266E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chromium  16065831  5.204E-02  NUMERICAL    1.000E+05     1.000E+05 (100.0%)

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

Chromium    16065831   SS          1.000E+05            .7 to      1.3

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

Cobalt           1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Cobalt             41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Cobalt       7440484  2.74E+17   1.04E-06  5.00E+00     1.00E+20    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  4.550E+00        1    2     4.21E-10  4.970E+00
1    3     6.31E-06  5.400E+00        1    4     1.71E-03  5.820E+00
1    5     9.27E-02  6.240E+00        1    6     2.17E+00  6.660E+00
1    7     2.67E+01  7.080E+00        1    8     1.96E+02  7.500E+00
1    9     9.47E+02  7.920E+00        1   10     3.21E+03  8.350E+00
1   11     8.10E+03  8.770E+00        1   12     1.58E+04  9.190E+00
1   13     2.49E+04  9.610E+00        1   14     3.23E+04  1.000E+01
1   15     3.55E+04  1.050E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  4.550E+00        1    2     4.21E-10  4.970E+00
1    3     6.31E-06  5.400E+00        1    4     1.71E-03  5.820E+00
1    5     9.27E-02  6.240E+00        1    6     2.17E+00  6.660E+00
1    7     2.67E+01  7.080E+00        1    8     1.96E+02  7.500E+00
1    9     9.47E+02  7.920E+00        1   10     3.21E+03  8.350E+00
1   11     8.10E+03  8.770E+00        1   12     1.58E+04  9.190E+00
1   13     2.49E+04  9.610E+00        1   14     3.23E+04  1.000E+01
1   15     3.55E+04  1.050E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Cobalt                1.685E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.52E+03 cm
X-Direction Dispersivity (A1)       =   1.50E+01 cm
Bulk Density (R2)                   =   1.49E+00 g/cm**3
Total Porosity (AN4)                =   4.37E-01 (fraction)
Field Capacity (AN5)                =   1.20E-01 (fraction)
Permeability (X2)                   =   9.81E+03 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   4.38E+00



Moisture Content (AN5)   =  25.18 %
Pore water velocity (U)  =   5.96E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Cobalt     2.740E+17   5.000E+00   3.059E+01     2.116E+00     2.137E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Cobalt       7440484  2.74E+17   1.04E-06  5.00E+00     1.00E+20    1.01E+05



Cobalt   (7440484 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  3.266E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Cobalt    7440484   1.256E-01  NUMERICAL    1.002E+05     1.002E+05 (100.0%)

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

Cobalt      7440484    PEAK        3.401E+04          12.7

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad6

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Lead             1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Lead               41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Lead         7439921  2.74E+17   1.81E-07  3.00E+01     1.00E+20    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  2.640E+01        1    2     2.42E-10  2.880E+01
1    3     4.14E-06  3.120E+01        1    4     1.06E-03  3.360E+01
1    5     5.18E-02  3.590E+01        1    6     1.09E+00  3.830E+01
1    7     1.20E+01  4.070E+01        1    8     7.89E+01  4.310E+01
1    9     3.37E+02  4.550E+01        1   10     1.00E+03  4.790E+01
1   11     2.22E+03  5.030E+01        1   12     3.83E+03  5.260E+01
1   13     5.34E+03  5.500E+01        1   14     6.20E+03  5.740E+01
1   15     6.17E+03  5.980E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  2.640E+01        1    2     2.42E-10  2.880E+01
1    3     4.14E-06  3.120E+01        1    4     1.06E-03  3.360E+01
1    5     5.18E-02  3.590E+01        1    6     1.09E+00  3.830E+01
1    7     1.20E+01  4.070E+01        1    8     7.89E+01  4.310E+01
1    9     3.37E+02  4.550E+01        1   10     1.00E+03  4.790E+01
1   11     2.22E+03  5.030E+01        1   12     3.83E+03  5.260E+01
1   13     5.34E+03  5.500E+01        1   14     6.20E+03  5.740E+01
1   15     6.17E+03  5.980E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Lead                  9.560E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.52E+03 cm
X-Direction Dispersivity (A1)       =   1.50E+01 cm
Bulk Density (R2)                   =   1.49E+00 g/cm**3
Total Porosity (AN4)                =   4.37E-01 (fraction)
Field Capacity (AN5)                =   1.20E-01 (fraction)
Permeability (X2)                   =   9.81E+03 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   4.38E+00



Moisture Content (AN5)   =  25.18 %
Pore water velocity (U)  =   5.96E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Lead       2.740E+17   3.000E+01   1.785E+02     1.235E+01     1.247E+01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Lead         7439921  2.74E+17   1.81E-07  3.00E+01     1.00E+20    1.00E+05



Lead     (7439921 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  3.266E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Lead      7439921   1.292E-01  NUMERICAL    1.000E+05     1.000E+05 (100.0%)

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

Lead        7439921    PEAK        5.888E+03          69.9

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
Usage Location Name   = ad6

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Manganes         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Manganes           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Manganes     7439965  2.74E+17   3.64E-07  1.50E+01     1.00E+20    9.99E+04


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  1.330E+01        1    2     7.56E-10  1.460E+01
1    3     1.08E-05  1.580E+01        1    4     2.81E-03  1.710E+01
1    5     1.48E-01  1.840E+01        1    6     3.27E+00  1.970E+01
1    7     3.66E+01  2.090E+01        1    8     2.36E+02  2.220E+01
1    9     9.71E+02  2.350E+01        1   10     2.75E+03  2.480E+01
1   11     5.69E+03  2.600E+01        1   12     9.12E+03  2.730E+01
1   13     1.17E+04  2.860E+01        1   14     1.25E+04  2.990E+01
1   15     1.13E+04  3.120E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  1.330E+01        1    2     7.56E-10  1.460E+01
1    3     1.08E-05  1.580E+01        1    4     2.81E-03  1.710E+01
1    5     1.48E-01  1.840E+01        1    6     3.27E+00  1.970E+01
1    7     3.66E+01  2.090E+01        1    8     2.36E+02  2.220E+01
1    9     9.71E+02  2.350E+01        1   10     2.75E+03  2.480E+01
1   11     5.69E+03  2.600E+01        1   12     9.12E+03  2.730E+01
1   13     1.17E+04  2.860E+01        1   14     1.25E+04  2.990E+01
1   15     1.13E+04  3.120E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Manganes              2.804E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.52E+03 cm
X-Direction Dispersivity (A1)       =   1.50E+01 cm
Bulk Density (R2)                   =   1.49E+00 g/cm**3
Total Porosity (AN4)                =   4.37E-01 (fraction)
Field Capacity (AN5)                =   1.20E-01 (fraction)
Permeability (X2)                   =   9.81E+03 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   4.38E+00



Moisture Content (AN5)   =  25.18 %
Pore water velocity (U)  =   5.96E+01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Manganes   2.740E+17   1.500E+01   8.976E+01     6.209E+00     6.271E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Manganes     7439965  2.74E+17   3.65E-07  1.50E+01     1.00E+20    9.99E+04



Manganes (7439965 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  3.266E+05 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Manganes  7439965   2.214E-01  NUMERICAL    9.990E+04     9.991E+04 (100.0%)

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

Manganes    7439965    PEAK        1.184E+04          36.4

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
"vad7",667
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
Usage Location Name   = ad7

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Chromium         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Chromium           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Chromium     16065831 2.74E+17   2.92E-06  0.00E+00     1.00E+20    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  2.080E-01        1    2     1.03E-03  2.450E-01
1    3     2.36E-01  2.810E-01        1    4     1.07E+01  3.170E-01
1    5     4.52E+02  3.530E-01        1    6     4.73E+03  3.890E-01
1    7     1.89E+04  4.260E-01        1    8     4.57E+04  4.620E-01
1    9     7.47E+04  4.980E-01        1   10     9.17E+04  5.340E-01
1   11     9.80E+04  5.700E-01        1   12     9.97E+04  6.070E-01
1   13     9.99E+04  6.430E-01        1   14     1.00E+05  6.790E-01
1   15     1.00E+05  7.150E-01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  2.080E-01        1    2     1.03E-03  2.450E-01
1    3     2.36E-01  2.810E-01        1    4     1.07E+01  3.170E-01
1    5     4.52E+02  3.530E-01        1    6     4.73E+03  3.890E-01
1    7     1.89E+04  4.260E-01        1    8     4.57E+04  4.620E-01
1    9     7.47E+04  4.980E-01        1   10     9.17E+04  5.340E-01
1   11     9.80E+04  5.700E-01        1   12     9.97E+04  6.070E-01
1   13     9.99E+04  6.430E-01        1   14     1.00E+05  6.790E-01
1   15     1.00E+05  7.150E-01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Chromium              1.452E+00
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.22E+03 cm
X-Direction Dispersivity (A1)       =   1.20E+01 cm
Bulk Density (R2)                   =   1.39E+00 g/cm**3
Total Porosity (AN4)                =   4.75E-01 (fraction)
Field Capacity (AN5)                =   4.00E-01 (fraction)
Permeability (X2)                   =   2.00E-01 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   1.14E+01



Moisture Content (AN5)   =  47.50 %
Pore water velocity (U)  =   4.21E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chromium   2.740E+17   0.000E+00   1.000E+00     7.847E+00     7.933E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chromium     16065831 2.74E+17   2.19E-04  0.00E+00     1.00E+20    1.01E+05



Chromium (16065831): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  2.035E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chromium  16065831  5.404E+00  NUMERICAL    1.001E+05     1.003E+05 (100.1%)

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

Chromium    16065831   PEAK        3.351E+04           8.7

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
Usage Location Name   = ad7

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Cobalt           1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Cobalt             41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Cobalt       7440484  2.74E+17   9.93E-07  5.00E+00     1.00E+20    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  5.510E+00        1    2     2.84E-13  6.020E+00
1    3     1.43E-08  6.540E+00        1    4     1.23E-05  7.050E+00
1    5     2.08E-03  7.560E+00        1    6     1.21E-01  8.070E+00
1    7     3.14E+00  8.580E+00        1    8     4.22E+01  9.090E+00
1    9     3.28E+02  9.610E+00        1   10     1.59E+03  1.010E+01
1   11     5.25E+03  1.060E+01        1   12     1.25E+04  1.110E+01
1   13     2.23E+04  1.170E+01        1   14     3.08E+04  1.220E+01
1   15     3.40E+04  1.270E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  5.510E+00        1    2     2.84E-13  6.020E+00
1    3     1.43E-08  6.540E+00        1    4     1.23E-05  7.050E+00
1    5     2.08E-03  7.560E+00        1    6     1.21E-01  8.070E+00
1    7     3.14E+00  8.580E+00        1    8     4.22E+01  9.090E+00
1    9     3.28E+02  9.610E+00        1   10     1.59E+03  1.010E+01
1   11     5.25E+03  1.060E+01        1   12     1.25E+04  1.110E+01
1   13     2.23E+04  1.170E+01        1   14     3.08E+04  1.220E+01
1   15     3.40E+04  1.270E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Cobalt                2.049E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.22E+03 cm
X-Direction Dispersivity (A1)       =   1.20E+01 cm
Bulk Density (R2)                   =   1.39E+00 g/cm**3
Total Porosity (AN4)                =   4.75E-01 (fraction)
Field Capacity (AN5)                =   4.00E-01 (fraction)
Permeability (X2)                   =   2.00E-01 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   1.14E+01



Moisture Content (AN5)   =  47.50 %
Pore water velocity (U)  =   4.21E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Cobalt     2.740E+17   5.000E+00   1.563E+01     1.227E+02     1.240E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Cobalt       7440484  2.74E+17   7.45E-05  5.00E+00     1.00E+20    1.01E+05



Cobalt   (7440484 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  2.035E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Cobalt    7440484   5.987E+00  NUMERICAL    9.982E+04     9.987E+04 (100.1%)

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

Cobalt      7440484    PEAK        2.179E+03         131.8

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad7

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Lead             1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Lead               41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Lead         7439921  2.74E+17   1.72E-07  3.00E+01     1.00E+20    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  3.200E+01        1    2     1.43E-13  3.490E+01
1    3     8.01E-09  3.790E+01        1    4     7.16E-06  4.080E+01
1    5     1.15E-03  4.370E+01        1    6     6.45E-02  4.660E+01
1    7     1.48E+00  4.950E+01        1    8     1.75E+01  5.240E+01
1    9     1.20E+02  5.540E+01        1   10     5.13E+02  5.830E+01
1   11     1.49E+03  6.120E+01        1   12     3.11E+03  6.410E+01
1   13     4.85E+03  6.700E+01        1   14     5.89E+03  6.990E+01
1   15     5.81E+03  7.290E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  3.200E+01        1    2     1.43E-13  3.490E+01
1    3     8.01E-09  3.790E+01        1    4     7.16E-06  4.080E+01
1    5     1.15E-03  4.370E+01        1    6     6.45E-02  4.660E+01
1    7     1.48E+00  4.950E+01        1    8     1.75E+01  5.240E+01
1    9     1.20E+02  5.540E+01        1   10     5.13E+02  5.830E+01
1   11     1.49E+03  6.120E+01        1   12     3.11E+03  6.410E+01
1   13     4.85E+03  6.700E+01        1   14     5.89E+03  6.990E+01
1   15     5.81E+03  7.290E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Lead                  6.734E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.22E+03 cm
X-Direction Dispersivity (A1)       =   1.20E+01 cm
Bulk Density (R2)                   =   1.39E+00 g/cm**3
Total Porosity (AN4)                =   4.75E-01 (fraction)
Field Capacity (AN5)                =   4.00E-01 (fraction)
Permeability (X2)                   =   2.00E-01 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   1.14E+01



Moisture Content (AN5)   =  47.50 %
Pore water velocity (U)  =   4.21E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Lead       2.740E+17   3.000E+01   8.879E+01     6.968E+02     7.044E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Lead         7439921  2.74E+17   1.29E-05  3.00E+01     1.00E+20    1.00E+05



Lead     (7439921 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  2.035E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Lead      7439921   1.035E+01  NUMERICAL    1.000E+05     1.000E+05 (100.0%)

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

Lead        7439921    PEAK        3.872E+02         767.6

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
Usage Location Name   = ad7

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Manganes         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Manganes           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.500E+01 cm/day

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

Manganes     7439965  2.74E+17   3.46E-07  1.50E+01     1.00E+20    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  1.610E+01        1    2     1.04E-14  1.710E+01
1    3     1.07E-11  1.810E+01        1    4     1.08E-08  1.900E+01
1    5     1.12E-06  2.000E+01        1    6     6.06E-05  2.100E+01
1    7     1.38E-03  2.190E+01        1    8     2.03E-02  2.290E+01
1    9     2.16E-01  2.390E+01        1   10     1.66E+00  2.480E+01
1   11     1.01E+01  2.580E+01        1   12     4.65E+01  2.680E+01
1   13     1.63E+02  2.770E+01        1   14     4.67E+02  2.870E+01
1   15     1.12E+03  2.970E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  1.610E+01        1    2     1.04E-14  1.710E+01
1    3     1.07E-11  1.810E+01        1    4     1.08E-08  1.900E+01
1    5     1.12E-06  2.000E+01        1    6     6.06E-05  2.100E+01
1    7     1.38E-03  2.190E+01        1    8     2.03E-02  2.290E+01
1    9     2.16E-01  2.390E+01        1   10     1.66E+00  2.480E+01
1   11     1.01E+01  2.580E+01        1   12     4.65E+01  2.680E+01
1   13     1.63E+02  2.770E+01        1   14     4.67E+02  2.870E+01
1   15     1.12E+03  2.970E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Manganes              3.870E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   1.22E+03 cm
X-Direction Dispersivity (A1)       =   1.20E+01 cm
Bulk Density (R2)                   =   1.39E+00 g/cm**3
Total Porosity (AN4)                =   4.75E-01 (fraction)
Field Capacity (AN5)                =   4.00E-01 (fraction)
Permeability (X2)                   =   2.00E-01 cm/day
Darcy Velocity (VLEACH)             =   1.50E+01 cm/day
Soil-Type Coefficient (A2)          =   1.14E+01



Moisture Content (AN5)   =  47.50 %
Pore water velocity (U)  =   4.21E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Manganes   2.740E+17   1.500E+01   4.489E+01     3.523E+02     3.561E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Manganes     7439965  2.74E+17   2.59E-05  1.50E+01     1.00E+20    1.00E+05



Manganes (7439965 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  2.035E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Manganes  7439965   9.103E+00  NUMERICAL    9.965E+04     9.964E+04 (100.0%)

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

Manganes    7439965    PEAK        7.617E+02         389.6

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
"vad8",667
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
Usage Location Name   = ad8

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Chromium         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Chromium           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.000E-01 cm/day

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

Chromium     16065831 2.74E+17   7.34E-05  0.00E+00     1.00E+20    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  3.650E+00        1    2     1.28E-03  4.040E+00
1    3     8.26E-02  4.430E+00        1    4     1.85E+00  4.820E+00
1    5     2.12E+01  5.210E+00        1    6     1.46E+02  5.600E+00
1    7     6.71E+02  5.990E+00        1    8     2.22E+03  6.380E+00
1    9     5.58E+03  6.770E+00        1   10     1.15E+04  7.160E+00
1   11     1.85E+04  7.550E+00        1   12     2.59E+04  7.940E+00
1   13     3.14E+04  8.330E+00        1   14     3.35E+04  8.720E+00
1   15     3.20E+04  9.110E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  3.650E+00        1    2     1.28E-03  4.040E+00
1    3     8.26E-02  4.430E+00        1    4     1.85E+00  4.820E+00
1    5     2.12E+01  5.210E+00        1    6     1.46E+02  5.600E+00
1    7     6.71E+02  5.990E+00        1    8     2.22E+03  6.380E+00
1    9     5.58E+03  6.770E+00        1   10     1.15E+04  7.160E+00
1   11     1.85E+04  7.550E+00        1   12     2.59E+04  7.940E+00
1   13     3.14E+04  8.330E+00        1   14     3.35E+04  8.720E+00
1   15     3.20E+04  9.110E+00

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Chromium              1.555E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   2.74E+03 cm
X-Direction Dispersivity (A1)       =   2.70E+01 cm
Bulk Density (R2)                   =   1.64E+00 g/cm**3
Total Porosity (AN4)                =   3.80E-01 (fraction)
Field Capacity (AN5)                =   9.00E-02 (fraction)
Permeability (X2)                   =   8.81E+04 cm/day
Darcy Velocity (VLEACH)             =   2.00E-01 cm/day
Soil-Type Coefficient (A2)          =   4.05E+00



Moisture Content (AN5)   =  11.78 %
Pore water velocity (U)  =   1.70E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chromium   2.740E+17   0.000E+00   1.000E+00     4.376E+00     4.420E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chromium     16065831 2.74E+17   7.34E-05  0.00E+00     1.00E+20    1.01E+05



Chromium (16065831): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  1.693E+04 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chromium  16065831  2.814E-01  NUMERICAL    1.005E+05     1.005E+05 (100.0%)

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

Chromium    16065831   PEAK        2.924E+04          13.1

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
Usage Location Name   = ad8

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Cobalt           1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Cobalt             41

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.000E-01 cm/day

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

Cobalt       7440484  2.74E+17   4.77E-06  5.00E+00     1.00E+20    1.00E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  5.930E+01        1    2     4.92E-05  6.530E+01
1    3     7.77E-03  7.140E+01        1    4     1.96E-01  7.740E+01
1    5     2.39E+00  8.350E+01        1    6     1.66E+01  8.950E+01
1    7     7.40E+01  9.550E+01        1    8     2.31E+02  1.020E+02
1    9     5.42E+02  1.080E+02        1   10     1.00E+03  1.140E+02
1   11     1.52E+03  1.200E+02        1   12     1.95E+03  1.260E+02
1   13     2.18E+03  1.320E+02        1   14     2.16E+03  1.380E+02
1   15     1.92E+03  1.440E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  5.930E+01        1    2     4.92E-05  6.530E+01
1    3     7.77E-03  7.140E+01        1    4     1.96E-01  7.740E+01
1    5     2.39E+00  8.350E+01        1    6     1.66E+01  8.950E+01
1    7     7.40E+01  9.550E+01        1    8     2.31E+02  1.020E+02
1    9     5.42E+02  1.080E+02        1   10     1.00E+03  1.140E+02
1   11     1.52E+03  1.200E+02        1   12     1.95E+03  1.260E+02
1   13     2.18E+03  1.320E+02        1   14     2.16E+03  1.380E+02
1   15     1.92E+03  1.440E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Cobalt                2.417E+02
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   2.74E+03 cm
X-Direction Dispersivity (A1)       =   2.70E+01 cm
Bulk Density (R2)                   =   1.64E+00 g/cm**3
Total Porosity (AN4)                =   3.80E-01 (fraction)
Field Capacity (AN5)                =   9.00E-02 (fraction)
Permeability (X2)                   =   8.81E+04 cm/day
Darcy Velocity (VLEACH)             =   2.00E-01 cm/day
Soil-Type Coefficient (A2)          =   4.05E+00



Moisture Content (AN5)   =  11.78 %
Pore water velocity (U)  =   1.70E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Cobalt     2.740E+17   5.000E+00   7.058E+01     3.089E+02     3.120E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Cobalt       7440484  2.74E+17   4.78E-06  5.00E+00     1.00E+20    1.00E+05



Cobalt   (7440484 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  1.693E+04 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Cobalt    7440484   1.278E+00  NUMERICAL    9.925E+04     9.926E+04 (100.0%)

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

Cobalt      7440484    PEAK        8.284E+02         450.3

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad8

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Lead             1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Lead               41

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.000E-01 cm/day

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

Lead         7439921  2.74E+17   8.48E-07  3.00E+01     1.00E+20    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  3.380E+02        1    2     8.59E-06  3.710E+02
1    3     1.21E-03  4.040E+02        1    4     2.84E-02  4.370E+02
1    5     3.35E-01  4.700E+02        1    6     2.30E+00  5.030E+02
1    7     1.03E+01  5.360E+02        1    8     3.29E+01  5.690E+02
1    9     7.94E+01  6.020E+02        1   10     1.52E+02  6.350E+02
1   11     2.39E+02  6.680E+02        1   12     3.21E+02  7.010E+02
1   13     3.74E+02  7.350E+02        1   14     3.87E+02  7.680E+02
1   15     3.62E+02  8.010E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  3.380E+02        1    2     8.59E-06  3.710E+02
1    3     1.21E-03  4.040E+02        1    4     2.84E-02  4.370E+02
1    5     3.35E-01  4.700E+02        1    6     2.30E+00  5.030E+02
1    7     1.03E+01  5.360E+02        1    8     3.29E+01  5.690E+02
1    9     7.94E+01  6.020E+02        1   10     1.52E+02  6.350E+02
1   11     2.39E+02  6.680E+02        1   12     3.21E+02  7.010E+02
1   13     3.74E+02  7.350E+02        1   14     3.87E+02  7.680E+02
1   15     3.62E+02  8.010E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Lead                  1.322E+03
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   2.74E+03 cm
X-Direction Dispersivity (A1)       =   2.70E+01 cm
Bulk Density (R2)                   =   1.64E+00 g/cm**3
Total Porosity (AN4)                =   3.80E-01 (fraction)
Field Capacity (AN5)                =   9.00E-02 (fraction)
Permeability (X2)                   =   8.81E+04 cm/day
Darcy Velocity (VLEACH)             =   2.00E-01 cm/day
Soil-Type Coefficient (A2)          =   4.05E+00



Moisture Content (AN5)   =  11.78 %
Pore water velocity (U)  =   1.70E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Lead       2.740E+17   3.000E+01   4.185E+02     1.831E+03     1.850E+03

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Lead         7439921  2.74E+17   8.48E-07  3.00E+01     1.00E+20    1.01E+05



Lead     (7439921 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  1.693E+04 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Lead      7439921   1.385E+00  NUMERICAL    1.001E+05     1.001E+05 (100.0%)

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

Lead        7439921    PEAK        1.423E+02        2620.1

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
Usage Location Name   = ad8

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Manganes         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Manganes           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.000E-01 cm/day

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

Manganes     7439965  2.74E+17   1.67E-06  1.50E+01     1.00E+20    9.97E+04


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  1.710E+02        1    2     1.61E-05  1.870E+02
1    3     2.42E-03  2.040E+02        1    4     5.78E-02  2.210E+02
1    5     6.86E-01  2.380E+02        1    6     4.73E+00  2.550E+02
1    7     2.13E+01  2.720E+02        1    8     6.76E+01  2.880E+02
1    9     1.62E+02  3.050E+02        1   10     3.08E+02  3.220E+02
1   11     4.82E+02  3.390E+02        1   12     6.41E+02  3.560E+02
1   13     7.42E+02  3.730E+02        1   14     7.62E+02  3.900E+02
1   15     7.05E+02  4.060E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  1.710E+02        1    2     1.61E-05  1.870E+02
1    3     2.42E-03  2.040E+02        1    4     5.78E-02  2.210E+02
1    5     6.86E-01  2.380E+02        1    6     4.73E+00  2.550E+02
1    7     2.13E+01  2.720E+02        1    8     6.76E+01  2.880E+02
1    9     1.62E+02  3.050E+02        1   10     3.08E+02  3.220E+02
1   11     4.82E+02  3.390E+02        1   12     6.41E+02  3.560E+02
1   13     7.42E+02  3.730E+02        1   14     7.62E+02  3.900E+02
1   15     7.05E+02  4.060E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Manganes              6.730E+02
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   2.74E+03 cm
X-Direction Dispersivity (A1)       =   2.70E+01 cm
Bulk Density (R2)                   =   1.64E+00 g/cm**3
Total Porosity (AN4)                =   3.80E-01 (fraction)
Field Capacity (AN5)                =   9.00E-02 (fraction)
Permeability (X2)                   =   8.81E+04 cm/day
Darcy Velocity (VLEACH)             =   2.00E-01 cm/day
Soil-Type Coefficient (A2)          =   4.05E+00



Moisture Content (AN5)   =  11.78 %
Pore water velocity (U)  =   1.70E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Manganes   2.740E+17   1.500E+01   2.097E+02     9.179E+02     9.271E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Manganes     7439965  2.74E+17   1.67E-06  1.50E+01     1.00E+20    9.97E+04



Manganes (7439965 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  1.693E+04 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Manganes  7439965   1.364E+00  NUMERICAL    9.966E+04     9.966E+04 (100.0%)

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

Manganes    7439965    PEAK        2.821E+02        1318.2

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
"vad9",0000000667
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
Usage Location Name   = ad9

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Chromium         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Chromium           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.000E-01 cm/day

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

Chromium     16065831 2.74E+17   6.40E-05  0.00E+00     1.00E+20    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  5.630E+00        1    2     1.27E-08  6.210E+00
1    3     1.05E-05  6.790E+00        1    4     1.77E-03  7.360E+00
1    5     1.02E-01  7.940E+00        1    6     2.58E+00  8.520E+00
1    7     3.40E+01  9.090E+00        1    8     2.61E+02  9.670E+00
1    9     1.27E+03  1.020E+01        1   10     4.24E+03  1.080E+01
1   11     1.01E+04  1.140E+01        1   12     1.82E+04  1.200E+01
1   13     2.57E+04  1.260E+01        1   14     2.92E+04  1.310E+01
1   15     2.77E+04  1.370E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  5.630E+00        1    2     1.27E-08  6.210E+00
1    3     1.05E-05  6.790E+00        1    4     1.77E-03  7.360E+00
1    5     1.02E-01  7.940E+00        1    6     2.58E+00  8.520E+00
1    7     3.40E+01  9.090E+00        1    8     2.61E+02  9.670E+00
1    9     1.27E+03  1.020E+01        1   10     4.24E+03  1.080E+01
1   11     1.01E+04  1.140E+01        1   12     1.82E+04  1.200E+01
1   13     2.57E+04  1.260E+01        1   14     2.92E+04  1.310E+01
1   15     2.77E+04  1.370E+01

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Chromium              2.307E+01
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   9.10E+02 cm
X-Direction Dispersivity (A1)       =   9.10E+00 cm
Bulk Density (R2)                   =   1.64E+00 g/cm**3
Total Porosity (AN4)                =   3.80E-01 (fraction)
Field Capacity (AN5)                =   9.00E-02 (fraction)
Permeability (X2)                   =   8.81E+04 cm/day
Darcy Velocity (VLEACH)             =   2.00E-01 cm/day
Soil-Type Coefficient (A2)          =   4.05E+00



Moisture Content (AN5)   =  11.78 %
Pore water velocity (U)  =   1.70E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chromium   2.740E+17   0.000E+00   1.000E+00     1.453E+00     1.468E+00

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chromium     16065831 2.74E+17   6.40E-05  0.00E+00     1.00E+20    1.01E+05



Chromium (16065831): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  5.831E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chromium  16065831  6.298E-02  NUMERICAL    1.004E+05     1.004E+05 (100.1%)

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

Chromium    16065831   PEAK        2.842E+04          14.6

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
Usage Location Name   = ad9

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Cobalt           1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Cobalt             41

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.000E-01 cm/day

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

Cobalt       7440484  2.74E+17   1.81E-06  5.00E+00     1.00E+20    9.94E+04


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  1.990E+02        1    2     2.51E-09  2.190E+02
1    3     3.48E-06  2.380E+02        1    4     4.32E-04  2.570E+02
1    5     1.76E-02  2.770E+02        1    6     3.26E-01  2.960E+02
1    7     3.24E+00  3.150E+02        1    8     1.92E+01  3.340E+02
1    9     7.46E+01  3.540E+02        1   10     2.03E+02  3.730E+02
1   11     4.08E+02  3.920E+02        1   12     6.35E+02  4.120E+02
1   13     7.96E+02  4.310E+02        1   14     8.28E+02  4.500E+02
1   15     7.33E+02  4.700E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  1.990E+02        1    2     2.51E-09  2.190E+02
1    3     3.48E-06  2.380E+02        1    4     4.32E-04  2.570E+02
1    5     1.76E-02  2.770E+02        1    6     3.26E-01  2.960E+02
1    7     3.24E+00  3.150E+02        1    8     1.92E+01  3.340E+02
1    9     7.46E+01  3.540E+02        1   10     2.03E+02  3.730E+02
1   11     4.08E+02  3.920E+02        1   12     6.35E+02  4.120E+02
1   13     7.96E+02  4.310E+02        1   14     8.28E+02  4.500E+02
1   15     7.33E+02  4.700E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Cobalt                7.730E+02
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   9.10E+02 cm
X-Direction Dispersivity (A1)       =   9.10E+00 cm
Bulk Density (R2)                   =   1.64E+00 g/cm**3
Total Porosity (AN4)                =   3.80E-01 (fraction)
Field Capacity (AN5)                =   9.00E-02 (fraction)
Permeability (X2)                   =   8.81E+04 cm/day
Darcy Velocity (VLEACH)             =   2.00E-01 cm/day
Soil-Type Coefficient (A2)          =   4.05E+00



Moisture Content (AN5)   =  11.78 %
Pore water velocity (U)  =   1.70E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Cobalt     2.740E+17   5.000E+00   7.058E+01     1.026E+02     1.036E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Cobalt       7440484  2.74E+17   1.81E-06  5.00E+00     1.00E+20    9.94E+04



Cobalt   (7440484 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  5.831E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Cobalt    7440484   1.327E-01  NUMERICAL    9.937E+04     9.937E+04 (100.0%)

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

Cobalt      7440484    PEAK        7.808E+02         554.5

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad9

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Lead             1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Lead               41

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.000E-01 cm/day

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

Lead         7439921  2.74E+17   3.12E-07  3.00E+01     1.00E+20    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  1.170E+03        1    2     5.50E-10  1.280E+03
1    3     7.01E-07  1.390E+03        1    4     8.23E-05  1.500E+03
1    5     3.18E-03  1.610E+03        1    6     5.70E-02  1.730E+03
1    7     5.51E-01  1.840E+03        1    8     3.21E+00  1.950E+03
1    9     1.23E+01  2.060E+03        1   10     3.34E+01  2.170E+03
1   11     6.73E+01  2.290E+03        1   12     1.06E+02  2.400E+03
1   13     1.34E+02  2.510E+03        1   14     1.42E+02  2.620E+03
1   15     1.29E+02  2.730E+03

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  1.170E+03        1    2     5.50E-10  1.280E+03
1    3     7.01E-07  1.390E+03        1    4     8.23E-05  1.500E+03
1    5     3.18E-03  1.610E+03        1    6     5.70E-02  1.730E+03
1    7     5.51E-01  1.840E+03        1    8     3.21E+00  1.950E+03
1    9     1.23E+01  2.060E+03        1   10     3.34E+01  2.170E+03
1   11     6.73E+01  2.290E+03        1   12     1.06E+02  2.400E+03
1   13     1.34E+02  2.510E+03        1   14     1.42E+02  2.620E+03
1   15     1.29E+02  2.730E+03

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Lead                  4.470E+03
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   9.10E+02 cm
X-Direction Dispersivity (A1)       =   9.10E+00 cm
Bulk Density (R2)                   =   1.64E+00 g/cm**3
Total Porosity (AN4)                =   3.80E-01 (fraction)
Field Capacity (AN5)                =   9.00E-02 (fraction)
Permeability (X2)                   =   8.81E+04 cm/day
Darcy Velocity (VLEACH)             =   2.00E-01 cm/day
Soil-Type Coefficient (A2)          =   4.05E+00



Moisture Content (AN5)   =  11.78 %
Pore water velocity (U)  =   1.70E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Lead       2.740E+17   3.000E+01   4.185E+02     6.080E+02     6.144E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Lead         7439921  2.74E+17   3.11E-07  3.00E+01     1.00E+20    1.01E+05



Lead     (7439921 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  5.831E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Lead      7439921   1.360E-01  NUMERICAL    9.992E+04     9.991E+04 (100.0%)

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

Lead        7439921    PEAK        1.336E+02        3241.0

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
Usage Location Name   = ad9

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Manganes         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Manganes           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 2.000E-01 cm/day

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

Manganes     7439965  2.74E+17   6.18E-07  1.50E+01     1.00E+20    9.98E+04


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     0.00E+00  5.870E+02        1    2     1.12E-09  6.430E+02
1    3     1.36E-06  7.000E+02        1    4     1.62E-04  7.560E+02
1    5     6.34E-03  8.120E+02        1    6     1.14E-01  8.680E+02
1    7     1.11E+00  9.240E+02        1    8     6.45E+00  9.810E+02
1    9     2.47E+01  1.040E+03        1   10     6.69E+01  1.090E+03
1   11     1.35E+02  1.150E+03        1   12     2.11E+02  1.210E+03
1   13     2.67E+02  1.260E+03        1   14     2.82E+02  1.320E+03
1   15     2.54E+02  1.370E+03

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     0.00E+00  5.870E+02        1    2     1.12E-09  6.430E+02
1    3     1.36E-06  7.000E+02        1    4     1.62E-04  7.560E+02
1    5     6.34E-03  8.120E+02        1    6     1.14E-01  8.680E+02
1    7     1.11E+00  9.240E+02        1    8     6.45E+00  9.810E+02
1    9     2.47E+01  1.040E+03        1   10     6.69E+01  1.090E+03
1   11     1.35E+02  1.150E+03        1   12     2.11E+02  1.210E+03
1   13     2.67E+02  1.260E+03        1   14     2.82E+02  1.320E+03
1   15     2.54E+02  1.370E+03

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Manganes              2.253E+03
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   9.10E+02 cm
X-Direction Dispersivity (A1)       =   9.10E+00 cm
Bulk Density (R2)                   =   1.64E+00 g/cm**3
Total Porosity (AN4)                =   3.80E-01 (fraction)
Field Capacity (AN5)                =   9.00E-02 (fraction)
Permeability (X2)                   =   8.81E+04 cm/day
Darcy Velocity (VLEACH)             =   2.00E-01 cm/day
Soil-Type Coefficient (A2)          =   4.05E+00



Moisture Content (AN5)   =  11.78 %
Pore water velocity (U)  =   1.70E+00 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Manganes   2.740E+17   1.500E+01   2.097E+02     3.047E+02     3.079E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Manganes     7439965  2.74E+17   6.18E-07  1.50E+01     1.00E+20    9.98E+04



Manganes (7439965 ): DECAY CONSTANT =  2.530E-18 1/YR

Longitudinal Dispersion Coefficient =  5.831E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Manganes  7439965   1.353E-01  NUMERICAL    9.938E+04     9.939E+04 (100.0%)

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

Manganes    7439965    PEAK        2.648E+02        1629.1

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
