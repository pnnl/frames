"vad3",0000003282
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

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    7.50E+14   2.35E-09  5.00E-03     1.00E+00    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Acetone    7.500E+14   5.000E-03   1.029E+00     7.553E+01     7.633E+01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    7.50E+14   2.36E-09  5.00E-03     1.00E+00    1.01E+02



Acetone  (67641   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Acetone   67641     7.553E-01  NUMERICAL    1.000E+02     9.999E+01 (  **  )

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

Acetone     67641      PEAK        1.000E+00         138.8

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

AMERICIU         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          AMERICIU            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

AMERICIU     AM241    4.33E+02   2.35E-09  2.00E+02     1.00E+23    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  AMERICIU   4.330E+02   2.000E+02   1.176E+03     3.307E+04     8.721E+04

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

AMERICIU     AM241    4.33E+02   2.36E-09  2.00E+02     1.00E+23    1.01E+02



AMERICIU (AM241   ): DECAY CONSTANT =  1.601E-03 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

AMERICIU  AM241     3.307E+02  INSTANT      1.000E+02     1.213E-39 (  **  )

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

AMERICIU    AM241      SS          1.219E-43         252.5 to  10101.0

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Aroclor          1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Aroclor             2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Aroclor      11104282 7.50E+14   2.35E-09  4.76E+01     4.00E-05    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Aroclor    7.500E+14   4.760E+01   2.807E+02     2.059E+04     2.081E+04

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Aroclor      11104282 7.50E+14   2.36E-09  4.76E+01     4.00E-05    1.01E+02



Aroclor  (11104282): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Aroclor   11104282  2.059E+02  INSTANT      1.000E+02     1.830E-05 (  **  )

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

Aroclor     11104282   PEAK        7.105E-08       10101.0

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

Benzo(a)         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Benzo(a)            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Benzo(a)     50328    7.50E+14   2.35E-09  4.51E+04     1.60E-09    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Benzo(a)   7.500E+14   4.510E+04   2.650E+05     1.944E+07     1.965E+07

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Benzo(a)     50328    7.50E+14   2.36E-09  4.51E+04     1.60E-09    1.01E+02



Benzo(a) (50328   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Benzo(a)  50328     1.944E+05  INSTANT      1.000E+02     1.213E-39 (  **  )

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

Benzo(a)    50328      SS          1.219E-43         252.5 to  10101.0

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

CESIUM-1         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          CESIUM-1            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

CESIUM-1     CS137    3.01E+01   2.35E-09  2.49E+01     1.00E+23    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  CESIUM-1   3.010E+01   2.490E+01   1.473E+02     3.193E+03     1.092E+04

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

CESIUM-1     CS137    3.01E+01   2.36E-09  2.49E+01     1.00E+23    1.01E+02



CESIUM-1 (CS137   ): DECAY CONSTANT =  2.303E-02 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

CESIUM-1  CS137     3.193E+01  INSTANT      1.000E+02     1.213E-39 (  **  )

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

CESIUM-1    CS137      SS          1.219E-43         252.5 to  10101.0

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Chlorobe         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Chlorobe            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chlorobe     108907   7.50E+14   2.35E-09  2.71E+00     4.66E-04    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chlorobe   7.500E+14   2.710E+00   1.692E+01     1.242E+03     1.255E+03

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chlorobe     108907   7.50E+14   2.36E-09  2.71E+00     4.66E-04    1.01E+02



Chlorobe (108907  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chlorobe  108907    1.242E+01  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Chlorobe    108907     PEAK        2.169E-01        1299.2

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

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chromium     7440473  7.50E+14   2.35E-09  1.00E+00     1.00E+17    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Chromium   7.500E+14   1.000E+00   6.876E+00     5.045E+02     5.098E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Chromium     7440473  7.50E+14   2.36E-09  1.00E+00     1.00E+17    1.01E+02



Chromium (7440473 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Chromium  7440473   5.045E+00  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Chromium    7440473    PEAK        5.033E-01         547.1

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Dichloro         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Dichloro            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Dichloro     72559    7.50E+14   2.35E-09  6.83E+04     1.20E-07    1.01E+02


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time      Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

Constituent #     Flux      Time    Constituent #     Flux      Time

1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

#      Constituent         New TLIFE (Years)

1       Dichloro              1.000E+02
Modeling Medium #             1
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Dichloro   7.500E+14   6.830E+04   4.013E+05     2.945E+07     2.976E+07

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Dichloro     72559    7.50E+14   2.36E-09  6.83E+04     1.20E-07    1.01E+02



Dichloro (72559   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Dichloro  72559     2.945E+05  INSTANT      1.000E+02     1.213E-39 (  **  )

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

Dichloro    72559      SS          1.219E-43         252.5 to  10101.0

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Ethylene         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Ethylene            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Ethylene     107211   7.50E+14   2.35E-09  2.21E-04     5.82E-01    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Ethylene   7.500E+14   2.210E-04   1.001E+00     7.346E+01     7.425E+01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Ethylene     107211   7.50E+14   2.36E-09  2.21E-04     5.82E-01    1.01E+02



Ethylene (107211  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Ethylene  107211    7.346E-01  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Ethylene    107211     SS          1.000E+00         130.5 to    142.0

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Hexachlo         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Hexachlo            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Hexachlo     118741   7.50E+14   2.35E-09  3.20E+01     6.00E-09    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Hexachlo   7.500E+14   3.200E+01   1.890E+02     1.387E+04     1.402E+04

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Hexachlo     118741   7.50E+14   2.36E-09  3.20E+01     6.00E-09    1.01E+02



Hexachlo (118741  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Hexachlo  118741    1.387E+02  INSTANT      1.000E+02     1.181E+00 (  **  )

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

Hexachlo    118741     PEAK        2.123E-03       10101.0

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

IODINE-1         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          IODINE-1            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

IODINE-1     I129     1.57E+07   2.35E-09  0.00E+00     1.00E+23    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  IODINE-1   1.570E+07   0.000E+00   1.000E+00     7.337E+01     7.415E+01

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

IODINE-1     I129     1.57E+07   2.36E-09  0.00E+00     1.00E+23    1.01E+02



IODINE-1 (I129    ): DECAY CONSTANT =  4.415E-08 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

IODINE-1  I129      7.337E-01  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

IODINE-1    I129       SS          1.000E+00         130.4 to    141.9

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Mercury          1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Mercury             2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Mercury      7439976  7.50E+14   2.35E-09  6.00E+01     3.00E-08    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Mercury    7.500E+14   6.000E+01   3.536E+02     2.594E+04     2.622E+04

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Mercury      7439976  7.50E+14   2.36E-09  6.00E+01     3.00E-08    1.01E+02



Mercury  (7439976 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Mercury   7439976   2.594E+02  INSTANT      1.000E+02     5.436E-10 (  **  )

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

Mercury     7439976    PEAK        2.523E-12       10101.0

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Naphthal         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Naphthal            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Naphthal     91203    7.50E+14   2.35E-09  7.71E+00     3.20E-05    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Naphthal   7.500E+14   7.710E+00   4.630E+01     3.397E+03     3.433E+03

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Naphthal     91203    7.50E+14   2.36E-09  7.71E+00     3.20E-05    1.01E+02



Naphthal (91203   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Naphthal  91203     3.397E+01  INSTANT      1.000E+02     1.000E+02 (  **  )

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

Naphthal    91203      PEAK        8.075E-02        3344.9

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

PLUTONIU         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          PLUTONIU            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

PLUTONIU     PU240    6.54E+03   2.35E-09  4.00E+00     1.00E+23    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  PLUTONIU   6.540E+03   4.000E+00   2.450E+01     1.791E+03     1.817E+03

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

PLUTONIU     PU240    6.54E+03   2.36E-09  4.00E+00     1.00E+23    1.01E+02



PLUTONIU (PU240   ): DECAY CONSTANT =  1.060E-04 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

PLUTONIU  PU240     1.791E+01  NUMERICAL    1.000E+02     8.235E+01 (  **  )

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

PLUTONIU    PU240      PEAK        1.245E-01        1858.0

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Pyrene           1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Pyrene              2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Pyrene       129000   7.50E+14   2.35E-09  3.12E+02     1.35E-07    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Pyrene     7.500E+14   3.120E+02   1.834E+03     1.346E+05     1.360E+05

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Pyrene       129000   7.50E+14   2.36E-09  3.12E+02     1.35E-07    1.01E+02



Pyrene   (129000  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Pyrene    129000    1.346E+03  INSTANT      1.000E+02     1.213E-39 (  **  )

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

Pyrene      129000     SS          1.219E-43         252.5 to  10101.0

(a)
SS: Fluxes vary by less than 1/100th of one percent of the
maximum flux over the given time range (steady state).


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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Selenium         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Selenium            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Selenium     7782492  7.50E+14   2.35E-09  1.49E+01     1.00E+17    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Selenium   7.500E+14   1.490E+01   8.855E+01     6.497E+03     6.566E+03

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Selenium     7782492  7.50E+14   2.36E-09  1.49E+01     1.00E+17    1.01E+02



Selenium (7782492 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Selenium  7782492   6.497E+01  INSTANT      1.000E+02     9.986E+01 (  **  )

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

Selenium    7782492    PEAK        4.216E-02        6490.1

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

TECHNETI         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          TECHNETI            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

TECHNETI     TC99     2.13E+05   2.35E-09  2.00E+01     1.00E+23    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  TECHNETI   2.130E+05   2.000E+01   1.185E+02     8.691E+03     8.788E+03

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

TECHNETI     TC99     2.13E+05   2.36E-09  2.00E+01     1.00E+23    1.01E+02



TECHNETI (TC99    ): DECAY CONSTANT =  3.254E-06 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

TECHNETI  TC99      8.691E+01  INSTANT      1.000E+02     8.083E+01 (  **  )

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

TECHNETI    TC99       PEAK        3.066E-02        8538.8

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Toluene          1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Toluene             2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Toluene      108883   7.50E+14   2.35E-09  2.46E+00     5.35E-04    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Toluene    7.500E+14   2.460E+00   1.546E+01     1.134E+03     1.146E+03

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Toluene      108883   7.50E+14   2.36E-09  2.46E+00     5.35E-04    1.01E+02



Toluene  (108883  ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Toluene   108883    1.134E+01  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Toluene     108883     PEAK        2.372E-01        1189.2

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad3

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

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

Source-Term Type (ISTYPE)                     =  1
Source-Term Flux Boundary Conditions (ISOURC) =  2
Liquid Impoundment Index (IPOND)              =  2
Source-Term Decay Index (IDECAY)              =  2
Direct Discharge Surface Water Index (IDDSW)  =  0
Number of Parent Constituents (NUMCON)        =  9

Source-Term Configuration (ISCONF)        =  4
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  1

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

URANIUM-     U238     4.46E+09   2.35E-09  0.00E+00     1.00E+23    1.01E+02
THORIUM-     TH234    6.60E-02  -9.99E+01  6.00E+01     1.00E+23    1.01E-30
URANIUM-     U234     2.44E+05  -9.99E+01  5.00E+00     1.00E+23    1.01E-30
THORIUM-     TH230    7.69E+04  -9.99E+01  6.00E+01     1.00E+23    1.01E-30
RADIUM-2     RA226    1.60E+03  -9.99E+01  1.00E+02     1.00E+23    1.01E-30
RADON-22     RN222    1.05E-02  -9.99E+01  0.00E+00     1.00E+23    1.01E-30
LEAD-210     PB210    2.23E+01  -9.99E+01  1.00E+01     1.00E+23    1.01E-30
BISMUTH-     BI210    1.37E-02  -9.99E+01  0.00E+00     1.00E+23    1.01E-30
POLONIUM     PO210    3.78E-01  -9.99E+01  1.49E+01     1.00E+23    1.01E-30


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  URANIUM-   4.460E+09   0.000E+00   1.000E+00     7.337E+01     7.415E+01
2  THORIUM-   6.600E-02   6.000E+01   3.536E+02     2.430E+02     2.622E+04
3  URANIUM-   2.440E+05   5.000E+00   3.038E+01     2.229E+03     2.253E+03
4  THORIUM-   7.690E+04   6.000E+01   3.536E+02     2.581E+04     2.622E+04
5  RADIUM-2   1.600E+03   1.000E+02   5.886E+02     3.229E+04     4.364E+04
6  RADON-22   1.050E-02   0.000E+00   1.000E+00     5.140E+00     7.415E+01
7  LEAD-210   2.230E+01   1.000E+01   5.976E+01     1.690E+03     4.431E+03
8  BISMUTH-   1.370E-02   0.000E+00   1.000E+00     5.866E+00     7.415E+01
9  POLONIUM   3.780E-01   1.490E+01   8.855E+01     2.907E+02     6.566E+03

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

URANIUM-     U238     4.46E+09   2.36E-09  0.00E+00     1.00E+23    1.01E+02
THORIUM-     TH234    6.60E-02   1.00E+30  6.00E+01     1.00E+23    1.01E-30
URANIUM-     U234     2.44E+05   1.00E+30  5.00E+00     1.00E+23    1.01E-30
THORIUM-     TH230    7.69E+04   1.00E+30  6.00E+01     1.00E+23    1.01E-30
RADIUM-2     RA226    1.60E+03   1.00E+30  1.00E+02     1.00E+23    1.01E-30
RADON-22     RN222    1.05E-02   1.00E+30  0.00E+00     1.00E+23    1.01E-30
LEAD-210     PB210    2.23E+01   1.00E+30  1.00E+01     1.00E+23    1.01E-30
BISMUTH-     BI210    1.37E-02   1.00E+30  0.00E+00     1.00E+23    1.01E-30
POLONIUM     PO210    3.78E-01   1.00E+30  1.49E+01     1.00E+23    1.01E-30



URANIUM- (U238    ): DECAY CONSTANT =  1.554E-10 1/YR
THORIUM- (TH234   ): DECAY CONSTANT =  1.050E+01 1/YR
URANIUM- (U234    ): DECAY CONSTANT =  2.841E-06 1/YR
THORIUM- (TH230   ): DECAY CONSTANT =  9.014E-06 1/YR
RADIUM-2 (RA226   ): DECAY CONSTANT =  4.332E-04 1/YR
RADON-22 (RN222   ): DECAY CONSTANT =  6.601E+01 1/YR
LEAD-210 (PB210   ): DECAY CONSTANT =  3.108E-02 1/YR
BISMUTH- (BI210   ): DECAY CONSTANT =  5.059E+01 1/YR
POLONIUM (PO210   ): DECAY CONSTANT =  1.834E+00 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

URANIUM-  U238      7.337E-01  NUMERICAL    1.000E+02     1.000E+02 (  **  )
THORIUM-  TH234     2.430E+02  INSTANT      0.000E+00     0.000E+00 (  **  )
URANIUM-  U234      2.229E+03  INSTANT      0.000E+00     0.000E+00 (  **  )
THORIUM-  TH230     2.581E+04  INSTANT      0.000E+00     0.000E+00 (  **  )
RADIUM-2  RA226     3.229E+04  INSTANT      0.000E+00     0.000E+00 (  **  )
RADON-22  RN222     5.140E+00  NUMERICAL    0.000E+00     0.000E+00 (  **  )
LEAD-210  PB210     1.690E+03  INSTANT      0.000E+00     0.000E+00 (  **  )
BISMUTH-  BI210     5.866E+00  NUMERICAL    0.000E+00     0.000E+00 (  **  )
POLONIUM  PO210     2.907E+02  INSTANT      0.000E+00     0.000E+00 (  **  )

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

URANIUM-    U238       SS          1.000E+00         130.4 to    141.9
THORIUM-    TH234      SS          1.000E+00         130.4 to    141.9
URANIUM-    U234       SS          2.103E-04         130.4 to    141.9
THORIUM-    TH230      SS          7.019E-08         130.4 to    141.9
RADIUM-2    RA226      SS          7.447E-10         130.4 to    141.9
RADON-22    RN222      SS          7.442E-10         130.4 to    141.9
LEAD-210    PB210      SS          2.878E-10         130.4 to    141.9
BISMUTH-    BI210      SS          2.876E-10         130.4 to    141.9
POLONIUM    PO210      SS          2.800E-10         130.4 to    141.9

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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Vinyl ch         1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Vinyl ch            2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Vinyl ch     75014    7.50E+14   2.35E-09  4.68E-01     2.67E-03    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Vinyl ch   7.500E+14   4.680E-01   3.750E+00     2.751E+02     2.781E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Vinyl ch     75014    7.50E+14   2.36E-09  4.68E-01     2.67E-03    1.01E+02



Vinyl ch (75014   ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Vinyl ch  75014     2.751E+00  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Vinyl ch    75014      PEAK        7.867E-01         327.9

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


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
Usage Location Name   = ad3

TLIFE is the duration of release for each contaminant expressed in years.
TDIFF is the time difference in years between the start date for risk
calculations and the date when a contaminant was first released into
the environment (WS-TRISK minus WS-CDATE).


Constituent         TLIFE     TDIFF          TFINAL
Name           (years)   (years)         (years)

Xylene           1.000E+04 0.000E+00      1.000E+04

#           Constituent         NUM

1          Xylene              2

Known Darcian Infiltration Rate from Site (VLEACH)   = 4.650E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+03 cm
Width of Release Unit (B1)                          =   5.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)     Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Xylene       1330207  7.50E+14   2.35E-09  1.97E+00     1.98E-04    1.01E+02


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
Medium Type (MED)          =  1
Source Configuration       =  4
Source Term Discharge Type =  1
PARTIALLY SATURATED ZONE



Thickness of Unsaturated Layer (H1) =   5.00E+03 cm
X-Direction Dispersivity (A1)       =   5.00E+01 cm
Bulk Density (R2)                   =   1.48E+00 g/cm**3
Total Porosity (AN4)                =   4.42E-01 (fraction)
Field Capacity (AN5)                =   1.75E-01 (fraction)
Permeability (X2)                   =   6.22E+01 cm/day
Darcy Velocity (VLEACH)             =   4.65E-02 cm/day
Soil-Type Coefficient (A2)          =   4.90E+00



Moisture Content (AN5)   =  25.19 %
Pore water velocity (U)  =   1.85E-01 cm/day



CENTERLINE    CENTERLINE
ESTIMATED     ADVECTIVE
NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
YEARS        ML/G       FACTOR         YEARS         YEARS

1  Xylene     7.500E+14   1.970E+00   1.258E+01     9.227E+02     9.325E+02

Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
(Years)      Conc.    Coeff.      limit     Inventory
(ml/g)      (g/ml)    (g OR pCi)

Xylene       1330207  7.50E+14   2.36E-09  1.97E+00     1.98E-04    1.01E+02



Xylene   (1330207 ): DECAY CONSTANT =  9.242E-16 1/YR

Longitudinal Dispersion Coefficient =  3.562E+03 cm^2/yr


Mass Released       Mass Passed
Solution   From Source        This Medium
Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
--------  --------  ---------  --------  -------------  --------------------

Xylene    1330207   9.227E+00  NUMERICAL    1.000E+02     1.000E+02 (  **  )

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

Xylene      1330207    PEAK        2.899E-01         973.7

(a)
PEAK: The maximum flux occurs as a peak flux at the given time.


******************************************************************************
