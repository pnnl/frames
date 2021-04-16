"vad5",368
 ****************************VERSION 11/12/2002****************************


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
 Usage Location Name   = all                  

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Benzene          1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Benzene            41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.370E-02 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   5.00E+02 cm
Width of Release Unit (B1)                          =   5.00E+02 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Benzene      71432    2.74E+17   6.84E-05  1.11E+00     1.00E+00    4.89E+03


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     8.55E+01  1.000E+00
        1    3     8.41E+01  2.000E+00        1    4     7.99E+01  5.000E+00
        1    5     7.08E+01  1.200E+01        1    6     6.96E+01  1.300E+01
        1    7     6.29E+01  1.900E+01        1    8     5.87E+01  2.300E+01
        1    9     5.48E+01  2.700E+01        1   10     5.39E+01  2.800E+01
        1   11     4.46E+01  3.900E+01        1   12     4.24E+01  4.200E+01
        1   13     3.83E+01  4.800E+01        1   14     3.70E+01  5.000E+01
        1   15     3.34E+01  5.600E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     8.55E+01  1.000E+00
        1    3     8.41E+01  2.000E+00        1    4     7.99E+01  5.000E+00
        1    5     7.08E+01  1.200E+01        1    6     6.96E+01  1.300E+01
        1    7     6.29E+01  1.900E+01        1    8     5.87E+01  2.300E+01
        1    9     5.48E+01  2.700E+01        1   10     5.39E+01  2.800E+01
        1   11     4.46E+01  3.900E+01        1   12     4.24E+01  4.200E+01
        1   13     3.83E+01  4.800E+01        1   14     3.70E+01  5.000E+01
        1   15     3.34E+01  5.600E+01

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Benzene               2.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  1
 Source Configuration       =  4
 Source Term Discharge Type =  1
 PARTIALLY SATURATED ZONE



 Thickness of Unsaturated Layer (H1) =   3.40E+01 cm
 X-Direction Dispersivity (A1)       =   3.00E+00 cm
 Bulk Density (R2)                   =   1.36E+00 g/cm**3
 Total Porosity (AN4)                =   4.86E-01 (fraction)
 Field Capacity (AN5)                =   4.20E-01 (fraction)
 Permeability (X2)                   =   2.25E+00 cm/day
 Darcy Velocity (VLEACH)             =   1.37E-02 cm/day
 Soil-Type Coefficient (A2)          =   1.04E+01



 Moisture Content (AN5)   =  42.00 %
 Pore water velocity (U)  =   3.26E-02 cm/day



                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Benzene    2.740E+17   1.110E+00   4.594E+00     7.703E+00     1.311E+01

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Benzene      71432    2.74E+17   6.84E-05  1.11E+00     1.00E+00    4.89E+03



 Benzene  (71432   ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  2.257E+02 cm^2/yr


                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 Benzene   71432     3.851E-02  NUMERICAL    4.884E+03     4.845E+03 ( 99.2%)

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

     Benzene     71432      PEAK        5.025E+01          31.0

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
 Usage Location Name   = all                  

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               STRONTIU         1.000E+04 0.000E+00      1.000E+04
               YTTRIUM-         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          STRONTIU           41
    2          YTTRIUM-           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 1.370E-02 cm/day

 Source-Term Type (ISTYPE)                     =  1
 Source-Term Flux Boundary Conditions (ISOURC) =  2
 Liquid Impoundment Index (IPOND)              =  2
 Source-Term Decay Index (IDECAY)              =  2
 Direct Discharge Surface Water Index (IDDSW)  =  0
 Number of Parent Constituents (NUMCON)        =  2

Source-Term Configuration (ISCONF)        =  4
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  1

Length of Release Unit (AL1 OR CLEN)                =   5.00E+02 cm
Width of Release Unit (B1)                          =   5.00E+02 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   2.66E+05  1.24E+02     1.00E+20    1.41E+13
YTTRIUM-     Y90      7.39E-03   7.68E+06  3.37E-02     1.00E+20    4.04E+14


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     3.33E+11  1.000E+00
        1    3     2.88E+11  7.000E+00        1    4     2.56E+11  1.200E+01
        1    5     2.16E+11  1.900E+01        1    6     2.11E+11  2.000E+01
        1    7     2.01E+11  2.200E+01        1    8     1.87E+11  2.500E+01
        1    9     1.83E+11  2.600E+01        1   10     1.79E+11  2.700E+01
        1   11     1.70E+11  2.900E+01        1   12     1.58E+11  3.200E+01
        1   13     1.51E+11  3.400E+01        1   14     1.41E+11  3.700E+01
        1   15     1.31E+11  4.000E+01
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
        2    3     9.60E+12  2.000E+00        2    4     9.15E+12  4.000E+00
        2    5     7.38E+12  1.300E+01        2    6     7.20E+12  1.400E+01
        2    7     6.55E+12  1.800E+01        2    8     5.95E+12  2.200E+01
        2    9     5.40E+12  2.600E+01        2   10     4.68E+12  3.200E+01
        2   11     4.05E+12  3.800E+01        2   12     3.43E+12  4.500E+01
        2   13     3.35E+12  4.600E+01        2   14     3.27E+12  4.700E+01
        2   15     2.90E+12  5.200E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     3.33E+11  1.000E+00
        1    3     2.88E+11  7.000E+00        1    4     2.56E+11  1.200E+01
        1    5     2.16E+11  1.900E+01        1    6     2.11E+11  2.000E+01
        1    7     2.01E+11  2.200E+01        1    8     1.87E+11  2.500E+01
        1    9     1.83E+11  2.600E+01        1   10     1.79E+11  2.700E+01
        1   11     1.70E+11  2.900E+01        1   12     1.58E+11  3.200E+01
        1   13     1.51E+11  3.400E+01        1   14     1.41E+11  3.700E+01
        1   15     1.31E+11  4.000E+01
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
        2    3     9.60E+12  2.000E+00        2    4     9.15E+12  4.000E+00
        2    5     7.38E+12  1.300E+01        2    6     7.20E+12  1.400E+01
        2    7     6.55E+12  1.800E+01        2    8     5.95E+12  2.200E+01
        2    9     5.40E+12  2.600E+01        2   10     4.68E+12  3.200E+01
        2   11     4.05E+12  3.800E+01        2   12     3.43E+12  4.500E+01
        2   13     3.35E+12  4.600E+01        2   14     3.27E+12  4.700E+01
        2   15     2.90E+12  5.200E+01

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       STRONTIU              1.950E+02
    2       YTTRIUM-              1.960E+02
 Modeling Medium #             1
 Medium Type (MED)          =  1
 Source Configuration       =  4
 Source Term Discharge Type =  1
 PARTIALLY SATURATED ZONE



 Thickness of Unsaturated Layer (H1) =   3.40E+01 cm
 X-Direction Dispersivity (A1)       =   3.00E+00 cm
 Bulk Density (R2)                   =   1.36E+00 g/cm**3
 Total Porosity (AN4)                =   4.86E-01 (fraction)
 Field Capacity (AN5)                =   4.20E-01 (fraction)
 Permeability (X2)                   =   2.25E+00 cm/day
 Darcy Velocity (VLEACH)             =   1.37E-02 cm/day
 Soil-Type Coefficient (A2)          =   1.04E+01



 Moisture Content (AN5)   =  42.00 %
 Pore water velocity (U)  =   3.26E-02 cm/day



                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  STRONTIU   2.900E+01   1.240E+02   4.025E+02     1.357E+02     1.149E+03
  2  YTTRIUM-   7.390E-03   3.370E-02   1.109E+00     1.203E-01     3.165E+00

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   2.66E+05  1.24E+02     1.00E+20    1.41E+13
YTTRIUM-     Y90      7.39E-03   7.68E+06  3.37E-02     1.00E+20    4.04E+14



 STRONTIU (SR90    ): DECAY CONSTANT =  2.390E-02 1/YR
 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR

 Longitudinal Dispersion Coefficient =  2.257E+02 cm^2/yr


                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 STRONTIU  SR90      6.960E-01  NUMERICAL    1.395E+13     1.629E+10 (  **  )
 YTTRIUM-  Y90       6.138E-04  NUMERICAL    4.026E+14     4.668E+04 (  **  )

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

     STRONTIU    SR90       PEAK        1.048E+08         146.5
     YTTRIUM-    Y90        PEAK        1.004E+05          51.6

     (a)
         PEAK: The maximum flux occurs as a peak flux at the given time.


 ******************************************************************************
"aqu8",403
 ****************************VERSION 11/12/2002****************************


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
 Usage Location Name   = riv7                 

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

Medium #  1 Equals Medium Type (MED) #  4

Length of Release Unit (AL1 OR CLEN)                =   5.00E+02 cm
Width of Release Unit (B1)                          =   5.00E+02 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   4.57E-01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Benzene      71432    2.74E+17   4.02E-05  1.00E+00     1.00E+00    4.86E+03


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  2.709E-01        1    2     4.20E+01  1.562E+01
        1    3     5.02E+01  3.097E+01        1    4     4.63E+01  4.632E+01
        1    5     3.93E+01  6.167E+01        1    6     3.21E+01  7.702E+01
        1    7     2.57E+01  9.238E+01        1    8     2.03E+01  1.077E+02
        1    9     1.59E+01  1.231E+02        1   10     1.24E+01  1.384E+02
        1   11     9.61E+00  1.538E+02        1   12     7.44E+00  1.691E+02
        1   13     5.75E+00  1.845E+02        1   14     4.45E+00  1.998E+02
        1   15     2.04E+00  2.152E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  2.709E-01        1    2     4.20E+01  1.562E+01
        1    3     5.02E+01  3.097E+01        1    4     4.63E+01  4.632E+01
        1    5     3.93E+01  6.167E+01        1    6     3.21E+01  7.702E+01
        1    7     2.57E+01  9.238E+01        1    8     2.03E+01  1.077E+02
        1    9     1.59E+01  1.231E+02        1   10     1.24E+01  1.384E+02
        1   11     9.61E+00  1.538E+02        1   12     7.44E+00  1.691E+02
        1   13     5.75E+00  1.845E+02        1   14     4.45E+00  1.998E+02
        1   15     2.04E+00  2.152E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Benzene               6.140E+02
 Modeling Medium #             1
 Medium Type (MED)          =  4
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A RIVER




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Benzene    2.740E+17   1.000E+00   9.200E+00     3.039E-01     3.358E-01

 Darcy Velocity (DARCYV)                         =   1.50E+01 cm/day
 Pore-water Velocity (U)                         =   7.50E+01 cm/day
 Aquifer Thickness (H1)                          =   5.00E+02 cm
 X-Direction Dispersivity (A1)                   =   1.00E+02 cm
 Y-Direction Dispersivity (A2)                   =   3.30E+01 cm
 Z-Direction Dispersivity (A3)                   =   2.50E-01 cm
 Bulk Density (R2)                               =   1.64E+00 g/cm**3
 Total Porosity (AN4)                            =   3.80E-01 (fraction)
 Effective Porosity (AN5)                        =   2.00E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   1.00E+03 cm
 Perpendicular Distance Off Flow Centerline (Y2) =   0.00E+00 cm
 Angle Between Flow Direction and Receptor
 Direction (ANGLE)                               =   0.00E+00 deg
 Depth Below Watertable of Calculated
 Concentration at Receptor (Z1)                  =   0.00E+00 cm
 Length of Burial Area (AL1)                     =   5.00E+02 cm
 Width of Burial Area (B1)                       =   5.00E+02 cm
 Depth of Release Unit Below Water Table (HW)    =   0.00E+00 cm

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Benzene      71432    2.74E+17   4.02E-05  1.00E+00     1.00E+00    4.86E+03



 Benzene  (71432   ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  2.740E+06 cm^2/yr
 Lateral Dispersion Coefficient =       9.042E+05 cm^2/yr
 Vertical Dispersion Coefficient =      7.038E+03 cm^2/yr

 SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

 Width of Plume (ALEN)                      =   8.20E+02 cm
 Depth to top of Plume (ZTOP)               =   0.00E+00 cm
 Depth to bottom of Plume (ZBOT)            =   1.87E+01 cm



                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 Benzene   71432     4.950E-04  NUMERICAL    4.845E+03     4.847E+03 (100.0%)

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

     Benzene     71432      PEAK        5.013E+01          31.1

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
 Usage Location Name   = riv7                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               STRONTIU         1.000E+04 0.000E+00      1.000E+04
               YTTRIUM-         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

  #           Constituent         NUM

    1          STRONTIU           41
    2          YTTRIUM-           41
 Source-Term Type (ISTYPE)                     =  2
 Source-Term Flux Boundary Conditions (ISOURC) =  2
 Liquid Impoundment Index (IPOND)              =  2
 Source-Term Decay Index (IDECAY)              =  2
 Direct Discharge Surface Water Index (IDDSW)  =  0
 Number of Parent Constituents (NUMCON)        =  2

Source-Term Configuration (ISCONF)        =  6
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  4

Length of Release Unit (AL1 OR CLEN)                =   5.00E+02 cm
Width of Release Unit (B1)                          =   5.00E+02 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   4.57E-01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   8.38E+01  2.00E+00     1.00E+20    1.64E+10
YTTRIUM-     Y90      7.39E-03   8.02E-02  3.33E+00     1.00E+20    6.30E+06


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  2.022E+01        1    2     2.45E+05  4.549E+01
        1    3     1.02E+07  7.075E+01        1    4     4.47E+07  9.602E+01
        1    5     8.41E+07  1.213E+02        1    6     1.05E+08  1.466E+02
        1    7     1.04E+08  1.718E+02        1    8     8.85E+07  1.971E+02
        1    9     6.88E+07  2.223E+02        1   10     4.99E+07  2.476E+02
        1   11     3.43E+07  2.729E+02        1   12     2.24E+07  2.981E+02
        1   13     1.40E+07  3.234E+02        1   14     8.37E+06  3.487E+02
        1   15     4.88E+06  3.739E+02
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  3.801E-02
        2    3     2.55E+04  2.581E+01        2    4     1.00E+05  5.158E+01
        2    5     5.43E+04  7.735E+01        2    6     2.94E+04  1.031E+02
        2    7     1.59E+04  1.289E+02        2    8     8.60E+03  1.546E+02
        2    9     4.65E+03  1.804E+02        2   10     2.51E+03  2.062E+02
        2   11     1.36E+03  2.320E+02        2   12     7.33E+02  2.577E+02
        2   13     3.96E+02  2.835E+02        2   14     2.14E+02  3.093E+02
        2   15     1.16E+02  3.350E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  2.022E+01        1    2     2.45E+05  4.549E+01
        1    3     1.02E+07  7.075E+01        1    4     4.47E+07  9.602E+01
        1    5     8.41E+07  1.213E+02        1    6     1.05E+08  1.466E+02
        1    7     1.04E+08  1.718E+02        1    8     8.85E+07  1.971E+02
        1    9     6.88E+07  2.223E+02        1   10     4.99E+07  2.476E+02
        1   11     3.43E+07  2.729E+02        1   12     2.24E+07  2.981E+02
        1   13     1.40E+07  3.234E+02        1   14     8.37E+06  3.487E+02
        1   15     4.88E+06  3.739E+02
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  3.801E-02
        2    3     2.55E+04  2.581E+01        2    4     1.00E+05  5.158E+01
        2    5     5.43E+04  7.735E+01        2    6     2.94E+04  1.031E+02
        2    7     1.59E+04  1.289E+02        2    8     8.60E+03  1.546E+02
        2    9     4.65E+03  1.804E+02        2   10     2.51E+03  2.062E+02
        2   11     1.36E+03  2.320E+02        2   12     7.33E+02  2.577E+02
        2   13     3.96E+02  2.835E+02        2   14     2.14E+02  3.093E+02
        2   15     1.16E+02  3.350E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       STRONTIU              1.011E+03
    2       YTTRIUM-              1.031E+03
 Modeling Medium #             1
 Medium Type (MED)          =  4
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A RIVER




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  STRONTIU   2.900E+01   2.000E+00   1.740E+01     5.733E-01     6.352E-01
  2  YTTRIUM-   7.390E-03   3.330E+00   2.831E+01     1.613E-01     1.033E+00

 Darcy Velocity (DARCYV)                         =   1.50E+01 cm/day
 Pore-water Velocity (U)                         =   7.50E+01 cm/day
 Aquifer Thickness (H1)                          =   5.00E+02 cm
 X-Direction Dispersivity (A1)                   =   1.00E+02 cm
 Y-Direction Dispersivity (A2)                   =   3.30E+01 cm
 Z-Direction Dispersivity (A3)                   =   2.50E-01 cm
 Bulk Density (R2)                               =   1.64E+00 g/cm**3
 Total Porosity (AN4)                            =   3.80E-01 (fraction)
 Effective Porosity (AN5)                        =   2.00E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   1.00E+03 cm
 Perpendicular Distance Off Flow Centerline (Y2) =   0.00E+00 cm
 Angle Between Flow Direction and Receptor
 Direction (ANGLE)                               =   0.00E+00 deg
 Depth Below Watertable of Calculated
 Concentration at Receptor (Z1)                  =   0.00E+00 cm
 Length of Burial Area (AL1)                     =   5.00E+02 cm
 Width of Burial Area (B1)                       =   5.00E+02 cm
 Depth of Release Unit Below Water Table (HW)    =   0.00E+00 cm

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   8.38E+01  2.00E+00     1.00E+20    1.64E+10
YTTRIUM-     Y90      7.39E-03   8.02E-02  3.33E+00     1.00E+20    6.30E+06



 STRONTIU (SR90    ): DECAY CONSTANT =  2.390E-02 1/YR
 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR

 Longitudinal Dispersion Coefficient =  2.740E+06 cm^2/yr
 Lateral Dispersion Coefficient =       9.042E+05 cm^2/yr
 Vertical Dispersion Coefficient =      7.038E+03 cm^2/yr

 SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

 Width of Plume (ALEN)                      =   8.20E+02 cm
 Depth to top of Plume (ZTOP)               =   0.00E+00 cm
 Depth to bottom of Plume (ZBOT)            =   1.87E+01 cm



                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 STRONTIU  SR90      5.673E-04  NUMERICAL    1.629E+10     1.603E+10 (  **  )
 YTTRIUM-  Y90       1.565E-04  NUMERICAL    6.292E+06     6.276E-04 (  **  )

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

     STRONTIU    SR90       PEAK        1.030E+08         147.2
     YTTRIUM-    Y90        SS          6.369E+01          51.8 to    828.6

     (a)
         PEAK: The maximum flux occurs as a peak flux at the given time.

           SS: Fluxes vary by less than 1/100th of one percent of the
               maximum flux over the given time range (steady state).


 ******************************************************************************
"riv7",307                              
 ****************************VERSION 11/12/2002****************************


          RADCON -- WATERBORNE TRANSPORT COMPONENT for the Multimedia
                    Environmental Pollutant Assessment System (MEPAS):
                    Models movement of radionuclides and other chemicals
                    in groundwater, surface water, and overland pathways.

                    Battelle, Pacific Northwest Laboratory (PNL)
                    P.O. Box 999, Richland, WA 99352

                    Developed for the U.S. Department of Energy


            Input file for this run is MEPAS_RV.WIN
 ******************************************************************************
          Modeling Parent Contaminant Number   1 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp6                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Benzene          1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Benzene            41
Length of Surface Water Source Term (AL1)  =  8.200E+02 cm
 Source-Term Type (ISTYPE)                     =  3
 Source-Term Flux Boundary Conditions (ISOURC) =  2
 Liquid Impoundment Index (IPOND)              =  2
 Source-Term Decay Index (IDECAY)              =  2
 Direct Discharge Surface Water Index (IDDSW)  =  1
 Number of Parent Constituents (NUMCON)        =  1

Source-Term Configuration (ISCONF)        =  0
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  5
Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Benzene      71432    2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    4.86E+03


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  2.950E-01        1    2     4.13E+01  1.571E+01
        1    3     5.01E+01  3.113E+01        1    4     4.64E+01  4.654E+01
        1    5     3.93E+01  6.196E+01        1    6     3.21E+01  7.737E+01
        1    7     2.57E+01  9.279E+01        1    8     2.02E+01  1.082E+02
        1    9     1.59E+01  1.236E+02        1   10     1.23E+01  1.390E+02
        1   11     9.57E+00  1.544E+02        1   12     7.40E+00  1.699E+02
        1   13     5.72E+00  1.853E+02        1   14     4.37E+00  2.007E+02
        1   15     2.00E+00  2.161E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  2.950E-01        1    2     4.13E+01  1.571E+01
        1    3     5.01E+01  3.113E+01        1    4     4.64E+01  4.654E+01
        1    5     3.93E+01  6.196E+01        1    6     3.21E+01  7.737E+01
        1    7     2.57E+01  9.279E+01        1    8     2.02E+01  1.082E+02
        1    9     1.59E+01  1.236E+02        1   10     1.23E+01  1.390E+02
        1   11     9.57E+00  1.544E+02        1   12     7.40E+00  1.699E+02
        1   13     5.72E+00  1.853E+02        1   14     4.37E+00  2.007E+02
        1   15     2.00E+00  2.161E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Benzene               6.166E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   7.31E+03 cm/year
 Depth of Flow (H1)                             =   1.00E+01 cm
 Width of River (B1)                            =   1.00E+01 cm
 Downstream Discharge at First Receptor (QNEW)  =   7.31E+05 cm**3/year
 Distance Downstream from Center of Source (X3) =   2.00E+03 cm
 Lateral Dispersion Coefficient (E2)            =   4.38E+03 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Benzene      71432    2.74E+17   1.00E+00 -9.90E+01     1.00E+00    4.86E+03



 Benzene  (71432   ): DECAY CONSTANT =  2.530E-18 1/YR
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

     Benzene     71432      PEAK        6.863E-05          31.1

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
 Usage Location Medium =  3
 Usage Location Name   = exp6                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               STRONTIU         1.000E+04 0.000E+00      1.000E+04
               YTTRIUM-         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          STRONTIU           41
    2          YTTRIUM-           13
Length of Surface Water Source Term (AL1)  =  8.200E+02 cm
 Source-Term Type (ISTYPE)                     =  3
 Source-Term Flux Boundary Conditions (ISOURC) =  2
 Liquid Impoundment Index (IPOND)              =  2
 Source-Term Decay Index (IDECAY)              =  2
 Direct Discharge Surface Water Index (IDDSW)  =  1
 Number of Parent Constituents (NUMCON)        =  2

Source-Term Configuration (ISCONF)        =  0
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  5
Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01  -9.99E+01 -9.90E+01     1.00E+20    1.61E+10
YTTRIUM-     Y90      7.39E-03  -9.99E+01 -9.90E+01     1.00E+20    5.34E+04


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  2.027E+01        1    2     2.36E+05  4.565E+01
        1    3     9.85E+06  7.104E+01        1    4     4.36E+07  9.643E+01
        1    5     8.24E+07  1.218E+02        1    6     1.03E+08  1.472E+02
        1    7     1.02E+08  1.726E+02        1    8     8.69E+07  1.980E+02
        1    9     6.74E+07  2.234E+02        1   10     4.88E+07  2.487E+02
        1   11     3.35E+07  2.741E+02        1   12     2.18E+07  2.995E+02
        1   13     1.36E+07  3.249E+02        1   14     8.11E+06  3.503E+02
        1   15     4.72E+06  3.757E+02
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  3.894E-02
        2    3     1.42E+01  2.593E+01        2    4     6.37E+01  5.182E+01
        2    5     6.37E+01  8.286E+02        2    6     3.94E+01  8.545E+02
        2    7     1.86E+01  8.803E+02        2    8     8.18E+00  9.062E+02
        2    9     3.45E+00  9.321E+02        2   10     1.35E+00  9.580E+02
        2   11     4.66E-01  9.839E+02        2   12     1.14E-01  1.010E+03
        2   13     0.00E+00  1.036E+03

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  2.027E+01        1    2     2.36E+05  4.565E+01
        1    3     9.85E+06  7.104E+01        1    4     4.36E+07  9.643E+01
        1    5     8.24E+07  1.218E+02        1    6     1.03E+08  1.472E+02
        1    7     1.02E+08  1.726E+02        1    8     8.69E+07  1.980E+02
        1    9     6.74E+07  2.234E+02        1   10     4.88E+07  2.487E+02
        1   11     3.35E+07  2.741E+02        1   12     2.18E+07  2.995E+02
        1   13     1.36E+07  3.249E+02        1   14     8.11E+06  3.503E+02
        1   15     4.72E+06  3.757E+02
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  3.894E-02
        2    3     1.42E+01  2.593E+01        2    4     6.37E+01  5.182E+01
        2    5     6.37E+01  8.286E+02        2    6     3.94E+01  8.545E+02
        2    7     1.86E+01  8.803E+02        2    8     8.18E+00  9.062E+02
        2    9     3.45E+00  9.321E+02        2   10     1.35E+00  9.580E+02
        2   11     4.66E-01  9.839E+02        2   12     1.14E-01  1.010E+03
        2   13     0.00E+00  1.036E+03

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       STRONTIU              1.015E+03
    2       YTTRIUM-              1.036E+03
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   7.31E+03 cm/year
 Depth of Flow (H1)                             =   1.00E+01 cm
 Width of River (B1)                            =   1.00E+01 cm
 Downstream Discharge at First Receptor (QNEW)  =   7.31E+05 cm**3/year
 Distance Downstream from Center of Source (X3) =   2.00E+03 cm
 Lateral Dispersion Coefficient (E2)            =   4.38E+03 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   1.00E+30 -9.90E+01     1.00E+20    1.61E+10
YTTRIUM-     Y90      7.39E-03   1.00E+30 -9.90E+01     1.00E+20    5.34E+04



 STRONTIU (SR90    ): DECAY CONSTANT =  2.390E-02 1/YR
 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for STRONTIU when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for YTTRIUM- when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     STRONTIU    SR90       PEAK        1.400E+02         147.2
     YTTRIUM-    Y90        PEAK        1.395E+02         155.4

     (a)
         PEAK: The maximum concentration occurs as a peak concentration
               at the given time.


 ******************************************************************************
