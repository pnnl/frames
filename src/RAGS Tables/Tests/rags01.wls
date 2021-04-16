"vad3",531
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

               Antimony         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Antimony           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 8.320E-03 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Antimony     7440360  2.74E+17   4.44E-07  2.00E+00     1.00E+00    7.45E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     1.35E+00  1.000E+00
        1    3     1.31E+00  3.000E+00        1    4     1.30E+00  4.000E+00
        1    5     1.21E+00  9.000E+00        1    6     1.20E+00  1.000E+01
        1    7     1.15E+00  1.300E+01        1    8     1.09E+00  1.700E+01
        1    9     1.06E+00  1.900E+01        1   10     1.03E+00  2.100E+01
        1   11     1.02E+00  2.200E+01        1   12     9.64E-01  2.600E+01
        1   13     9.25E-01  2.900E+01        1   14     8.77E-01  3.300E+01
        1   15     8.42E-01  3.600E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     1.35E+00  1.000E+00
        1    3     1.31E+00  3.000E+00        1    4     1.30E+00  4.000E+00
        1    5     1.21E+00  9.000E+00        1    6     1.20E+00  1.000E+01
        1    7     1.15E+00  1.300E+01        1    8     1.09E+00  1.700E+01
        1    9     1.06E+00  1.900E+01        1   10     1.03E+00  2.100E+01
        1   11     1.02E+00  2.200E+01        1   12     9.64E-01  2.600E+01
        1   13     9.25E-01  2.900E+01        1   14     8.77E-01  3.300E+01
        1   15     8.42E-01  3.600E+01

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Antimony              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  1
 Source Configuration       =  4
 Source Term Discharge Type =  1
 PARTIALLY SATURATED ZONE



 Thickness of Unsaturated Layer (H1) =   2.13E+02 cm
 X-Direction Dispersivity (A1)       =   2.13E+00 cm
 Bulk Density (R2)                   =   1.64E+00 g/cm**3
 Total Porosity (AN4)                =   3.80E-01 (fraction)
 Field Capacity (AN5)                =   9.00E-02 (fraction)
 Permeability (X2)                   =   5.70E+02 cm/day
 Darcy Velocity (VLEACH)             =   8.32E-03 cm/day
 Soil-Type Coefficient (A2)          =   4.05E+00



 Moisture Content (AN5)   =  13.94 %
 Pore water velocity (U)  =   5.97E-02 cm/day



                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Antimony   2.740E+17   2.000E+00   2.454E+01     2.278E+02     2.397E+02

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Antimony     7440360  2.74E+17   4.45E-07  2.00E+00     1.00E+00    7.45E+01



 Antimony (7440360 ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  2.364E+02 cm^2/yr


                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 Antimony  7440360   2.278E+00  NUMERICAL    7.442E+01     7.442E+01 (100.0%)

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

     Antimony    7440360    PEAK        3.772E-01         254.9

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

Known Darcian Infiltration Rate from Site (VLEACH)   = 8.320E-03 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   4.44E+05  2.43E+01     1.00E+20    3.51E+13
YTTRIUM-     Y90      7.39E-03   4.36E+05  2.28E+02     1.00E+20    3.45E+13


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     1.35E+12  1.000E+00
        1    3     1.30E+12  2.000E+00        1    4     1.25E+12  3.000E+00
        1    5     1.16E+12  5.000E+00        1    6     1.00E+12  9.000E+00
        1    7     9.29E+11  1.100E+01        1    8     8.95E+11  1.200E+01
        1    9     8.62E+11  1.300E+01        1   10     7.71E+11  1.600E+01
        1   11     7.42E+11  1.700E+01        1   12     6.64E+11  2.000E+01
        1   13     5.93E+11  2.300E+01        1   14     5.51E+11  2.500E+01
        1   15     5.30E+11  2.600E+01
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
        2    3     1.33E+12  2.000E+00        2    4     1.28E+12  3.000E+00
        2    5     1.18E+12  5.000E+00        2    6     1.06E+12  8.000E+00
        2    7     9.83E+11  1.000E+01        2    8     9.47E+11  1.100E+01
        2    9     9.12E+11  1.200E+01        2   10     8.78E+11  1.300E+01
        2   11     8.46E+11  1.400E+01        2   12     7.29E+11  1.800E+01
        2   13     6.76E+11  2.000E+01        2   14     6.04E+11  2.300E+01
        2   15     5.61E+11  2.500E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     1.35E+12  1.000E+00
        1    3     1.30E+12  2.000E+00        1    4     1.25E+12  3.000E+00
        1    5     1.16E+12  5.000E+00        1    6     1.00E+12  9.000E+00
        1    7     9.29E+11  1.100E+01        1    8     8.95E+11  1.200E+01
        1    9     8.62E+11  1.300E+01        1   10     7.71E+11  1.600E+01
        1   11     7.42E+11  1.700E+01        1   12     6.64E+11  2.000E+01
        1   13     5.93E+11  2.300E+01        1   14     5.51E+11  2.500E+01
        1   15     5.30E+11  2.600E+01
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
        2    3     1.33E+12  2.000E+00        2    4     1.28E+12  3.000E+00
        2    5     1.18E+12  5.000E+00        2    6     1.06E+12  8.000E+00
        2    7     9.83E+11  1.000E+01        2    8     9.47E+11  1.100E+01
        2    9     9.12E+11  1.200E+01        2   10     8.78E+11  1.300E+01
        2   11     8.46E+11  1.400E+01        2   12     7.29E+11  1.800E+01
        2   13     6.76E+11  2.000E+01        2   14     6.04E+11  2.300E+01
        2   15     5.61E+11  2.500E+01

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       STRONTIU              8.300E+01
    2       YTTRIUM-              8.400E+01
 Modeling Medium #             1
 Medium Type (MED)          =  1
 Source Configuration       =  4
 Source Term Discharge Type =  1
 PARTIALLY SATURATED ZONE



 Thickness of Unsaturated Layer (H1) =   2.13E+02 cm
 X-Direction Dispersivity (A1)       =   2.13E+00 cm
 Bulk Density (R2)                   =   1.64E+00 g/cm**3
 Total Porosity (AN4)                =   3.80E-01 (fraction)
 Field Capacity (AN5)                =   9.00E-02 (fraction)
 Permeability (X2)                   =   5.70E+02 cm/day
 Darcy Velocity (VLEACH)             =   8.32E-03 cm/day
 Soil-Type Coefficient (A2)          =   4.05E+00



 Moisture Content (AN5)   =  13.94 %
 Pore water velocity (U)  =   5.97E-02 cm/day



                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  STRONTIU   2.900E+01   2.430E+01   2.870E+02     7.228E+02     2.803E+03
  2  YTTRIUM-   7.390E-03   2.280E+02   2.684E+03     3.705E+01     2.622E+04

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   4.45E+05  2.43E+01     1.00E+20    3.51E+13
YTTRIUM-     Y90      7.39E-03   4.36E+05  2.28E+02     1.00E+20    3.45E+13



 STRONTIU (SR90    ): DECAY CONSTANT =  2.390E-02 1/YR
 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR

 Longitudinal Dispersion Coefficient =  2.364E+02 cm^2/yr


                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 STRONTIU  SR90      8.709E+00  NUMERICAL    3.505E+13     1.939E+01 (  **  )
 YTTRIUM-  Y90       4.410E-01  NUMERICAL    3.439E+13     9.807E-38 (  **  )

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

     STRONTIU    SR90       PEAK        6.447E-02         751.0
     YTTRIUM-    Y90        PEAK        1.981E-02         756.4

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
 Usage Location Name   = all                  

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Trichlor         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Trichlor           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 8.320E-03 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    2.74E+17   3.13E-07  1.07E-01     1.00E+00    6.15E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     9.50E-01  1.000E+00
        1    3     9.32E-01  3.000E+00        1    4     9.06E-01  6.000E+00
        1    5     8.72E-01  1.000E+01        1    6     8.56E-01  1.200E+01
        1    7     8.32E-01  1.500E+01        1    8     8.08E-01  1.800E+01
        1    9     7.93E-01  2.000E+01        1   10     7.78E-01  2.200E+01
        1   11     7.63E-01  2.400E+01        1   12     7.35E-01  2.800E+01
        1   13     7.28E-01  2.900E+01        1   14     7.21E-01  3.000E+01
        1   15     7.01E-01  3.300E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  0.000E+00        1    2     9.50E-01  1.000E+00
        1    3     9.32E-01  3.000E+00        1    4     9.06E-01  6.000E+00
        1    5     8.72E-01  1.000E+01        1    6     8.56E-01  1.200E+01
        1    7     8.32E-01  1.500E+01        1    8     8.08E-01  1.800E+01
        1    9     7.93E-01  2.000E+01        1   10     7.78E-01  2.200E+01
        1   11     7.63E-01  2.400E+01        1   12     7.35E-01  2.800E+01
        1   13     7.28E-01  2.900E+01        1   14     7.21E-01  3.000E+01
        1   15     7.01E-01  3.300E+01

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Trichlor              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  1
 Source Configuration       =  4
 Source Term Discharge Type =  1
 PARTIALLY SATURATED ZONE



 Thickness of Unsaturated Layer (H1) =   2.13E+02 cm
 X-Direction Dispersivity (A1)       =   2.13E+00 cm
 Bulk Density (R2)                   =   1.64E+00 g/cm**3
 Total Porosity (AN4)                =   3.80E-01 (fraction)
 Field Capacity (AN5)                =   9.00E-02 (fraction)
 Permeability (X2)                   =   5.70E+02 cm/day
 Darcy Velocity (VLEACH)             =   8.32E-03 cm/day
 Soil-Type Coefficient (A2)          =   4.05E+00



 Moisture Content (AN5)   =  13.94 %
 Pore water velocity (U)  =   5.97E-02 cm/day



                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Trichlor   2.740E+17   1.070E-01   2.259E+00     2.097E+01     2.207E+01

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    2.74E+17   3.13E-07  1.07E-01     1.00E+00    6.15E+01



 Trichlor (79016   ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  2.364E+02 cm^2/yr


                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 Trichlor  79016     2.097E-01  NUMERICAL    6.143E+01     6.143E+01 (100.0%)

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

     Trichlor    79016      PEAK        7.881E-01          39.4

     (a)
         PEAK: The maximum flux occurs as a peak flux at the given time.


 ******************************************************************************
"vad8",527
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

               Antimony         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Antimony           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 8.320E-03 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Antimony     7440360  2.74E+17   1.24E-07  2.00E+00     1.00E+00    7.45E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  4.353E+01        1    2     5.29E-05  7.372E+01
        1    3     4.07E-03  1.039E+02        1    4     3.59E-02  1.341E+02
        1    5     1.19E-01  1.643E+02        1    6     2.35E-01  1.945E+02
        1    7     3.34E-01  2.247E+02        1    8     3.77E-01  2.549E+02
        1    9     3.59E-01  2.851E+02        1   10     3.02E-01  3.153E+02
        1   11     2.31E-01  3.455E+02        1   12     1.65E-01  3.757E+02
        1   13     1.12E-01  4.059E+02        1   14     7.31E-02  4.360E+02
        1   15     4.62E-02  4.662E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  4.353E+01        1    2     5.29E-05  7.372E+01
        1    3     4.07E-03  1.039E+02        1    4     3.59E-02  1.341E+02
        1    5     1.19E-01  1.643E+02        1    6     2.35E-01  1.945E+02
        1    7     3.34E-01  2.247E+02        1    8     3.77E-01  2.549E+02
        1    9     3.59E-01  2.851E+02        1   10     3.02E-01  3.153E+02
        1   11     2.31E-01  3.455E+02        1   12     1.65E-01  3.757E+02
        1   13     1.12E-01  4.059E+02        1   14     7.31E-02  4.360E+02
        1   15     4.62E-02  4.662E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Antimony              1.208E+03
 Modeling Medium #             1
 Medium Type (MED)          =  1
 Source Configuration       =  4
 Source Term Discharge Type =  1
 PARTIALLY SATURATED ZONE



 Thickness of Unsaturated Layer (H1) =   1.52E+01 cm
 X-Direction Dispersivity (A1)       =   0.00E+00 cm
 Bulk Density (R2)                   =   1.64E+00 g/cm**3
 Total Porosity (AN4)                =   3.80E-01 (fraction)
 Field Capacity (AN5)                =   9.00E-02 (fraction)
 Permeability (X2)                   =   5.70E+02 cm/day
 Darcy Velocity (VLEACH)             =   8.32E-03 cm/day
 Soil-Type Coefficient (A2)          =   4.00E+00



 Moisture Content (AN5)   =  13.81 %
 Pore water velocity (U)  =   6.02E-02 cm/day



                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Antimony   2.740E+17   2.000E+00   2.475E+01     9.951E+00     1.710E+01

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Antimony     7440360  2.74E+17   1.24E-07  2.00E+00     1.00E+00    7.45E+01



 Antimony (7440360 ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  1.900E+02 cm^2/yr


                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 Antimony  7440360   8.239E-03  NUMERICAL    7.442E+01     7.443E+01 (100.0%)

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

     Antimony    7440360    PEAK        3.411E-01         307.4

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
    2          YTTRIUM-           12

Known Darcian Infiltration Rate from Site (VLEACH)   = 8.320E-03 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   2.12E-08  2.43E+01     1.00E+06    1.95E+01
YTTRIUM-     Y90      7.39E-03   6.52E-09  2.28E+02     1.00E+06    6.24E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  2.897E+02        1    2     1.42E-07  3.282E+02
        1    3     4.58E-06  3.666E+02        1    4     6.33E-05  4.050E+02
        1    5     4.67E-04  4.435E+02        1    6     2.14E-03  4.819E+02
        1    7     6.77E-03  5.203E+02        1    8     1.59E-02  5.588E+02
        1    9     2.94E-02  5.972E+02        1   10     4.45E-02  6.357E+02
        1   11     5.74E-02  6.741E+02        1   12     6.44E-02  7.125E+02
        1   13     6.45E-02  7.510E+02        1   14     5.83E-02  7.894E+02
        1   15     4.84E-02  8.279E+02
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.151E-05
        2    3     9.97E-42  2.521E+02        2    4     4.83E-03  5.043E+02
        2    5     1.98E-02  7.564E+02        2    6     5.06E-05  1.009E+03
        2    7     1.19E-07  1.261E+03        2    8     2.89E-10  1.513E+03
        2    9     7.07E-13  1.765E+03        2   10     9.97E-42  2.017E+03
        2   11     9.97E-42  9.833E+03        2   12     0.00E+00  1.009E+04

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  2.897E+02        1    2     1.42E-07  3.282E+02
        1    3     4.58E-06  3.666E+02        1    4     6.33E-05  4.050E+02
        1    5     4.67E-04  4.435E+02        1    6     2.14E-03  4.819E+02
        1    7     6.77E-03  5.203E+02        1    8     1.59E-02  5.588E+02
        1    9     2.94E-02  5.972E+02        1   10     4.45E-02  6.357E+02
        1   11     5.74E-02  6.741E+02        1   12     6.44E-02  7.125E+02
        1   13     6.45E-02  7.510E+02        1   14     5.83E-02  7.894E+02
        1   15     4.84E-02  8.279E+02
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.151E-05
        2    3     9.97E-42  2.521E+02        2    4     4.83E-03  5.043E+02
        2    5     1.98E-02  7.564E+02        2    6     5.06E-05  1.009E+03
        2    7     1.19E-07  1.261E+03        2    8     2.89E-10  1.513E+03
        2    9     7.07E-13  1.765E+03        2   10     9.97E-42  2.017E+03
        2   11     9.97E-42  9.833E+03        2   12     0.00E+00  1.009E+04

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       STRONTIU              1.538E+03
    2       YTTRIUM-              1.009E+04
 Modeling Medium #             1
 Medium Type (MED)          =  1
 Source Configuration       =  4
 Source Term Discharge Type =  1
 PARTIALLY SATURATED ZONE



 Thickness of Unsaturated Layer (H1) =   1.52E+01 cm
 X-Direction Dispersivity (A1)       =   0.00E+00 cm
 Bulk Density (R2)                   =   1.64E+00 g/cm**3
 Total Porosity (AN4)                =   3.80E-01 (fraction)
 Field Capacity (AN5)                =   9.00E-02 (fraction)
 Permeability (X2)                   =   5.70E+02 cm/day
 Darcy Velocity (VLEACH)             =   8.32E-03 cm/day
 Soil-Type Coefficient (A2)          =   4.00E+00



 Moisture Content (AN5)   =  13.81 %
 Pore water velocity (U)  =   6.02E-02 cm/day



                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  STRONTIU   2.900E+01   2.430E+01   2.896E+02     4.928E+01     2.000E+02
  2  YTTRIUM-   7.390E-03   2.280E+02   2.709E+03     2.960E+00     1.871E+03

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   2.12E-08  2.43E+01     1.00E+06    1.95E+01
YTTRIUM-     Y90      7.39E-03   6.52E-09  2.28E+02     1.00E+06    6.24E+00



 STRONTIU (SR90    ): DECAY CONSTANT =  2.390E-02 1/YR
 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR

 Longitudinal Dispersion Coefficient =  1.900E+02 cm^2/yr


                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 STRONTIU  SR90      3.205E-02  NUMERICAL    1.939E+01     1.455E+00 (  **  )
 YTTRIUM-  Y90       2.935E-04  NUMERICAL    6.225E+00     1.226E-35 (  **  )

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

     STRONTIU    SR90       PEAK        4.619E-03         780.3
     YTTRIUM-    Y90        PEAK        6.272E-13         502.1

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
 Usage Location Name   = all                  

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Trichlor         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Trichlor           41

Known Darcian Infiltration Rate from Site (VLEACH)   = 8.320E-03 cm/day

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

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    2.74E+17   2.59E-07  1.00E-01     1.00E+00    6.15E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  4.008E+00        1    2     1.04E-03  9.057E+00
        1    3     5.95E-02  1.411E+01        1    4     2.77E-01  1.916E+01
        1    5     5.34E-01  2.421E+01        1    6     7.03E-01  2.926E+01
        1    7     7.75E-01  3.431E+01        1    8     7.88E-01  3.936E+01
        1    9     7.72E-01  4.441E+01        1   10     7.44E-01  4.946E+01
        1   11     7.12E-01  5.451E+01        1   12     6.80E-01  5.956E+01
        1   13     6.49E-01  6.461E+01        1   14     6.18E-01  6.966E+01
        1   15     5.89E-01  7.471E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  4.008E+00        1    2     1.04E-03  9.057E+00
        1    3     5.95E-02  1.411E+01        1    4     2.77E-01  1.916E+01
        1    5     5.34E-01  2.421E+01        1    6     7.03E-01  2.926E+01
        1    7     7.75E-01  3.431E+01        1    8     7.88E-01  3.936E+01
        1    9     7.72E-01  4.441E+01        1   10     7.44E-01  4.946E+01
        1   11     7.12E-01  5.451E+01        1   12     6.80E-01  5.956E+01
        1   13     6.49E-01  6.461E+01        1   14     6.18E-01  6.966E+01
        1   15     5.89E-01  7.471E+01

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Trichlor              2.020E+02
 Modeling Medium #             1
 Medium Type (MED)          =  1
 Source Configuration       =  4
 Source Term Discharge Type =  1
 PARTIALLY SATURATED ZONE



 Thickness of Unsaturated Layer (H1) =   1.52E+01 cm
 X-Direction Dispersivity (A1)       =   0.00E+00 cm
 Bulk Density (R2)                   =   1.64E+00 g/cm**3
 Total Porosity (AN4)                =   3.80E-01 (fraction)
 Field Capacity (AN5)                =   9.00E-02 (fraction)
 Permeability (X2)                   =   5.70E+02 cm/day
 Darcy Velocity (VLEACH)             =   8.32E-03 cm/day
 Soil-Type Coefficient (A2)          =   4.00E+00



 Moisture Content (AN5)   =  13.81 %
 Pore water velocity (U)  =   6.02E-02 cm/day



                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Trichlor   2.740E+17   1.000E-01   2.188E+00     8.795E-01     1.511E+00

                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    2.74E+17   2.60E-07  1.00E-01     1.00E+00    6.15E+01



 Trichlor (79016   ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  1.900E+02 cm^2/yr


                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 Trichlor  79016     4.354E-03  NUMERICAL    6.143E+01     6.144E+01 (100.0%)

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

     Trichlor    79016      PEAK        7.809E-01          41.6

     (a)
         PEAK: The maximum flux occurs as a peak flux at the given time.


 ******************************************************************************
"aqu4",1144
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
 Usage Location Name   = riv10                

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Antimony         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

  #           Constituent         NUM

    1          Antimony           41
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

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   1.61E+01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Antimony     7440360  2.74E+17   1.12E-07  6.00E+00     1.00E+00    7.45E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  4.387E+01        1    2     3.98E-04  8.779E+01
        1    3     1.44E-02  1.317E+02        1    4     9.11E-02  1.756E+02
        1    5     2.31E-01  2.196E+02        1    6     3.38E-01  2.635E+02
        1    7     3.41E-01  3.074E+02        1    8     2.70E-01  3.513E+02
        1    9     1.81E-01  3.952E+02        1   10     1.08E-01  4.391E+02
        1   11     5.95E-02  4.831E+02        1   12     3.08E-02  5.270E+02
        1   13     1.52E-02  5.709E+02        1   14     7.28E-03  6.148E+02
        1   15     3.37E-03  6.587E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  4.387E+01        1    2     3.98E-04  8.779E+01
        1    3     1.44E-02  1.317E+02        1    4     9.11E-02  1.756E+02
        1    5     2.31E-01  2.196E+02        1    6     3.38E-01  2.635E+02
        1    7     3.41E-01  3.074E+02        1    8     2.70E-01  3.513E+02
        1    9     1.81E-01  3.952E+02        1   10     1.08E-01  4.391E+02
        1   11     5.95E-02  4.831E+02        1   12     3.08E-02  5.270E+02
        1   13     1.52E-02  5.709E+02        1   14     7.28E-03  6.148E+02
        1   15     3.37E-03  6.587E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Antimony              1.757E+03
 Modeling Medium #             1
 Medium Type (MED)          =  4
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A RIVER




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Antimony   2.740E+17   6.000E+00   4.390E+01     4.200E+02     4.641E+02

 Darcy Velocity (DARCYV)                         =   5.18E-01 cm/day
 Pore-water Velocity (U)                         =   2.59E+00 cm/day
 Aquifer Thickness (H1)                          =   1.22E+03 cm
 X-Direction Dispersivity (A1)                   =   1.00E+03 cm
 Y-Direction Dispersivity (A2)                   =   3.30E+02 cm
 Z-Direction Dispersivity (A3)                   =   2.50E+00 cm
 Bulk Density (R2)                               =   1.43E+00 g/cm**3
 Total Porosity (AN4)                            =   4.60E-01 (fraction)
 Effective Porosity (AN5)                        =   2.00E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   1.00E+04 cm
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

Antimony     7440360  2.74E+17   1.12E-07  6.00E+00     1.00E+00    7.45E+01



 Antimony (7440360 ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  9.462E+05 cm^2/yr
 Lateral Dispersion Coefficient =       3.124E+05 cm^2/yr
 Vertical Dispersion Coefficient =      2.555E+03 cm^2/yr

 SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

 Width of Plume (ALEN)                      =   5.55E+03 cm
 Depth to top of Plume (ZTOP)               =   0.00E+00 cm
 Depth to bottom of Plume (ZBOT)            =   3.31E+02 cm



                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 Antimony  7440360   2.390E-01  NUMERICAL    7.443E+01     7.443E+01 (100.0%)

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

     Antimony    7440360    PEAK        1.363E-01         749.5

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
 Usage Location Name   = riv10                

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
    2          YTTRIUM-           35
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

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   1.61E+01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   1.52E-09  1.00E+02     1.00E+06    1.46E+00
YTTRIUM-     Y90      7.39E-03   2.06E-19  5.38E+02     1.00E+06    1.22E-09


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  2.936E+02        1    2     3.83E-09  3.477E+02
        1    3     3.96E-07  4.018E+02        1    4     9.09E-06  4.558E+02
        1    5     8.52E-05  5.099E+02        1    6     4.29E-04  5.640E+02
        1    7     1.29E-03  6.181E+02        1    8     2.63E-03  6.722E+02
        1    9     3.94E-03  7.262E+02        1   10     4.62E-03  7.803E+02
        1   11     4.41E-03  8.344E+02        1   12     3.57E-03  8.885E+02
        1   13     2.52E-03  9.426E+02        1   14     1.59E-03  9.967E+02
        1   15     9.04E-04  1.051E+03
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.010E-05
        2    3     6.27E-13  5.021E+02        2    4     5.32E-13  1.004E+03
        2    5     5.32E-13  2.009E+03        2    6     3.05E-40  2.511E+03
        2    7     3.66E-40  3.013E+03        2    8     4.27E-40  3.515E+03
        2    9     5.50E-40  4.519E+03        2   10     6.11E-40  5.022E+03
        2   11     7.33E-40  6.026E+03        2   12     7.94E-40  6.528E+03
        2   13     8.55E-40  7.030E+03        2   14     9.16E-40  7.532E+03
        2   15     9.77E-40  8.034E+03

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  2.936E+02        1    2     3.83E-09  3.477E+02
        1    3     3.96E-07  4.018E+02        1    4     9.09E-06  4.558E+02
        1    5     8.52E-05  5.099E+02        1    6     4.29E-04  5.640E+02
        1    7     1.29E-03  6.181E+02        1    8     2.63E-03  6.722E+02
        1    9     3.94E-03  7.262E+02        1   10     4.62E-03  7.803E+02
        1   11     4.41E-03  8.344E+02        1   12     3.57E-03  8.885E+02
        1   13     2.52E-03  9.426E+02        1   14     1.59E-03  9.967E+02
        1   15     9.04E-04  1.051E+03
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.010E-05
        2    3     6.27E-13  5.021E+02        2    4     5.32E-13  1.004E+03
        2    5     5.32E-13  2.009E+03        2    6     3.05E-40  2.511E+03
        2    7     3.66E-40  3.013E+03        2    8     4.27E-40  3.515E+03
        2    9     5.50E-40  4.519E+03        2   10     6.11E-40  5.022E+03
        2   11     7.33E-40  6.026E+03        2   12     7.94E-40  6.528E+03
        2   13     8.55E-40  7.030E+03        2   14     9.16E-40  7.532E+03
        2   15     9.77E-40  8.034E+03

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       STRONTIU              2.163E+03
    2       YTTRIUM-              2.009E+04
 Modeling Medium #             1
 Medium Type (MED)          =  4
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A RIVER




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  STRONTIU   2.900E+01   1.000E+02   7.160E+02     8.733E+02     7.569E+03
  2  YTTRIUM-   7.390E-03   5.380E+02   3.848E+03     3.292E+01     4.067E+04

 Darcy Velocity (DARCYV)                         =   5.18E-01 cm/day
 Pore-water Velocity (U)                         =   2.59E+00 cm/day
 Aquifer Thickness (H1)                          =   1.22E+03 cm
 X-Direction Dispersivity (A1)                   =   1.00E+03 cm
 Y-Direction Dispersivity (A2)                   =   3.30E+02 cm
 Z-Direction Dispersivity (A3)                   =   2.50E+00 cm
 Bulk Density (R2)                               =   1.43E+00 g/cm**3
 Total Porosity (AN4)                            =   4.60E-01 (fraction)
 Effective Porosity (AN5)                        =   2.00E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   1.00E+04 cm
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

STRONTIU     SR90     2.90E+01   1.52E-09  1.00E+02     1.00E+06    1.46E+00
YTTRIUM-     Y90      7.39E-03   2.06E-19  5.38E+02     1.00E+06    1.22E-09



 STRONTIU (SR90    ): DECAY CONSTANT =  2.390E-02 1/YR
 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR

 Longitudinal Dispersion Coefficient =  9.462E+05 cm^2/yr
 Lateral Dispersion Coefficient =       3.124E+05 cm^2/yr
 Vertical Dispersion Coefficient =      2.555E+03 cm^2/yr

 SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

 Width of Plume (ALEN)                      =   5.55E+03 cm
 Depth to top of Plume (ZTOP)               =   0.00E+00 cm
 Depth to bottom of Plume (ZBOT)            =   3.31E+02 cm



                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 STRONTIU  SR90      4.037E-01  NUMERICAL    1.455E+00     5.218E-17 (  **  )
 YTTRIUM-  Y90       1.639E-03  NUMERICAL    1.116E-09     0.000E+00 (  **  )

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

     STRONTIU    SR90       PEAK        1.110E-19        1685.5
     YTTRIUM-    Y90        PEAK        1.205E-21        1212.3

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
 Usage Location Name   = riv10                

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Trichlor         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

  #           Constituent         NUM

    1          Trichlor           41
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

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   1.61E+01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    2.74E+17   2.57E-07  3.82E-01     1.00E+00    6.15E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  4.038E+00        1    2     3.58E-03  1.030E+01
        1    3     9.14E-02  1.656E+01        1    4     3.50E-01  2.283E+01
        1    5     6.13E-01  2.909E+01        1    6     7.49E-01  3.535E+01
        1    7     7.81E-01  4.162E+01        1    8     7.64E-01  4.788E+01
        1    9     7.29E-01  5.414E+01        1   10     6.90E-01  6.040E+01
        1   11     6.51E-01  6.667E+01        1   12     6.13E-01  7.293E+01
        1   13     5.78E-01  7.919E+01        1   14     5.45E-01  8.546E+01
        1   15     5.13E-01  9.172E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  4.038E+00        1    2     3.58E-03  1.030E+01
        1    3     9.14E-02  1.656E+01        1    4     3.50E-01  2.283E+01
        1    5     6.13E-01  2.909E+01        1    6     7.49E-01  3.535E+01
        1    7     7.81E-01  4.162E+01        1    8     7.64E-01  4.788E+01
        1    9     7.29E-01  5.414E+01        1   10     6.90E-01  6.040E+01
        1   11     6.51E-01  6.667E+01        1   12     6.13E-01  7.293E+01
        1   13     5.78E-01  7.919E+01        1   14     5.45E-01  8.546E+01
        1   15     5.13E-01  9.172E+01

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Trichlor              2.505E+02
 Modeling Medium #             1
 Medium Type (MED)          =  4
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A RIVER




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Trichlor   2.740E+17   3.820E-01   3.731E+00     3.569E+01     3.944E+01

 Darcy Velocity (DARCYV)                         =   5.18E-01 cm/day
 Pore-water Velocity (U)                         =   2.59E+00 cm/day
 Aquifer Thickness (H1)                          =   1.22E+03 cm
 X-Direction Dispersivity (A1)                   =   1.00E+03 cm
 Y-Direction Dispersivity (A2)                   =   3.30E+02 cm
 Z-Direction Dispersivity (A3)                   =   2.50E+00 cm
 Bulk Density (R2)                               =   1.43E+00 g/cm**3
 Total Porosity (AN4)                            =   4.60E-01 (fraction)
 Effective Porosity (AN5)                        =   2.00E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   1.00E+04 cm
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

Trichlor     79016    2.74E+17   2.57E-07  3.82E-01     1.00E+00    6.15E+01



 Trichlor (79016   ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  9.462E+05 cm^2/yr
 Lateral Dispersion Coefficient =       3.124E+05 cm^2/yr
 Vertical Dispersion Coefficient =      2.555E+03 cm^2/yr

 SOURCE DIMENSIONS FOR THE NEXT MEDIUM:

 Width of Plume (ALEN)                      =   5.55E+03 cm
 Depth to top of Plume (ZTOP)               =   0.00E+00 cm
 Depth to bottom of Plume (ZBOT)            =   3.31E+02 cm



                                          Mass Released       Mass Passed
                                Solution   From Source        This Medium
   Name     CASID    RATIO(a)    Method     (g or pCi)         (g or pCi)
 --------  --------  ---------  --------  -------------  --------------------

 Trichlor  79016     1.425E-01  NUMERICAL    6.144E+01     6.143E+01 (100.0%)

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

     Trichlor    79016      PEAK        6.278E-01          90.4

     (a)
         PEAK: The maximum flux occurs as a peak flux at the given time.


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
 Usage Location Name   = fcm5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Antimony         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

  #           Constituent         NUM

    1          Antimony           41
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
Depth to bottom of Release Unit (ZBOT)              =   1.61E+01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Antimony     7440360  2.74E+17   1.12E-07  6.00E+00     1.00E+00    7.45E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  4.387E+01        1    2     3.98E-04  8.779E+01
        1    3     1.44E-02  1.317E+02        1    4     9.11E-02  1.756E+02
        1    5     2.31E-01  2.196E+02        1    6     3.38E-01  2.635E+02
        1    7     3.41E-01  3.074E+02        1    8     2.70E-01  3.513E+02
        1    9     1.81E-01  3.952E+02        1   10     1.08E-01  4.391E+02
        1   11     5.95E-02  4.831E+02        1   12     3.08E-02  5.270E+02
        1   13     1.52E-02  5.709E+02        1   14     7.28E-03  6.148E+02
        1   15     3.37E-03  6.587E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  4.387E+01        1    2     3.98E-04  8.779E+01
        1    3     1.44E-02  1.317E+02        1    4     9.11E-02  1.756E+02
        1    5     2.31E-01  2.196E+02        1    6     3.38E-01  2.635E+02
        1    7     3.41E-01  3.074E+02        1    8     2.70E-01  3.513E+02
        1    9     1.81E-01  3.952E+02        1   10     1.08E-01  4.391E+02
        1   11     5.95E-02  4.831E+02        1   12     3.08E-02  5.270E+02
        1   13     1.52E-02  5.709E+02        1   14     7.28E-03  6.148E+02
        1   15     3.37E-03  6.587E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Antimony              1.757E+03
 Modeling Medium #             1
 Medium Type (MED)          =  3
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A WELL




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Antimony   2.740E+17   6.000E+00   4.390E+01     6.047E+02     6.682E+02

 Darcy Velocity (DARCYV)                         =   5.18E-01 cm/day
 Pore-water Velocity (U)                         =   2.59E+00 cm/day
 Aquifer Thickness (H1)                          =   1.22E+03 cm
 X-Direction Dispersivity (A1)                   =   1.44E+03 cm
 Y-Direction Dispersivity (A2)                   =   4.75E+02 cm
 Z-Direction Dispersivity (A3)                   =   3.60E+00 cm
 Bulk Density (R2)                               =   1.43E+00 g/cm**3
 Total Porosity (AN4)                            =   4.60E-01 (fraction)
 Effective Porosity (AN5)                        =   2.00E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   1.44E+04 cm
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

Antimony     7440360  2.74E+17   1.12E-07  6.00E+00     1.00E+00    7.45E+01



 Antimony (7440360 ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  1.362E+06 cm^2/yr
 Lateral Dispersion Coefficient =       4.495E+05 cm^2/yr
 Vertical Dispersion Coefficient =      3.596E+03 cm^2/yr


                                          Mass Released
                                Solution   From Source
   Name     CASID    RATIO(a)    Method     (g or pCi)
 --------  --------  ---------  --------  -------------

 Antimony  7440360   3.442E-01  NUMERICAL    7.443E+01

 (a) RRATIO= 3.000E+01.  RATIO is the estimated time to peak divided by the
     duration of time over which the contaminant enters this medium.  When
     RATIO >= RRATIO, the equation for an instantaneous release is used,
     otherwise the numerical solution for a time varying release is used.


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Antimony when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Antimony    7440360    PEAK        1.476E-10         916.4

     (a)
         PEAK: The maximum concentration occurs as a peak concentration
               at the given time.


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
 Usage Location Name   = fcm5                 

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
    2          YTTRIUM-           35
 Source-Term Type (ISTYPE)                     =  2
 Source-Term Flux Boundary Conditions (ISOURC) =  2
 Liquid Impoundment Index (IPOND)              =  2
 Source-Term Decay Index (IDECAY)              =  2
 Direct Discharge Surface Water Index (IDDSW)  =  0
 Number of Parent Constituents (NUMCON)        =  2

Source-Term Configuration (ISCONF)        =  6
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  3

Length of Release Unit (AL1 OR CLEN)                =   1.00E+03 cm
Width of Release Unit (B1)                          =   1.00E+03 cm
Depth to top of Release Unit (ZTOP)                 =   0.00E+00 cm
Depth to bottom of Release Unit (ZBOT)              =   1.61E+01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   1.52E-09  1.00E+02     1.00E+06    1.46E+00
YTTRIUM-     Y90      7.39E-03   2.06E-19  5.38E+02     1.00E+06    1.22E-09


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  2.936E+02        1    2     3.83E-09  3.477E+02
        1    3     3.96E-07  4.018E+02        1    4     9.09E-06  4.558E+02
        1    5     8.52E-05  5.099E+02        1    6     4.29E-04  5.640E+02
        1    7     1.29E-03  6.181E+02        1    8     2.63E-03  6.722E+02
        1    9     3.94E-03  7.262E+02        1   10     4.62E-03  7.803E+02
        1   11     4.41E-03  8.344E+02        1   12     3.57E-03  8.885E+02
        1   13     2.52E-03  9.426E+02        1   14     1.59E-03  9.967E+02
        1   15     9.04E-04  1.051E+03
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.010E-05
        2    3     6.27E-13  5.021E+02        2    4     5.32E-13  1.004E+03
        2    5     5.32E-13  2.009E+03        2    6     3.05E-40  2.511E+03
        2    7     3.66E-40  3.013E+03        2    8     4.27E-40  3.515E+03
        2    9     5.50E-40  4.519E+03        2   10     6.11E-40  5.022E+03
        2   11     7.33E-40  6.026E+03        2   12     7.94E-40  6.528E+03
        2   13     8.55E-40  7.030E+03        2   14     9.16E-40  7.532E+03
        2   15     9.77E-40  8.034E+03

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  2.936E+02        1    2     3.83E-09  3.477E+02
        1    3     3.96E-07  4.018E+02        1    4     9.09E-06  4.558E+02
        1    5     8.52E-05  5.099E+02        1    6     4.29E-04  5.640E+02
        1    7     1.29E-03  6.181E+02        1    8     2.63E-03  6.722E+02
        1    9     3.94E-03  7.262E+02        1   10     4.62E-03  7.803E+02
        1   11     4.41E-03  8.344E+02        1   12     3.57E-03  8.885E+02
        1   13     2.52E-03  9.426E+02        1   14     1.59E-03  9.967E+02
        1   15     9.04E-04  1.051E+03
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.010E-05
        2    3     6.27E-13  5.021E+02        2    4     5.32E-13  1.004E+03
        2    5     5.32E-13  2.009E+03        2    6     3.05E-40  2.511E+03
        2    7     3.66E-40  3.013E+03        2    8     4.27E-40  3.515E+03
        2    9     5.50E-40  4.519E+03        2   10     6.11E-40  5.022E+03
        2   11     7.33E-40  6.026E+03        2   12     7.94E-40  6.528E+03
        2   13     8.55E-40  7.030E+03        2   14     9.16E-40  7.532E+03
        2   15     9.77E-40  8.034E+03

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       STRONTIU              2.163E+03
    2       YTTRIUM-              2.009E+04
 Modeling Medium #             1
 Medium Type (MED)          =  3
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A WELL




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  STRONTIU   2.900E+01   1.000E+02   7.160E+02     1.052E+03     1.090E+04
  2  YTTRIUM-   7.390E-03   5.380E+02   3.848E+03     3.951E+01     5.857E+04

 Darcy Velocity (DARCYV)                         =   5.18E-01 cm/day
 Pore-water Velocity (U)                         =   2.59E+00 cm/day
 Aquifer Thickness (H1)                          =   1.22E+03 cm
 X-Direction Dispersivity (A1)                   =   1.44E+03 cm
 Y-Direction Dispersivity (A2)                   =   4.75E+02 cm
 Z-Direction Dispersivity (A3)                   =   3.60E+00 cm
 Bulk Density (R2)                               =   1.43E+00 g/cm**3
 Total Porosity (AN4)                            =   4.60E-01 (fraction)
 Effective Porosity (AN5)                        =   2.00E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   1.44E+04 cm
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

STRONTIU     SR90     2.90E+01   1.52E-09  1.00E+02     1.00E+06    1.46E+00
YTTRIUM-     Y90      7.39E-03   2.06E-19  5.38E+02     1.00E+06    1.22E-09



 STRONTIU (SR90    ): DECAY CONSTANT =  2.390E-02 1/YR
 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR

 Longitudinal Dispersion Coefficient =  1.362E+06 cm^2/yr
 Lateral Dispersion Coefficient =       4.495E+05 cm^2/yr
 Vertical Dispersion Coefficient =      3.596E+03 cm^2/yr


                                          Mass Released
                                Solution   From Source
   Name     CASID    RATIO(a)    Method     (g or pCi)
 --------  --------  ---------  --------  -------------

 STRONTIU  SR90      4.864E-01  NUMERICAL    1.455E+00
 YTTRIUM-  Y90       1.967E-03  NUMERICAL    1.116E-09

 (a) RRATIO= 3.000E+01.  RATIO is the estimated time to peak divided by the
     duration of time over which the contaminant enters this medium.  When
     RATIO >= RRATIO, the equation for an instantaneous release is used,
     otherwise the numerical solution for a time varying release is used.


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

     STRONTIU    SR90       PEAK        4.775E-32        1836.4
     YTTRIUM-    Y90        PEAK        4.776E-32        1836.4

     (a)
         PEAK: The maximum concentration occurs as a peak concentration
               at the given time.


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number   3 for Scenario Number   2
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  2
 Usage Location Medium =  1
 Usage Location Name   = fcm5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Trichlor         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

  #           Constituent         NUM

    1          Trichlor           41
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
Depth to bottom of Release Unit (ZBOT)              =   1.61E+01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    2.74E+17   2.57E-07  3.82E-01     1.00E+00    6.15E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  4.038E+00        1    2     3.58E-03  1.030E+01
        1    3     9.14E-02  1.656E+01        1    4     3.50E-01  2.283E+01
        1    5     6.13E-01  2.909E+01        1    6     7.49E-01  3.535E+01
        1    7     7.81E-01  4.162E+01        1    8     7.64E-01  4.788E+01
        1    9     7.29E-01  5.414E+01        1   10     6.90E-01  6.040E+01
        1   11     6.51E-01  6.667E+01        1   12     6.13E-01  7.293E+01
        1   13     5.78E-01  7.919E+01        1   14     5.45E-01  8.546E+01
        1   15     5.13E-01  9.172E+01

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  4.038E+00        1    2     3.58E-03  1.030E+01
        1    3     9.14E-02  1.656E+01        1    4     3.50E-01  2.283E+01
        1    5     6.13E-01  2.909E+01        1    6     7.49E-01  3.535E+01
        1    7     7.81E-01  4.162E+01        1    8     7.64E-01  4.788E+01
        1    9     7.29E-01  5.414E+01        1   10     6.90E-01  6.040E+01
        1   11     6.51E-01  6.667E+01        1   12     6.13E-01  7.293E+01
        1   13     5.78E-01  7.919E+01        1   14     5.45E-01  8.546E+01
        1   15     5.13E-01  9.172E+01

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Trichlor              2.505E+02
 Modeling Medium #             1
 Medium Type (MED)          =  3
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A WELL




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  Trichlor   2.740E+17   3.820E-01   3.731E+00     5.140E+01     5.680E+01

 Darcy Velocity (DARCYV)                         =   5.18E-01 cm/day
 Pore-water Velocity (U)                         =   2.59E+00 cm/day
 Aquifer Thickness (H1)                          =   1.22E+03 cm
 X-Direction Dispersivity (A1)                   =   1.44E+03 cm
 Y-Direction Dispersivity (A2)                   =   4.75E+02 cm
 Z-Direction Dispersivity (A3)                   =   3.60E+00 cm
 Bulk Density (R2)                               =   1.43E+00 g/cm**3
 Total Porosity (AN4)                            =   4.60E-01 (fraction)
 Effective Porosity (AN5)                        =   2.00E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   1.44E+04 cm
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

Trichlor     79016    2.74E+17   2.57E-07  3.82E-01     1.00E+00    6.15E+01



 Trichlor (79016   ): DECAY CONSTANT =  2.530E-18 1/YR

 Longitudinal Dispersion Coefficient =  1.362E+06 cm^2/yr
 Lateral Dispersion Coefficient =       4.495E+05 cm^2/yr
 Vertical Dispersion Coefficient =      3.596E+03 cm^2/yr


                                          Mass Released
                                Solution   From Source
   Name     CASID    RATIO(a)    Method     (g or pCi)
 --------  --------  ---------  --------  -------------

 Trichlor  79016     2.052E-01  NUMERICAL    6.144E+01

 (a) RRATIO= 3.000E+01.  RATIO is the estimated time to peak divided by the
     duration of time over which the contaminant enters this medium.  When
     RATIO >= RRATIO, the equation for an instantaneous release is used,
     otherwise the numerical solution for a time varying release is used.


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Trichlor when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Trichlor    79016      PEAK        8.084E-10         107.0

     (a)
         PEAK: The maximum concentration occurs as a peak concentration
               at the given time.


 ******************************************************************************
"riv10",442                             
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
 Usage Location Name   = fcm5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Antimony         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Antimony           41
Length of Surface Water Source Term (AL1)  =  5.550E+03 cm
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

Antimony     7440360  2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    7.45E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  9.173E+01        1    2     1.05E-05  2.233E+02
        1    3     4.29E-03  3.549E+02        1    4     4.99E-02  4.864E+02
        1    5     1.20E-01  6.180E+02        1    6     1.36E-01  7.495E+02
        1    7     1.06E-01  8.811E+02        1    8     6.75E-02  1.013E+03
        1    9     3.87E-02  1.144E+03        1   10     2.09E-02  1.276E+03
        1   11     1.09E-02  1.407E+03        1   12     5.55E-03  1.539E+03
        1   13     2.79E-03  1.671E+03        1   14     1.39E-03  1.802E+03
        1   15     6.87E-04  1.934E+03

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  9.173E+01        1    2     1.05E-05  2.233E+02
        1    3     4.29E-03  3.549E+02        1    4     4.99E-02  4.864E+02
        1    5     1.20E-01  6.180E+02        1    6     1.36E-01  7.495E+02
        1    7     1.06E-01  8.811E+02        1    8     6.75E-02  1.013E+03
        1    9     3.87E-02  1.144E+03        1   10     2.09E-02  1.276E+03
        1   11     1.09E-02  1.407E+03        1   12     5.55E-03  1.539E+03
        1   13     2.79E-03  1.671E+03        1   14     1.39E-03  1.802E+03
        1   15     6.87E-04  1.934E+03

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Antimony              5.263E+03
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   1.61E+06 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   3.05E+03 cm
 Downstream Discharge at First Receptor (QNEW)  =   3.16E+11 cm**3/year
 Distance Downstream from Center of Source (X3) =   3.05E+03 cm
 Lateral Dispersion Coefficient (E2)            =   2.95E+07 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Antimony     7440360  2.74E+17   1.00E+00 -9.90E+01     1.00E+00    7.45E+01



 Antimony (7440360 ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Antimony when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Antimony    7440360    PEAK        3.146E-12         749.6

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
 Usage Location Name   = fcm5                 

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
    2          YTTRIUM-           30
Length of Surface Water Source Term (AL1)  =  5.550E+03 cm
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

STRONTIU     SR90     2.90E+01  -9.99E+01 -9.90E+01     1.00E+06    5.32E-17
YTTRIUM-     Y90      7.39E-03  -9.99E+01 -9.90E+01     1.00E+06    1.62E-19


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  6.444E+02        1    2     8.57E-31  7.391E+02
        1    3     1.06E-27  8.337E+02        1    4     2.11E-25  9.283E+02
        1    5     1.17E-23  1.023E+03        1    6     2.44E-22  1.118E+03
        1    7     2.35E-21  1.212E+03        1    8     1.20E-20  1.307E+03
        1    9     3.70E-20  1.402E+03        1   10     7.45E-20  1.496E+03
        1   11     1.05E-19  1.591E+03        1   12     1.11E-19  1.686E+03
        1   13     9.08E-20  1.780E+03        1   14     6.00E-20  1.875E+03
        1   15     3.29E-20  1.969E+03
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  6.444E+02
        2    3     8.57E-31  7.391E+02        2    4     1.06E-27  8.337E+02
        2    5     2.11E-25  9.283E+02        2    6     1.17E-23  1.023E+03
        2    7     2.44E-22  1.118E+03        2    8     1.20E-21  1.212E+03
        2    9     1.25E-22  1.307E+03        2   10     1.31E-23  1.402E+03
        2   11     1.36E-24  1.496E+03        2   12     1.42E-25  1.591E+03
        2   13     1.47E-26  1.686E+03        2   14     1.54E-27  1.780E+03
        2   15     1.60E-28  1.875E+03

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  6.444E+02        1    2     8.57E-31  7.391E+02
        1    3     1.06E-27  8.337E+02        1    4     2.11E-25  9.283E+02
        1    5     1.17E-23  1.023E+03        1    6     2.44E-22  1.118E+03
        1    7     2.35E-21  1.212E+03        1    8     1.20E-20  1.307E+03
        1    9     3.70E-20  1.402E+03        1   10     7.45E-20  1.496E+03
        1   11     1.05E-19  1.591E+03        1   12     1.11E-19  1.686E+03
        1   13     9.08E-20  1.780E+03        1   14     6.00E-20  1.875E+03
        1   15     3.29E-20  1.969E+03
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  6.444E+02
        2    3     8.57E-31  7.391E+02        2    4     1.06E-27  8.337E+02
        2    5     2.11E-25  9.283E+02        2    6     1.17E-23  1.023E+03
        2    7     2.44E-22  1.118E+03        2    8     1.20E-21  1.212E+03
        2    9     1.25E-22  1.307E+03        2   10     1.31E-23  1.402E+03
        2   11     1.36E-24  1.496E+03        2   12     1.42E-25  1.591E+03
        2   13     1.47E-26  1.686E+03        2   14     1.54E-27  1.780E+03
        2   15     1.60E-28  1.875E+03

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       STRONTIU              3.786E+03
    2       YTTRIUM-              4.430E+03
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   1.61E+06 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   3.05E+03 cm
 Downstream Discharge at First Receptor (QNEW)  =   3.16E+11 cm**3/year
 Distance Downstream from Center of Source (X3) =   3.05E+03 cm
 Lateral Dispersion Coefficient (E2)            =   2.95E+07 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   1.00E+30 -9.90E+01     1.00E+06    5.32E-17
YTTRIUM-     Y90      7.39E-03   1.00E+30 -9.90E+01     1.00E+06    1.62E-19



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

     STRONTIU    SR90       ZERO        0.000E+00         644.4 to   4430.1
     YTTRIUM-    Y90        ZERO        0.000E+00            .0 to   4430.1

     (a)
         ZERO: Concentrations are zero over the given time range.


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number   3 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = fcm5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Trichlor         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Trichlor           41
Length of Surface Water Source Term (AL1)  =  5.550E+03 cm
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

Trichlor     79016    2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    6.15E+01


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     0.00E+00  8.106E+00        1    2     7.63E-05  2.182E+01
        1    3     1.50E-02  3.553E+01        1    4     1.41E-01  4.924E+01
        1    5     3.73E-01  6.295E+01        1    6     5.54E-01  7.667E+01
        1    7     6.28E-01  9.038E+01        1    8     6.24E-01  1.041E+02
        1    9     5.83E-01  1.178E+02        1   10     5.25E-01  1.315E+02
        1   11     4.33E-01  1.452E+02        1   12     2.93E-01  1.589E+02
        1   13     1.64E-01  1.726E+02        1   14     8.09E-02  1.864E+02
        1   15     3.72E-02  2.001E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     0.00E+00  8.106E+00        1    2     7.63E-05  2.182E+01
        1    3     1.50E-02  3.553E+01        1    4     1.41E-01  4.924E+01
        1    5     3.73E-01  6.295E+01        1    6     5.54E-01  7.667E+01
        1    7     6.28E-01  9.038E+01        1    8     6.24E-01  1.041E+02
        1    9     5.83E-01  1.178E+02        1   10     5.25E-01  1.315E+02
        1   11     4.33E-01  1.452E+02        1   12     2.93E-01  1.589E+02
        1   13     1.64E-01  1.726E+02        1   14     8.09E-02  1.864E+02
        1   15     3.72E-02  2.001E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Trichlor              5.485E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   1.61E+06 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   3.05E+03 cm
 Downstream Discharge at First Receptor (QNEW)  =   3.16E+11 cm**3/year
 Distance Downstream from Center of Source (X3) =   3.05E+03 cm
 Lateral Dispersion Coefficient (E2)            =   2.95E+07 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    2.74E+17   1.00E+00 -9.90E+01     1.00E+00    6.15E+01



 Trichlor (79016   ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Trichlor when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Trichlor    79016      PEAK        1.449E-11          90.4

     (a)
         PEAK: The maximum concentration occurs as a peak concentration
               at the given time.


 ******************************************************************************
