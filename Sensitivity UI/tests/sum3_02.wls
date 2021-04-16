"aqu2",360                              
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
 Usage Location Name   = exp5                 

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

    1          STRONTIU            2
    2          YTTRIUM-            2
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
Depth to bottom of Release Unit (ZBOT)              =   2.74E-01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

STRONTIU     SR90     2.90E+01   1.00E-05  2.43E+01     1.00E+23    5.01E+02
YTTRIUM-     Y90      7.39E-03  -9.99E+01  0.00E+00     1.00E+23    1.01E-30


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+02  0.000E+00        1    2     1.00E+02  5.000E+00
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+02  0.000E+00        1    2     1.00E+02  5.000E+00
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       STRONTIU              5.000E+00
    2       YTTRIUM-              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  3
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A WELL




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  STRONTIU   2.900E+01   2.430E+01   3.027E+02     4.407E+00     4.973E+00
  2  YTTRIUM-   7.390E-03   0.000E+00   1.000E+00     1.194E-02     1.643E-02

 Darcy Velocity (DARCYV)                         =   1.00E+02 cm/day
 Pore-water Velocity (U)                         =   8.33E+02 cm/day
 Aquifer Thickness (H1)                          =   1.00E+03 cm
 X-Direction Dispersivity (A1)                   =   5.00E+02 cm
 Y-Direction Dispersivity (A2)                   =   1.65E+02 cm
 Z-Direction Dispersivity (A3)                   =   1.25E+00 cm
 Bulk Density (R2)                               =   1.49E+00 g/cm**3
 Total Porosity (AN4)                            =   4.37E-01 (fraction)
 Effective Porosity (AN5)                        =   1.20E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   5.00E+03 cm
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

STRONTIU     SR90     2.90E+01   1.00E-05  2.43E+01     1.00E+23    5.01E+02
YTTRIUM-     Y90      7.39E-03   1.00E+30  0.00E+00     1.00E+23    1.01E-30



 STRONTIU (SR90    ): DECAY CONSTANT =  2.390E-02 1/YR
 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR

 Longitudinal Dispersion Coefficient =  1.522E+08 cm^2/yr
 Lateral Dispersion Coefficient =       5.022E+07 cm^2/yr
 Vertical Dispersion Coefficient =      3.807E+05 cm^2/yr


                                          Mass Released
                                Solution   From Source
   Name     CASID    RATIO(a)    Method     (g or pCi)
 --------  --------  ---------  --------  -------------

 STRONTIU  SR90      8.814E-01  NUMERICAL    5.000E+02
 YTTRIUM-  Y90       1.194E-02  NUMERICAL    0.000E+00

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

     STRONTIU    SR90       PEAK        4.293E-09           7.2
     YTTRIUM-    Y90        PEAK        4.294E-09           7.2

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
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               YTTRIUM-         1.000E+04 0.000E+00      1.000E+04

Source-Term Moisture Content (AN5) = -99.00 %
Source-Term Bulk Density (R2)      =  -9.90E+01 g/cm**3

  #           Constituent         NUM

    1          YTTRIUM-            2
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
Depth to bottom of Release Unit (ZBOT)              =   2.74E-01 cm

Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

YTTRIUM-     Y90      7.39E-03   1.00E-04  0.00E+00     1.00E+23    5.01E+03


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  5.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  5.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       YTTRIUM-              5.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  3
 Source Configuration       =  6
 Source Term Discharge Type =  2
 SATURATED ZONE AT A WELL




                                                      CENTERLINE    CENTERLINE
                                                      ESTIMATED     ADVECTIVE
 NO  PARENT     HALFLIFE       KD      RETARDATION   TIME TO PEAK  TRAVEL TIME
                 YEARS        ML/G       FACTOR         YEARS         YEARS

  1  YTTRIUM-   7.390E-03   0.000E+00   1.000E+00     1.194E-02     1.643E-02

 Darcy Velocity (DARCYV)                         =   1.00E+02 cm/day
 Pore-water Velocity (U)                         =   8.33E+02 cm/day
 Aquifer Thickness (H1)                          =   1.00E+03 cm
 X-Direction Dispersivity (A1)                   =   5.00E+02 cm
 Y-Direction Dispersivity (A2)                   =   1.65E+02 cm
 Z-Direction Dispersivity (A3)                   =   1.25E+00 cm
 Bulk Density (R2)                               =   1.49E+00 g/cm**3
 Total Porosity (AN4)                            =   4.37E-01 (fraction)
 Effective Porosity (AN5)                        =   1.20E-01 (fraction)
 Centerline Downgradient Distance from
 Center of Source (X2)                           =   5.00E+03 cm
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

YTTRIUM-     Y90      7.39E-03   1.00E-04  0.00E+00     1.00E+23    5.01E+03



 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR

 Longitudinal Dispersion Coefficient =  1.522E+08 cm^2/yr
 Lateral Dispersion Coefficient =       5.022E+07 cm^2/yr
 Vertical Dispersion Coefficient =      3.807E+05 cm^2/yr


                                          Mass Released
                                Solution   From Source
   Name     CASID    RATIO(a)    Method     (g or pCi)
 --------  --------  ---------  --------  -------------

 YTTRIUM-  Y90       2.389E-03  NUMERICAL    5.000E+03

 (a) RRATIO= 3.000E+01.  RATIO is the estimated time to peak divided by the
     duration of time over which the contaminant enters this medium.  When
     RATIO >= RRATIO, the equation for an instantaneous release is used,
     otherwise the numerical solution for a time varying release is used.


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for YTTRIUM- when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     YTTRIUM-    Y90        SS          1.522E-08            .1 to      5.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
