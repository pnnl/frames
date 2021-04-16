"riv3",281                              
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
 Usage Location Name   = exp4                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               EINSTEIN         1.000E+04 0.000E+00      1.000E+04
               CALIFORN         1.000E+04 0.000E+00      1.000E+04
               CURIUM-2         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          EINSTEIN            2
    2          CALIFORN            2
    3          CURIUM-2            2
Length of Surface Water Source Term (AL1)  =  2.500E+03 cm
 Source-Term Type (ISTYPE)                     =  3
 Source-Term Flux Boundary Conditions (ISOURC) =  2
 Liquid Impoundment Index (IPOND)              =  2
 Source-Term Decay Index (IDECAY)              =  2
 Direct Discharge Surface Water Index (IDDSW)  =  1
 Number of Parent Constituents (NUMCON)        =  3

Source-Term Configuration (ISCONF)        =  0
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  5
Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

EINSTEIN     ES254    7.56E-01  -9.99E+01 -9.90E+01     1.00E+20    1.01E+19
CALIFORN     CF250    1.31E+01  -9.99E+01 -9.90E+01     1.00E+20    1.01E-30
CURIUM-2     CM246    4.74E+03  -9.99E+01 -9.90E+01     1.00E+20    1.01E-30


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+18  0.000E+00        1    2     1.00E+18  1.000E+01
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
        3    1     0.00E+00  0.000E+00        3    2     0.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+18  0.000E+00        1    2     1.00E+18  1.000E+01
        2    1     0.00E+00  0.000E+00        2    2     0.00E+00  1.000E+00
        3    1     0.00E+00  0.000E+00        3    2     0.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       EINSTEIN              1.000E+01
    2       CALIFORN              1.000E+00
    3       CURIUM-2              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   3.84E+09 cm/year
 Depth of Flow (H1)                             =   6.01E+02 cm
 Width of River (B1)                            =   4.57E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   1.06E+17 cm**3/year
 Distance Downstream from Center of Source (X3) =   5.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   1.38E+11 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

EINSTEIN     ES254    7.56E-01   1.00E+30 -9.90E+01     1.00E+20    1.01E+19
CALIFORN     CF250    1.31E+01   1.00E+30 -9.90E+01     1.00E+20    1.01E-30
CURIUM-2     CM246    4.74E+03   1.00E+30 -9.90E+01     1.00E+20    1.01E-30



 EINSTEIN (ES254   ): DECAY CONSTANT =  9.169E-01 1/YR
 CALIFORN (CF250   ): DECAY CONSTANT =  5.291E-02 1/YR
 CURIUM-2 (CM246   ): DECAY CONSTANT =  1.462E-04 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for EINSTEIN when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for CALIFORN when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for CURIUM-2 when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     EINSTEIN    ES254      SS          1.819E+02            .3 to     10.0
     CALIFORN    CF250      SS          1.255E-04            .3 to     10.0
     CURIUM-2    CM246      SS          1.196E-13            .3 to     10.0

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
 Usage Location Medium =  3
 Usage Location Name   = exp4                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Trichlor         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Trichlor            2
Length of Surface Water Source Term (AL1)  =  2.500E+03 cm
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

Trichlor     79016    2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    5.16E+04


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     5.15E+04  0.000E+00        1    2     5.15E+04  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     5.15E+04  0.000E+00        1    2     5.15E+04  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Trichlor              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   3.84E+09 cm/year
 Depth of Flow (H1)                             =   6.01E+02 cm
 Width of River (B1)                            =   4.57E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   1.06E+17 cm**3/year
 Distance Downstream from Center of Source (X3) =   5.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   1.38E+11 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Trichlor     79016    2.74E+17   1.00E+00 -9.90E+01     1.00E+00    5.16E+04



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

     Trichlor    79016      SS          9.368E-12            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
