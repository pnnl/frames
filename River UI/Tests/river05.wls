"riv3",2556                             
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
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               1,2 DICH         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          1,2 DICH            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

1,2 DICH     156605   2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       1,2 DICH              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

1,2 DICH     156605   2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 1,2 DICH (156605  ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for 1,2 DICH when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     1,2 DICH    156605     SS          5.631E-16            .0 to      1.0

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
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Acetone          1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Acetone             2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Acetone      67641    2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Acetone               1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Acetone  (67641   ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Acetone  when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Acetone     67641      SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


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
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               AMERICIU         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          AMERICIU            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

AMERICIU     AM241    4.33E+02  -9.99E+01 -9.90E+01     1.00E+20    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       AMERICIU              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

AMERICIU     AM241    4.33E+02   1.00E+30 -9.90E+01     1.00E+20    1.01E+00



 AMERICIU (AM241   ): DECAY CONSTANT =  1.601E-03 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for AMERICIU when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     AMERICIU    AM241      SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number   4 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Aroclor          1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Aroclor             2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Aroclor      11104282 2.74E+17  -9.99E+01 -9.90E+01     1.00E+14    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Aroclor               1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Aroclor      11104282 2.74E+17   1.00E+30 -9.90E+01     1.00E+14    1.01E+00



 Aroclor  (11104282): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Aroclor  when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Aroclor     11104282   SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number   5 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Benzo(a)         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Benzo(a)            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Benzo(a)     50328    2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Benzo(a)              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Benzo(a)     50328    2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Benzo(a) (50328   ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Benzo(a) when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Benzo(a)    50328      SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number   6 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               CESIUM-1         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          CESIUM-1            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

CESIUM-1     CS137    3.01E+01  -9.99E+01 -9.90E+01     1.00E+20    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       CESIUM-1              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

CESIUM-1     CS137    3.01E+01   1.00E+30 -9.90E+01     1.00E+20    1.01E+00



 CESIUM-1 (CS137   ): DECAY CONSTANT =  2.303E-02 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for CESIUM-1 when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     CESIUM-1    CS137      SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number   7 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Chlorobe         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Chlorobe            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Chlorobe     108907   2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Chlorobe              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Chlorobe     108907   2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Chlorobe (108907  ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Chlorobe when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Chlorobe    108907     SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number   8 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Chromium         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Chromium            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Chromium     7440473  2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Chromium              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Chromium     7440473  2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Chromium (7440473 ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Chromium when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Chromium    7440473    SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number   9 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Ethylene         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Ethylene            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Ethylene     107211   2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Ethylene              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Ethylene     107211   2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Ethylene (107211  ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Ethylene when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Ethylene    107211     SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  10 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Hexachlo         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Hexachlo            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Hexachlo     118741   2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Hexachlo              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Hexachlo     118741   2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Hexachlo (118741  ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Hexachlo when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Hexachlo    118741     SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  11 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               IODINE-1         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          IODINE-1            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

IODINE-1     I129     1.57E+07  -9.99E+01 -9.90E+01     1.00E+20    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       IODINE-1              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

IODINE-1     I129     1.57E+07   1.00E+30 -9.90E+01     1.00E+20    1.01E+00



 IODINE-1 (I129    ): DECAY CONSTANT =  4.415E-08 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for IODINE-1 when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     IODINE-1    I129       SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  12 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Mercury          1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Mercury             2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Mercury      7439976  2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Mercury               1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Mercury      7439976  2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Mercury  (7439976 ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Mercury  when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Mercury     7439976    SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  13 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Naphthal         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Naphthal            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Naphthal     91203    2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Naphthal              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Naphthal     91203    2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Naphthal (91203   ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Naphthal when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Naphthal    91203      SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  14 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               PLUTONIU         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          PLUTONIU            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

PLUTONIU     PU240    6.54E+03  -9.99E+01 -9.90E+01     1.00E+20    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       PLUTONIU              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

PLUTONIU     PU240    6.54E+03   1.00E+30 -9.90E+01     1.00E+20    1.01E+00



 PLUTONIU (PU240   ): DECAY CONSTANT =  1.060E-04 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for PLUTONIU when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     PLUTONIU    PU240      SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  15 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Pyrene           1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Pyrene              2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Pyrene       129000   2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Pyrene                1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Pyrene       129000   2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Pyrene   (129000  ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Pyrene   when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Pyrene      129000     SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  16 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Selenium         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Selenium            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Selenium     7782492  2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Selenium              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Selenium     7782492  2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Selenium (7782492 ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Selenium when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Selenium    7782492    SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  17 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               TECHNECI         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          TECHNECI            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

TECHNECI     TC99     2.13E+05  -9.99E+01 -9.90E+01     1.00E+20    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       TECHNECI              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

TECHNECI     TC99     2.13E+05   1.00E+30 -9.90E+01     1.00E+20    1.01E+00



 TECHNECI (TC99    ): DECAY CONSTANT =  3.254E-06 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for TECHNECI when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     TECHNECI    TC99       SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  18 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Toluene          1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Toluene             2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Toluene      108883   2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Toluene               1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Toluene      108883   2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Toluene  (108883  ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Toluene  when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Toluene     108883     SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  19 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               URANIUM-         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          URANIUM-            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

URANIUM-     U238     4.46E+09  -9.99E+01 -9.90E+01     1.00E+20    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       URANIUM-              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

URANIUM-     U238     4.46E+09   1.00E+30 -9.90E+01     1.00E+20    1.01E+00



 URANIUM- (U238    ): DECAY CONSTANT =  1.554E-10 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for URANIUM- when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     URANIUM-    U238       SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  20 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Vinyl ch         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Vinyl ch            2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Vinyl ch     75014    2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Vinyl ch              1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Vinyl ch     75014    2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Vinyl ch (75014   ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Vinyl ch when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Vinyl ch    75014      SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
 ******************************************************************************
          Modeling Parent Contaminant Number  21 for Scenario Number   1
 ******************************************************************************

 Name of Facility      = Facility Name                   
 Name of Waste Unit    = Ranking Unit Name               
 Transport Scenario    = Transport Scenario              
 Exposure Scenario     = Exposure Scenario               
 Usage Location Number =  1
 Usage Location Medium =  3
 Usage Location Name   = exp5                 

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               Xylene           1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Xylene              2
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
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

Xylene       1330207  2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+00


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+00  0.000E+00        1    2     1.00E+00  1.000E+00

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Xylene                1.000E+00
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   8.36E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Xylene       1330207  2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+00



 Xylene   (1330207 ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for Xylene   when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     Xylene      1330207    SS          5.631E-16            .0 to      1.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
