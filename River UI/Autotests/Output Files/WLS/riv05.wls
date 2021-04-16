"riv11",2676                            
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
 Usage Location Name   = exp12                

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               1,2-Dich         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          1,2-Dich            2
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

1,2-Dich     156605   2.74E+17  -9.99E+01 -9.90E+01     6.30E-03    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       1,2-Dich              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

1,2-Dich     156605   2.74E+17   6.30E-03 -9.90E+01     6.30E-03    1.01E+05



 1,2-Dich (156605  ): DECAY CONSTANT =  2.530E-18 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for 1,2-Dich when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------


     1,2-Dich      ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           6.300E-03(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     1,2-Dich    156605     TRUNC       4.866E-02           2.5

  WARNING! --> 1,2-Dich: The computed maximum concentration EXCEEDS
               the solubility limit of  6.300E-03 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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

Acetone      67641    2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Acetone               1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Acetone      67641    2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+05



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

     Acetone     67641      SS          4.866E-02           2.5 to    100.0

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
 Usage Location Name   = exp12                

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

Aroclor      11104282 2.74E+17  -9.99E+01 -9.90E+01     4.00E-05    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Aroclor               1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Aroclor      11104282 2.74E+17   4.00E-05 -9.90E+01     4.00E-05    1.01E+05



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


     Aroclor       ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           4.000E-05(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Aroclor     11104282   TRUNC       4.866E-02           2.5

  WARNING! --> Aroclor : The computed maximum concentration EXCEEDS
               the solubility limit of  4.000E-05 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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

Benzo(a)     50328    2.74E+17  -9.99E+01 -9.90E+01     1.63E-09    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Benzo(a)              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Benzo(a)     50328    2.74E+17   1.63E-09 -9.90E+01     1.63E-09    1.01E+05



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


     Benzo(a)      ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           1.630E-09(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Benzo(a)    50328      TRUNC       4.866E-02           2.5

  WARNING! --> Benzo(a): The computed maximum concentration EXCEEDS
               the solubility limit of  1.630E-09 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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

CESIUM-1     CS137    3.01E+01  -9.99E+01 -9.90E+01     1.00E+06    1.01E+02


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
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

CESIUM-1     CS137    3.01E+01   1.00E+30 -9.90E+01     1.00E+06    1.01E+02



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

     CESIUM-1    CS137      SS          4.866E-05           2.5 to    100.0

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
 Usage Location Name   = exp12                

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

Chlorobe     108907   2.74E+17  -9.99E+01 -9.90E+01     4.66E-04    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Chlorobe              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Chlorobe     108907   2.74E+17   4.66E-04 -9.90E+01     4.66E-04    1.01E+05



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


     Chlorobe      ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           4.660E-04(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Chlorobe    108907     TRUNC       4.866E-02           2.5

  WARNING! --> Chlorobe: The computed maximum concentration EXCEEDS
               the solubility limit of  4.660E-04 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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

Chromium     7440473  2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Chromium              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Chromium     7440473  2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+05



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

     Chromium    7440473    SS          4.866E-02           2.5 to    100.0

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
 Usage Location Name   = exp12                

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

Ethylene     107211   2.74E+17  -9.99E+01 -9.90E+01     5.82E-01    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Ethylene              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Ethylene     107211   2.74E+17   5.82E-01 -9.90E+01     5.82E-01    1.01E+05



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

     Ethylene    107211     SS          4.866E-02           2.5 to    100.0

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
 Usage Location Name   = exp12                

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

Hexachlo     118741   2.74E+17  -9.99E+01 -9.90E+01     6.00E-09    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Hexachlo              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Hexachlo     118741   2.74E+17   6.00E-09 -9.90E+01     6.00E-09    1.01E+05



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


     Hexachlo      ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           6.000E-09(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Hexachlo    118741     TRUNC       4.866E-02           2.5

  WARNING! --> Hexachlo: The computed maximum concentration EXCEEDS
               the solubility limit of  6.000E-09 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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

IODINE-1     I129     1.57E+07  -9.99E+01 -9.90E+01     1.00E+06    1.01E+02


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
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

IODINE-1     I129     1.57E+07   1.00E+30 -9.90E+01     1.00E+06    1.01E+02



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

     IODINE-1    I129       SS          4.866E-05           2.5 to    100.0

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
 Usage Location Name   = exp12                

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

Mercury      7439976  2.74E+17  -9.99E+01 -9.90E+01     3.00E-08    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Mercury               1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Mercury      7439976  2.74E+17   3.00E-08 -9.90E+01     3.00E-08    1.01E+05



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


     Mercury       ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           3.000E-08(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Mercury     7439976    TRUNC       4.866E-02           2.5

  WARNING! --> Mercury : The computed maximum concentration EXCEEDS
               the solubility limit of  3.000E-08 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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

Naphthal     91203    2.74E+17  -9.99E+01 -9.90E+01     3.20E-05    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Naphthal              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Naphthal     91203    2.74E+17   3.20E-05 -9.90E+01     3.20E-05    1.01E+05



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


     Naphthal      ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           3.200E-05(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Naphthal    91203      TRUNC       4.866E-02           2.5

  WARNING! --> Naphthal: The computed maximum concentration EXCEEDS
               the solubility limit of  3.200E-05 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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

PLUTONIU     PU240    6.54E+03  -9.99E+01 -9.90E+01     1.00E+06    1.01E+02


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
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

PLUTONIU     PU240    6.54E+03   1.00E+30 -9.90E+01     1.00E+06    1.01E+02



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

     PLUTONIU    PU240      SS          4.866E-05           2.5 to    100.0

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
 Usage Location Name   = exp12                

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

Pyrene       129000   2.74E+17  -9.99E+01 -9.90E+01     1.35E-07    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Pyrene                1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Pyrene       129000   2.74E+17   1.35E-07 -9.90E+01     1.35E-07    1.01E+05



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


     Pyrene        ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           1.350E-07(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Pyrene      129000     TRUNC       4.866E-02           2.5

  WARNING! --> Pyrene  : The computed maximum concentration EXCEEDS
               the solubility limit of  1.350E-07 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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

Selenium     7782492  2.74E+17  -9.99E+01 -9.90E+01     1.00E+00    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Selenium              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Selenium     7782492  2.74E+17   1.00E+00 -9.90E+01     1.00E+00    1.01E+05



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

     Selenium    7782492    SS          4.866E-02           2.5 to    100.0

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
 Usage Location Name   = exp12                

 TLIFE is the duration of release for each contaminant expressed in years. 
 TDIFF is the time difference in years between the start date for risk
 calculations and the date when a contaminant was first released into
 the environment (WS-TRISK minus WS-CDATE).


              Constituent         TLIFE     TDIFF          TFINAL
                  Name           (years)   (years)         (years)

               TECHNETI         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          TECHNETI            2
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

TECHNETI     TC99     2.13E+05  -9.99E+01 -9.90E+01     1.00E+06    1.01E+02


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
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

TECHNETI     TC99     2.13E+05   1.00E+30 -9.90E+01     1.00E+06    1.01E+02



 TECHNETI (TC99    ): DECAY CONSTANT =  3.254E-06 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for TECHNETI when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     TECHNETI    TC99       SS          4.866E-05           2.5 to    100.0

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
 Usage Location Name   = exp12                

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

Toluene      108883   2.74E+17  -9.99E+01 -9.90E+01     5.35E-04    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Toluene               1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Toluene      108883   2.74E+17   5.35E-04 -9.90E+01     5.35E-04    1.01E+05



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


     Toluene       ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           5.350E-04(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Toluene     108883     TRUNC       4.866E-02           2.5

  WARNING! --> Toluene : The computed maximum concentration EXCEEDS
               the solubility limit of  5.350E-04 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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
Length of Surface Water Source Term (AL1)  =  1.500E+04 cm
 Source-Term Type (ISTYPE)                     =  3
 Source-Term Flux Boundary Conditions (ISOURC) =  2
 Liquid Impoundment Index (IPOND)              =  2
 Source-Term Decay Index (IDECAY)              =  2
 Direct Discharge Surface Water Index (IDDSW)  =  1
 Number of Parent Constituents (NUMCON)        =  9

Source-Term Configuration (ISCONF)        =  0
Number of Media (NMED)                    =  1
Number of Integration Time Steps (NTIMES) = 40

Medium #  1 Equals Medium Type (MED) #  5
Water concentration units are: MICR (1.000E-06) pCi OR g

                                           Surface
Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)     Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

URANIUM-     U238     4.46E+09  -9.99E+01 -9.90E+01     1.00E+06    1.01E+02
THORIUM-     TH234    6.60E-02  -9.99E+01 -9.90E+01     1.00E+06    1.01E-30
URANIUM-     U234     2.44E+05  -9.99E+01 -9.90E+01     1.00E+06    1.01E-30
THORIUM-     TH230    7.69E+04  -9.99E+01 -9.90E+01     1.00E+06    1.01E-30
RADIUM-2     RA226    1.60E+03  -9.99E+01 -9.90E+01     1.00E+06    1.01E-30
RADON-22     RN222    1.05E-02  -9.99E+01 -9.90E+01     1.00E+06    1.01E-30
LEAD-210     PB210    2.23E+01  -9.99E+01 -9.90E+01     1.00E+06    1.01E-30
BISMUTH-     BI210    1.37E-02  -9.99E+01 -9.90E+01     1.00E+06    1.01E-30
POLONIUM     PO210    3.78E-01  -9.99E+01 -9.90E+01     1.00E+06    1.01E-30


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
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

URANIUM-     U238     4.46E+09   1.00E+30 -9.90E+01     1.00E+06    1.01E+02
THORIUM-     TH234    6.60E-02   1.00E+30 -9.90E+01     1.00E+06    1.01E-30
URANIUM-     U234     2.44E+05   1.00E+30 -9.90E+01     1.00E+06    1.01E-30
THORIUM-     TH230    7.69E+04   1.00E+30 -9.90E+01     1.00E+06    1.01E-30
RADIUM-2     RA226    1.60E+03   1.00E+30 -9.90E+01     1.00E+06    1.01E-30
RADON-22     RN222    1.05E-02   1.00E+30 -9.90E+01     1.00E+06    1.01E-30
LEAD-210     PB210    2.23E+01   1.00E+30 -9.90E+01     1.00E+06    1.01E-30
BISMUTH-     BI210    1.37E-02   1.00E+30 -9.90E+01     1.00E+06    1.01E-30
POLONIUM     PO210    3.78E-01   1.00E+30 -9.90E+01     1.00E+06    1.01E-30



 URANIUM- (U238    ): DECAY CONSTANT =  1.554E-10 1/YR
 THORIUM- (TH234   ): DECAY CONSTANT =  1.050E+01 1/YR
 URANIUM- (U234    ): DECAY CONSTANT =  2.841E-06 1/YR
 THORIUM- (TH230   ): DECAY CONSTANT =  9.014E-06 1/YR
 RADIUM-2 (RA226   ): DECAY CONSTANT =  4.332E-04 1/YR
 RADON-22 (RN222   ): DECAY CONSTANT =  6.601E+01 1/YR
 LEAD-210 (PB210   ): DECAY CONSTANT =  3.108E-02 1/YR
 BISMUTH- (BI210   ): DECAY CONSTANT =  5.059E+01 1/YR
 POLONIUM (PO210   ): DECAY CONSTANT =  1.834E+00 1/YR
 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

 Concentration for URANIUM- when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for THORIUM- when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for URANIUM- when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for THORIUM- when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for RADIUM-2 when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for RADON-22 when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for LEAD-210 when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for BISMUTH- when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)

 Concentration for POLONIUM when risk calculations begin (0.0 yrs) is:
           0.000E+00 (g/ml or pCi/ml)


 ******************************************************************************
 Times are given as years since the start of risk calculations (WS-TRISK)

     Maximum Constituent Concentrations:


                                         Maximum
                                      Concentration
       Name       CASID    Type(a)   (g/mL or pCi/mL)      Time (Years)
     --------    --------  -------   ---------------   --------------------

     URANIUM-    U238       SS          4.866E-05           2.5 to    100.0
     THORIUM-    TH234      SS          1.506E-08           2.5 to    100.0
     URANIUM-    U234       SS          6.320E-19           2.5 to    100.0
     THORIUM-    TH230      ZERO        0.000E+00            .0 to    100.0
     RADIUM-2    RA226      SS          2.024E-21           2.5 to    100.0
     RADON-22    RN222      SS          1.538E-21           2.5 to    100.0
     LEAD-210    PB210      SS          5.530E-21           2.5 to    100.0
     BISMUTH-    BI210      SS          6.356E-21           2.5 to    100.0
     POLONIUM    PO210      SS          2.560E-21           2.5 to    100.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).

         ZERO: Concentrations are zero over the given time range.


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
 Usage Location Name   = exp12                

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

Vinyl ch     75014    2.74E+17  -9.99E+01 -9.90E+01     2.67E-03    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Vinyl ch              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Vinyl ch     75014    2.74E+17   2.67E-03 -9.90E+01     2.67E-03    1.01E+05



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


     Vinyl ch      ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           2.670E-03(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Vinyl ch    75014      TRUNC       4.866E-02           2.5

  WARNING! --> Vinyl ch: The computed maximum concentration EXCEEDS
               the solubility limit of  2.670E-03 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


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
 Usage Location Name   = exp12                

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

Xylene       1330207  2.74E+17  -9.99E+01 -9.90E+01     1.98E-04    1.01E+05


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+03  0.000E+00        1    2     1.00E+03  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       Xylene                1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   2.71E+09 cm/year
 Depth of Flow (H1)                             =   3.05E+02 cm
 Width of River (B1)                            =   1.01E+04 cm
 Downstream Discharge at First Receptor (QNEW)  =   9.68E+04 cm**3/year
 Distance Downstream from Center of Source (X3) =   8.00E+04 cm
 Lateral Dispersion Coefficient (E2)            =   4.97E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

Xylene       1330207  2.74E+17   1.98E-04 -9.90E+01     1.98E-04    1.01E+05



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


     Xylene        ----- IMPORTANT NOTE FOR THIS RUN -------
          The computed maximum concentration is greater than the
          designated initial concentration at the source of
           1.980E-04(g/ml or pCi/ml), after accounting for decay.
          The current receptor is located too close to the source,
          creating a near-field condition which cannot be properly
          assessed by a flux boundary condition model.
     Xylene      1330207    TRUNC       4.866E-02           2.5

  WARNING! --> Xylene  : The computed maximum concentration EXCEEDS
               the solubility limit of  1.980E-04 (g/mL).


     (a)
        TRUNC: For parent constituents, concentrations at a well have been
               truncated to the initial dissolved concentration (accounting
               for decay if necessary) due to near-field conditions.  For
               decay products, concentrations are based on a truncated parent.


 ******************************************************************************
