"riv3",257                              
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

               Acetone          1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          Acetone             2
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
 Flow Velocity (U1)                             =   3.25E+09 cm/year
 Depth of Flow (H1)                             =   2.44E+02 cm
 Width of River (B1)                            =   3.81E+03 cm
 Downstream Discharge at First Receptor (QNEW)  =   3.02E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   2.40E+07 cm
 Lateral Dispersion Coefficient (E2)            =   4.76E+10 cm**2/year



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

     Acetone     67641      SS          3.311E-13           2.5 to    100.0

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

               YTTRIUM-         1.000E+04 0.000E+00      1.000E+04

  #           Constituent         NUM

    1          YTTRIUM-            2
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

YTTRIUM-     Y90      7.39E-03  -9.99E+01 -9.90E+01     1.00E+20    1.01E+12


Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time      Constituent #     Flux      Time

        1    1     1.00E+10  0.000E+00        1    2     1.00E+10  1.000E+02

New Constituent Flux (g/yr or pCi/yr) and Time (Years) at the Release Unit

 Constituent #     Flux      Time    Constituent #     Flux      Time

        1    1     1.00E+10  0.000E+00        1    2     1.00E+10  1.000E+02

The New Duration of Time that Constituents are Released from the Release Unit

    #      Constituent         New TLIFE (Years)

    1       YTTRIUM-              1.000E+02
 Modeling Medium #             1
 Medium Type (MED)          =  5
 Source Configuration       =  0
 Source Term Discharge Type =  3
 SURFACE WATER ENVIRONMENT



 Number of River Receptor Locations (NUMREC)    =          1
 Flow Velocity (U1)                             =   3.25E+09 cm/year
 Depth of Flow (H1)                             =   2.44E+02 cm
 Width of River (B1)                            =   3.81E+03 cm
 Downstream Discharge at First Receptor (QNEW)  =   3.02E+15 cm**3/year
 Distance Downstream from Center of Source (X3) =   2.40E+07 cm
 Lateral Dispersion Coefficient (E2)            =   4.76E+10 cm**2/year



                                           Surface
 Constituent   CAS ID  Half-Life  Initial Equilibrium  Solubility  Constituent
                       (Years)      Conc.    Coeff.      limit     Inventory
                                            (ml/g)      (g/ml)    (g OR pCi)

YTTRIUM-     Y90      7.39E-03   1.00E+30 -9.90E+01     1.00E+20    1.01E+12



 YTTRIUM- (Y90     ): DECAY CONSTANT =  9.380E+01 1/YR
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

     YTTRIUM-    Y90        SS          1.656E-06           2.5 to    100.0

     (a)
           SS: Concentrations vary by less than 1/100th of one percent
               of the maximum concentration over the given time range
               (steady state).


 ******************************************************************************
