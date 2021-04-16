C*******************************************************************
C
C     NUC_DATA.INC
C     
C     Date:         April 26, 2000
C
C     Description:  Radionuclide & dose factor data
C
C*******************************************************************

      REAL*8        dk_const(MaxNucs), dkfract(MaxNucs,MaxNucs), 
     &              br_fract(2,MaxNucs)
      
      INTEGER       par_indx(2, MaxNucs)

      INTEGER       nnucs

      CHARACTER*8   NucName(MaxNucs)
      CHARACTER*20  NucFulName(MaxNucs)
     
      COMMON  /nucdata/                      
     &          dk_const, dkfract,
     &          br_fract,
     &          par_indx,
     &          nnucs,
     &          NucName, NucFulName

