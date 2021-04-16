!     Last change:  PWE  16 Apr 2009    1:44 pm
!
MODULE Control_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  History: Paul W. Eslinger : 01 Jul 1992
!           Paul W. Eslinger : 14 Apr 2009 : Convert to Fortran 95
!
!     Dimension variables
      INTEGER :: MAXN   ! Maximum number of samples which can be generated
      INTEGER :: MAXP   ! Maximum number of stochastic variables which can be used
      INTEGER :: MXPP   ! Length of vector storage for symmetric matrices
      INTEGER :: MAXTBL ! Maximum number of pairs of values that can be entered
!                         in defining all tabular statistical distributions
!
!     Counter variables
      INTEGER :: N      ! Number of iterations of data to generate
      INTEGER :: NP     ! Length of vector with lower triangular correlation matrix for P variables
      INTEGER :: P      ! Number of variables for stochastic generation
      INTEGER :: PCON   ! Number of "constant" variables for generation
      INTEGER :: PTOT   ! Total number of variables for generation
      INTEGER :: NCOR   ! Number of values read into the correlation matrix
!
!     Generated values and work space
      REAL, ALLOCATABLE :: X(:,:)      ! Array(MAXN,MAXP) for realizations of stochastic variables
      REAL, ALLOCATABLE :: SCORE(:,:)  ! Score matrix (MAXN,MAXP) corresponding to X(*,*)
      INTEGER, ALLOCATABLE :: IWORK(:) ! Work vector(MAXN)
      REAL, ALLOCATABLE :: RWORK(:)    ! Work vector(MAXN)
      REAL, ALLOCATABLE :: XTMP(:)     ! Work vector(MAXN)
      REAL, ALLOCATABLE :: XOUT(:)     ! Vector(MAXP) holding output realizations for all variables
!                                        for a single iteration
!
!     Correlation matrix variables
      REAL, ALLOCATABLE :: COR(:)      ! Input correlation matrix (MXPP)
      REAL, ALLOCATABLE :: CORA(:)     ! Score matrix correlation matrix (MXPP)
      REAL, ALLOCATABLE :: CORSAV(:)   ! Saved input correlation matrix (MXPP)
      REAL, ALLOCATABLE :: CORO(:)     ! Output correlation matrix (MXPP)
!
!     Random number generation control
      REAL(KIND=8) :: DSEED ! Seed for the random number generator
!
!     Logical control flags
      LOGICAL :: BADDAT ! Logical flag indicating bad data in subroutine ERRCHK
      LOGICAL :: BADKEY ! Logical flag indicating bad data on a keyword entry
      LOGICAL :: CIDENT ! Logical flag indicating identity correlation matrix
      LOGICAL :: EXECUT ! Logical flag for execution of the problem setup
      LOGICAL :: OCORR  ! Logical flag to output correlation matrix for generated data
      LOGICAL :: ODATA  ! Logical flag controling output of data to the report file
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
!
MODULE Data_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  History: Paul W. Eslinger : 01 Jul 1992
!           Paul W. Eslinger : 14 Apr 2009 : Convert to Fortran 95
!
!     VTYPE  : Vector of statistical distribution types for all variables
!     VTRUN  : Vector of truncation flags for statistical distributions for all variables.
!                0 = none                          1 = truncated on the left tail
!                2 = truncated on the right tail   3 = truncated on both tails
      INTEGER, ALLOCATABLE :: VTYPE(:)    ! Nominal dimension MAXP
      INTEGER, ALLOCATABLE :: VTRUN(:)    ! Nominal dimension MAXP
!
!     VPARMS : Array of parameters for the statistical distribution for all variables.  Up to
!              4 parameters are allowed.  The row of the matrix corresponds to a single variable
!     VTLIM  : Array of truncation limits for the statistical distribution for all variables.
!              The row of the matrix corresponds to a single variable.  The first entry is
!              the left limit (if used).  The second entry is the right limit (if used).
!     UMIN   : Vector of minimum uniform values to use for generating statistical distributions.
!              Untruncated distributions have an entry of 0.  The value is set in subroutine SINV.
!     UMAX   : Vector of maximum uniform values to use for generating statistical distributions.
!              Untruncated distributions have an entry of 1.  The value is set in subroutine SINV.
      REAL, ALLOCATABLE :: VPARMS(:,:)    ! Nominal dimension MAXP,4
      REAL, ALLOCATABLE :: VTLIM(:,:)     ! Nominal dimension MAXP,2
      REAL, ALLOCATABLE :: UMIN(:)        ! Nominal dimension MAXP
      REAL, ALLOCATABLE :: UMAX(:)        ! Nominal dimension MAXP
!
!     PTABLE : Array of pointers for the user defined distributions.  The row indicates the
!              variable number.  The fist entry on the row indicates the starting location in
!              the vectors XTABLE and FTABLE for this variable.  The sencond entry on the row
!              gives the number of pairs of data for this distribution.
!     XTABLE : Table of (X) values for tabular statistical distributions. Values for all
!              distributions are stored sequentially in this vector (see PTABLE)
!     FTABLE : Table of F(X) values for tabular statistical distributions. Values for all
!              distributions are stored sequentially in this vector (see PTABLE)
      REAL, ALLOCATABLE :: XTABLE(:)      ! Nominal dimension MAXTBL
      REAL, ALLOCATABLE :: FTABLE(:)      ! Nominal dimension MAXTBL
      INTEGER, ALLOCATABLE :: PTABLE(:,:) ! Nominal dimension MAXP,2
!
!     MAP    : Vector of mapping indices from all of the input variables into the set of variables
!              that are stochastic (for output of stochastic variable labels).
      INTEGER, ALLOCATABLE :: MAP(:)      ! Nominal dimension MAXP
!
      CHARACTER(LEN=72) :: TITLE ! Problem title
      CHARACTER(LEN= 9), ALLOCATABLE :: VNAME(:)  ! Vector(MAXP) of variable names (labels)
      CHARACTER(LEN= 7), ALLOCATABLE :: UNIQUE(:) ! Vector(MAXP) of unique variable names
      CHARACTER(LEN=76), ALLOCATABLE :: VDESC(:)  ! Vector(MAXP) of variable descriptions
!
!    XSTATS : Array of summary statistics for generated "stochastic" variables.
!             A row corresponds to a variable.  Columns contain:
!             1) minimum, 2) maximum, 3) average (mean), 4) median,
!             5) standard deviation, and 6) coefficient of variation.
      REAL, ALLOCATABLE :: XSTATS(:,:) ! Nominal dimension (MAXP,6)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
!
MODULE Errors_Mod
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains variables related to writing user defined error messages to an ASCII
!    file opened on unit IRPT_ERR.  The messages are written using the routine PRTERR.
!
!  History:
!
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!    Paul W. Eslinger : 10 Oct 2002 : Extend message length
!    Paul W. Eslinger : 30 May 2007 : Update comments
!
!     Unit number for writing the error messages
      INTEGER :: IRPT_ERR
!
!     Maximum number of lines in an error message
      INTEGER, PARAMETER :: MAXMES = 5
!
!     Vector of message lines
      CHARACTER(LEN=256), DIMENSION(MAXMES) :: MESSAG
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Errors_Mod
!
!
MODULE Files_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  History: Paul W. Eslinger : 01 Jul 1992
!           Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!
      INTEGER :: IDAT ! Unit number for the output data file
      INTEGER :: IERF ! Unit number for the output error message file
      INTEGER :: IKEY ! Unit number for the input keyword control file
      INTEGER :: IRPT ! Unit number for the output report file
      INTEGER :: IMID ! Unit number for the input FRAMES ID file
!
      CHARACTER(LEN=256) :: FNDAT ! File name for the output data file
      CHARACTER(LEN=256) :: FNERR ! File name for the output error message file
      CHARACTER(LEN=256) :: FNKEY ! File name for the input keyword control file
      CHARACTER(LEN=256) :: FNRPT ! File name for the output report file
      CHARACTER(LEN=256) :: FNMID ! File name for the FRAMES ID file
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
!
MODULE Iden_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This module contains run and user identification information
!
!  History:
!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!
      CHARACTER(LEN=14) :: CRUNID ! Run identification number
!       Digit 1 is the leftmost digit
!         Digits  1- 4 = Year
!         Digits  5- 6 = Month
!         Digits  7- 8 = Day (1 to 31)
!         Digits  9-10 = Hour (0 to 23)
!         Digits 11-12 = Minute (0 to 59)
!         Digits 13-14 = Second (0 to 59)
      CHARACTER(LEN=12) :: PRGDAT ! Program modification date
      CHARACTER(LEN=10) :: PRGNAM ! Program name
      CHARACTER(LEN= 8) :: PRGVER ! Program version number
      CHARACTER(LEN=10) :: SYSDAT ! System date in the format mm-dd-yyyy
      CHARACTER(LEN=12) :: SYSTIM ! System time in the format hh:mm:ss
      CHARACTER(LEN=16) :: USRNAM ! User name
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
!
MODULE QA_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!    This module contains quality assurance variables
!
!  History:
!    Paul W. Eslinger : 01 Jul 1992
!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!
      CHARACTER(LEN=16) :: VERNUM ! Document number of verification test report
      CHARACTER(LEN=72) :: VERTIT ! Title of verification test report
      CHARACTER(LEN=10) :: VERDAT ! Date of release of verification test report
      LOGICAL :: QA ! Logical flag
!                      .TRUE.  = Verification tests and report completed
!                      .FALSE. = Verification tests and report not completed
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
!
MODULE Rdblk_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Purpose:
!
!    This module contains information related to the RDBLK keyword
!    decoding routines
!
!  History:
!
!    Dave W. Langford : Original Programmer
!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!    Paul W. Eslinger : 11 Feb 2001 : Tie NVALS and NKEYS
!    Paul W. Eslinger : 13 Mar 2003 : Tie NVALS and MAXQQQ, extend lengths
!    Paul W. Eslinger :  5 Jan 2006 : Extend keyword line length
!    Paul W. Eslinger : 16 Apr 2009 : Allow more values per keyword
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Parameters:
!
!     LENCRD : Maximum number of characters per input card
      INTEGER, PARAMETER :: LENCRD = 2048
!
!     LENQQQ : Maximum number of characters per quoted string
      INTEGER, PARAMETER :: LENQQQ = 200
!
!     NKEYS  : Maximum number of keyword modifiers per block
      INTEGER, PARAMETER :: NKEYS  = 500501
!
!     NVALS  : Maximum number of data values per block
      INTEGER, PARAMETER :: NVALS  = NKEYS
!
!     MAXQQQ : Maximum number of strings enclosed in quotes per block
      INTEGER, PARAMETER :: MAXQQQ = NKEYS
!
! Character Information:
!
!     KNAME   : Input line keyword name
      CHARACTER(LEN=8) :: KNAME
!
!     CVALUE : List of character modifiers in input block
      CHARACTER(LEN=8), DIMENSION(NKEYS) :: CVALUE
!
!     INFO : Input line with the keyword stripped off
      CHARACTER(LEN=1), DIMENSION(LENCRD) :: INFO
!
!     QUOTE  : Input items which were enclosed in double quotes
      CHARACTER(LEN=LENQQQ), DIMENSION(MAXQQQ) :: QUOTE
!
! Numeric information:
!
!     VALUE : Numeric data read from input data block
      REAL, DIMENSION(NVALS) :: VALUE
!
! Block size/location tracking information
!
!     ILINE  : Line number of the next line to read from the input file
      INTEGER :: ILINE
!
!     LSTLIN : Line number of last keyword.  Identifies current command.
      INTEGER :: LSTLIN
!
!     NCVALU : Number of character items read from the last block.
      INTEGER :: NCVALU
!
!     NDXCHR : Command line indices for input string entities.
      INTEGER, DIMENSION(NKEYS) :: NDXCHR
!
!     NDXQQQ : Command line indices for strings enclosed in double quotes
      INTEGER, DIMENSION(MAXQQQ) :: NDXQQQ
!
!     NDXVAL : Command line indices for input value entities.
      INTEGER, DIMENSION(NVALS) :: NDXVAL
!
!     NEXIST : Indicates the index of the keyword or modifier
!              found by the logical function EXIST.
      INTEGER :: NEXIST
!
!     NQUOTE : Number of character strings found in double quotes.
      INTEGER :: NQUOTE
!
!     NTITY  : Number of string and value entities read from last block.
      INTEGER :: NTITY
!
!     NVALUE : Number of numeric items read from the last block.
      INTEGER :: NVALUE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE Rdblk_Mod
!
!
MODULE Stochastic_Mod
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  History: Paul W. Eslinger : 01 Jul 1992
!           Paul W. Eslinger : 14 Apr 2009 : Convert to Fortran 95
!
! Variables for random distributions
!   DLABEL: Vector of distribution labels (subroutines SGEN and SINV). Defined in subroutine INIT
!   MXDIST: Upper dimension for distribution type labels (number of nonconstant distributions)
!
      INTEGER, PARAMETER :: MXDIST = 15
      CHARACTER(LEN=16), DIMENSION(0:MXDIST) :: DLABEL
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
END MODULE
!
!
      PROGRAM LATIN
!!**************************************************************************************************
!!
!!                                       LATIN
!!                       Latin Hypercube Stochastic Data Generation
!!             Multimedia Environmental Pollutant Assessment System (FRAMES)
!!             Battelle, Pacific Northwest Laboratory, Richland, Washington
!!
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This program will generate a data file for use by the FRAMES code where the variables in the
!!    file are realizations of random variables.  The output variables can be cross-correlated using
!!    a Latin Hypercube Sampling scheme.
!!
!!    Control for this program comes through reading a file of control keywords. The keyword file
!!    is typically generated by the calling shell, but it can be built using any ASCII text editor.
!!
!!
!!  Notes:
!!
!!    1. Each column of the output data matrix contains values for a different variable and is
!!       initially generated independent of the other variables using a stratified sampling scheme
!!       on equally probable intervals with one value per strata.  The values from each variable are
!!       then matched with those of the other variables to yield a prespecified rank correlation
!!       structure.
!!
!!    2. The special purpose matrix handling routines are written to yield a minimal memory storage
!!       configuration -- not necessarily minimal CPU time.
!!
!!    3. The keyword decoding routines were initially written by Dave Langford.  They allow entering
!!       of control commands using an English-type command structure.
!!
!!**************************************************************************************************
!!
!!  References:
!!
!!    Iman, R. L. and W. J. Conover.  1982.
!!    "A Distribution-free Approach to Inducing Rank Correlations Among Input Variables",
!!    Communications in Statistics, Vol. B11, No. 3, pp. 311-334.
!!
!!    Liebetrau, A. M., and P. G. Doctor.  1987.
!!    "The Generation of Dependent Input Variables to a Performance Assessment Simulation Code",
!!    Presented at the OECD/DOE Workshop on Uncertainty Analysis for System Performance Assessments,
!!    Seattle, Washington.
!!
!!**************************************************************************************************
!!
!!  History:
!!
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 25 Aug 1992 : Change to add PRELATIN index to
!!            the VARIABLE, DESCRIBE, UNIQUE, and LABEL cards.
!!    Paul W. Eslinger : 27 Aug 1992 : Change indexing scheme on the
!!            VARIABLE card in INPUTS and format statements in PRTDST
!!    Paul W. Eslinger : 23 Oct 1992 : Version 1.11 : Add copyright notice
!!    Paul W. Eslinger :  3 Feb 1993 : Version 1.11.1 : Correct index
!!         on write statement for ITRUNC = 2 in Subroutine PRTDST
!!    Paul W. Eslinger : 13 Jul 1993 : Version 1.11.2
!!         Add check on large SUM in CORRPT
!!    Paul W. Eslinger : 16 Apr 2009 : Version 1.12.0
!!         Change to Fortran 95 to dynamically allocate array sizes.
!!         Simplify and expand error reporting.  Remove calls to 3rd party (DOS) windowing product.
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Data_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      INTEGER :: IERR
      CHARACTER(LEN=5) :: CALLER = 'LATIN' ! Name of this routine
!
!---- Executable code -------------------------------------------------------------
!
! *** Generate identification and quality assurance variables
      CALL IDEN_SET( )
      CALL QA_SET( )
!
! *** Write an opening screen
      CALL SPLASH( )
!
! *** File operations for the error message and keyword control files
      CALL FIRSTF( IERR )
!
! *** Read the keyword data to get variable dimensions
      CALL INPUTS_DIM( IERR )
      IF( IERR .NE. 0 ) STOP 'INPUTS_DIM'
!
! *** Initialize several variables needed later
      CALL INIT( IERR )
      IF( IERR .NE. 0 ) STOP 'INIT'
!
! *** Read the keyword data
      CALL INPUTS( IERR )
      IF( IERR .NE. 0 ) STOP 'INPUTS'
!
! *** Open the output files
      CALL OPENER( IERR )
      IF( IERR .NE. 0 ) STOP 'OPENER'
!
! *** Initial banner page to the report file
      CALL BANNER( IERR )
      IF( IERR .NE. 0 ) STOP 'BANNER'
!
! *** Perform more error checking on the inputs
      CALL ERRCHK( IERR )
      IF( IERR .NE. 0 ) STOP 'ERRCHK'
!
! *** Echo the problem definition to the report file
      CALL ECHO( IERR )
      IF( IERR .NE. 0 ) STOP 'ECHO'
!
! *** Don't continue execution unless requested
      IF( .NOT.EXECUT )  THEN
        IERR = 1
        MESSAG(1) = 'An EXECUTE card was not entered for this run.'
        MESSAG(2) = 'Execution has been stopped after the inputs'
        MESSAG(3) = 'were checked for consistency.'
        CALL PRTERR( IERR, CALLER, 3 )
        STOP
      END IF
!
! *** Generate marginal distributions for all variables
      CALL MARGIN( IERR )
      IF( IERR .NE. 0 ) STOP 'MARGIN'
!
! *** Perform the rearrangement of values for the LHS correlations
!     Skip this operation if an identity correlation matrix was used
!     Skip this operation unless there are at least 2 stochastic
!     variables to rearrange relative to each other
      IF( .NOT.CIDENT .AND. P.GT.1 ) CALL REARNG( IERR )
      IF( IERR .NE. 0 ) STOP 'REARNG'
!
! *** Perform the next sequence of operations only if data were
!     generated for "stochastic" variables
      IF( P .GT. 0 ) THEN
!
! ***   Compute univariate statistics on the "stochastic" variables
!       and output a summary to the report file
        CALL UNIV( IERR )
        IF( IERR .NE. 0 ) STOP 'UNIV'
!
! ***   (Option)
!       Compute the correlation matrix of the "stochastic" variables
!       Output a summary to the report file
        IF( OCORR ) THEN
          CALL CORRPT( IERR )
          IF( IERR .NE. 0 ) STOP 'CORRPT'
        END IF
!
      END IF
!
! *** Write the data to the output data file
      CALL WRDATA( IERR )
      IF( IERR .NE. 0 ) STOP 'WRDATA'
!
! *** (Option)
!     Write the data to the report file
      IF( ODATA ) THEN
        CALL DATRPT( IERR )
        IF( IERR .NE. 0 ) STOP 'DATRPT'
      END IF
!
      STOP 'NORMAL'
      END PROGRAM
!
!
      REAL FUNCTION ALNGAM( XVALUE, IFAULT )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This program calculates the logarithm of the Gamma function for the argument XVALUE.
!!
!!
!!  Formal Parameters:
!!    Variable  Definition
!!    --------  --------------------------------------------------------
!!    XVALUE    Input - Real: The value at which to evaluate the
!!              logarithm of the Gamma function.  XVALUE must be greater
!!              than zero, yet smaller than XLGST (defined below).
!!
!!    IFAULT    Output - Integer: Error indicator.
!!              IFAULT = 0 : Normal termination
!!              IFAULT = 1 : XVALUE was greater than XLGST
!!              IFAULT = 2 : XVALUE was less than or equal to zero.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!
!!  Reference:
!!     Macleod, Allan.  " A Robust and Reliable Algorithm for the
!!     Logarithm of the Gamma Function".
!!     Applied Statistics
!!     (Journal of the Royal Statistical Society, Series C.)
!!     Volume 38, No. 2, pp. 397-423.
!!
!!**************************************************************************************************
!
      INTEGER :: IFAULT
!
      REAL(KIND=8) :: ALR2PI, FOUR, HALF, ONE, ONEP5, R1(9), R2(9), &
            R3(9), R4(5), TWELVE, X, X1, X2, XLGE, XLGST, Y, ZERO
!
! *** Coefficients of rational functions
!
      DATA R1(1) /-2.66685511495D0/,  R1(2) /-2.44387534237D1/, &
          R1(3) /-2.19698958928D1/,  R1(4) / 1.11667541262D1/, &
          R1(5) / 3.13060547623D0/, &
          R1(6) / 6.07771387771D-1/, R1(7) /1.19400905721D1/, &
          R1(8) / 3.14690115749D1/,  R1(9) / 1.52346874070D1/
!
      DATA R2(1) /-7.83359299449D1/, R2(2) /-1.42046296688D2/, &
          R2(3) / 1.37519416416E2/, R2(4) / 7.86994924154D1/, &
          R2(5) / 4.16438922228D0/, &
          R2(6) / 4.70668766060D1/, R2(7) / 3.13399215894D2/, &
          R2(8) / 2.63505074721D2/, R2(9) /4.33400022514D1/
!
      DATA R3(1) /-2.12159572323D5/, R3(2) / 2.30661510616D5/, &
          R3(3) / 2.74647644705D4/, R3(4) /-4.02621119975D4/, &
          R3(5) /-2.29660729780D3/, &
          R3(6) /-1.16328495004D5/, R3(7) /-1.46025937511D5/, &
          R3(8) /-2.42357409629D4/, R3(9) /-5.70691009324D2/
!
      DATA R4(1) /2.79195317918525D-1/, R4(2) /4.917317610505968D-1/, &
          R4(3) / 6.92910599291889D-2/, &
          R4(4) / 3.350343815022304D0/, R4(5) /6.012459259764103D0/
!
! *** Fixed constants
!
      DATA ALR2PI, FOUR, HALF, ONE, ONEP5, TWELVE, ZERO &
      /9.18938533204673D-1, 4.0D0, 5.0D-1, 1.0D0, 1.5D0, 1.2D1, 0.0D0/
!
! *** Machine dependent constants (32 bit computer)
!     See the reference for computation for other hardware configurations
!
      DATA XLGE, XLGST /3.7671D2, 1.32D36/
!
!---- Executable code -------------------------------------------------------------
!
      X = XVALUE
      ALNGAM = ZERO
!
! *** Test for valid function argument
!
      IFAULT = 2
      IF(X.GE.XLGST) RETURN
      IFAULT = 1
      IF(X.LE. ZERO) RETURN
      IFAULT = 0
!
! *** Calculation for 0 < X < 0.5 and 0.5 <=X < 1.5 combined
!
      IF (X.LT.ONEP5) THEN
        IF (X.LT.HALF) THEN
          ALNGAM = -LOG(X)
          Y = X + ONE
!
! ***     Test whether X < machine epsilon
!
          IF (Y .EQ. ONE) RETURN
        ELSE
          ALNGAM = ZERO
          Y = X
          X = (X - HALF) - HALF
        END IF
        ALNGAM = ALNGAM + X * ((((R1(5) * Y + R1(4)) * Y + R1(3)) &
         * Y + R1(2)) * Y + R1(1)) / ((((Y + R1(9)) * Y + R1(8)) &
         * Y + R1(7)) * Y + R1(6))
        RETURN
      END IF
!
! *** Calculation for 1.5 <= X < 4.0
!
      IF (X .LT. FOUR) THEN
        Y = (X- ONE) - ONE
        ALNGAM = Y * ((((R2(5) * X + R2(4)) * X + R2(3)) * X + R2(2)) &
          * X + R2(1)) / ((((X+R2(9)) * X + R2(8)) * X + R2(7)) &
          * X + R2(6))
        RETURN
      END IF
!
! *** Calculation for 4.0 <= X < 12.0
!
      IF(X .LT. TWELVE) THEN
        ALNGAM = ((((R3(5) * X + R3(4)) * X + R3(3)) * X + R3(2)) &
          * X + R3(1)) / ((((X + R3(9)) * X + R3(8)) * X + R3(7)) &
          * X + R3(6))
        RETURN
      END IF
!
! *** Calculation for X > 12.0
!
      Y = LOG(X)
      ALNGAM = X * (Y - ONE) - HALF * Y + ALR2PI
      IF ( X .GT. XLGE) RETURN
      X1 = ONE / X
      X2 = X1 * X1
      ALNGAM = ALNGAM + X1 * ((R4(3)*X2+R4(2))*X2+R4(1)) / ((X2+R4(5))*X2+R4(4))
!
      RETURN
      END FUNCTION
!
!
      REAL FUNCTION ALNORM( X, UPPER )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This function evaluates the area under a standard normal curve.  The region for integration
!!    is from minus infinity to X if UPPER is .FALSE., or from X to infinity if UPPER is .TRUE.
!!    A polynomial approximation method is used.
!!
!!
!!  Call List Definitions:
!!    X     : Real input argument
!!    UPPER : Logical input argument
!!
!!
!!  Reference:
!!    Algorithm AS66
!!    Applied Statistics
!!    (Journal of the Royal Statistical Society, Series C)
!!    Vol. 22, No. 3, 1973.  Pages 424-427
!!
!!
!!  Notes:
!!    LTONE =  Argument value at which the lower tail area becomes unity to the accuracy of
!!             the machine.
!!             LTONE = (N+9)/3, Where N is the number of digits of accuracy, is sufficient.
!!             CRAY:         N = 14, so LTONE = 7.333
!!             IBM (32 BIT): N = 7,  so LTONE = 5.333
!!
!!    UTZERO = Argument at which the upper tail area becomes zero to the precision of the machine.
!!             UTZERO = SQRT(-2*ALOG(X)+1)) - 0.3 where X is the smallest allowable real number
!!             CRAY:         X = 2**(-8191), so UTZERO = 106.25
!!             IBM (32 BIT): X = 1.18E-38,   so UTZERO =  12.84
!!
!!    About nine significant figures (decimal) are correct on a machine that works to such accuracy.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
      REAL :: LTONE, UTZERO, ZERO, HALF, ONE, CON, X
!
      REAL(KIND=8) :: Z, Y
      REAL(KIND=8) C1, C2, C3, C4, C5, C6, C7
      REAL(KIND=8) D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12
!
      LOGICAL UPPER, UP
!
! *** LTONE and UTZERO are machine dependent - see comments above
!
      DATA LTONE, UTZERO / 5.333, 12.84 /
      DATA ZERO, HALF, ONE, CON / 0.0, 0.5, 1.0, 1.28 /
!
!---- Executable code -------------------------------------------------------------
!
      UP = UPPER
      Z  = X
      IF( Z .GE. ZERO ) GO TO 10
      UP = .NOT. UP
      Z = -Z
   10 CONTINUE
!
      IF( Z.LE.LTONE .OR. UP.AND.Z.LE.UTZERO ) GO TO 20
      ALNORM = ZERO
      GO TO 40
!
   20 CONTINUE
      Y = HALF * Z * Z
      IF( Z .GT. CON ) GO TO 30
!
      DATA C1 /   0.398942280444D0 /
      DATA C2 /  -0.399903438504D0 /
      DATA C3 /   5.75885480458D0  /
      DATA C4 / -29.8213557808D0   /
      DATA C5 /   2.62433121679D0  /
      DATA C6 /  48.6959930692D0   /
      DATA C7 /   5.92885724438D0  /
!
      ALNORM = HALF -  Z * (C1+C2*Y/(Y+C3+C4/(Y+C5+C6/(Y+C7))))
      GO TO 40
!
      DATA D1  /   0.398942280385D0 /
      DATA D2  /  -3.8052D-8      /
      DATA D3  /   1.00000615302D0  /
      DATA D4  /   3.98064794D-4  /
      DATA D5  /   1.98615381364D0  /
      DATA D6  /  -0.151679116635D0 /
      DATA D7  /   5.29330324926D0  /
      DATA D8  /   4.8385912808D0   /
      DATA D9  / -15.1508972451D0   /
      DATA D10 /   0.742380924027D0 /
      DATA D11 /  30.789933034D0    /
      DATA D12 /   3.99019417011D0  /
!
   30 CONTINUE
      ALNORM = D1 * DEXP(-Y) / &
        (Z+D2+D3/(Z+D4+D5/(Z+D6+D7/(Z+D8+D9/(Z+D10+D11/(Z+D12))))))
!
   40 CONTINUE
      IF( .NOT. UP ) ALNORM = ONE - ALNORM
!
      RETURN
      END FUNCTION
!
!
      SUBROUTINE BANNER( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints a banner page to the report file.  The banner page contains program
!!    name, programmer credits, and run identification.
!!
!!
!!  Calling Sequence:
!!    A call to subroutine IDEN_SET must preceed a call to this subroutine.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE QA_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'BANNER' ! Name of this routine
      CHARACTER(LEN=58), DIMENSION(7) :: HEAD
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Define the header
!
      HEAD(1) = 'LL              AA      TTTTTTTTTT IIIIIIIIII  NNN     NN'
      HEAD(2) = 'LL             AAAA         TT         II      NN N    NN'
      HEAD(3) = 'LL            AA  AA        TT         II      NN  N   NN'
      HEAD(4) = 'LL            AA  AA        TT         II      NN   N  NN'
      HEAD(5) = 'LL           AAAAAAAA       TT         II      NN    N NN'
      HEAD(6) = 'LL          AA      AA      TT         II      NN     NNN'
      HEAD(7) = 'LLLLLLLLLL  AA      AA      TT     IIIIIIIIII  NN      NN'
!
      WRITE(IRPT,7010,ERR=9999)
 7010 FORMAT(80('*'))
!
      WRITE(IRPT,7050,ERR=9999)
!
      WRITE(IRPT,7015,ERR=9999) HEAD(1)
      WRITE(IRPT,7015,ERR=9999) HEAD(2)
      WRITE(IRPT,7015,ERR=9999) HEAD(3)
      WRITE(IRPT,7015,ERR=9999) HEAD(4)
      WRITE(IRPT,7015,ERR=9999) HEAD(5)
      WRITE(IRPT,7015,ERR=9999) HEAD(6)
      WRITE(IRPT,7015,ERR=9999) HEAD(7)
 7015 FORMAT(11X,A)
!
      WRITE(IRPT,7020,ERR=9999) PRGNAM, PRGVER, PRGDAT
 7020 FORMAT(/32X,A,1X,A/26X,'Last Modified on ',A)
!
      WRITE(IRPT,7025,ERR=9999)
 7025 FORMAT(/ &
       14X,'         Latin Hypercube Sampling Program '/ &
       14X,'Multimedia Environmental Pollutant Assessment System'/ &
       14X,'           Developer:  Paul W. Eslinger')
!
      WRITE(IRPT,7030,ERR=9999) CRUNID, TRIM(USRNAM)
 7030 FORMAT(/9X,'Current Run ID = ',A,'   User Name = ',A)
!
      WRITE(IRPT,7040,ERR=9999) SYSDAT, SYSTIM
 7040 FORMAT(13X,'System Date = ',A,'   System Time = ',A)
!
      WRITE(IRPT,7050,ERR=9999)
 7050 FORMAT(' ')
!
      WRITE(IRPT,7010,ERR=9999)
!
! *** QA Status
      IF( QA ) THEN
        WRITE(IRPT,7060,ERR=9999) TRIM(VERNUM), TRIM(VERDAT), TRIM(VERTIT)
 7060   FORMAT(/ &
         3X,'This program has been developed and tested according to project procedures.'/ &
         3X,'The verification report number is ',A,' dated ',A,/ &
         3X,'Report Title: ',A)
      ELSE
        WRITE(IRPT,7070,ERR=9999)
 7070   FORMAT(/ &
         3X,'This program is experimental and has not been formally tested according to'/ &
         3X,'project procedures.  All results are preliminary in nature.')
      END IF
!
      WRITE(IRPT,7050,ERR=9999)
!
! *** Normal return point
      RETURN
!
! *** Write error encountered
 9999 CONTINUE
      IERR = 1
      MESSAG(1) = 'Write error encountered in the report file'
      CALL PRTERR( IERR, CALLER, 1 )
!
      RETURN
      END SUBROUTINE
!
!
      REAL FUNCTION BETAIN( X, P, Q, LBETA, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This function computes the ratio of the incomplete Beta function with the complete Beta
!!    function for arguments X between zero and one, and P and Q positive.
!!
!!    Another way to state the purpose is: This subroutine evaluates the cumulative distribution
!!    function at the location X of the Beta distribution with parameters P and Q.
!!
!!
!!  Formal Parameters:
!!    Variable  Definition
!!    --------  --------------------------------------------------------
!!    X         Input - Real: The value X at which to evaluate the
!!              Beta distribution on (0,1) with parameters P and Q.
!!
!!    P         Input - Real: The first parameter (i.e. X**P) for the
!!              Beta density.
!!
!!    Q         Input - Real: The second parameter for the Beta density.
!!
!!    LBETA     Input - Real : The natural log of the complete Beta
!!              function using the parameters P and Q on the
!!              interval (0,1).
!!
!!    IERR      Output - Integer: Error indicator
!!              0=Good, >0=Error
!!
!!
!!  Reference:
!!    Algorithm AS 63
!!    Applied Statistics (1973),
!!    Vol. 22, No. 3
!!
!!    Modified as per remark ASR 19
!!    Applied Statistics (1977),
!!    Vol. 26, No. 1
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      REAL :: X, P, Q, LBETA
      INTEGER :: IERR
!
! *** Local variables
      LOGICAL :: INDX
      REAL :: PSQ, CX, XX, PP, QQ, AI, TERM, RX, TEMP
      INTEGER :: NS
!
! *** Define accuracy
      REAL :: ACU
      DATA ACU /0.1E-7/
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Initialize
!
      BETAIN = X
!
! *** Test for admissibility of arguments
      IF( P.LE.0.0 .OR. Q.LE.0.0 ) THEN
        IERR = 1
        RETURN
      END IF
!
      IF( X.LT.0.0 .OR. X.GT.1.0 ) THEN
        IERR = 2
        RETURN
      END IF
!
! *** Exit for trivial calculation
      IF( X.EQ.0.0 .OR. X.EQ.1.0 ) RETURN
!
! *** Change tail if necessary and determine S
!
      PSQ = P + Q
      CX = 1.0 - X
      IF( P .GE. PSQ*X ) THEN
        XX = X
        PP = P
        QQ = Q
        INDX = .FALSE.
      ELSE
        XX = CX
        CX = X
        PP = Q
        QQ = P
        INDX = .TRUE.
      END IF
!
      AI = 1.0
      NS = QQ + CX * PSQ
      TERM = 1.0
      BETAIN = 1.0
!
! *** Use Soper's reduction formulae
!
      RX = XX / CX
    3 TEMP = QQ - AI
      IF( NS .EQ. 0 ) RX = XX
    4 TERM = TERM * TEMP * RX / (PP + AI)
      BETAIN = BETAIN + TERM
      TEMP = ABS(TERM)
      IF( TEMP.LE.ACU .AND. TEMP.LE.(ACU*BETAIN) ) GOTO 5
      AI = AI + 1.0
      NS = NS - 1
      IF( NS .GE. 0 ) GOTO 3
      TEMP = PSQ
      PSQ = PSQ + 1.0
      GOTO 4
!
! *** Calculate result
!
    5 CONTINUE
      BETAIN = BETAIN * EXP(PP*ALOG(XX) + (QQ-1.0)*ALOG(CX) - LBETA) / PP
      IF( INDX ) BETAIN = 1.0 - BETAIN
!
      RETURN
      END FUNCTION
!
!
      SUBROUTINE BINVER( U, X, A, B, P, Q, LBETA, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine computes the inverse of the beta distribution defined on the interval (A,B)
!!    with parameters P and Q. The method used is a bisection method to solve F(X) = U for X.
!!
!!
!!  Auxiliary Routines:
!!    The function BETAIN is used to compute the cumulative distribution function for the
!!    Beta(P,Q) distribution on the interval (0,1).
!!
!!
!!  Formal Parameters:
!!    Variable  Definition
!!    --------  --------------------------------------------------------
!!    U         Input - Real: The uniform value to invert to the value
!!              X from a Beta distribution.
!!
!!    X         Output - Real: The value X which satisfies F(X) = U,
!!              where F is the Beta distribution on (A,B) with
!!              parameters P and Q.
!!
!!    A         Input - Real: The left end of the range of the Beta
!!              distribution.
!!
!!    B         Input - Real: The right end of the range of the Beta
!!              distribution.
!!
!!    P         Input - Real: The first parameter (i.e. X**P) for the
!!              Beta density.
!!
!!    Q         Input - Real: The second parameter for the Beta density.
!!
!!    LBETA     Input - Real: The natural log of the complete beta
!!              function for the interval A=0, B=1.  If LBETA is set to
!!              zero on entry, it will be computed internally, otherwise
!!              the value entered will be used.
!!
!!    IERR      Output - Integer: Error indicator
!!              0=Good, >0=Error
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: ALNGAM
      REAL, EXTERNAL :: BETAIN
!
! *** Call list variables
      REAL :: U, X, A, B, P, Q, LBETA
      INTEGER :: IERR
!
! *** Local variables
      INTEGER :: IFT1, IFT2, IFT3
      INTEGER :: I, ITER
      REAL :: XLOW, XMID, XHGH
      REAL :: BMID
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Check for computation of LBETA
      IF( LBETA .EQ. 0.0 ) THEN
        IF( P .EQ. Q ) THEN
          LBETA = 2.0*ALNGAM(P,IFT1) - ALNGAM((2*P),IFT2)
          IF( IFT1.NE.0 .OR. IFT2.NE.0 ) THEN
            IERR = 1
            RETURN
          END IF
        ELSE
          LBETA = ALNGAM(P,IFT1) + ALNGAM(Q,IFT2) - ALNGAM((P+Q),IFT3)
          IF( IFT1.NE.0 .OR. IFT2.NE.0 .OR. IFT3.NE.0 ) THEN
            IERR = 2
            RETURN
          END IF
        END IF
      END IF
!
! *** Iterate to solve for X given U
!
! *** ITER = 21 yields 2**(-21) = 4.8E-7 Accuracy on X
!     Because X is confined to the interval (0,1)
!
      ITER = 21
!
      XLOW = 0.0
      XHGH = 1.0
      DO I = 1, ITER
        XMID = (XLOW+XHGH)/2.0
        BMID = BETAIN( XMID, P, Q, LBETA, IERR )
        IF( IERR .NE. 0 ) RETURN
        IF( BMID .GT. U ) THEN
          XHGH = XMID
        ELSE
          XLOW = XMID
        END IF
      END DO
!
      X = A + (B-A)*XMID
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE CHOLP( A, N, NN, NULLTY, IFAULT )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine computes the Cholesky decomposition of a matrix.
!!
!!    Given a symmetric matrix of dimension N stored as a lower triangle in the vector A(*),
!!    this subroutine calculates the lower triangular matrix L(*), such that L(transpose)*L=A.
!!    However, the vector L overwrites the vector A.
!!
!!
!!  Notes:
!!    1. The matrix A must be positive semi-definite.
!!    2. The factor ETA is set for determining an effective zero for the pivoting operations.
!!           ETA = 1.0E-5 for a 32 bit machine
!!           ETA = 1.0E-9 for a 48 to 64 bit machine
!!    3. An efficient method for inverting submatrices is given in Freedman, P.R. (1982),
!!       Remark AS R44, Applied Statistics, Vol. 31, pp. 336-339.
!!
!!
!!  Reference:
!!    Algorithm AS 6, Triangular Decomposition of a Symmetric Matrix
!!    Applied Statistics, (Journal of the Royal Statistical Society, Series C), 1968,
!!    Vol. 17, p. 195
!!
!!
!!  Formal Parameters:
!!    Variable       Type        Description
!!    --------  ---------------  -------------------------------------------------------------------
!!    A         Real Array (NN)  The input matrix stored as a one-dimensional array in the sequence
!!                               A(1,1), A(2,1), A(2,2), A(3,1), A(3,2), A(3,3), A(4,1) ... On ouput
!!                               the A matrix is overwritten by the decomposition.
!!
!!    N         Integer          Input: The order (dimension) of A
!!
!!    NN        Integer          Input: The size of the A and L arrays.  It must always be N(N+1)/2.
!!
!!    NULLTY    Integer          Output: The nullity of A, i.e., the number of L(i,i) that have been
!!                               set to zero.  The rank of A is N-NULLTY.
!!
!!    IFAULT    Integer          Output fault indicator equal to:
!!                                 1 if N is less than 1
!!                                 2 if A is not positive semidefinite
!!                                 3 if NN .NE. N(N+1)/2
!!                                 0 otherwise.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** To convert to double precision, change REAL to DOUBLE PRECISION,
!     ABS to DABS, SQRT to DSQRT, change ETA as above and give double
!     precision versions of the constants in the data statement
!
      REAL :: A(NN), ETA, ETA2, X, W, ZERO, ZABS, ZSQRT
!
      DATA ETA, ZERO / 1.0E-5, 0.0 /
!
!---- Executable code -------------------------------------------------------------
!
! *** Inline function definition
!
      ZABS(X)  = ABS(X)
      ZSQRT(X) = SQRT(X)
!
! *** Error checking on the inputs
!
      IFAULT = 1
      IF( N .LE. 0 ) RETURN
!
      IFAULT = 3
      IF( NN .NE. N*(N+1)/2 ) RETURN
!
      IFAULT = 2
!
! *** Set up constants
!
      ETA2 = ETA * ETA
!
! *** Start of the decomposition
!
      J  = 1
      K  = 0
      II = 0
      NULLTY = 0
!
      DO 80 ICOL = 1, N
        II = II + ICOL
        X  = ETA2 * A(II)
        L  = 0
        KK = 0
        DO 40 IROW = 1, ICOL
          KK = KK + IROW
          K  = K + 1
          W  = A(K)
          M  = J
          DO 10 I = 1, IROW
            L = L + 1
            IF( I .EQ. IROW ) GO TO 20
            W = W - A(L)*A(M)
            M = M + 1
   10     CONTINUE
   20     IF( IROW .EQ. ICOL ) GO TO 50
          IF( A(L) .EQ. ZERO ) GO TO 30
          A(K) = W / A(L)
          GO TO 40
   30     IF( W*W .GT. ZABS(X*A(KK)) ) RETURN
          A(K) = ZERO
   40   CONTINUE
   50   IF( ZABS(W) .LE. ZABS(ETA*A(K)) ) GO TO 60
        IF( W .LT. ZERO ) RETURN
        A(K) = ZSQRT(W)
        GO TO 70
   60   A(K) = ZERO
        NULLTY = NULLTY + 1
   70   J = J + ICOL
   80 CONTINUE
!
      IFAULT = 0
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE CORRPT( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine performs the following functions for the correlation matrix:
!!
!!      1. Computes the product-moment correlation matrix for the generated variables that
!!         were declared "stochastic"
!!
!!      2. Outputs the computed matrix to the report file
!!
!!      3. Outputs the desired rank correlation matrix to the report file.
!!
!!      4. Computes the maximum difference between the requested rank correlation matrix and the
!!         computed product-moment correlation matrix and outputs it to the report file.
!!
!!
!!  Calling Sequence:
!!    This subroutine must be preceeded by a call to subroutine UNIV to compute the mean
!!    XSTATS(*,3) and standard deviation XSTATS(*,5) for each of the stochastic variables.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 13 Jul 1993 : Add check on large SUM
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Data_Mod
      USE Stochastic_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'CORRPT'
      REAL(KIND=8) :: SUMX
      INTEGER :: IDX, I, J, K, IEND
      REAL :: SMALL, RLARGE, CDIF
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Page header and problem title
!
      CALL PAGER( IRPT )
      WRITE(IRPT,1000) TITLE
 1000 FORMAT('Title: ',A)
      WRITE(IRPT,1010)
 1010 FORMAT(/'Correlation matix of the generated data.')
!
! *** Compute the correlations
!
      IDX = 0
      DO I = 1, P
        DO J = 1, I
          SUMX = 0.0D0
          DO K = 1, N
            SUMX = SUMX + DBLE(X(K,I)-XSTATS(I,3))*DBLE(X(K,J)-XSTATS(J,3))
          END DO
          IDX = IDX + 1
          DATA SMALL /1.0E-20/
          DATA RLARGE /1.0E+37/
          IF( XSTATS(I,5).GT.SMALL .AND. XSTATS(J,5).GT.SMALL) THEN
            IF( SUMX .GT. RLARGE ) THEN
              IERR = 1
              MESSAG(1)='Data range is beyond the capability of'
              MESSAG(2)='single precision arithemetic.  Change'
              MESSAG(3)='your variable data definitions.'
              CALL PRTERR( IERR, CALLER, 3 )
              RETURN
            END IF
            CORO(IDX) = (SUMX/N) / (XSTATS(I,5)*XSTATS(J,5))
          ELSE
            CORO(IDX) = 0.0
            IERR = 1
          END IF
        END DO
      END DO
!
      IF( IERR .NE. 0 ) THEN
        MESSAG(1)='One or more stochastic variables had a variance of essentially zero.'
        MESSAG(2)='Change the variables definitions using the VARIABLE keyword.'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Output the computed correlation matrix
!
      IDX = 1
      IEND = 0
      WRITE(IRPT,1030) (VNAME(MAP(K)),K=1,P)
 1030 FORMAT(/10X,50(2X,A9))
      DO I = 1, P
        IEND = IEND + I
        WRITE(IRPT,1040) VNAME(MAP(I)),(CORO(K),K=IDX,IEND)
 1040   FORMAT(1X,A9,1P,50(1X,E10.3))
        IDX = IDX + I
      END DO
!
! *** Output the input correlation matrix
!     Assume that a maximum of 50 variables will be generated
!
      WRITE(IRPT,1015)
 1015 FORMAT(//'Input rank correlation matix.')
!
! *** If a correlation matrix was not entered then build it here
!
      IF( CIDENT ) THEN
        IDX = 0
        DO I = 1, P
          DO J = 1, I
            IDX = IDX + 1
            IF( I .EQ. J ) THEN
              CORSAV(IDX) = 1.0
            ELSE
              CORSAV(IDX) = 0.0
            END IF
          END DO
        END DO
      END IF
!
      IDX = 1
      IEND = 0
      WRITE(IRPT,1030) (VNAME(MAP(K)),K=1,P)
      DO I = 1, P
        IEND = IEND + I
        WRITE(IRPT,1040) VNAME(MAP(I)), (CORSAV(K),K=IDX,IEND)
        IDX = IDX + I
      END DO
!
! *** Determine the maximum difference between the computed product
!     moment correlation matrix and the input rank correlation matrix
!
      CDIF = 0.0
      DO I = 1, MXPP
        CDIF = AMAX1( ABS(CORSAV(I)-CORO(I)), CDIF )
      END DO
!
      WRITE(IRPT,1080) CDIF
 1080 FORMAT(/'The maximum difference between the input rank correlation matrix and the',&
              /'computed product moment correlation matrix is ',1P,E10.3)
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE DATRPT( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine writes the computed data to the report file.  The data is written out in
!!    pages to facilitate keeping a copy of the the report file in a notebook.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Errors_Mod
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Data_Mod
      USE Stochastic_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call List variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'DATRPT' ! Name of this routine
      INTEGER :: I, J, K, IEND, JNDX
!
!---- Executable code -------------------------------------------------------------
!
      DO I = 1, PTOT, 5
        IEND = MIN(PTOT,(I+4))
!       Assume that a page has 50 lines of data
        DO J = 1, N
          IF( MOD(J,50) .EQ. 1 ) THEN
! ***       Page header and problem title
            CALL PAGER( IRPT )
            WRITE(IRPT,1000,ERR=9999) TITLE
 1000       FORMAT('Title: ',A)
            WRITE(IRPT,1005,ERR=9999)
 1005       FORMAT(/'Echo of all generated data for all variables.')
            WRITE(IRPT,1010,ERR=9999) (VNAME(K),K=I,IEND)
 1010       FORMAT(/7X,5(4X,A9))
          END IF
! ***     Data output
          JNDX = J
          CALL SMOVE( JNDX )
          WRITE(IRPT,1020,ERR=9999) J,(XOUT(K),K=I,IEND)
 1020     FORMAT(1X,I6,1P,5(1X,E12.5))
        END DO
!
      END DO
!
! *** Normal return point
      RETURN
!
! *** Write error encountered
 9999 CONTINUE
      IERR = 1
      MESSAG(1) = 'Write error encountered in the output file'
      CALL PRTERR( IERR, CALLER, 1 )
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE ECHO( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine schedules the writing of a banner page to the report file.  In addition,
!!    it writes a summary of the files used and the problem definition.
!!
!!
!!  Calling sequence:
!!    1. A call to this subroutine must be preceeded with calls to IDEN_SET and QA_SET.
!!    2. A call to this subroutine must be preceeded with a call to INPUTS.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Files_Mod
      USE Control_Mod
      USE Data_Mod
      USE Stochastic_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      INTEGER :: I ! Looping control
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Page header and problem title
      CALL PAGER( IRPT )
      WRITE(IRPT,1000) TITLE
 1000 FORMAT('Title: ',A)
!
! *** Give a summary of input and output file names
!
      WRITE(IRPT,1100)
 1100 FORMAT(/'Summary of files used')
!
      WRITE(IRPT,1110) FNERR
 1110 FORMAT(/'Error message file'/'Name: ',A)
!
      WRITE(IRPT,1120) FNKEY
 1120 FORMAT(/'Input keyword control file'/'Name: ',A)
!
      WRITE(IRPT,1130) FNDAT
 1130 FORMAT(/'Output data file'/'Name: ',A)
!
! *** Summary of variables and distributions
!
      WRITE(IRPT,1195) PTOT, P, PCON, N, DSEED
 1195 FORMAT(// &
        'The run contains ',I0,' total variables.'/ &
        'Of them, ',I0,' are "stochastic" and ',I0,' are "constant".'/ &
        'A total of ',I0,' iterations of data will be generated.'/ &
        'The random number seed is ',1P,D17.10)
!
      WRITE(IRPT,1200)
 1200 FORMAT(/'Summary of variables and distributions for this run.')
      DO I = 1, PTOT
        CALL PRTDST( I, IERR )
        IF( IERR .NE. 0 ) RETURN
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE ERRCHK( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine performs error checking on the inputs for the problem definition.
!!
!!
!!  Notes:
!!    1. Initial error checking is performed in subroutine INPUTS. This subroutine performs error
!!       checks for consistency of the input data.
!!    2. As much as possible, this subroutine outputs messages for all errors in the input data
!!       before execution is terminated.
!!
!!
!!  Calling Sequence:
!!    This subroutine must be preceeded with a call to subroutine INPUTS.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Data_Mod
      USE Files_Mod
      USE Stochastic_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'ERRCHK'
      CHARACTER(LEN=10) :: CH1, CH2, CH3
      INTEGER :: IRNG, I, K, INDX, IFAULT, NULLTY, NTMP, NUSER, NSTRT
!
      REAL, ALLOCATABLE :: XTBL(:), FTBL(:) ! User table storage space
!
!---- Executable code -------------------------------------------------------------
!
      BADDAT = .FALSE.
      IERR = 0
!
! ---------------------------------------------------------------------
!        Checks done on definitions of statistical distributions
! ---------------------------------------------------------------------
!
      IRNG = 0
      DO I = 1, PTOT
!
        IF( VTYPE(I).LT.0 .OR. VTYPE(I).GT.MXDIST ) THEN
          IERR = 1
          WRITE(CH1,'(I0)') I
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Variable type index out of range'
          MESSAG(3) = 'Problem with variable with index '//TRIM(CH1)
          MESSAG(4) = 'Modify the VARIABLE keyword'
          CALL PRTERR( IERR, CALLER, 4 )
          BADDAT = .TRUE.
        END IF
!
        IF( VNAME(I) .EQ. ' ' ) THEN
          IERR = 2
          WRITE(CH1,'(I0)') I
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Variable name was not supplied'
          MESSAG(3) = 'Problem with variable with index '//TRIM(CH1)
          MESSAG(4) = 'Modify the VARIABLE keyword'
          CALL PRTERR( IERR, CALLER, 4 )
          BADDAT = .TRUE.
        END IF
!
      END DO
!
! ---------------------------------------------------------------------
!          Check for the minimum set of required files
! ---------------------------------------------------------------------
!
!  The file names are initialized to ' ' in subroutine INPUTS
!  FNRPT : File name for unit IRPT - Output report file
!  FNDAT : File name for unit IDAT - Output data file
!
      IF( FNRPT .EQ. ' ' ) THEN
        IERR = 3
        MESSAG(1) = 'Terminal error encountered'
        MESSAG(2) = 'File name not defined for the report file'
        MESSAG(3) = 'Modify the FILE REPORT keyword'
        CALL PRTERR( IERR, CALLER, 3 )
        BADDAT = .TRUE.
      END IF
!
      IF( FNDAT .EQ. ' ' ) THEN
        IERR = 4
        MESSAG(1) = 'Terminal error encountered'
        MESSAG(2) = 'File name not defined for the data file'
        MESSAG(3) = 'Modify the FILE DATA keyword'
        CALL PRTERR( IERR, CALLER, 3 )
        BADDAT = .TRUE.
      END IF
!
! ---------------------------------------------------------------------
!            Checks done on the correlation matrix
! ---------------------------------------------------------------------
!
      IF( .NOT.CIDENT ) THEN
!
! ***   Check on the number of values entered
!
        NP = P * (P+1) / 2
        IF( NCOR .NE. NP ) THEN
          IERR = 5
          WRITE(CH1,'(I0)') NCOR
          WRITE(CH2,'(I0)') NP
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Correlation matrix has improper number of entries'
          MESSAG(3) = 'Values entered: '//TRIM(CH1)
          MESSAG(4) = 'Values required: '//TRIM(CH2)
          MESSAG(5) = 'The correlation matrix applies only to nonconstant variables.'
          CALL PRTERR( IERR, CALLER, 5 )
          BADDAT = .TRUE.
        END IF
!
! ***   Save the correlation matrix and check the individual entries
!       Individual values must fall in the range -1 to 1 and the
!       diagonal terms must all be equal to 1
!
        IF( NCOR .GT. 0 ) THEN
          IRNG = 0
          DO I = 1, NP
            IF( COR(I).LT.-1.0 .OR. COR(I).GT.1.0 ) IRNG = 1
          END DO
          INDX = 0
          DO I = 1, P
            INDX = INDX + I
            IF( COR(INDX) .NE. 1.0 ) IRNG = 1
          END DO
          IF( IRNG .NE. 0 ) THEN
            IERR = 6
            MESSAG(1) = 'Terminal error encountered'
            MESSAG(2) = 'The correlation matrix input contained entries'
            MESSAG(3) = 'either less than -1 or greater than 1'
            MESSAG(4) = 'or the diagonal terms were not all equal to 1'
            MESSAG(5) = 'Modify the CORRELATION keyword.'
            CALL PRTERR( IERR, CALLER, 5 )
            BADDAT = .TRUE.
          END IF
        END IF
!
! ***   Cholesky decomposition of the correlation matrix - stored as
!       a vector.  The matrix is overwritten by the decomposition.
!       This decomposition is needed later, and, it checks to see
!       if the input matrix is positive definite (i.e. the input
!       values form a consistent set of pairwise correlations).
!       Don't attempt the decomposition if the data set was already
!       found to contain errors.
!
        IF( .NOT.BADDAT ) THEN
          CALL CHOLP( COR, P, NP, NULLTY, IFAULT )
          IF( IFAULT .NE. 0 ) THEN
            IERR = 7
            MESSAG(1) = 'Terminal error encountered'
            MESSAG(2) = 'The correlation matrix was not positive definite.'
            MESSAG(3) = 'Modify the CORRELATION keyword.'
            CALL PRTERR( IERR, CALLER, 3 )
            BADDAT = .TRUE.
          END IF
        END IF
!
      END IF
!
! ---------------------------------------------------------------------
!            Checks done on the number of iterations
! ---------------------------------------------------------------------
!
      NTMP = 1.25*P
      NTMP = MAX(NTMP,P+1)
      IF( N .LT. P ) THEN
        IERR = 8
        WRITE(CH1,'(I0)') N
        WRITE(CH2,'(I0)') P
        WRITE(CH3,'(I0)') NTMP
        MESSAG(1) = 'Terminal error encountered'
        MESSAG(2) = 'The number of iterations entered was '//TRIM(CH1)
        MESSAG(3) = 'It must be as large as the number of stochastic variables: '//TRIM(CH2)
        MESSAG(4) = 'Recommended value is at least: '//TRIM(CH3)
        MESSAG(5) = 'Modify the ITERATE keyword.'
        CALL PRTERR( IERR, CALLER, 5 )
        BADDAT = .TRUE.
      END IF
!
      IF( N .LT. NTMP ) THEN
        IERR = 0
        WRITE(CH1,'(I0)') N
        WRITE(CH2,'(I0)') P
        WRITE(CH3,'(I0)') NTMP
        MESSAG(1) = 'Warning message'
        MESSAG(2) = 'The number of iterations entered was '//TRIM(CH1)
        MESSAG(3) = 'It must be as large as the number of stochastic variables: '//TRIM(CH2)
        MESSAG(4) = 'Recommended value is at least: '//TRIM(CH3)
        MESSAG(5) = 'You may wish to modify the ITERATE keyword.'
        CALL PRTERR( IERR, CALLER, 5 )
      END IF
!
! ---------------------------------------------------------------------
! Checks done on the choice and definition of statistical distributions
! ---------------------------------------------------------------------
!
      ALLOCATE( XTBL(MAXTBL), STAT=IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 20
        MESSAG(1) = 'Error allocating memory for XTBL'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( FTBL(MAXTBL), STAT=IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 21
        MESSAG(1) = 'Error allocating memory for FTBL'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** The fudge on NUSER and NSTRT are to prevent an illegal index
!     error in SINV when a user distribution has NOT been defined
!
      DO I = 1, PTOT
        IF( VTYPE(I) .EQ. 14 ) THEN
          NSTRT = PTABLE(I,1)
          NUSER = PTABLE(I,2)
          DO K = 1, NUSER
            XTBL(K) = XTABLE(NSTRT+K-1)
            FTBL(K) = FTABLE(NSTRT+K-1)
          END DO
        ELSE
          NUSER = 1
          NSTRT = 1
        END IF
        CALL SINV( VTYPE(I), VTLIM(I,1), VTLIM(I,2), UMIN(I), UMAX(I), &
            VPARMS(I,1), VPARMS(I,2), VPARMS(I,3), VPARMS(I,4), &
            VTRUN(I), NUSER, FTBL, XTBL, IERF, BADDAT )
      END DO
      DEALLOCATE( XTBL )
      DEALLOCATE( FTBL )
!
! ---------------------------------------------------------------------
!                Stop if any bad data were detected
! ---------------------------------------------------------------------
!
      IF( BADDAT ) THEN
        IERR = 9999
        MESSAG(1)='One or more errors were encountered in the input data set'
        MESSAG(2)='for this sensitivity case.  The input keyword file is'
        MESSAG(3)='File: ' // TRIM(FNKEY)
        MESSAG(4)='The keyword data must be modified before continuing.'
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE FIRSTF( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine handles opening the first three files needed by the simulation.
!!      1) Input file containing "case" names
!!      2) Ouput error log file
!!      3) Input keyword file
!!    Execution is terminated if any file action is not completed successfully.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Iden_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'FIRSTF' ! Name of this routine
      LOGICAL :: THERE
      CHARACTER(LEN=256) :: FCASE
      INTEGER :: LENCASE, LENPERIOD
!
!---- Executable code -------------------------------------------------------------
!
! *** Obtain the file containing the file "case" names
      IMID = 20
      FNMID = 'SENS.ID'
      INQUIRE(FILE=FNMID,EXIST=THERE)
      IF( THERE ) THEN
        OPEN(IMID,FILE=FNMID,STATUS='OLD',ERR=9010)
      ELSE
        WRITE(*,*) ' The file containing the run "case" names was not found.'
        WRITE(*,*) ' The run cannot continue without this file.'
        WRITE(*,*) ' File: ' // TRIM(FNMID)
        STOP 'FNMID'
      END IF
!
! *** Get the case names for the other file names
!     Read and discard the case name for the base case
      READ(IMID,*,END=9020,ERR=9020) FCASE
      READ(IMID,*,END=9020,ERR=9020) FCASE
!
! *** Close the file
      CLOSE(IMID)
!
! *** Check on the case name
      LENCASE = LEN_TRIM( FCASE )
!
! *** Look for the first embedded period
      LENPERIOD = INDEX( FCASE, '.' )
      IF( LENPERIOD .GT. 0 ) THEN
        LENCASE = LENPERIOD - 1
      END IF
!
! *** Error check on the length of the case name
      IF( LENCASE .LT. 1 ) THEN
        WRITE(*,*) 'The number of characters in the "case" name is 0.'
        WRITE(*,*) 'Check the file ' // TRIM(FNMID)
        WRITE(*,*) 'To see that it contains proper case names.'
        STOP 'LENCASE'
      END IF
!
! *** Now form the error message file name
      IERF = 21
      FNERR = FCASE(1:LENCASE) // '.LER'
!
! *** Open the output error file
      INQUIRE(FILE=FNERR,EXIST=THERE)
      IF( THERE ) THEN
        OPEN(IERF,FILE=FNERR,ERR=9000)
        CLOSE(IERF,STATUS='DELETE')
        OPEN(IERF,FILE=FNERR,ERR=9000)
      ELSE
        OPEN(IERF,FILE=FNERR,ERR=9000)
      END IF
      CALL PAGER( IERF )
!
! *** Map output from PRTERR to the error file
      IRPT_ERR = IERF
!
! *** Obtain the input keyword file name
      IKEY = 22
      FNKEY = FCASE(1:LENCASE) // '.KEY'
!
! *** Open the input keyword file
      INQUIRE(FILE=FNKEY,EXIST=THERE)
      IF( THERE ) THEN
        OPEN(IKEY,FILE=FNKEY,STATUS='OLD',ERR=9010)
      ELSE
        IERR = 1
        MESSAG(1) = 'Error opening the input keyword file to ' // TRIM(PRGNAM)
        MESSAG(2) = 'The run cannot continue without this file.'
        MESSAG(3) = 'File not Found: ' // TRIM(FNKEY)
        CALL PRTERR( IERR, CALLER, 3 )
        STOP 'FNKEY'
      END IF
!
! *** Message for the user
      WRITE(*,*) 'Working on Sensitivity Case ' // FCASE(1:LENCASE)
!
      RETURN
!
! *** Error opening the output error file
 9000 CONTINUE
!
      WRITE(*,*) 'Error opening the output file that will contain error messages.'
      WRITE(*,*) 'The run cannot continue without this file.'
      WRITE(*,*) 'File: ' // TRIM(FNERR)
      STOP
!
! *** Error opening the input case name file
 9010 CONTINUE
!
      WRITE(*,*) 'Error opening the input file containing "case" names.'
      WRITE(*,*) 'The run cannot continue without this file.'
      WRITE(*,*) 'File: ' // TRIM(FNMID)
      STOP
!
! *** Error reading the input case name file
!
 9020 CONTINUE
!
      WRITE(*,*) 'Error reading the input file containing "case" names.'
      WRITE(*,*) 'The run cannot continue without this file.'
      WRITE(*,*) 'File: ' // TRIM(FNMID)
      STOP
!
      END SUBROUTINE
!
!
      FUNCTION GAMAIN( X, P, LGAMMA, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This function computes the incomplete Gamma ratio for positive values of the arguments X
!!    and P.  In other words, this function evaluates the cumulative distribution function for
!!    the Gamma distribution with a single parameter (i.e. X**(P-1)) in the density).
!!
!!
!!  Method:
!!     This function uses a series expansion if P.GT.X or X.LE.1, otherwise a continued
!!     fraction approximation is used.
!!
!!
!!  Formal Parameters:
!!    Variable  Definition
!!    --------  -------------------------------------------------------------------------------
!!    X         Input - Real: The value X at which to evaluate the standard Gamma distribution.
!!
!!    P         Input - Real: The power parameter (i.e. X**(P-1)) for the Gamma density.
!!
!!    LGAMMA    Input - Real: The logarithm of the complete gamma function with argument P.
!!
!!
!!  Reference:
!!    Algorithm AS 32
!!    Journal of the Royal Statistical Society, Series C
!!    (Applied Statistics). 1970,  Vol. 19,  No. 3
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Call list variables
      REAL :: X, P, LGAMMA
      INTEGER :: IERR
!
! *** Local variables
      DIMENSION PN(6)
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Define accuracy and initialize
!
      ACU  = 1.0E-8
      OFLO = 1.0E+30
      GIN  = 0.0
!
! *** Test for admissibility of arguments
!
      IF( P .LE. 0.0 ) THEN
        IERR = 1
        RETURN
      END IF
!
      IF( X .LT. 0.0 ) THEN
        IERR = 2
        RETURN
      END IF
!
      IF( X .EQ. 0.0 ) GO TO 50
!
! *** Change to pass in the log of the complete Gamma function
!
      G = LGAMMA
      FACTOR = EXP(P * ALOG(X) - X - G)
      IF( X.GT.1.0 .AND. X.GE.P ) GO TO 30
!
! *** Calculation by series expansion
!
      GIN = 1.0
      TERM = 1.0
      RN = P
   20 RN = RN + 1.0
      TERM = TERM * X / RN
      GIN = GIN + TERM
      IF (TERM .GT. ACU) GOTO 20
      GIN = GIN * FACTOR / P
      GOTO 50
!
! *** Calculation by continued fraction
!
   30 A = 1.0 - P
      B = A + X + 1.0
      TERM = 0.0
      PN(1) = 1.0
      PN(2) = X
      PN(3) = X + 1.0
      PN(4) = X * B
      GIN = PN(3) / PN(4)
   32 A = A + 1.0
      B = B + 2.0
      TERM = TERM + 1.0
      AN = A * TERM
      DO 33 I = 1, 2
   33 PN(I+4) = B * PN(I+2) - AN * PN(I)
      IF (PN(6) .EQ. 0.0) GOTO 35
      RN = PN(5) / PN(6)
      DIF = ABS(GIN - RN)
      IF (DIF .GT. ACU) GOTO 34
      IF (DIF .LE. ACU * RN) GOTO 42
   34 GIN = RN
   35 DO 36 I = 1, 4
   36 PN(I) = PN(I+2)
      IF (ABS(PN(5)).LT.OFLO) GOTO 32
      DO 41 I = 1, 4
   41 PN(I) = PN(I) / OFLO
      GOTO 32
   42 GIN = 1.0 - FACTOR * GIN
!
   50 GAMAIN = GIN
!
      RETURN
      END FUNCTION
!
!
      SUBROUTINE GINVER( U, X, P, A, LGAMMA, UHGH, XHGH, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine computes the inverse of the gamma distribution with parameters A and P.
!!    The method used is a bisection method to solve F(X) = U for X.
!!
!!
!!  Auxiliary Routines:
!!    The function GAMAIN is used to compute the cumulative distribution function for the
!!    Gamma(P) distribution.
!!
!!
!!  Formal Parameters:
!!    Variable  Definition
!!    --------  --------------------------------------------------------
!!    U         Input - Real: The uniform value to invert to the value
!!              X from a Gamma distribution.
!!
!!    X         Output - Real: The value X which satisfies F(X) = U,
!!              where F is the Gamma distribution parameters A and P.
!!
!!    P         Input - Real: The power parameter (i.e. X**(P-1)) for th
!!              Gamma density.
!!
!!    A         Input - Real: The multiplier on X in the exponent of
!!              the distribution function.  The standard gamma function
!!              has A = 1.
!!
!!    UHGH      Input - Real: A value in the range (0,1) near 1 where
!!              if U .GE. UHGH, X is assigned to XHGH.  It is suggested
!!              to set UHGH = 0.99999 on a 32-bit machine.
!!
!!    XHGH      Input - Real: A value of X which is greater than any
!!              expected from the gamma distribution being generated.
!!              If XHGH is set to zero on entry, it is calculated in
!!              this subroutine.  It is suggested to set UHGH = 0.99999,
!!              and find the XHGH which maps to this value or a larger
!!              value for UHGH.
!!
!!    LGAMMA    Input - Real: The natural log of the complete gamma
!!              function with A=1.  If LGAMMA is set to zero on entry,
!!              it will be computed internally, otherwise the value
!!              entered will be used.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** User defined functions
      REAL, EXTERNAL :: ALNGAM
      REAL, EXTERNAL :: GAMAIN
!
! *** Call list variables
      REAL :: U, X, P, A, LGAMMA, UHGH, XHGH
      INTEGER :: IERR
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Check for computation of LGAMMA
      IF( LGAMMA .EQ. 0.0 ) THEN
        LGAMMA = ALNGAM(P,IFT)
        IF( IFT .NE. 0 ) THEN
          IERR = 1
          RETURN
        END IF
      END IF
!
! *** XLOW = minimum possible value for X
! *** XHGH = Value corresponding to the uniform level of UHGH or higher.
      XLOW = 0.0
!
! *** Start with the mean plus 3 standard deviations when looking for XHGH
      XHGH = P + 3.0*P
   10 CONTINUE
      G = GAMAIN( XHGH, P, LGAMMA, IERR )
      IF( IERR .NE. 0 ) RETURN
      IF( G .LT. UHGH ) THEN
        XHGH = XHGH + P
        GO TO 10
      END IF
!
! *** Check for a large value of X corresponding to a large U
      IF( U .GE. UHGH ) THEN
        XMID = XHGH
        GO TO 30
      END IF
!
! *** Iterate to solve for X given U
!     ITER = 21 yields 2**(-21) = XHGH * 4.8E-7 Accuracy on X
      ITER = 21
      DO I = 1, ITER
        XMID = (XLOW+XHGH)/2.0
        GMID = GAMAIN( XMID, P, LGAMMA, IERR )
        IF( IERR .NE. 0 ) RETURN
        IF( GMID .GT. U ) THEN
          XHGH = XMID
        ELSE
          XLOW = XMID
        END IF
      END DO
!
   30 CONTINUE
!
      X = XMID / A
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE IDEN_SET
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine sets the program name, version number and modification date.  In addition,
!!    it gets the system date and time and generates identification variables for this run.
!!
!!
!!  Operating system:
!!    This subroutine uses standard Fortran 95 calls, there is no operating system dependency.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Local variables
      CHARACTER(LEN=10) :: SDATE ! System date in the form YYYYMMDD
      CHARACTER(LEN=10) :: STIME ! System time in the form HHMMSS.SSS
!
!---- Executable code --------------------------------------------------
!
! *** User name
      USRNAM = 'Anonymous User'
!
! *** Program name and version number
      PRGNAM = 'LATIN'
      PRGVER = '2.0.00'
!
! *** Program date
      PRGDAT = '16 Apr 2009'
!
! *** System time in the (CHARACTER) form hh:mm:ss
      CALL TIME( SYSTIM )
!
! *** System date in the (CHARACTER) form mm-dd-yy
      CALL DATE( SYSDAT )
!
! *** Get the date and time from the operating system
      CALL DATE_AND_TIME( SDATE, STIME )
!
! *** System time in the (CHARACTER) form hh:mm:ss
      SYSTIM = STIME(1:2)//':'//STIME(3:4)//':'//STIME(5:10)
!
! *** System date in the (CHARACTER) form mm-dd-yyyy
      SYSDAT = SDATE(5:6)//'-'//SDATE(7:8)//'-'//SDATE(1:4)
!
! *** Run identification number
      CRUNID = SDATE(1:8)//STIME(1:6)
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE INIT( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine allocates memory for variables than depend in the number of variables or
!!    the number of realizations and also initializes some variables to allow later output or
!!    error checking for illegal values.
!!
!!
!!  Calling Sequence:
!!    A call to this subroutine can be preceeded by calls to the
!!    subroutines IDEN_SET, QA_SET and FIRSTF.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Data_Mod
      USE Stochastic_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=4) :: CALLER = 'INIT'
      INTEGER :: IERA ! Allocate error number
      INTEGER :: I    ! Looping variable
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
!----------------------------------------------------------------------------------
!     Initial error checking
!----------------------------------------------------------------------------------
!
!     MAXN   : Maximum number of samples which can be generated
      IF( MAXP .LT. 1 ) THEN
        IERR = 1
        MESSAG(1) = 'The number of interations (samples) is less than 1'
        MESSAG(2) = 'A number of 1 or greater is required'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!     MAXP   : Maximum number of stochastic variables which can be used
      IF( MAXP .LT. 1 ) THEN
        IERR = 2
        MESSAG(1) = 'The number of stochastic variables is less than 1'
        MESSAG(2) = 'A number of 1 or greater is required'
        MESSAG(3) = 'Check for VARIABLE keyword definitions in the keyword file'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! *** Define dimension limits and allocate memory
!     MAXTBL : Maximum number of pairs of values that can be entered
!              in defining all tabular statistical distributions
!     MXPP   : Length of vector storage for symmetric matrices
      MAXTBL = MAX( MAXTBL, 1 ) ! Allocation of a vector requires at least 1
      MXPP   = MAXP*(MAXP+1)/2
!
!----------------------------------------------------------------------------------
!     Memory allocation
!----------------------------------------------------------------------------------
!
! *** Allocate memory depending on the number of samples
!
      ALLOCATE( IWORK(MAXN), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Error allocating memory for IWORK'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( RWORK(MAXN), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Error allocating memory for RWORK'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( XTMP(MAXN), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 5
        MESSAG(1) = 'Error allocating memory for XTMP'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Allocate memory depending on the number of variables
!
      ALLOCATE( XOUT(MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 6
        MESSAG(1) = 'Error allocating memory for XOUT'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( UMIN(MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 7
        MESSAG(1) = 'Error allocating memory for UMIN'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( UMAX(MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 8
        MESSAG(1) = 'Error allocating memory for UMAX'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( MAP(MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 9
        MESSAG(1) = 'Error allocating memory for MAP'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( VTYPE(MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 10
        MESSAG(1) = 'Error allocating memory for VTYPE'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( VTRUN(MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 11
        MESSAG(1) = 'Error allocating memory for VTRUN'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( VNAME(MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 12
        MESSAG(1) = 'Error allocating memory for VNAME'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( UNIQUE(MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 13
        MESSAG(1) = 'Error allocating memory for UNIQUE'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( VDESC(MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 14
        MESSAG(1) = 'Error allocating memory for VDESC'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( PTABLE(MAXP,2), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 15
        MESSAG(1) = 'Error allocating memory for PTABLE'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( VTLIM(MAXP,2), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 16
        MESSAG(1) = 'Error allocating memory for VTLIM'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( VPARMS(MAXP,4), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 17
        MESSAG(1) = 'Error allocating memory for VPARMS'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( XSTATS(MAXP,6), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 18
        MESSAG(1) = 'Error allocating memory for XSTATS'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Allocate memory depending on the number of samples and variables
!
      ALLOCATE( X(MAXN,MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 19
        MESSAG(1) = 'Error allocating memory for X'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( SCORE(MAXN,MAXP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 20
        MESSAG(1) = 'Error allocating memory for SCORE'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Allocate memory number for user supplied stochastic tables
!
      ALLOCATE( XTABLE(MAXTBL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 21
        MESSAG(1) = 'Error allocating memory for XTABLE'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( FTABLE(MAXTBL), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 22
        MESSAG(1) = 'Error allocating memory for FTABLE'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Allocate memory number for correlation matrices
!
      ALLOCATE( COR(MXPP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 24
        MESSAG(1) = 'Error allocating memory for COR'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CORA(MXPP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 25
        MESSAG(1) = 'Error allocating memory for CORA'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CORSAV(MXPP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 26
        MESSAG(1) = 'Error allocating memory for CORSAV'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      ALLOCATE( CORO(MXPP), STAT=IERA )
      IF( IERA .NE. 0 ) THEN
        IERR = 27
        MESSAG(1) = 'Error allocating memory for CORO'
        MESSAG(2) = 'System error status was '
          WRITE(MESSAG(2)(25:),*) IERA
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
!----------------------------------------------------------------------------------
!     Initialize variables
!----------------------------------------------------------------------------------
!
! *** Initialize the list of variable types to an illegal value
!     to facilitate later error checking
      DO I = 1, MAXP
        VTYPE(I)  = -1
        VNAME(I)  = ' '
        UNIQUE(I) = ' '
        VDESC(I)  = ' '
      END DO
!
! *** Set statistical distribution labels
      DLABEL(0)  = 'Constant        '
      DLABEL(1)  = 'Uniform         '
      DLABEL(2)  = 'Loguniform (10) '
      DLABEL(3)  = 'Loguniform (e)  '
      DLABEL(4)  = 'Normal          '
      DLABEL(5)  = 'Lognormal (10)  '
      DLABEL(6)  = 'Lognormal (e)   '
      DLABEL(7)  = 'Exponential     '
      DLABEL(8)  = 'Triangular      '
      DLABEL(9)  = 'Gamma           '
      DLABEL(10) = 'Beta            '
      DLABEL(11) = 'Weibull         '
      DLABEL(12) = 'Logistic        '
      DLABEL(13) = 'Cauchy          '
      DLABEL(14) = 'User Defined    '
      DLABEL(15) = 'Discrete Uniform'
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE INPUTS( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine reads the control keywords from the input file.  It stores the data and
!!    performs preliminary checking.  Messages concerning error conditions encountered are written
!!    to the "error" file.  Execution is passed back to the calling routine with an error flag
!!    set if errors are detected.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 25 Aug 1992 : Change to add PRELATIN index to the VARIABLE, DESCRIBE,
!!                                     UNIQUE, and LABEL cards.
!!    Paul W. Eslinger : 27 Aug 1992 : Change indexing scheme for the VARIABLE card.
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!
!!  Keyword Conventions:
!!
!!    The data file contains control information given by keywords. A maximum of 8 letters are
!!    used to define individual keywords.
!!
!!    If the default value is desired, the keyword definition does not need to be placed in the
!!    input file.
!!
!!    Separators between numeric values, keywords and modifiers can be a space or the characters
!!    ",", "=", ":", ";", "'", "(", or ")".
!!
!!    Keyword   : Description
!!    ----------------------------------------------------------------------------------------------
!!
!!    $         : Comment designator.  Any text or numeric values following the $ are ignored.
!!
!!    !         : Comment designator.  Any text or numeric values following the ! are ignored.
!!
!!    /         : Comment designator if used in column 1 of a input data line.  It is ignored
!!                if embedded within a line.
!!
!!    *         : Comment designator if used in column 1 of a input data line.  It is treated
!!                as a repeat indicator if it is embedded within a line.  Thus, 6*50. means
!!                the same as entering the number 50.0 six times.
!!
!!    ----------------------------------------------------------------------------------------------
!!
!!    CORRELAT  This keyword controls specification of the correlation matrix for all "stochastic"
!!              variables.  If the modifier IDENTITY is present then all other data for this
!!              keyword is ignored and the identity matrix will be used (i.e. all variables will
!!              be statistically independent).  Otherwise, several numerical entries must be made.
!!              The first entry is an integer setting the number of variables for which the
!!              correlation matrix is being defined.  Subsequent entries given the values in the
!!              correlation matrix.  Only the lower triangular part of the matrix is entered, in a
!!              sequence that is row by row.  For example, for 4 variables the following card
!!              could be used:
!!                 CORRELATion for 4 variables
!!                  1.0
!!                  0.995 1.0
!!                  0.99  0.995 1.0
!!                  0.98  0.99  0.995 1.0
!!
!!    DESCRIBE  This keyword is used to input a long description for every variable.  The
!!              description can contain up to 76 characters.  The correct format is
!!                DESCRIBE N1 N2 "String..."
!!              The string is contained between double quote marks.  The number N1 is ignored
!!              (used only in PRELATIN). The number N2 must match with a variable number on
!!              a VARIABLE card.  One DESCRIBE card must be entered for each VARIABLE card.
!!              All of the entries for this keyword must be made on the same line.
!!
!!    END       This keyword signals the end of the entire set of keyword definitions.  It must
!!              come as the last keyword of the data set.  A fatal execution error occurs if this
!!              keyword is not found.
!!
!!    EXECUTE   This keyword tells the code to execute the problem being set up.  If this keyword
!!              is not entered the data will be read and checked for errors but the requested data
!!              generation will not occur.
!!
!!    ITERATE   This keyword is used to define the number of iterations of the output data to
!!              compute.  A single numeric value follows the keyword.  It must be at least as large
!!              as the number of "stochastic" variables in the problem definition.
!!
!!    LABEL     This keyword is used to define an output label for every variable.  The label can
!!              contain up to 9 characters.  The correct format is
!!                LABEL N1 N2 "Label..."
!!              The label is contained between double quote marks.  The number N1 is ignored (used
!!              only in PRELATIN). The number N2 must match with a variable number on a VARIABLE
!!              card.  One LABEL card must be entered for each VARIABLE card.  All of the entries
!!              for this keyword must be made on the same line.
!!
!!    FILE      This keyword controls the naming of two files, the report file and the data file.
!!              The correct syntax for opening the report file is:
!!                FILE REPORT "file name"
!!              The file name can be up to 256 characters long and can contain a path in the file
!!              name.  It must be enclosed in double quote marks.  The entries for this keyword
!!              must be entered all on the same line, no continuation lines can be used.  The
!!              correct syntax for opening the data file is:
!!                FILE DATA "file name"
!!              The same conventions hold for this file name as for the report file name.  Two
!!              separate FILE keywords are required to open the two files.
!!
!!    OUTPUT    This keyword controls the output of two things to the report file.  Two modifiers
!!              can be entered:
!!                DATA     If this modifier is present then a copy of all computed data is written
!!                         to the report file.
!!                CORRELAT If this modifier is present then the correlation matrix is computed and
!!                         written to the report file.  In addition a copy of the input data matrix
!!                         is written to the report file along with a comparison of the maximum
!!                         difference between the two matrices.
!!
!!    SEED      This keyword is used to define the seed for the random number generator.  A single
!!              numerical value follows the keyword.  It must be in the range 1 to 2147483646.
!!              Even though the seed is stored and used as a double precision value it is read as a
!!              single precision value.
!!
!!    TITLE     Keyword to define a problem title.  The title can contain up to 72 characters.
!!              The correct format is
!!                 TITLE "Title string"
!!              The title is contained between double quote marks. All of the entries for this
!!              keyword must be made on the same line.
!!
!!    UNIQUE    This keyword is used to define a unique name for every variable.  The name can
!!              contain up to 7 characters.  The correct format is
!!                UNIQUE N1 N2 "Name..."
!!              The name is contained between double quote marks.  The number N1 is ignored (used
!!              only in PRELATIN).  The number N2 must match with a variable number on a VARIABLE
!!              card.  One UNIQUE card must be entered for each VARIABLE card.  All of the entries
!!              for this keyword must be made on the same line.
!!
!!    USER      Keyword to define a user name.  The user name can contain up to 16 characters.
!!              The correct format is
!!                 USER "user-name"
!!              The name is contained between double quote marks.
!!              The default name is defined in subroutine IDEN_SET.
!!
!!    VARIABLE  This keyword controls entering the definition of the statistical distribution for
!!              each variable.  The distribution can be defined as a constant.  The first entry, N1,
!!              is ignored (used only in PRELATIN).  The second entry, N2, gives the variable number.
!!              The variable numbers must complete the set 1 to K, where K is the total number of
!!              variables being defined.  Variables do not have to be defined in sequential order.
!!              The third entry, N3, defines the distribution type for the variable.  Type 0 is a
!!              constant.  In all, MXDIST (currently 15) distributions are allowed in addition to
!!              the constant distribution.  See the header for subroutine SGEN for the specific
!!              distributions.  If the distribution defines a constant, the fourth, and last entry,
!!              N4 is the desired constant.  If the distribution is of other types, then N4 is a
!!              range truncation variable.
!!                N4 = 0 : No truncation of the range of the variable.
!!                N4 = 1 : The range of the variable is truncated at the left at the value N5.
!!                N4 = 2 : The range of the variable is truncated at the right at the value N5.
!!                N4 = 3 : The range of the variable is truncated at the left at N5 and on the
!!                         right at N6.
!!
!!              If variable types 1 through 13 are desired, then the remaining numerical entries
!!              give the parameters of the distributions.  See the header for subroutine SGEN for
!!              the specific parameters for each distribution. An example for a uniform(2,4)
!!              distribution is:
!!                VARIABLE 2 TYPE 1 truncation flag 0 Uniform on 2 to 4
!!
!!              If type 14 is desired, then the user must input a table of values specifying the
!!              cumulative distribution function desired.  In this case, the next numerical entry
!!              specifies the number of (X,F(X)) pairs to be entered.  The entries must start with
!!              F(X)=0 and conclude with F(X)=1.  Subsequent entries for X and F(X) must be in
!!              strictly increasing order. An example input is:
!!                VARIABLE 1 type 14 truncation flag 0 with 3 data pairs
!!                  0.0 0.0
!!                  2.0 0.5
!!                  4.0 1.0
!!
!!              A separate VARIABLE card must be used to define each variable.
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Data_Mod
      USE Files_Mod
      USE RDBLK_Mod
      USE Iden_Mod
      USE Stochastic_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined external functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'INPUTS'
!
      INTEGER :: VINDEX
      INTEGER :: IDXTBL
      INTEGER :: I, LINDEX, INDX, ITYPE, NPAIRS, ITMP, NUSE
!
      CHARACTER(LEN=LENCRD) :: CARD ! CARD image from RDBLK
!     Temporary character string to match with a RDBLK quote string
      CHARACTER(LEN=LENQQQ) :: TMP_ID
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Initialize the line counter (ILINE) for subroutine RDBLK and,
      ILINE =  0
!
! ---------------------------------------------------------------------
!                 Default values for the keywords
! ---------------------------------------------------------------------
!
! *** CORRELAT Keyword
      CIDENT = .FALSE.
      NCOR = 0
!
! *** EXECUTE Keyword
      EXECUT = .FALSE.
!
! *** FILE Keyword
      FNDAT = ' '
      FNRPT = ' '
!
! *** ITERATE Keyword
      N = 0
!
! *** OUTPUT Keyword
      ODATA = .FALSE.
      OCORR = .FALSE.
!
! *** SEED Keyword
      DSEED = 123456.0D0
!
! *** TITLE Keyword
      TITLE = 'Title Not Entered by the User'
!
! *** USER Keyword
      USRNAM = 'Anonymous User'
!
! *** VARIABLE Keyword
      P = 0
      PCON = 0
      PTOT = 0
      IDXTBL = 1
!
! ---------------------------------------------------------------------
!               Top of the loop on reading input cards
! ---------------------------------------------------------------------
!
      BADKEY = .FALSE.
   10 CONTINUE
!
      CALL RDBLK( IKEY, IERF, CARD, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Terminal error encountered'
        MESSAG(2) = 'Error in lower level RDBLK routines'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! ---------------------------------------------------------------------
! CORRELAT:            Correlation matrix definition
! ---------------------------------------------------------------------
!
      IF( KNAME .EQ. 'CORRELAT' ) THEN
!
! ***   Check first for duplicate entry that could cause processing problems
        IF( CIDENT .OR. NCOR.GT.0 ) THEN
          IERR = 2
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Duplicate CORRELATION keyword exist'
          MESSAG(3) = 'Only one entry is allowed.'
          MESSAG(4) = 'Card = CORRELAT '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
! ***   Check first for use of the identity matrix
        IF( CEXIST('IDENTITY') ) THEN
          CIDENT = .TRUE.
          GO TO 10
        END IF
!
! ***   Store the correlation matrix
! ***   Also save a copy of the correlation matrix for later use
!
        NUSE = VALUE(1)
        IF( NUSE.LT.1 .OR. NUSE.GT.MAXP ) THEN
          IERR = 3
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Illegal number of variables for the CORRELATION keyword'
          MESSAG(3) = 'Legal values are 1 to '
            WRITE(MESSAG(4)(23:),'(I0)') MAXP
          CALL PRTERR( IERR, CALLER, 3 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        NCOR = NUSE * (NUSE+1) / 2
        DO I = 1, NCOR
          COR(I) = VALUE(I+1)
          CORSAV(I) = VALUE(I+1)
        END DO
!
        GO TO 10
!
      END IF
!
! ---------------------------------------------------------------------
! DESCRIBE:     Check for variable description
! ---------------------------------------------------------------------
!     Expect two numbers and one quote string on this keyword
!
      IF( KNAME .EQ. 'DESCRIBE' ) THEN
!
        IF( NVALUE .NE. 2 ) THEN
          IERR = 4
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the DESCRIBE keyword'
          MESSAG(3) = 'Exactly two numerical values were expected'
          MESSAG(4) = 'Card = DESCRIBE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        LINDEX = VALUE(2)
!
! ***   Variable index
        LINDEX = VALUE(2)
        IF( LINDEX.LT.1 .OR. LINDEX.GT.MAXP ) THEN
          IERR = 5
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the DESCRIBE keyword'
          MESSAG(3) = 'Illegal variable index value encountered'
          MESSAG(4) = 'Legal values are 1 to'
            WRITE(MESSAG(4)(23:),'(I0)') MAXP
          MESSAG(5) = 'Card = DESCRIBE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 5 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
! ***   Variable description
        IF( NQUOTE .EQ. 1 ) THEN
          VDESC(LINDEX) = QUOTE(1)
        ELSE
          IERR = 6
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the DESCRIBE keyword'
          MESSAG(3) = 'Exactly one quote string was expected'
          MESSAG(4) = 'Card = DESCRIBE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        GO TO 10
!
      END IF
!
! ---------------------------------------------------------------------
! END:     Check for user supplied end to the keyword file
! ---------------------------------------------------------------------
!     This is the normal exit from the keyword file
!
      IF( KNAME .EQ. 'END' ) THEN
        CLOSE(IKEY)
        GO TO 20
      END IF
!
! ---------------------------------------------------------------------
! EXECUT:         Flag for problem execution after setup
! ---------------------------------------------------------------------
!
      IF( KNAME .EQ. 'EXECUTE ' ) THEN
        EXECUT = .TRUE.
        GO TO 10
      END IF
!
! ---------------------------------------------------------------------
! FILE:                  File name options
! ---------------------------------------------------------------------
!     Expect one quote string on this keyword (two forms allowed)
!       FILE REPORT "File name"
!       FILE DATA   "File name"
!
      IF( KNAME .EQ. 'FILE    ' ) THEN
!
        IF( NQUOTE .NE. 1 ) THEN
          IERR = 7
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the FILE keyword'
          MESSAG(3) = 'Exactly one quote string was expected'
          MESSAG(4) = 'Card = FILE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        IF( CEXIST('DATA') ) THEN
          CALL NXTQOT( INDX, TMP_ID )
          IF( INDX .GT. 0 ) THEN
            FNDAT = TMP_ID
          ELSE
            IERR = 8
            MESSAG(1) = 'Terminal error encountered'
            MESSAG(2) = 'Bad DATA file name on the FILE keyword'
            MESSAG(3) = 'Card = FILE '//TRIM(CARD)
            CALL PRTERR( IERR, CALLER, 3 )
            BADKEY = .TRUE.
          END IF
          GO TO 10
        END IF
!
        IF( CEXIST('REPORT') ) THEN
          CALL NXTQOT( INDX, TMP_ID )
          IF( INDX .GT. 0 ) THEN
            FNRPT = TMP_ID
          ELSE
            IERR = 9
            MESSAG(1) = 'Terminal error encountered'
            MESSAG(2) = 'Bad report file name on the FILE keyword'
            MESSAG(3) = 'Card = FILE '//TRIM(CARD)
            CALL PRTERR( IERR, CALLER, 3 )
            BADKEY = .TRUE.
          END IF
          GO TO 10
        END IF
!
      END IF
!
! ---------------------------------------------------------------------
! ITERATE:                Number of iterations
! ---------------------------------------------------------------------
!     Expect one numerical value on this keyword
!
      IF( KNAME .EQ. 'ITERATE ' ) THEN
!
        IF( NVALUE .NE. 1 ) THEN
          IERR = 10
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the ITERATE keyword'
          MESSAG(3) = 'Exactly one numerical value was expected'
          MESSAG(4) = 'Card = ITERATE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        N = VALUE(1)
        IF( N.LT.1 .OR. N.GT.MAXN ) THEN
          IERR = 11
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the ITERATE keyword'
          MESSAG(3) = 'Number of iterations must be 1 to '
            WRITE(MESSAG(3)(35:),'(I0)') MAXN
          MESSAG(4) = 'Card = ITERATE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        GO TO 10
      END IF
!
! ---------------------------------------------------------------------
! LABEL:                Process variable labels
! ---------------------------------------------------------------------
!     Expect two numbers and one quote string on this keyword
!
      IF( KNAME .EQ. 'LABEL   ' ) THEN
!
        IF( NVALUE .NE. 2 ) THEN
          IERR = 12
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the LABEL keyword'
          MESSAG(3) = 'Exactly two numerical values were expected'
          MESSAG(4) = 'Card = LABEL '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        LINDEX = VALUE(2)
!
! ***   Variable index
        LINDEX = VALUE(2)
        IF( LINDEX.LT.1 .OR. LINDEX.GT.MAXP ) THEN
          IERR = 13
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the LABEL keyword'
          MESSAG(3) = 'Illegal variable index value encountered'
          MESSAG(4) = 'Legal values are 1 to'
            WRITE(MESSAG(4)(23:),'(I0)') MAXP
          MESSAG(5) = 'Card = LABEL '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 5 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
! ***   Variable description
        IF( NQUOTE .EQ. 1 ) THEN
          VNAME(LINDEX) = QUOTE(1)
        ELSE
          IERR = 14
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the LABEL keyword'
          MESSAG(3) = 'Exactly one quote string was expected'
          MESSAG(4) = 'Card = LABEL '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        GO TO 10
!
      END IF
!
! ---------------------------------------------------------------------
! OUTPUT:     Options for output of correlation matrix and data
! ---------------------------------------------------------------------
!
      IF( KNAME .EQ. 'OUTPUT  ' ) THEN
        IF( CEXIST('DATA    ') ) ODATA = .TRUE.
        IF( CEXIST('CORRELAT') ) OCORR = .TRUE.
        GO TO 10
      END IF
!
! ---------------------------------------------------------------------
! SEED:               Random number generator seed
! ---------------------------------------------------------------------
!     Expect one numerical value on this keyword
!
      IF( KNAME .EQ. 'SEED    ' ) THEN
!
        IF( NVALUE .NE. 1 ) THEN
          IERR = 15
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the SEED keyword'
          MESSAG(3) = 'Exactly one numerical value was expected'
          MESSAG(4) = 'Card = SEED '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        DSEED = VALUE(1)
!
! ***   Error check on the seed
        IF( DSEED.LT.1.0D0 .OR. DSEED.GT.2147483646.0D0 ) THEN
          IERR = 16
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Illegal seed on the SEED keyword'
          MESSAG(3) = 'Value ='
            WRITE(MESSAG(3)(9:),*) DSEED
          MESSAG(4) = 'Card = SEED '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        GO TO 10
!
      END IF
!
! ---------------------------------------------------------------------
! TITLE:                    Problem title
! ---------------------------------------------------------------------
!     Expect one quote string on this keyword
!
      IF( KNAME .EQ. 'TITLE   ' ) THEN
        IF( NQUOTE .EQ. 1 ) THEN
          TITLE = QUOTE(1)
        ELSE
          IERR = 17
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the TITLE keyword'
          MESSAG(3) = 'Exactly one quote string was expected'
          MESSAG(4) = 'Card = TITLE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
        END IF
        GO TO 10
      END IF
!
! ---------------------------------------------------------------------
! UNIQUE                Process unique variable names
! ---------------------------------------------------------------------
!     Expect two numbers and one quote string on this keyword
!
      IF( KNAME .EQ. 'UNIQUE  ' ) THEN
!
        IF( NVALUE .NE. 2 ) THEN
          IERR = 18
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the UNIQUE keyword'
          MESSAG(3) = 'Exactly two numerical values were expected'
          MESSAG(4) = 'Card = UNIQUE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
! ***   Variable index
        LINDEX = VALUE(2)
        IF( LINDEX.LT.1 .OR. LINDEX.GT.MAXP ) THEN
          IERR = 19
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the UNIQUE keyword'
          MESSAG(3) = 'Illegal variable index value encountered'
          MESSAG(4) = 'Legal values are 1 to'
            WRITE(MESSAG(4)(23:),'(I0)') MAXP
          MESSAG(5) = 'Card = UNIQUE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 5 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
! ***   Unique variable name
        IF( NQUOTE .EQ. 1 ) THEN
          UNIQUE(LINDEX) = QUOTE(1)
        ELSE
          IERR = 20
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the UNIQUE keyword'
          MESSAG(3) = 'Exactly one quote string was expected'
          MESSAG(4) = 'Card = UNIQUE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        GO TO 10
!
      END IF
!
! ---------------------------------------------------------------------
! USER:                    User name
! ---------------------------------------------------------------------
!     Expect one quote string on this keyword
!
      IF( KNAME .EQ. 'USER' ) THEN
        IF( NQUOTE .EQ. 1 ) THEN
          USRNAM = QUOTE(1)
        ELSE
          IERR = 21
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the USER keyword'
          MESSAG(3) = 'Exactly one quote string was expected'
          MESSAG(4) = 'Card = USER '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 4 )
          BADKEY = .TRUE.
        END IF
        GO TO 10
      END IF
!
! ---------------------------------------------------------------------
! VARIABLE:            Stochastic variable definition
! ---------------------------------------------------------------------
!
      IF( KNAME .EQ. 'VARIABLE' ) THEN
!
        INDX = 2
        VINDEX = VALUE(INDX)
!
! ***   Error check on dimensions of the variable arrays
!
        IF( VINDEX.LT.1 .OR. VINDEX.GT.MAXP ) THEN
          IERR = 22
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the VARIABLE keyword'
          MESSAG(3) = 'Illegal variable index value encountered'
          MESSAG(4) = 'Legal values are 1 to'
            WRITE(MESSAG(4)(23:),'(I0)') MAXP
          MESSAG(5) = 'Card = VARIABLE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 5 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
! ***   Determine if this variable is "constant" or "stochastic"
!
        INDX = INDX + 1
        ITYPE = VALUE(INDX)
        IF( ITYPE.LT.0 .OR. ITYPE.GT.MXDIST ) THEN
          IERR = 23
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the VARIABLE keyword'
          MESSAG(3) = 'Illegal distribution index value encountered'
          MESSAG(4) = 'Legal values are 0 to'
            WRITE(MESSAG(4)(23:),'(I0)') MXDIST
          MESSAG(5) = 'Card = VARIABLE '//TRIM(CARD)
          CALL PRTERR( IERR, CALLER, 5 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
! ***   Increment variable counters
!
        PTOT = PTOT + 1
        IF( PTOT .GT. MAXP ) THEN
          IERR = 24
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Unexpected internal logic error on the VARIABLE keyword'
          CALL PRTERR( IERR, CALLER, 2 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
        IF( ITYPE .EQ. 0 ) THEN
          PCON = PCON + 1
        END IF
        IF( ITYPE.GE.1 .AND. ITYPE.LE.MXDIST ) THEN
          P = P + 1
          IF( P .GT. MAXP ) THEN
            IERR = 25
            MESSAG(1) = 'Terminal error encountered'
            MESSAG(2) = 'Unexpected internal logic error on the VARIABLE keyword'
            CALL PRTERR( IERR, CALLER, 2 )
            BADKEY = .TRUE.
            GO TO 10
          END IF
!
        END IF
!
! ***   Store the variable type
!
        VTYPE(VINDEX) = ITYPE
!
! ***   Process the "constant" variables
!
        IF( ITYPE .EQ. 0 ) THEN
          INDX = INDX + 1
          VPARMS(VINDEX,1) = VALUE(INDX)
          VPARMS(VINDEX,2) = 0.0
          VPARMS(VINDEX,3) = 0.0
          VPARMS(VINDEX,4) = 0.0
          GO TO 10
        END IF
!
! ***   Action for the truncation flag on the variable distribution
!
        INDX = INDX + 1
        VTRUN(VINDEX) = VALUE(INDX)
        IF( VTRUN(VINDEX) .EQ. 1 ) THEN
          INDX = INDX + 1
          VTLIM(VINDEX,1) = VALUE(INDX)
        END IF
        IF( VTRUN(VINDEX) .EQ. 2 ) THEN
          INDX = INDX + 1
          VTLIM(VINDEX,2) = VALUE(INDX)
        END IF
        IF( VTRUN(VINDEX) .EQ. 3 ) THEN
          INDX = INDX + 1
          VTLIM(VINDEX,1) = VALUE(INDX)
          INDX = INDX + 1
          VTLIM(VINDEX,2) = VALUE(INDX)
        END IF
        IF( VTRUN(VINDEX).LT.0 .OR. VTRUN(VINDEX).GT.3 ) THEN
          IERR = 26
          MESSAG(1) = 'Terminal error encountered'
          MESSAG(2) = 'Bad information on the VARIABLE keyword'
          MESSAG(3) = 'The truncation index must be in the range 0 to 3'
          CALL PRTERR( IERR, CALLER, 3 )
          BADKEY = .TRUE.
          GO TO 10
        END IF
!
! ***   Pick off the parameters for the "classical" distribution types
!       1 through 13
!       They contain up to 4 parameters for the distribution
!
        IF( ITYPE.GE.1 .AND. ITYPE.LE.13 ) THEN
          INDX = INDX + 1
          VPARMS(VINDEX,1) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,2) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,3) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,4) = VALUE(INDX)
          GO TO 10
        END IF
!
! ***   Process the "stochastic" variables for type 14
!
        IF( ITYPE .EQ. 14 ) THEN
          INDX = INDX + 1
          NPAIRS = VALUE(INDX)
          PTABLE(VINDEX,1) = IDXTBL
          PTABLE(VINDEX,2) = NPAIRS
          ITMP = IDXTBL + NPAIRS - 1
          IF( ITMP .GT. MAXTBL ) THEN
            IERR = 27
            MESSAG(1) = 'Terminal error encountered'
            MESSAG(2) = 'Unexpected internal logic error on the VARIABLE keyword'
            MESSAG(3) = 'Problem with user defined variable storage'
            CALL PRTERR( IERR, CALLER, 3 )
            BADKEY = .TRUE.
            GO TO 10
          END IF
!
          DO I = 1, NPAIRS
            INDX = INDX + 1
            XTABLE(IDXTBL) = VALUE(INDX)
            INDX = INDX + 1
            FTABLE(IDXTBL) = VALUE(INDX)
            IDXTBL = IDXTBL + 1
          END DO
          GO TO 10
        END IF
!
! ***   Process the "stochastic" variables for type 15
!       They contain up to 4 parameters for the distribution
!
        IF( ITYPE.GE.15 ) THEN
          INDX = INDX + 1
          VPARMS(VINDEX,1) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,2) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,3) = VALUE(INDX)
          INDX = INDX + 1
          VPARMS(VINDEX,4) = VALUE(INDX)
          GO TO 10
        END IF
!
        GO TO 10
!
      END IF
!
! ---------------------------------------------------------------------
! *** Go try for another card (ignore undefined keywords)
! ---------------------------------------------------------------------
!
      GO TO 10
!
   20 CONTINUE
!
! *** Determine the mapping vector of indices to be able to print
!     the variable labels for stochastic variables
!
      MAP = 0 ! Matrix assignment to zero
      ITMP = 0
      DO I = 1, PTOT
        IF( VTYPE(I) .NE. 0 ) THEN
          ITMP = ITMP + 1
          MAP(ITMP) = I
        END IF
      END DO
!
! *** Exit for bad keywords
!
      IF( BADKEY ) THEN
        IERR = 999
        MESSAG(1)='One or more errors were encountered in the keyword data'
        MESSAG(2)='for this sensitivity case.  The keyword file name is'
        MESSAG(3)='File: ' // TRIM(FNKEY)
        MESSAG(4)='The keyword data must be modified before continuing.'
        CALL PRTERR( IERR, CALLER, 4 )
        RETURN
      END IF
!
! *** Normal termination for the input routine
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE INPUTS_DIM( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine reads the keyword file for the data needed to set variable dimensions.
!!
!!  Call List Variables:
!!    IERR : Output integer error flag.
!!            0 = Normal execution
!!           >0 = Terminal error encountered
!!
!!  Auxiliary Routines:
!!    CEXIST, PRTERR, and all RDBLK related routines
!!
!!  History:
!!    Paul W. Eslinger : 16 Apr 2009 : Initial Source
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
      USE Rdblk_Mod
      USE Files_Mod
      USE Control_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined external functions
      LOGICAL, EXTERNAL :: CEXIST
!
! *** Call list variables
      INTEGER :: IERR ! Error number flag
!
! *** Local variables
      CHARACTER(LEN=10) :: CALLER = 'INPUTS_DIM'
      INTEGER :: IDX   ! Temporary index variable
      INTEGER :: ITMP  ! Temporary index variable
      CHARACTER*(LENCRD) :: CARD ! Keyword card image from RDBLK
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Initialize for RDBLK
      ILINE = 0
!
! *** Initialize needed counters
      MAXN = 0   ! Number of samples
      MAXP = 0   ! Number of variables
      MAXTBL = 0 ! Number of table values for user defined distributions
!
!----------------------------------------------------------------------C
!                Top of loop on reading keyword cards
!----------------------------------------------------------------------C
!
   10 CONTINUE
!
      CALL RDBLK( IKEY, IERF, CARD, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Terminal error encountered'
        MESSAG(2) = 'Error in lower level RDBLK routines'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
      SELECT CASE ( KNAME )
!
        CASE( 'END' ) ! ===> END keyword
          REWIND( IKEY )
          RETURN
!
        CASE( 'ITERATE' ) ! ===> ITERATE Keyword
          MAXN = VALUE(1)
!
        CASE( 'VARIABLE' ) ! ===> VARIABLE keyword
          MAXP = MAXP + 1
          IDX = VALUE(3)
!
!         Count entries on user defined distributions
          IF( IDX .EQ. 14 ) THEN
!           Skip over truncation data then pick up number of pairs of data
            IF( VALUE(4) .EQ. 0 ) ITMP = 5
            IF( VALUE(4) .EQ. 1 ) ITMP = 6
            IF( VALUE(4) .EQ. 2 ) ITMP = 6
            IF( VALUE(4) .EQ. 3 ) ITMP = 7
            MAXTBL = MAXTBL + VALUE(ITMP)
          END IF
!
        CASE DEFAULT ! Ignore all other keywords
!
      END SELECT
!
! *** Go get the next keyword (Exit routine on END keyword)
      GO TO 10
!
      END SUBROUTINE
!
!
      SUBROUTINE MARGIN( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine schedules generation of the marginal statistical distribution for each of the
!!    "stochastic" variables.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!
!!  Notes:
!!    1. The matrix of column ranks of the values in X must be nonsingular, thus, the stratified
!!       values cannot be generated in a sequential fashion across the range of each variable.
!!       Subroutine U01S1 generates the values sequentially and then randomly permutes them.
!!    2. A column of the X matrix contains the values for a single variable.
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Data_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'MARGIN' ! Name of this routine
      REAL, ALLOCATABLE :: XTBL(:), FTBL(:) ! User table storage space
      INTEGER :: I, J, K, ISTOC
      INTEGER :: NSTRT, NUSER, NNEED
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Temporary work space for user (table) distributions
      ALLOCATE( XTBL(MAXTBL), STAT=IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Error allocating memory for XTBL'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      ALLOCATE( FTBL(MAXTBL), STAT=IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 2
        MESSAG(1) = 'Error allocating memory for FTBL'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Loop over all of the variable distributions
!
      ISTOC = 0
      DO I = 1, PTOT
!
        IF( VTYPE(I) .EQ. 0 ) THEN
!
! ***     Don't need the uniform(0,1) values in SGEN for constant
!         distributions, just need one value
! ***     The fudge on NUSER and NSTRT are to prevent an illegal index
!         error in SINV when a user distribution has NOT been defined
          NUSER = 1
          NSTRT = 1
          NNEED = 1
          CALL SGEN( VTYPE(I), RWORK, NNEED, UMIN(I), UMAX(I), VPARMS(I,1), VPARMS(I,2), &
            VPARMS(I,3), VPARMS(I,4), NUSER, FTBL, XTBL, IERR )
          IF( IERR .NE. 0 ) RETURN
!
          XOUT(I) = RWORK(1)
!
        ELSE
!
! ***     Stochastic distributions
          ISTOC = ISTOC + 1
!
! ***     Generate Uniform(0,1) values with stratification
          CALL U01S1( N, RWORK, IWORK, DSEED, IERR )
          IF( IERR .NE. 0 ) RETURN
!
! ***     Transform to the desired distribution
! ***     The fudge on NUSER and NSTRT are to prevent an illegal index
!         error in SINV when a user distribution has NOT been defined
          IF( VTYPE(I) .EQ. 14 ) THEN
            NSTRT = PTABLE(I,1)
            NUSER = PTABLE(I,2)
            DO K = 1, NUSER
              XTBL(K) = XTABLE(NSTRT+K-1)
              FTBL(K) = FTABLE(NSTRT+K-1)
            END DO
          ELSE
            NUSER = 1
            NSTRT = 1
          END IF
          CALL SGEN( VTYPE(I), RWORK, N, UMIN(I), UMAX(I), VPARMS(I,1), VPARMS(I,2), &
            VPARMS(I,3), VPARMS(I,4), NUSER, FTBL, XTBL, IERR )
          IF( IERR .NE. 0 ) RETURN
!
! ***     Store the generated values in the proper place
          DO J = 1, N
            X(J,ISTOC) = RWORK(J)
          END DO
!
        END IF
!
      END DO
!
      DEALLOCATE( XTBL )
      DEALLOCATE( FTBL )
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE MCORR( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine calculates the correlation matrix between the P variables which are stored
!!    in the columns of the SCORE matrix. The vector RWORK is a work vector of length at least 2.
!!
!!    On output the correlation matrix is stored as a vector in the vector CORA.  The lower
!!    triangular portion of CORA is stored by rows; containing the elements (1,1), (2,1), (2,2),
!!    (3,1), ... corresponding to a full matrix representation.
!!
!!    The variable IFAULT returns 0 for normal ternmination, or 1 for a zero or negative variance.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Data_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=5) :: CALLER = 'MCORR' ! Name of this subroutine
      REAL(KIND=8) :: SUMX
      REAL :: RN
      INTEGER :: I, J, K, IDX
!
!---- Executable code -------------------------------------------------------------
!
      RN = N
!
! *** Compute the mean and store it temporarily
!     All columns have the same mean
!
      SUMX = 0.0D0
      DO J = 1, N
        SUMX = SUMX + SCORE(J,1)
      END DO
      RWORK(1) = SUMX / RN
!
! *** Compute the variance and store it temporarily
!     All columns have the same variance
!
      SUMX = 0.0D0
      DO K = 1, N
        SUMX = SUMX + (SCORE(K,1)-RWORK(1))*(SCORE(K,1)-RWORK(1))
      END DO
      RWORK(2) = SUMX / RN
!
! *** Don't compute the correlation matrix for zero or negative variances
      IF( SUMX .LE. 0.0D0 ) THEN
        IERR = 1
        MESSAG(1) = 'A zero or negative variance was encountered for a column of the score matrix'
        MESSAG(2) = 'Unexpected internal logic error.'
        CALL PRTERR( IERR, CALLER, 2 )
        RETURN
      END IF
!
! *** Compute the covariances and normalize them to correlations
! *** Include the diagonal elements as a check, and, they are
!     needed to get the proper elements in CORA.
!
      IDX = 0
      DO I = 1, P
        DO J = 1, I
          SUMX = 0.0D0
          DO K = 1, N
            SUMX = SUMX + (SCORE(K,I)-RWORK(1))*(SCORE(K,J)-RWORK(1))
          END DO
          IDX = IDX + 1
          CORA(IDX) = (SUMX/RN) / RWORK(2)
        END DO
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE MULABT( A, B, N, NDA, P, NP, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine performas a matrix multiply of the full matrix A and the transpose of the
!!    lower triangular matrix B.
!!
!!    The matrix A(N,P) is two-dimensional with leading dimension NDA in the calling program and
!!    IT is stored in full mode.  The matrix B [nominally (P,P)] is lower triangular and is stored
!!    in a vector, row by row.
!!
!!
!!  Formal Parameters:
!!    Variable      Type        Description
!!    -------- ---------------- --------------------------------------
!!    A        Real Matrix(N,P) Input matrix.  This matrix is over written upon output.
!!
!!    B        Real Array (NP)  Input matrix stored as a one-dimensional array in the sequence
!!                              B(1,1), B(2,1), B(2,2), B(3,1), B(3,2), B(3,3), B(4,1) ...
!!
!!    N        Integer          Input: The leading dimension of A for this set of data.
!!
!!    NDA      Integer          Input: The leading dimension of A as defined in the calling program
!!
!!    P        Integer          Input: The (order) dimension of the full matrix representation of B
!!
!!    NP       Integer          Input: The size of the B array. It must always be P(P+1)/2.
!!
!!    IERR     Integer          Output fault indicator equal to:
!!                                1 if N is > NDA
!!                                2 if NP .NE. N(N+1)/2
!!                                0 otherwise.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined in-line function
      INTEGER :: IVEC
!
! *** Call list variables
      INTEGER :: NDA, N, P, NP, IERR
      REAL :: A(NDA,P), B(NP)
!
! *** Local variables
      CHARACTER(LEN=8) :: CALLER = 'MULABT' ! Name of this subroutine
      REAL(KIND=8) :: SUMX
      INTEGER :: I, J, K, I0, IB
!
!---- Executable code -------------------------------------------------------------
!
! *** In-line (Statement) function for finding indices
!
      IVEC(I,J) = (I-1)*I/2 + J
!
! *** Error checking on the inputs
      IERR = 0
!
      IF( N .GT. NDA ) THEN
        IERR = 1
        MESSAG(1) = 'Dimension mismatch on the dimension for the full matrix A'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      IF( NP .NE. P*(P+1)/2 ) THEN
        IERR = 2
        MESSAG(1) = 'Dimension mismatch on the dimension for the vector B'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Start of the multiply
! *** Overwrite A by using the lower triangular property
! *** Explicitly account for B transpose in computing the index IB
!
      I0 = 0
      DO I = 1, N
        DO J = P, 1, -1
          SUMX = 0.0D0
          IB = IVEC(J,I0)
          DO K = 1, J
            IB = IB + 1
            SUMX = SUMX + A(I,K)*B(IB)
          END DO
          A(I,J) = SUMX
        END DO
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE OPENER( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine opens the files defined in the keyword control file.  File handling errors
!!    are detected and error messages printed when appropriate.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Iden_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Open the output report file
      IRPT = 10
      OPEN(IRPT,FILE=FNRPT,ERR=9010,STATUS='UNKNOWN')
!
! *** Open the output data file
      IDAT = 11
      OPEN(IDAT,FILE=FNDAT,ERR=9020,STATUS='UNKNOWN')
!
      RETURN
!
! *** Errors opening the files
!
 9010 CONTINUE
      IERR = 1
      MESSAG(1)='The requested report file cannot be opened'
      MESSAG(2)='File: '//TRIM(FNRPT)
      RETURN
!
 9020 CONTINUE
      IERR = 2
      MESSAG(1)='The requested output data file cannot be opened'
      MESSAG(2)='File: '//TRIM(FNDAT)
      RETURN
!
      END SUBROUTINE
!
!
      SUBROUTINE PAGER( IUNIT )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine writes a one line page header to an output file.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!
!!  Variable definitions:
!!    IUNIT  : Integer unit number for the output file
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IUNIT ! Unit number for the output file
!
!---- Executable code -------------------------------------------------------------
!
      WRITE(IUNIT,7000) TRIM(PRGNAM), TRIM(PRGVER), TRIM(USRNAM), TRIM(CRUNID)
 7000 FORMAT('Program: ',A,' Version: ',A,' User: ',A,' Run ID: ',A)
!
      RETURN
      END SUBROUTINE
!
!
      REAL FUNCTION PPND( P, IFAULT )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This function evaluates the normal deviate corresponding to the lower tail area of P.  In
!!    other words, it computes the inverse of the standard normal cumulative distribution function.
!!
!!
!!  Reference:
!!    Algorithm AS 111
!!    Applied Statistics (1977)
!!    Journal of the Royal Statistical Society, Series C
!!    Vol. 26, No. 1
!!
!!
!!  Note:
!!    The hash sums are the sums of the moduli of the coefficients they have no inherent meanings,
!!    but can be used for checking transcriptions.
!!
!!    Internal double precision has been added by P.W. Eslinger
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
      REAL(KIND=8) ZERO, SPLIT, HALF, ONE
      REAL(KIND=8) A0, A1, A2, A3, B1, B2, B3, B4, C0, C1, C2, C3
      REAL(KIND=8) D1, D2, DP, Q, R
!
      DATA ZERO /0.0D0/, HALF /0.5D0/, ONE /1.0D0/
      DATA SPLIT /0.42D0/
!
      DATA A0 /   2.50662823884D0/
      DATA A1 / -18.61500062529D0/
      DATA A2 /  41.39119773534D0/
      DATA A3 / -25.44106049637D0/
!
      DATA B1 /  -8.47351093090D0/
      DATA B2 /  23.08336743743D0/
      DATA B3 / -21.06224101826D0/
      DATA B4 /   3.13082909833D0/
!
! *** HASH SUM AB 143.70383558076
!
      DATA C0 /  -2.78718931138D0/
      DATA C1 /  -2.29796479134D0/
      DATA C2 /   4.85014127135D0/
      DATA C3 /   2.32121276858D0/
!
      DATA D1 /   3.54388924762D0/
      DATA D2 /   1.63706781897D0/
!
!---- Executable code -------------------------------------------------------------
!
! *** HASH SUM CD  17.43746520924
!
      DP = DBLE( P )
!
      IFAULT = 0
!
      Q = DP - HALF
      IF( DABS(Q) .GT. SPLIT ) GO TO 1
!
      R = Q * Q
      PPND = SNGL(Q * (((A3 * R + A2) * R + A1) * R + A0) / &
          ((((B4 * R + B3) * R + B2) * R + B1) * R + ONE))
      RETURN
!
    1 R = DP
      IF( Q .GT. ZERO ) R = ONE - DP
      IF( R .LE. ZERO ) GO TO 2
      R = DSQRT( -DLOG(R) )
      PPND = SNGL((((C3 * R + C2) * R + C1) * R + C0) / &
          ((D2 * R + D1) * R + ONE))
      IF( Q .LT. ZERO ) PPND = -PPND
      RETURN
!
    2 IFAULT = 1
      PPND = SNGL( ZERO )
      RETURN
!
      END FUNCTION
!
!
      SUBROUTINE PRTDST( IVAR, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine prints the definition of the statistical distribution for a single variable
!!    to the report file.
!!
!!
!!  Call List:
!!    IVAR : Variable number of the variable information to be printed.  Do not modify.
!!    IERR : Error indicator
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 25 Aug 1992 : Add unique variable label
!!    Paul W. Eslinger : 27 Aug 1992 : Correct format numbers in the print loop
!!                                     IF( ITRUNC .EQ. 3 ) THEN ...
!!    Paul W. Eslinger :  3 Feb 1993 : Correct index on write statement for ITRUNC = 2
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Data_Mod
      USE Files_Mod
      USE Stochastic_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IVAR
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'PRTDST' ! Name of this routine
      CHARACTER(LEN=10) :: CH1, CH2
      INTEGER :: IDIST, ITRUNC, NUSER, NSTRT, NTEND
      INTEGER :: I1, I2
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Check for a legal variable number
!
      IF( IVAR.LT.1 .OR. IVAR.GT.PTOT ) THEN
        IERR = 1
        WRITE(CH1,'(I0)') IVAR
        WRITE(CH2,'(I0)') PTOT
        MESSAG(1) = 'An illegal variable number was entered.'
        MESSAG(2) = 'The value entered was '//TRIM(CH1)
        MESSAG(3) = 'It must be in the range 1 to '//TRIM(CH2)
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
      WRITE(IRPT,1010) IVAR, VNAME(IVAR), UNIQUE(IVAR)
 1010 FORMAT(/'Definition for variable number ',I0,', name ',A, &
              ', Unique label ',A)
!
! *** Long variable description if entered
!
       IF( VDESC(IVAR) .NE. ' ' ) THEN
        WRITE(IRPT,1015) VDESC(IVAR)
 1015   FORMAT(3X,A)
      END IF
!
      IDIST  = VTYPE(IVAR)
      ITRUNC = VTRUN(IVAR)
      WRITE(IRPT,1020) DLABEL(IDIST)
 1020 FORMAT('   The distribution type is ',A)
!
!---- Constant distribution
!
      IF( IDIST .EQ. 0 ) THEN
        WRITE(IRPT,1030) VPARMS(IVAR,1)
 1030   FORMAT('   The constant value is ',1P,E11.4)
        RETURN
      END IF
!
!---- Uniform
!
      IF( IDIST .EQ. 1 ) THEN
        WRITE(IRPT,1040) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1040   FORMAT('   The lower limit is ',1P,E11.4/ &
               '   The upper limit is ',E11.4)
      END IF
!
!---- Log Uniform, base 10 (2) or base e (3)
!
      IF( IDIST.EQ.2 .OR. IDIST.EQ.3 ) THEN
        WRITE(IRPT,1040) VPARMS(IVAR,1), VPARMS(IVAR,2)
      END IF
!
!---- Normal
!
      IF( IDIST .EQ. 4 ) THEN
        WRITE(IRPT,1050) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1050   FORMAT('   The mean is',15X,1P,E11.4/ &
               '   The standard deviation is ',E11.4)
      END IF
!
!---- Log normal, base 10 (type 5), or base e (type 6)
!
      IF( IDIST.EQ.5 .OR. IDIST.EQ.6 ) THEN
        WRITE(IRPT,1050) VPARMS(IVAR,1), VPARMS(IVAR,2)
      END IF
!
!---- Exponential
!
      IF( IDIST .EQ. 7 ) THEN
        WRITE(IRPT,1060) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1060   FORMAT('   The mean is  ',1P,E11.4/ &
               '   The shift is ',E11.4)
      END IF
!
!---- Triangular
!
      IF( IDIST .EQ. 8 ) THEN
        WRITE(IRPT,1070) VPARMS(IVAR,1), VPARMS(IVAR,2), VPARMS(IVAR,3)
 1070   FORMAT('   The minimum is ',1P,E11.4/ &
               '   The mode is    ',E11.4/ &
               '   The maximum is ',E11.4)
      END IF
!
!---- Gamma
!
      IF( IDIST .EQ. 9 ) THEN
        WRITE(IRPT,1080) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1080   FORMAT('   The Exponent on x is',13X,1PE11.4/ &
               '   The parameter in the exponent is ',E11.4)
      END IF
!
!---- Beta
!
      IF( IDIST .EQ. 10 ) THEN
        WRITE(IRPT,1090) VPARMS(IVAR,1), VPARMS(IVAR,2), &
                         VPARMS(IVAR,3), VPARMS(IVAR,4)
 1090   FORMAT('   The left end of range is ',1P,E11.4, &
               ', the right end is ',E11.4/ &
               '   The parameter ''a'' is     ',E11.4, &
               ', parameter ''b'' is ',E11.4)
      END IF
!
!---- Weibull
!
      IF( IDIST .EQ. 11 ) THEN
        WRITE(IRPT,1100) VPARMS(IVAR,1), VPARMS(IVAR,2), VPARMS(IVAR,3)
 1100   FORMAT('   The scale is    ',1P,E11.4/ &
               '   The exponent is ',E11.4/ &
               '   The shift is    ',E11.4)
      END IF
!
!---- Logistic
!
      IF( IDIST .EQ. 12 ) THEN
        WRITE(IRPT,1110) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1110   FORMAT('   The mean is',12X,1P,E11.4/ &
               '   The scale parameter is ',E11.4)
      END IF
!
!---- Cauchy
!
      IF( IDIST .EQ. 13 ) THEN
        WRITE(IRPT,1120) VPARMS(IVAR,1), VPARMS(IVAR,2)
 1120   FORMAT('   The median is',10X,1P,E11.4/ &
               '   The scale parameter is ',E11.4)
      END IF
!
!---- User Defined
!
      IF( IDIST .EQ. 14 ) THEN
        NUSER = PTABLE(IVAR,2)
        NSTRT = PTABLE(IVAR,1)
        NTEND = NSTRT + NUSER - 1
        WRITE(IRPT,1130) NUSER, XTABLE(NSTRT), XTABLE(NTEND)
 1130   FORMAT('   There are ',I0,' entries in the function definition'/ &
               '   The first entry is at X = ',1P,E11.4/ &
               '   The last entry is at X  = ',E11.4)
      END IF
!
!---- Discrete Uniform
!
      IF( IDIST .EQ. 15 ) THEN
        I1 = VPARMS(IVAR,1)
        I2 = VPARMS(IVAR,2)
        WRITE(IRPT,1140) I1, I2
 1140   FORMAT('   The lower limit is ',I7/ &
               '   The upper limit is ',I7)
      END IF
!
!---- Echo of truncation statistics
!
      IF( ITRUNC .EQ. 1 ) THEN
        WRITE(IRPT,1150) VTLIM(IVAR,1), UMIN(IVAR)
 1150   FORMAT('   The lower truncation limit is ',1P,E11.4,'   (',E10.3,')')
      END IF
!
      IF( ITRUNC .EQ. 2 ) THEN
        WRITE(IRPT,1160) VTLIM(IVAR,2), UMAX(IVAR)
 1160   FORMAT('   The upper truncation limit is ',1P,E11.4,'   (',E10.3,')')
      END IF
!
      IF( ITRUNC .EQ. 3 ) THEN
        WRITE(IRPT,1150) VTLIM(IVAR,1), UMIN(IVAR)
        WRITE(IRPT,1160) VTLIM(IVAR,2), UMAX(IVAR)
      END IF
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE PRTERR( IERR, CALLER, MLINES )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine prints a user defined error message to an ASCII file opened for output on the
!!    unit IRPT_ERR.
!!
!!  Call List Variables:
!!
!!    Variable Type         Description
!!    -------- -----------  ----------------------------------------
!!    IERR     (Integer)    Error number from the calling routine
!!                          If 0, output message rather than error
!!    CALLER   (Character)  Name of the calling routine
!!    MLINES   (Integer)    Number of lines in the error message
!!
!!  Module Variables:
!!
!!    Variable Type         Description
!!    -------- -----------  ----------------------------------------
!!    IRPT_ERR (Integer)    File unit for output (opened previously)
!!    MESSAG   (Character)  Message vector
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  4 Sep 2002 : Update format statements
!!    Paul W. Eslinger :  9 Oct 2002 : SAC Rev. 1
!!    Paul W. Eslinger : 30 May 2007 : Update comments
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: IERR
      CHARACTER(LEN=*), INTENT(IN) :: CALLER
      INTEGER, INTENT(IN) :: MLINES
!
! *** Local variables
      INTEGER :: I ! Local looping control variable
!
!---- Executable code ---------------------------------------------------
!
! *** Write out the error number and the calling routine
      IF( IERR .EQ. 0 ) THEN
        WRITE(IRPT_ERR,1000) TRIM(CALLER)
 1000   FORMAT(/'Message originating in routine ',A)
      ELSE
        WRITE(IRPT_ERR,1010) IERR, TRIM(CALLER)
 1010   FORMAT(/'Error number ',I0,' encountered in routine ',A)
      END IF
!
! *** Write out the first line of the error message
      IF( MLINES .GT. 0 ) THEN
        WRITE(IRPT_ERR,1020) TRIM( MESSAG(1) )
 1020   FORMAT('Message: ',A)
      END IF
!
! *** Write out any trailing lines for the message
      IF( MLINES .GT. 1 ) THEN
        DO I = 2, MIN(MLINES,MAXMES)
          WRITE(IRPT_ERR,1030) TRIM( MESSAG(I) )
 1030   FORMAT(9X,A)
        END DO
      END IF
!
      RETURN
      END SUBROUTINE PRTERR
!
!
      SUBROUTINE QA_SET
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine sets some information for Quality Assurance purposes.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE QA_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- Executable code -------------------------------------------------------------
!
! *** Set when QA procedures have not been followed or the code has
!     not been formally tested
      QA     = .FALSE.
      VERNUM = 'No Report'
      VERTIT = 'Formal testing has not been performed.'
      VERDAT = '04/14/2009'
!
! *** Set when QA procedures have been followed and code tested
!      QA     = .TRUE.
!      VERNUM = 'Report Number'
!      VERTIT = 'Verification report title goes here.'
!      VERDAT = '04/14/2009'
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE REARNG( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine schedules all of the operations that must be performed to rearrange values
!!    for the Latin Hypercube Sampling scheme using data that has already been generated with a
!!    stratified sampling scheme.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Control_Mod
      USE Data_Mod
      USE Files_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: PPND
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'REARNG' ! Name of this routine
      INTEGER :: I, J, KFLAG, NULLTY
      REAL :: RNP1, U
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
!-----------------------------------------------------------------------
!                    Compute the score matrix
!        Based on the column rank of a variable in the X matrix
!-----------------------------------------------------------------------
!
! *** Compute the van der Warden scores used to fill the score matrix.
!     Each column of the score matrix contains the same scores, but
!     they appear in different order.  Use the temporary work location
!     XTMP to compute the scores once.
!
      RNP1 = N + 1
      DO J = 1, N
        U = J/RNP1
        XTMP(J) = PPND( U, IERR )
        IF( IERR .NE. 0 ) RETURN
      END DO
!
      DO I = 1, P
!
! ***   Extract the variable of interest
!       Set up indices for the rank vector in IWORK
!
        DO J = 1, N
          RWORK(J) = X(J,I)
          IWORK(J) = J
        END DO
!
! ***   Perform a sort to find the ranks
! ***   NOTE: SSORTI sorts RWORK in increasing order and carries
!       along the vector IWORK.
!
        DATA KFLAG /2/
        CALL SSORTI( RWORK, IWORK, N, KFLAG, IERR )
        IF( IERR .NE. 0 ) RETURN
!
! ***   Put the sorted X values back into X, thus location gives rank.
!       Also, fill the score matrix.
!
        DO J = 1, N
          X(J,I) = RWORK(J)
          SCORE(J,I) = XTMP(IWORK(J))
        END DO
!
      END DO
!
! ---------------------------------------------------------------------
!   Compute the correlation between the columns of the score matrix
! ---------------------------------------------------------------------
! *** The subroutine uses the matrix SCORE, the integers N and P,
!     and computes the matrix CORA.
!
      CALL MCORR( IERR )
      IF( IERR .GT. 0 ) THEN
        IERR = 3
        MESSAG(1) = 'Terminal error encountered'
        MESSAG(2) = 'Error in lower level routine MCORR'
        MESSAG(3) = 'The score matrix correlation matrix error'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! ---------------------------------------------------------------------
!   Cholesky decomposition of the score matrix correlation matrix
! ---------------------------------------------------------------------
!
      CALL CHOLP( CORA, P, NP, NULLTY, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 4
        MESSAG(1) = 'Terminal error encountered'
        MESSAG(2) = 'Error in lower level routine CHOLP'
        MESSAG(3) = ' The score matrix correlation matrix is not positive definite'
        CALL PRTERR( IERR, CALLER, 3 )
        RETURN
      END IF
!
! ---------------------------------------------------------------------
!      Compute the inverse of the score matrix correlation matrix
! ---------------------------------------------------------------------
! *** Compute the inverse of CORA and overwrite itself
!
      CALL TRMINO( CORA, P, NP, IERR )
      IF( IERR .NE. 0 ) RETURN
!
! ---------------------------------------------------------------------
!      Compute the product of COR(decomposed) and CORA(product,
!       decomposed and inverse)
! ---------------------------------------------------------------------
!     -> To save space, overwrite CORA
!
      CALL TRMULO( COR, CORA, P, NP, IERR )
      IF( IERR .NE. 0 ) RETURN
!
! ---------------------------------------------------------------------
!      Compute the modified score matrix and overwrite the
!        original score matrix
! ---------------------------------------------------------------------
!
      CALL MULABT( SCORE, CORA, N, MAXN, P, NP, IERR )
      IF( IERR .NE. 0 ) RETURN
!
!-----------------------------------------------------------------------
!                        Data Reshuffling
!-----------------------------------------------------------------------
!
! *** Force the X matrix to have the same column rank structure as
!     the modified score matrix
!
      DO I = 1, P
!
! ***   Extract the column of the score matrix to be shuffled
!       Set up indices for the rank vector in IWORK
!       Also, extract the (sorted) X values to be rearranged
!
        DO J = 1, N
          RWORK(J) = SCORE(J,I)
          XTMP(J)  = X(J,I)
          IWORK(J) = J
        END DO
!
! ***   Perform a sort to find the ranks in the score matrix
!
        CALL SSORTI( RWORK, IWORK, N, KFLAG, IERR )
        IF( IERR .NE. 0 ) RETURN
!
! ***   Put the XTMP values back into X at the locations given
!       by the ranks from the score matrix
!
        DO J = 1, N
          X(IWORK(J),I) = XTMP(J)
        END DO
!
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE SGEN( IDIST, RVAL, NVAL, UMIN, UMAX, PAR1, PAR2, PAR3, &
                 PAR4, NUSER, UUSER, XUSER, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine will generate NVAL values from a specified probability distribution and place
!!    them in the vector RVAL.  On input the vector RVAL contains values from the Uniform
!!    distribution.  On output it contains values from the desired statistical distribution.
!!
!!
!!  Notes:
!!    1. Each distribution is generated using the Probability Integral Transformation method.  Thus,
!!       the random value X is found implicitly from the equation F(X)=U where U is a generated
!!       value from the uniform distribution, and F(X) is the cumulative distribution function for
!!       the random variable X.
!!
!!    2. Stratified sampling may be used.  The Uniform(0,1) values in RVAL upon input can be a
!!!      stratified set of values.
!!
!!    3. Each distribution may be truncated between two specified limits.  The Uniform(0,1) values
!!       input in RVAL will be modified to implement the range truncation.
!!
!!    4. The user may specify a cumulative distribution function in the form of a table of values.
!!       Linear interpolation will be used to generate values from the tabled distribution.
!!
!!    5. Adding or deleting a distribution requires changes in four places.  This subroutine,
!!       subroutine SINV, the vector DLABEL of distribution labels, and the parameter MXDIST.
!!
!!
!!  Formal Parameters:
!!    Name    Description
!!    ------  --------------------------------------------------------------------------------------
!!    IDIST   Input - Integer: Flag to determine the distribution type to generate.  See the table
!!            below for the available distributions.
!!
!!    RVAL    Input - Real vector: Vector of a sample of values from the Uniform(0,1) distribution.
!!            It can have been generated using a statified sampling scheme.
!!            Output - Real Vector: Vector of length NVAL containing the generated random values
!!            from distribution IDIST.
!!
!!    NVAL    Input - Integer: Number of values to generate.
!!
!!    UMIN    Input - Real: Minimum value from the uniform distribution to use for generating the
!!            random values.  Set UMIN to 0.0 if the variable range is not truncated.  Otherwise,
!!            use subroutine SINV to set the appropriate value for UMIN.
!!            Note: 0 .LE. UMIN .LT. UMAX .LE. 1
!!
!!    UMAX    Input - Real: Maximum value from the uniform distribution to use for generating the
!!            random values.  Set UMAX to 1.0 if the variable range is not truncated.  Otherwise,
!!            use subroutine SINV to set the appropriate value for UMAX.
!!            Note: 0 .LE. UMIN .LT. UMAX .LE. 1
!!
!!    PAR1    Input - Real: Parameter #1 for the distribution.
!!            See the table below for specific distributions.
!!
!!    PAR2    Input - Real: Parameter #2 for the distribution.
!!            See the table below for specific distributions.
!!
!!    PAR3    Input - Real: Parameter #3 for the distribution.
!!            See the table below for specific distributions.
!!
!!    PAR4    Input - Real: Parameter #4 for the distribution.
!!            See the table below for specific distributions.
!!
!!    NUSER   Input - Integer: Number of values in the defining table for a user specified
!!            distribution function.
!!
!!    UUSER   Input - Real vector: Vector of probabilities associated with the values in the
!!            vector XUSER.
!!
!!    XUSER   Input - Real vector: Vector of values associated with the probabilities in the
!!            vector UUSER.  The probabilities will be used to generate random values from the
!!            user specified distribution using linear interpolation between the entries in XUSER.
!!
!!
!!  Distributions Available:
!!    IDIST Distribution     Parameters
!!    ----- ------------     -----------------------------------------
!!      0   Constant         PAR1 = Constant value
!!      1   Uniform          PAR1 = Lower limit, PAR2 = Upper limit
!!      2   Loguniform       (Base 10) PAR1 = Lower limit,
!!                           PAR2 = Upper limit
!!      3   Loguniform       (Base e) PAR1 = Lower limit,
!!                           PAR2 = Upper limit
!!      4   Normal           PAR1 = Mean, PAR2 = Standard deviation
!!      5   Lognormal        (Base 10) PAR1 = Mean,
!!                           PAR2 = Standard deviation
!!      6   Lognormal        (Base e) PAR1 = Mean,
!!                           PAR2 = Standard deviation
!!      7   Exponential      PAR1 = Mean Value, PAR2 = Shift
!!      8   Triangular       PAR1 = Minimum, PAR2 = Mode,
!!                           PAR3 = Maximum
!!      9   Gamma            PAR1 = Exponent on X,
!!                           PAR2 = Parameter in the exponential.
!!                           The mean is PAR1/PAR2
!!     10   Beta             PAR1 = Left end of range,
!!                           PAR2 = Right end of the range,
!!                           PAR3 = "a" parameter (exponent on (X-PAR1))
!!                           PAR4 = "b" Parameter (exponent on (PAR2-X))
!!                           The mean is a/(a+b)
!!     11   Weibull          PAR1  = Scale, PAR2 = Exponent,
!!                           PAR3 = Shift
!!     12   Logistic         PAR1 = Mean, PAR2 = Scale parameter
!!     13   Cauchy           PAR1 = Median, PAR2 = Scale parameter
!!     14   User             None - User specified Distribution
!!     15   Discrete Uniform PAR1 = Minimum, PAR2 = maximum
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: PPND
      REAL, EXTERNAL :: USRDST
!
! *** Call list variables
      INTEGER :: IDIST
      INTEGER :: NVAL
      REAL, DIMENSION(NVAL) :: RVAL
      REAL :: UMIN, UMAX
      REAL :: PAR1, PAR2, PAR3, PAR4
      INTEGER :: NUSER
      REAL, DIMENSION(NUSER) :: UUSER
      REAL, DIMENSION(NUSER) :: XUSER
      INTEGER :: IERR
!
! *** Local variables
      INTEGER :: I, I1, I2, IVAL, IDELP
      REAL :: LBETA, LGAMMA
      REAL :: UHGH, UDEL, DELP, PL1, PL2, RV, SQP1, SQP2, PI, XHGH, WEXP, X
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Check for a constant value
!
      IF( IDIST .EQ. 0 ) THEN
        DO I = 1, NVAL
          RVAL(I) = PAR1
        END DO
        RETURN
      END IF
!
!-----------------------------------------------------------------------
!
! *** Implement the (possible) range truncation on the uniform values
!     before inversion to data values.
!
      IF( UMIN.GT.0.0 .OR. UMAX.LT.1.0 ) THEN
        UDEL = UMAX - UMIN
        DO I = 1, NVAL
          RVAL(I) = UMIN + UDEL*RVAL(I)
        END DO
      END IF
!
!-----------------------------------------------------------------------
!         Choose the distribution type and generate the values
!-----------------------------------------------------------------------
!
! *** Uniform (PAR1,PAR2)
      IF( IDIST .EQ. 1 ) THEN
        DELP = PAR2 - PAR1
        DO I = 1, NVAL
          RVAL(I) = PAR1 + DELP*RVAL(I)
        END DO
        RETURN
      END IF
!
! *** Loguniform - Base 10
      IF( IDIST .EQ. 2 ) THEN
        PL1 = ALOG10( PAR1 )
        PL2 = ALOG10( PAR2 )
        DELP = PL2 - PL1
        DO I = 1, NVAL
          RV = PL1 + DELP * RVAL(I)
          RVAL(I) = 10.0**RV
        END DO
        RETURN
      END IF
!
! *** Loguniform - Base e
      IF( IDIST .EQ. 3 ) THEN
        PL1 = ALOG( PAR1 )
        PL2 = ALOG( PAR2 )
        DELP = PL2 - PL1
        DO I = 1, NVAL
          RV = PL1 + DELP * RVAL(I)
          RVAL(I) = EXP( RV )
        END DO
        RETURN
      END IF
!
! *** Normal distribution
      IF( IDIST .EQ. 4 ) THEN
        DO I = 1, NVAL
          RV = RVAL(I)
          RVAL(I) = PAR2*PPND(RV,IERR) + PAR1
          IF( IERR .NE. 0 ) RETURN
        END DO
        RETURN
      END IF
!
! *** Lognormal distribution - base 10
      IF( IDIST .EQ. 5 ) THEN
        DO I = 1, NVAL
          RV = RVAL(I)
          RVAL(I) = 10.0**(PAR2*PPND(RV,IERR) + PAR1)
          IF( IERR .NE. 0 ) RETURN
        END DO
        RETURN
      END IF
!
! *** Lognormal distribution - base e
      IF( IDIST .EQ. 6 ) THEN
        DO I = 1, NVAL
          RV = RVAL(I)
          RVAL(I) = EXP( PAR2*PPND(RV,IERR) + PAR1 )
          IF( IERR .NE. 0 ) RETURN
        END DO
        RETURN
      END IF
!
! *** Exponential distribution
      IF( IDIST .EQ. 7 ) THEN
        DO I = 1, NVAL
          RVAL(I) = PAR2 - PAR1*ALOG(1.0-RVAL(I))
        END DO
        RETURN
      END IF
!
! *** Triangular distribution
      IF( IDIST .EQ. 8 ) THEN
        DELP = (PAR2-PAR1) / (PAR3-PAR1)
        SQP1 = SQRT( (PAR3-PAR1)*(PAR2-PAR1) )
        SQP2 = SQRT( (PAR3-PAR1)*(PAR3-PAR2) )
        DO I = 1, NVAL
          IF( RVAL(I) .LE. DELP ) THEN
            RVAL(I) = PAR1 + SQP1*SQRT( RVAL(I) )
          ELSE
            RVAL(I) = PAR3 - SQP2*SQRT( 1.0-RVAL(I) )
          END IF
        END DO
        RETURN
      END IF
!
! *** Gamma distribution
      IF( IDIST .EQ. 9 ) THEN
        LGAMMA = 0.0
        UHGH   = 0.99999
        XHGH   = 0.0
        DO I = 1, NVAL
          CALL GINVER( RVAL(I), X, PAR1, PAR2, LGAMMA, UHGH, XHGH, IERR )
          IF( IERR .NE. 0 ) RETURN
          RVAL(I) = X
        END DO
        RETURN
      END IF
!
! *** Beta distribution
      IF( IDIST .EQ. 10 ) THEN
        LBETA = 0.0
        DO I = 1, NVAL
          CALL BINVER( RVAL(I), X, PAR1, PAR2, PAR3, PAR4, LBETA, IERR )
          IF( IERR .NE. 0 ) RETURN
          RVAL(I) = X
        END DO
        RETURN
      END IF
!
! *** Weibull Distribution
      IF( IDIST .EQ. 11 ) THEN
        WEXP = 1.0 / PAR2
        DO I = 1, NVAL
          RVAL(I) = (-ALOG(1.0-RVAL(I))/PAR1)**WEXP + PAR3
        END DO
        RETURN
      END IF
!
! *** Logistic distribution
      IF( IDIST .EQ. 12 ) THEN
        DO I = 1, NVAL
          RVAL(I) = PAR1 - PAR2*ALOG( 1.0/RVAL(I) - 1.0 )
        END DO
        RETURN
      END IF
!
! *** Cauchy distribution
      IF( IDIST .EQ. 13 ) THEN
        DATA PI / 3.14159265 /
        DO I = 1, NVAL
          RVAL(I) = PAR2 * TAN(PI*(RVAL(I)-0.5)) + PAR1
        END DO
        RETURN
      END IF
!
! *** User specified distribution
      IF( IDIST .EQ. 14 ) THEN
        DO I = 1, NVAL
          RV = RVAL(I)
          RVAL(I) = USRDST( RV, NUSER, UUSER, XUSER )
        END DO
        RETURN
      END IF
!
! *** Discrete Uniform (PAR1,PAR2)
      IF( IDIST .EQ. 15 ) THEN
        I1 = PAR1
        I2 = PAR2
        IDELP = I2 - I1 + 1
        DO I = 1, NVAL
          IVAL = IDELP * RVAL(I)
          RVAL(I) = I1 + IVAL
        END DO
        RETURN
      END IF
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE SINV( IDIST, XMIN, XMAX, UMIN, UMAX, PAR1, PAR2, PAR3, &
                 PAR4, ITRUNC, NUSER, UUSER, XUSER, IUNIT, BADDAT )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine assists the random number generation routines.  It has two purposes:
!!      1) Perform error checking on the definition of probability distributions.
!!      2) Perform the inverse mapping for handling truncation limits for distributions.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!
!!  Notes:
!!    1. Each distribution is generated (in SGEN) using the Probability Integral Transformation
!!       method.  Thus, the random value X is found implicitly from the equation F(X)=U where U
!!       is a value from the standard (0,1) uniform distribution, and F(X) is the cumulative
!!       distribution function for the random variable X.
!!
!!    2. Most distributions may be truncated between two specified limits.
!!
!!    3. The user may specify a cumulative distribution function in the form of a table of values.
!!       Linear interpolation will be used to generate values from the tabled distribution.
!!
!!    4. Adding or deleting a distribution requires changes in four places.  This subroutine,
!!       subroutine SGEN, the vector DLABEL of distribution labels (subroutine INIT), and the
!!       parameter MXDIST.
!!
!!
!!  Formal Parameters:
!!    Name    Description
!!    ------  --------------------------------------------------------------------------------------
!!    IDIST   Input - Integer: Flag to determine the distribution type to generate.  See the table
!!            below for the available distributions.
!!
!!    ITRUNC  Input - Integer: Flag to determine if range truncation is to be performed.
!!            If ITRUNC = 0 then no truncation will be done.
!             If ITRUNC = 1, truncation is done on the left at the value XMIN.
!             If ITRUNC = 2, truncation is done on the right at the value XMAX.
!!            If ITRUNC = 3, truncation is done at both XMIN and XMAX.
!!
!!    XMIN    Input - Real: Left truncation limit for the distribution.  The value is used only
!!            if ITRUNC takes the value 1 or 3.  XMIN must be less than XMAX if ITRUNC = 3.
!!
!!    XMAX    Input - Real: Right truncation limit for the distribution.  The value is used only
!!            if ITRUNC takes the value 2 or 3. XMIN must be less than XMAX if ITRUNC = 3.
!!
!!    UMIN    Output - Real: Minimum value from the uniform distribution to use for generating the
!!            random values.  Corresponds to the left limit XMIN if truncation is desired, otherwise
!!            it is set to 0.0.
!!
!!    UMAX    Output - Real: Maximum value from the uniform distribution to use for generating the
!!            random values.  Corresponds to the right limit XMAX if truncation is desired, otherwise
!!            it is set to 1.0.
!!
!!    PAR1    Input - Real: Parameter #1 for the distribution.
!!            See the table below for specific distributions.
!!
!!    PAR2    Input - Real: Parameter #2 for the distribution.
!!            See the table below for specific distributions.
!!
!!    PAR3    Input - Real: Parameter #3 for the distribution.
!!            See the table below for specific distributions.
!!
!!    PAR4    Input - Real: Parameter #4 for the distribution.
!!            See the table below for specific distributions.
!!
!!    NUSER   Input - Integer: Number of values in the defining table for a user specified
!!            distribution function.
!!
!!    UUSER   Input - Real vector: Vector of probabilities associated with the values in the
!!            vector XUSER.
!!
!!    XUSER   Input - Real vector: Vector of values associated with the probabilities in the vector
!!            UUSER.  The probabilities will be used to generate random values from the user
!!            specified distribution using linear interpolation between the table entries in XUSER.
!!
!!    IUNIT   Input - Integer: Unit number for error messages
!!
!!    BADDAT  Logical flag.  Initialized in the calling routine.  It will be set to .TRUE. if an
!!            error is detected.
!!
!!
!!  Variables from Modules:
!!
!!    DLABEL  Input - Character Vector: Vector of labels for the different distributions which can
!!            be generated.
!!
!!    MXDIST  Input - Integer: Maximum number for a distribution type.
!!
!!
!!  Distributions Available:
!!    IDIST Distribution     Parameters
!!    ----- ------------     -----------------------------------------
!!      0   Constant         PAR1 = Constant value
!!      1   Uniform          PAR1 = Lower limit, PAR2 = Upper limit
!!      2   Loguniform       (Base 10) PAR1 = Lower limit,
!!                           PAR2 = Upper limit
!!      3   Loguniform       (Base e) PAR1 = Lower limit,
!!                           PAR2 = Upper limit
!!      4   Normal           PAR1 = Mean, PAR2 = Standard deviation
!!      5   Lognormal        (Base 10) PAR1 = Mean,
!!                           PAR2 = Standard deviation
!!      6   Lognormal        (Base e) PAR1 = Mean,
!!                           PAR2 = Standard deviation
!!      7   Exponential      PAR1 = Mean Value, PAR2 = Shift
!!      8   Triangular       PAR1 = Minimum, PAR2 = Mode,
!!                           PAR3 = Maximum
!!      9   Gamma            PAR1 = Exponent on X,
!!                           PAR2 = Parameter in the exponential.
!!                           The mean is PAR1/PAR2
!!     10   Beta             PAR1 = Left end of range,
!!                           PAR2 = Right end of the range,
!!                           PAR3 = "a" parameter (exponent on (X-PAR1))
!!                           PAR4 = "b" Parameter (exponent on (PAR2-X))
!!                           The mean is a/(a+b)
!!     11   Weibull          PAR1  = Scale, PAR2 = Exponent,
!!                           PAR3 = Shift
!!     12   Logistic         PAR1 = Mean, PAR2 = Scale parameter
!!     13   Cauchy           PAR1 = Median, PAR2 = Scale parameter
!!     14   User             None - User specified Distribution
!!     15   Discrete Uniform PAR1 = Minimum, PAR2 = maximum
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Stochastic_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: ALNORM
      REAL, EXTERNAL :: ALNGAM
      REAL, EXTERNAL :: BETAIN
      REAL, EXTERNAL :: GAMAIN
      REAL, EXTERNAL :: USRDST
!
! *** Call list variables
      INTEGER :: IDIST
      REAL :: XMIN, XMAX
      REAL :: UMIN, UMAX
      REAL :: PAR1, PAR2, PAR3, PAR4
      INTEGER :: ITRUNC, NUSER
      REAL, DIMENSION(NUSER) :: UUSER, XUSER
      INTEGER :: IUNIT
      LOGICAL BADDAT
!
! *** Local variables
      REAL :: LBETA, LGAMMA
      REAL :: PL1, PL2, X, X01, PI
      INTEGER :: I, IBAD, IFT, IERR
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Set up the uniform limits for the case of no truncation
!
      UMIN = 0.0
      UMAX = 1.0
!
! *** Check for a valid distribution type
!
      IF( IDIST.LT.0 .OR. IDIST.GT.MXDIST ) THEN
        WRITE(IUNIT,1050) IDIST, MXDIST
 1050   FORMAT(/' Terminal error in Subroutine SINV'/ &
                ' The distribution type number ',I0,' is invalid'/ &
                ' The legal range is 0 to ',I0)
        BADDAT = .TRUE.
        RETURN
      END IF
!
! *** Check for a constant value
!
      IF( IDIST .EQ. 0 ) THEN
        RETURN
      END IF
!
! *** Uniform
      IF( IDIST .EQ. 1 ) THEN
        IF( PAR1 .GE. PAR2 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
 1000     FORMAT(/' Terminal Error in Subroutine SINV for distribution', &
                  ' type ',I0/' Distribution label is ',A16)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
 1010     FORMAT(' The four parameter values are'/1P,4(2X,E14.6))
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR2)) .OR. &
            (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR2)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR2)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
 1020     FORMAT(' Truncation error with truncation flag = ',I0/ &
                 ' XMIN = ',1P,E14.6,'   XMAX = ',E14.6)
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) UMIN = (XMIN-PAR1) / (PAR2-PAR1)
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) UMAX = (XMAX-PAR1) / (PAR2-PAR1)
        RETURN
      END IF
!
! *** Loguniform - Base 10
      IF( IDIST .EQ. 2 ) THEN
        IF( PAR1.GE.PAR2 .OR. PAR1.LE.0.0 .OR. PAR2.LE.0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR2)) .OR. &
            (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR2)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR2)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        PL1 = ALOG10( PAR1 )
        PL2 = ALOG10( PAR2 )
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) UMIN = (ALOG10(XMIN)-PL1) / (PL2-PL1)
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) UMAX = (ALOG10(XMAX)-PL1) / (PL2-PL1)
        RETURN
      END IF
!
! *** Loguniform - Base e
      IF( IDIST .EQ. 3 ) THEN
        IF( PAR1.GE.PAR2 .OR. PAR1.LE.0.0 .OR. PAR2.LE.0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR2)) .OR. &
            (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR2)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR2)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        PL1 = ALOG( PAR1 )
        PL2 = ALOG( PAR2 )
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) UMIN = (ALOG(XMIN)-PL1) / (PL2-PL1)
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) UMAX = (ALOG(XMAX)-PL1) / (PL2-PL1)
        RETURN
      END IF
!
! *** Normal distribution
      IF( IDIST .EQ. 4 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMIN-PAR1) / PAR2
          UMIN = ALNORM( X, .FALSE. )
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMAX-PAR1) / PAR2
          UMAX = ALNORM( X, .FALSE. )
        END IF
        RETURN
      END IF
!
! *** Lognormal distribution - base 10
      IF( IDIST .EQ. 5 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. XMIN.LE.0.0) .OR.  &
          (ITRUNC.EQ.2 .AND. XMAX.LE.0.0) .OR. &
          (ITRUNC.EQ.3 .AND. (XMIN.LE.0.0.OR.XMAX.LE.0.0) ).OR. &
          (ITRUNC.EQ.3 .AND. XMAX.LE.XMIN) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (ALOG10(XMIN)-PAR1) / PAR2
          UMIN = ALNORM( X, .FALSE. )
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (ALOG10(XMAX)-PAR1) / PAR2
          UMAX = ALNORM( X, .FALSE. )
        END IF
        RETURN
      END IF
!
! *** Lognormal distribution - base e
      IF( IDIST .EQ. 6 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. XMIN.LE.0.0) .OR. &
          (ITRUNC.EQ.2 .AND. XMAX.LE.0.0) .OR. &
          (ITRUNC.EQ.3 .AND. (XMIN.LE.0.0.OR.XMAX.LE.0.0) ).OR. &
          (ITRUNC.EQ.3 .AND. XMAX.LE.XMIN) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (ALOG(XMIN)-PAR1) / PAR2
          UMIN = ALNORM( X, .FALSE. )
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (ALOG(XMAX)-PAR1) / PAR2
          UMAX = ALNORM( X, .FALSE. )
        END IF
        RETURN
      END IF
!
! *** Exponential distribution
      IF( IDIST .EQ. 7 ) THEN
        IF( PAR1 .LE. PAR2 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. XMIN.LT.PAR2) .OR. &
            (ITRUNC.EQ.2 .AND. XMAX.LT.PAR2) .OR. &
            (ITRUNC.EQ.3 .AND. XMAX.LE.XMIN) .OR. &
            (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR2.OR.XMAX.LT.PAR2)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          UMIN = 1.0 - EXP(-(XMIN-PAR2)/PAR1)
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          UMAX = 1.0 - EXP(-(XMAX-PAR2)/PAR1)
        END IF
        RETURN
      END IF
!
! *** Triangular distribution
      IF( IDIST .EQ. 8 ) THEN
        IF( (PAR1.GE.PAR2) .OR. (PAR1.GE.PAR3) .OR. (PAR2.GE.PAR3) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR3)) .OR. &
            (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR3)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR3)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          IF( XMIN .LE. PAR2 ) THEN
            UMIN = (XMIN-PAR1)**2/((PAR3-PAR1)*(PAR2-PAR1))
          ELSE
            UMIN = 1.0 - (XMIN-PAR3)**2/((PAR3-PAR1)*(PAR3-PAR2))
          END IF
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          IF( XMAX .LE. PAR2 ) THEN
            UMAX = (XMAX-PAR1)**2/((PAR3-PAR1)*(PAR2-PAR1))
          ELSE
            UMAX = 1.0 - (XMAX-PAR3)**2/((PAR3-PAR1)*(PAR3-PAR2))
          END IF
        END IF
        RETURN
      END IF
!
! *** Gamma distribution
      IF( IDIST .EQ. 9 ) THEN
        IF( PAR1.LE.0.0 .OR. PAR2.LE.0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.0.0) ) .OR. &
            (ITRUNC.EQ.2 .AND. (XMAX.LT.0.0) ) .OR. &
            (ITRUNC.EQ.3 .AND. (XMIN.LT.0.0) ) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.LT.0.0) ) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC .GT. 0 ) LGAMMA = ALNGAM(PAR1,IFT)
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X01 = XMIN*PAR2
          UMIN = GAMAIN( X01, PAR1, LGAMMA, IERR )
          IF( IERR .NE. 0 ) RETURN
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X01 = XMAX*PAR2
          UMAX = GAMAIN( X01, PAR1, LGAMMA, IERR )
          IF( IERR .NE. 0 ) RETURN
        END IF
        RETURN
      END IF
!
! *** Beta distribution
      IF( IDIST .EQ. 10 ) THEN
        IF( PAR1.GE.PAR2 .OR. PAR3.LE.0.0 .OR. PAR4.LE.0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.PAR1.OR.XMIN.GT.PAR2)) .OR. &
            (ITRUNC.EQ.2 .AND. (XMAX.LT.PAR1.OR.XMAX.GT.PAR2)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR1)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.GT.PAR2)) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC .GT. 0 ) LBETA = ALNGAM(PAR3,IFT) + ALNGAM(PAR4,IFT) &
                                  - ALNGAM((PAR3+PAR4),IFT)
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X01 = (XMIN-PAR1) / (PAR2 - PAR1)
          UMIN = BETAIN( X01, PAR3, PAR4, LBETA, IERR )
          IF( IERR .NE. 0 ) THEN
            BADDAT = .TRUE.
            RETURN
          END IF
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X01 = (XMAX-PAR1) / (PAR2 - PAR1)
          UMAX = BETAIN( X01, PAR3, PAR4, LBETA, IERR )
          IF( IERR .NE. 0 ) THEN
            BADDAT = .TRUE.
            RETURN
          END IF
        END IF
        RETURN
      END IF
!
! *** Weibull Distribution
      IF( IDIST .EQ. 11 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. XMIN.LT.PAR3) .OR. &
            (ITRUNC.EQ.2 .AND. XMAX.LT.PAR3) .OR. &
            (ITRUNC.EQ.3 .AND. XMAX.LE.XMIN) .OR. &
            (ITRUNC.EQ.3 .AND. (XMIN.LT.PAR3.OR.XMAX.LT.PAR3)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          UMIN = 1.0 - EXP(-PAR1*(XMIN-PAR3)**PAR2)
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          UMAX = 1.0 - EXP(-PAR1*(XMAX-PAR3)**PAR2)
        END IF
        RETURN
      END IF
!
! *** Logistic distribution
      IF( IDIST .EQ. 12 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMIN-PAR1) / PAR2
          UMIN = 1.0 / ( 1.0 + EXP(-X) )
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMAX-PAR1) / PAR2
          UMAX = 1.0 / ( 1.0 + EXP(-X) )
        END IF
        RETURN
      END IF
!
! *** Cauchy distribution
      IF( IDIST .EQ. 13 ) THEN
        IF( PAR2 .LE. 0.0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.3 .AND. XMAX.LE.XMIN ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1010) PAR1, PAR2, PAR3, PAR4
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        DATA PI / 3.14159265 /
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMIN-PAR1) / PAR2
          UMIN = 0.5 + ATAN(X) / PI
        END IF
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) THEN
          X    = (XMAX-PAR1) / PAR2
          UMAX = 0.5 + ATAN(X) / PI
        END IF
        RETURN
      END IF
!
! *** User specified distribution
      IF( IDIST .EQ. 14 ) THEN
        IF( NUSER .LT. 2 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          BADDAT = .TRUE.
          RETURN
        END IF
        IBAD = 0
        IF( UUSER(1) .NE. 0.0 ) IBAD = 1
        IF( UUSER(NUSER) .NE. 1.0 ) IBAD = 1
        DO I = 1, NUSER
           IF( UUSER(I).LT.0.0 .OR. UUSER(I).GT.1.0 ) IBAD = 1
           IF( I .GT. 1 ) THEN
             IF( UUSER(I).LE.UUSER(I-1) .OR. XUSER(I).LE.XUSER(I-1) ) IBAD = 1
           END IF
        END DO
        IF( IBAD .NE. 0 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( (ITRUNC.EQ.1 .AND. (XMIN.LT.XUSER(1).OR. XMIN.GE.XUSER(NUSER))) .OR. &
            (ITRUNC.EQ.2 .AND. (XMAX.LT.XUSER(1).OR. XMAX.GE.XUSER(NUSER))) .OR. &
            (ITRUNC.EQ.3 .AND. (XMIN.LT.XUSER(1))) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.GT.XUSER(NUSER))) .OR. &
            (ITRUNC.EQ.3 .AND. (XMAX.LE.XMIN)) ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1020) ITRUNC, XMIN, XMAX
          BADDAT = .TRUE.
          RETURN
        END IF
        IF( ITRUNC.EQ.1 .OR. ITRUNC.EQ.3 ) UMIN = USRDST( XMIN, NUSER, XUSER, UUSER )
        IF( ITRUNC.EQ.2 .OR. ITRUNC.EQ.3 ) UMAX = USRDST( XMAX, NUSER, XUSER, UUSER )
        RETURN
      END IF
!
! *** Discrete Uniform distribution
!     Truncation does not apply
      IF( IDIST .EQ. 15 ) THEN
        IF( PAR2 .LT. PAR1 ) THEN
          WRITE(IUNIT,1000) IDIST, DLABEL(IDIST)
          WRITE(IUNIT,1040) PAR1, PAR2
 1040     FORMAT(' The two limits parameter values are'/1P,2(2X,E14.6))
          BADDAT = .TRUE.
          RETURN
        END IF
      END IF
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE SMOVE( JNDX )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine moves the "stochastic" data from storage locations in the X matrix into
!!    storage locations in the output vector XOUT.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Data_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT(IN) :: JNDX ! Variable index
!
! *** Local variables
      INTEGER :: I, ISTOC
!
!---- Executable code -------------------------------------------------------------
!
! *** Loop over all of the variable distributions
!     But only move the "stochastic" variables
!
      ISTOC = 0
      DO I = 1, PTOT
        IF( VTYPE(I) .NE. 0 ) THEN
          ISTOC = ISTOC + 1
          XOUT(I) = X(JNDX,ISTOC)
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE SORT( X, N, KFLAG, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine will sort the array X in increasing or decreasing order.
!!
!!  Reference:
!!
!!    Singleton, R. C.
!!    Algorithm 347, An Efficient Algorithm for Sorting with Minimal Storage
!!    C.A.C.M., 12(3), 1969, Pages 185-187.
!!
!!  Variable Descriptions:
!!
!!    X     - Array of values to be sorted
!!    N     - Number of values in array X to be sorted
!!    KFLAG - Control parameter
!!             =  1 Means sort X in increasing order (ignoring Y)
!!             = -1 Means sort X in decreasing order (ignoring Y)
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      REAL, DIMENSION(*) :: X
      INTEGER, INTENT (IN) :: N, KFLAG
      INTEGER :: IERR ! Error flag, nonzero if no match is found
!
! *** Local variables
      CHARACTER(LEN=4) :: CALLER = 'SORT' ! Name of this routine
      INTEGER, DIMENSION(21) :: IL, IU
      INTEGER :: NN, I, M, J, K, IJ, L
      REAL :: R, T, TT
!
!---- Executable code ---------------------------------------------------
!
      IERR = 0
!
! *** Check for proper dimensioning
!
      IF( N .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid number of data - negative entry.'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Check for valid sort order indicator
!
      IF( .NOT.(KFLAG.EQ.1 .OR. KFLAG.EQ.-1) ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid sort indicator.'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
      NN = N
!
! *** CHECK FOR SORT IN INCREASING ORDER
!
      IF (KFLAG .EQ. 1 ) GO TO 100
!
! *** ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
!
      DO I = 1, NN
        X(I) = -X(I)
      END DO
!
! *** SORT THE VECTOR
!
  100 CONTINUE
!
      M=1
      I=1
      J=NN
      R=.375
  110 IF (I .EQ. J) GO TO 155
  115 IF (R .GT. .5898437) GO TO 120
      R=R+3.90625E-2
      GO TO 125
  120 R=R-.21875
  125 K=I
!
!  *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
!
      IJ = I + IFIX (FLOAT (J-I) * R)
      T=X(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 130
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
  130 L=J
!
! *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
!
      IF (X(J) .GE. T) GO TO 140
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 140
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
      GO TO 140
  135 TT=X(L)
      X(L)=X(K)
      X(K)=TT
!
! *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
! *** SMALLER THAN T
!
  140 L=L-1
      IF (X(L) .GT. T) GO TO 140
!
! *** FIND AN ELEMENT IN THE FIRST HALF OF THE ARRAY WHICH IS
! *** GREATER THAN T
!
  145 K=K+1
      IF (X(K) .LT. T) GO TO 145
!
! *** INTERCHANGE THESE ELEMENTS
!
      IF (K .LE. L) GO TO 135
!
! *** SAVE UPPER AND LOWER SUBSCRIPTS OF THE ARRAY YET TO BE SORTED
!
      IF (L-I .LE. J-K) GO TO 150
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 160
  150 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 160
!
! *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
!
  155 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  160 IF (J-I .GE. 1) GO TO 125
      IF (I .EQ. 1) GO TO 110
      I=I-1
  165 I=I+1
      IF (I .EQ. J) GO TO 155
      T=X(I+1)
      IF (X(I) .LE. T) GO TO 165
      K=I
  170 X(K+1)=X(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 170
      X(K+1)=T
      GO TO 165
!
! *** CLEAN UP FOR SORT IN DECREASING ORDER
!
  300 IF( KFLAG .EQ. 1 ) RETURN
!
      DO I = 1, NN
        X(I) = -X(I)
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE SSORTI( X, Y, N, KFLAG, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    SSORTI sorts array X and optionally makes the same interchanges in array Y.  The array X may
!!    be sorted in increasing order or decreasing order. A modified QUICKSORT algorithm is used.
!!    The vector X is real and the vector Y is integer.
!!
!!  Reference:
!!
!!    Singleton, R. C., Algorithm 347, "An Efficient Algorithm for Sorting with Minimal Storage"
!!    Comm. Assoc. Comput. Mach., Vol. 12, No. 3, 1969, pp. 185-187.
!!
!!  Auxiliary Routines Required:
!!
!!    PRTERR
!!
!!  Variable Descriptions:
!!
!!    X     : real array of values to be sorted
!!    Y     : integer array to be (optionally) carried along
!!    N     : number of values in array X to be sorted
!!    KFLAG : control parameter
!!            =  2 means sort X in increasing order and carry Y along.
!!            =  1 means sort X in increasing order (ignoring Y)
!!            = -1 means sort X in decreasing order (ignoring Y)
!!            = -2 means sort X in decreasing order and carry Y along.
!!    IERR  : Returned error flag
!!            = 0 Means no errors
!!            = 1 Means error on the number of data values
!!            = 2 Means error on the sort indicator
!!
!!  History:
!!
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER, INTENT (IN) :: N, KFLAG
      REAL, DIMENSION(*) :: X
      INTEGER, DIMENSION(*) :: Y
      INTEGER :: IERR ! Error flag, nonzero if no match is found
!
! *** Local variables
      INTEGER :: TY, TTY
      INTEGER, DIMENSION(21) :: IL, IU
      CHARACTER(LEN=6) :: CALLER = 'SSORTI' ! Name of this routine
      INTEGER :: NN, I, M, J, K, KK, IJ, L
      REAL :: R, T, TT
!
!---- Executable code ---------------------------------------------------
!
      NN = N
      KK = IABS(KFLAG)
!
! *** Check for proper dimensioning
!
      IF( N .LE. 0 ) THEN
        IERR = 1
        MESSAG(1) = 'Invalid number of data - negative entry.'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Check for valid sort order indicator
!
      IF( .NOT.(KK.EQ.1 .OR. KK.EQ.2) ) THEN
        IERR = 2
        MESSAG(1) = 'Invalid sort indicator.'
        CALL PRTERR( IERR, CALLER, 1 )
        RETURN
      END IF
!
! *** Alter array X to get decreasing order if needed
!
   15 IF (KFLAG.GE.1) GO TO 30
!
      DO 20 I = 1, NN
        X(I) = -X(I)
   20 CONTINUE
!
   30 GO TO (100,200),KK
!
! *** Sort X only
!
  100 CONTINUE
      M = 1
      I = 1
      J = NN
      R = 0.375
  110 IF(I .EQ. J) GO TO 155
  115 IF(R .GT. 0.5898437) GO TO 120
      R = R + 3.90625E-2
      GO TO 125
!
  120 R = R - 0.21875
  125 K = I
!
! *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
!
      IJ = I + IFIX( FLOAT(J-I) * R)
      T = X(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 130
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
  130 L = J
!
! *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
!
      IF (X(J) .GE. T) GO TO 140
      X(IJ) = X(J)
      X(J)  = T
      T     = X(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 140
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      GO TO 140
  135 TT = X(L)
      X(L) = X(K)
      X(K) = TT
!
! *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
! *** SMALLER THAN T
!
  140 L = L - 1
      IF( X(L) .GT. T ) GO TO 140
!
! *** FIND AN ELEMENT IN THE FIRST HALF OF THE ARRAY WHICH IS
! *** GREATER THAN T
!
  145 K=K+1
      IF (X(K) .LT. T) GO TO 145
!
! *** INTERCHANGE THESE ELEMENTS
!
      IF (K .LE. L) GO TO 135
!
! *** SAVE UPPER AND LOWER SUBSCRIPTS OF THE ARRAY YET TO BE SORTED
!
      IF( L-I .LE. J-K ) GO TO 150
      IL(M) = I
      IU(M) = L
      I = K
      M = M+1
      GO TO 160
!
  150 IL(M) = K
      IU(M) = J
      J = L
      M = M+1
      GO TO 160
!
! *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
!
  155 M=M-1
      IF( M .EQ. 0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  160 IF( J-I .GE. 1 ) GO TO 125
      IF( I .EQ. 1 ) GO TO 110
      I = I-1
  165 I = I+1
      IF( I .EQ. J ) GO TO 155
      T = X(I+1)
      IF( X(I) .LE. T ) GO TO 165
      K = I
  170 X(K+1) = X(K)
      K = K-1
      IF( T .LT. X(K) ) GO TO 170
      X(K+1) = T
      GO TO 165
!
! *** SORT X AND CARRY Y ALONG
!
  200 CONTINUE
      M = 1
      I = 1
      J = NN
      R = 0.375
  210 IF( I .EQ. J ) GO TO 255
  215 IF( R .GT. 0.5898437 ) GO TO 220
      R = R + 3.90625E-2
      GO TO 225
!
  220 R = R - 0.21875
  225 K = I
!
! *** SELECT A CENTRAL ELEMENT OF THE ARRAY AND SAVE IT IN LOCATION T
!
      IJ = I + IFIX( FLOAT(J-I) * R )
      T  = X(IJ)
      TY = Y(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 230
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      Y(IJ) = Y(I)
      Y(I)  = TY
      TY    = Y(IJ)
  230 L = J
!
! *** IF LAST ELEMENT OF ARRAY IS LESS THAN T, INTERCHANGE WITH T
!
      IF (X(J) .GE. T) GO TO 240
      X(IJ) = X(J)
      X(J)  = T
      T     = X(IJ)
      Y(IJ) = Y(J)
      Y(J)  = TY
      TY    = Y(IJ)
!
! *** IF FIRST ELEMENT OF ARRAY IS GREATER THAN T, INTERCHANGE WITH T
!
      IF (X(I) .LE. T) GO TO 240
      X(IJ) = X(I)
      X(I)  = T
      T     = X(IJ)
      Y(IJ) = Y(I)
      Y(I)  = TY
      TY    = Y(IJ)
      GO TO 240
!
  235 TT=X(L)
      X(L) = X(K)
      X(K) = TT
      TTY  = Y(L)
      Y(L) = Y(K)
      Y(K) = TTY
!
! *** FIND AN ELEMENT IN THE SECOND HALF OF THE ARRAY WHICH IS
! *** SMALLER THAN T
!
  240 L = L-1
      IF( X(L) .GT. T ) GO TO 240
!
! *** FIND AN ELEMENT IN THE FIRST HALF OF THE ARRAY WHICH IS
! *** GREATER THAN T
!
  245 K=K+1
      IF (X(K) .LT. T) GO TO 245
!
! *** INTERCHANGE THESE ELEMENTS
!
      IF (K .LE. L) GO TO 235
!
! *** SAVE UPPER AND LOWER SUBSCRIPTS OF THE ARRAY YET TO BE SORTED
!
      IF( L-I .LE. J-K ) GO TO 250
      IL(M) = I
      IU(M) = L
      I = K
      M = M + 1
      GO TO 260
!
  250 IL(M) = K
      IU(M) = J
      J = L
      M = M + 1
      GO TO 260
!
! *** BEGIN AGAIN ON ANOTHER PORTION OF THE UNSORTED ARRAY
!
  255 M = M-1
      IF( M .EQ. 0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  260 IF( J-I .GE. 1 ) GO TO 225
      IF( I .EQ. 1 ) GO TO 210
      I = I-1
  265 I = I+1
      IF( I .EQ. J ) GO TO 255
      T  = X(I+1)
      TY = Y(I+1)
      IF( X(I) .LE. T ) GO TO 265
      K = I
  270 X(K+1) = X(K)
      Y(K+1) = Y(K)
      K = K-1
      IF( T .LT. X(K) ) GO TO 270
      X(K+1) = T
      Y(K+1) = TY
      GO TO 265
!
! *** CLEAN UP
!
  300 IF( KFLAG .GE. 1 ) RETURN
!
      DO 310 I = 1, NN
        X(I) = -X(I)
  310 CONTINUE
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE SPLASH( )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine writes an opening screen to the terminal.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Version 1.0
!!    Paul W. Eslinger : 23 Oct 1992 : Version 1.11
!!      Add copyright notice
!!    Paul W. Eslinger :  3 Feb 1993 : Version 1.11.1
!!      Modify copyright notice
!!    Paul W. Eslinger : 11 April 2009 : Version 1.12.0
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Iden_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
!---- Executable code -------------------------------------------------------------
!
      WRITE(*,1000) PRGNAM, PRGVER, PRGDAT
 1000 FORMAT(/ &
        1X,'   Latin Hypercube Sample Generation for FRAMES'/ &
        1X,'--------------------------------------------------'/ &
        1X,'                Copyright Notice'/&
        1X,'   Copyright, Battelle Memorial Institute, 2009.'/&
        1X,'              All Rights Reserved.'/&
        1X,'--------------------------------------------------'/ &
        1X,1X,A,1X,A,'   Last Modified: ',A/ &
        1X,'--------------------------------------------------')
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE TRMINO( A, N, NN, IFAULT )
!!**************************************************************************************************
!!
!!  Purpose:
!!    Given a lower triangular matrix of dimension N stored in the vector A(*), this subroutine
!!    calculates the lower triangular inverse of A.  The matrix A is overwritten by the inverse.
!!
!!
!!  Formal Parameters:
!!    Variable       Type        Description
!!    --------  ---------------  --------------------------------------
!!    A         Real Array (NN)  The input matrix stored as a one-dimensional array in the sequence
!!                               A(1,1), A(2,1), A(2,2), A(3,1), A(3,2), A(3,3), A(4,1) ...
!!
!!    N         Integer          Input: The order (dimension) of A
!!
!!    NN        Integer          Input: The size of the A arrays.  It must always be N(N+1)/2.
!!
!!    IFAULT    Integer          Output fault indicator equal to:
!!                                 1 : if N is less than 1
!!                                 2 : if A is singular
!!                                 3 : if NN .NE. N(N+1)/2
!!                                 0 : otherwise.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      INTEGER :: IVEC
!
! *** Call list variables
      INTEGER :: N, NN
      REAL, DIMENSION(NN) :: A
      INTEGER :: IFAULT
!
! *** Local variables
      REAL :: SMALL
      REAL(KIND=8) :: SUMX
      INTEGER :: I, J, K, IDXU, IDI, IXA, IXU
!
! *** A small divisor indicates singularity
!
      DATA SMALL / 1.0E-30 /
!
!---- Executable code -------------------------------------------------------------
!
! *** In-line (Statement) function for finding indices
!
      IVEC(I,J) = (I-1)*I/2 + J
!
! *** Error checking on the inputs
!
      IFAULT = 1
      IF( N .LE. 0 ) RETURN
      IFAULT = 3
      IF( NN .NE. N*(N+1)/2 ) RETURN
      IFAULT = 2
!
! *** Start of the inverse
!
      IDXU = NN
      DO K = N, 1, -1
        IF( ABS(A(IDXU)) .LT. SMALL ) RETURN
        A(IDXU) = 1.0 / A(IDXU)
        IDXU = IDXU - 1
        DO I = K-1, 1, -1
          SUMX = 0.0D0
          DO J = I+1, K
            IXA = IVEC(J,I)
            IXU = IVEC(K,J)
            SUMX = SUMX + A(IXA)*A(IXU)
          END DO
          IDI = IVEC(I,I)
          IF( ABS(A(IDI)) .LT. SMALL ) RETURN
          A(IDXU) = -SUMX / A(IDI)
          IDXU = IDXU - 1
        END DO
      END DO
!
      IFAULT = 0
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE TRMULO( A, B, N, NN, IFAULT )
!!**************************************************************************************************
!!
!!  Purpose:
!!    Given two lower triangular matrices of dimension N stored in the vectors A(*) and B(*), this
!!    subroutine calculates the lower triangular product A*B and puts the result in the vector B.
!!
!!
!!  Formal Parameters:
!!    Variable       Type        Description
!!    --------  ---------------  -------------------------------------------------------------------
!!    A         Real Array (NN)  Input matrix stored as a one-dimensional array in the sequence
!!                               A(1,1), A(2,1), A(2,2), A(3,1), A(3,2), A(3,3), A(4,1) ...
!!
!!    B         Real Array (NN)  Input: Matrix stored as a one-dimensional array as in the A vector
!!                               Output: Product matrix stored as a one-dimensional array as in the
!!                               A vector.
!!
!!    N         Integer          Input: The order (dimension) of A
!!
!!    NN        Integer          Input: The size of the A and B arrays.  It must always be N(N+1)/2.
!!
!!    IFAULT    Integer          Output fault indicator equal to:
!!                                 1 if N is less than 1
!!                                 2 if NN .NE. N(N+1)/2
!!                                 0 otherwise.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL :: IVEC
!
! *** Call list variables
      INTEGER :: N, NN
      REAL, DIMENSION(NN) :: A, B
      INTEGER :: IFAULT
!
! *** Local variables
      REAL(KIND=8) :: SUMX
      INTEGER :: I, J, K, IA, IB
!
!---- Executable code -------------------------------------------------------------
!
! *** In-line (Statement) function for finding indices
!
      IVEC(I,J) = (I-1)*I/2 + J
!
! *** Error checking on the inputs
!
      IFAULT = 1
      IF( N .LE. 0 ) RETURN
      IFAULT = 2
      IF( NN .NE. N*(N+1)/2 ) RETURN
      IFAULT = 0
!
! *** Start of the multiply
!
      DO I = N, 1, -1
        DO J = 1, I
          SUMX = 0.0
          DO K = J, I
            IA = IVEC(I,K)
            IB = IVEC(K,J)
            SUMX = SUMX + A(IA)*B(IB)
          END DO
          IB = IVEC(I,J)
          B(IB) = SUMX
        END DO
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      REAL FUNCTION U01( DSEED )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This function generates a real random uniform(0,1) variate given an initial value of the
!!    seed, DSEED.  The input seed is updated to a new seed using a linear congruential method.
!!
!!  Implementation:
!!    The current usage requires a system with at least a 32 bit word length for a REAL type
!!    variable.  Internal double precision computations are performed.
!!
!!  Call List:
!!    DSEED : Double precision seed for random number generator.  On input, it must take a value
!!            between 1.0 and 2147483646.0, inclusive.  On output, it has been updated to a new
!!            value in the same range.
!!
!!  Note:
!!    The user must provide an initial value for DSEED.  The value of DSEED should never be modified
!!    by the user after a call to U01.
!!
!!  Auxiliary Routines Needed:
!!    None
!!
!!  Reference:
!!    Lewis, Goodman, and Miller.  1969.
!!    "A Pseudo-Random Number Generator for the System/360"
!!    IBM Systems Journal, Vol. 8, No. 2, pp. 136-145.
!!
!!  History:
!!    Paul W. Eslinger : 19 Jan 2000 : Version 1.0
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      REAL(KIND=8) :: DSEED ! Random seed
!
! *** Local variables
      REAL(KIND=8) :: B ! Multiplier for the function
      REAL(KIND=8) :: M ! Modulus for the function
!
! *** Data definitions
      DATA B /      16807.0D0 /
      DATA M / 2147483647.0D0 /
!
!---- Executable code ---------------------------------------------------
!
      DSEED = IDINT( DMOD(B*DSEED,M) )
      U01   = SNGL( DSEED/M )
!
      RETURN
      END FUNCTION
!
!
      SUBROUTINE U01S1( N, RVAL, IWORK, DSEED, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This routine computes a vector of randomly generated numbers on the interval (0,1) with
!!    stratification using equal probable intervals and exactly one value per strata.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!
!!  Formal Parameters:
!!    DSEED  : Input - Double precision: Seed for the random number generator. It must lie
!!             between 1.0 and 2147483646.0 inclusive.
!!    N      : Input - Integer: Number of random values to generate, also, the number of strata
!!    RVAL   : Output - Real: Vector of length N of uniform (0,1) values
!!    IWORK  : Input - Integer: Work array of length N
!!    IERR   : Output - Integer: Error indication  (>0 means error, 0=good)
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      REAL, EXTERNAL :: U01
!
! *** Call list variables
      INTEGER :: N
      REAL, DIMENSION(N) :: RVAL
      INTEGER, DIMENSION(N) :: IWORK
      REAL(KIND=8) :: DSEED
      INTEGER :: IERR
!
! *** Local variables
      INTEGER :: I, KFLAG
      REAL :: RN, RIM1
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** The stratified samples will be generated sequentially across
!     the interval [0,1] - they must then be randomly permuted.
!     Generate a vector of values from the uniform distribution
!     and use their ranks to permute the generated values.
!
      DO I = 1, N
        RVAL(I) = U01( DSEED )
        IWORK(I)  = I
      END DO
!
! *** After the sort, the work vector contains the ranks for doing
!     the permutations
!
      KFLAG = 2
      CALL SSORTI( RVAL, IWORK, N, KFLAG, IERR )
      IF( IERR .NE. 0 ) RETURN
!
! *** Generate the stratified values sequentially and store them in
!     the permuted locations
!
      RN = N
      DO I = 1, N
        RIM1 = I - 1
        RVAL(IWORK(I)) = (RIM1 + U01(DSEED)) / RN
!       Logic to prevent roundoff to 0 or 1
        IF( RVAL(IWORK(I)) .LE. 0.0 )  RVAL(IWORK(I)) = TINY(1.0)
        IF( RVAL(IWORK(I)) .GE. 1.0 )  RVAL(IWORK(I)) = 0.9999999
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE UNIV( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine scheduls computation of some univariate statistics for each of the
!!    "stochastic" variables.  The computed values are then written to the report file.
!!    The statistics are also stored for later use in the variable XSTATS.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Data_Mod
      USE Iden_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR
!
! *** Local variables
      CHARACTER(LEN=4) :: CALLER = 'UNIV'
      INTEGER :: ISTOC, I, J
      REAL :: XMIN, XMAX, XAVG, XMED, XSTD, XCOV, XSKW
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Don't compute the statistics if all of the variables are defined as constants
!
      IF( PTOT .LE. 0 ) THEN
        RETURN
      END IF
!
      CALL PAGER( IRPT )
      WRITE(IRPT,1005) TITLE
 1005 FORMAT('Title: ',A)
      WRITE(IRPT,1000)
 1000 Format(/'Univariate statistics for the "stochastic" variables.')
!
      WRITE(IRPT,1010)
 1010 FORMAT(/'Variable     Minimum     Maximum        Mean      Median    St. Dev.   C. of Var.')
!
! *** Loop over all of the variables defined for this problem
!
      ISTOC = 0
      DO I = 1, PTOT
        IF( VTYPE(I) .NE. 0 ) THEN
          ISTOC = ISTOC + 1
!
! ***     Pull out one "stochastic" variable at a time
          DO J = 1, N
            XTMP(J) = X(J,ISTOC)
          END DO
!
! ***     Compute the univariate statistics
          CALL USTAT( XTMP, N, RWORK, XMIN, XMAX, XAVG, XMED, XSTD, XCOV, XSKW, IERR )
          IF( IERR .NE. 0 ) THEN
            IERR = 999
            MESSAG(1) = 'Error in lower level routine USTST'
            CALL PRTERR( IERR, CALLER, 1 )
            RETURN
          END IF
!
! ***     Output the univariate statistics
!          WRITE(*,*) VNAME(I), XMIN, XMAX, XAVG, XMED, XSTD, XCOV
          WRITE(IRPT,1050) VNAME(I), XMIN, XMAX, XAVG, XMED, XSTD, XCOV
 1050     FORMAT(A9,1P,6(1X,E11.4))
!
! ***     Save the statistics for later use
          XSTATS(ISTOC,1) = XMIN
          XSTATS(ISTOC,2) = XMAX
          XSTATS(ISTOC,3) = XAVG
          XSTATS(ISTOC,4) = XMED
          XSTATS(ISTOC,5) = XSTD
          XSTATS(ISTOC,6) = XCOV
!
        END IF
!
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE UPCASE( CVAR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine converts all of the lower case (a-z) characters in CVAR to upper case (A-Z)
!!    characters.  All other characters are unmodified.  The number of characters in CVAR to be
!!    examined are found by triming all blanks from the implied length of the character string.
!!
!!  History:
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*) :: CVAR ! Variable to convert to uppercase
!
! *** Local variables
      INTEGER :: CLEN ! Number of characters in the input character variable
      INTEGER :: I  ! Looping variable
      INTEGER :: IC ! Ascii sequence number for a character
!
!---- Executable code ---------------------------------------------------
!
! *** Number of characters in the string
      CLEN = LEN_TRIM( CVAR )
!
! *** Return without action for a null string
      IF( CLEN .EQ. 0 ) RETURN
!
! *** Convert all lowercase alphabetic characters to uppercase
      DO I = 1, CLEN
        IC = ICHAR( CVAR(I:I) )
        IF( IC.GE.97 .AND. IC.LE.122 ) CVAR(I:I) = CHAR(IC-32)
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      REAL FUNCTION USRDST( RV, NUSER, UUSER, XUSER )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This function transforms a uniform value on the interval [0,1] to a user defined distribution.
!!    The user distribution is defined by the piecewise linear cummulative confidence function given
!!    by data points (UUSER(I),XUSER(I)).
!!
!!    Note: This function is really a general purpose interpolation routine.  Given the definition
!!    of a function using X,F(X) pairs of data, USRDEF returns the F(X) value corresponding to a new
!!    X value (passed in RV).
!!
!!
!!  Formal Parameters:
!!    RV     = Input - Real: The uniform value to transform to a value from the distribution.
!!
!!    NUSER  : Input - Integer: The number of data points in the definition of the cumulative
!!             distribution function.
!!
!!    XUSER  : Input - Real Vector: The distribution values corresponding to the probability
!!             levels in UUSER.
!!
!!    UUSER  : Input - Real Vector: The probability levels corresponding to the values of the
!!             distribution given in XUSER.  No duplicate values are allowed. The entries in UUSER
!!             must be given in strictly increasing algebraic order.
!!
!!   Notes:
!!     If RV is less than UUSER(1), then USRDST = XUSER(1)
!!     If RV is greater than UUSER(NUSER) then USRDST = XUSR(NUSER)
!!
!!     That is, extreme values are moved in to the nearest point of definition for the distribution.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      REAL :: RV
      INTEGER :: NUSER
      REAL, DIMENSION(NUSER) :: UUSER, XUSER
!
! *** Local variables
      INTEGER :: J, IDX
      REAL :: DEL
!
!---- Executable code -------------------------------------------------------------
!
! *** Map the small value to the first defined pair
      IF( RV .LE. UUSER(1) ) THEN
        USRDST = XUSER(1)
        RETURN
      END IF
!
! *** Map the large value to the last defined pair
      IF( RV .GE. UUSER(NUSER) ) THEN
        USRDST = XUSER(NUSER)
        RETURN
      END IF
!
! *** Find the index for linear interpolation
      DO J = 1, (NUSER-1)
        IF( (RV.GE.UUSER(J)) .AND. (RV.LT.UUSER(J+1)) ) THEN
          IDX = J
          GO TO 20
        END IF
      END DO
!
  20  CONTINUE
!
! *** Compute the interpolated value
      DEL    = (RV-UUSER(IDX)) / (UUSER(IDX+1)-UUSER(IDX))
      USRDST = XUSER(IDX) + DEL*(XUSER(IDX+1)-XUSER(IDX))
!
      RETURN
      END FUNCTION
!
!
      SUBROUTINE USTAT( X, N, WORK, XMIN, XMAX, XAVG, XMED, XSTD, XCOV, XSKW, IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine will compute some summary statistics for the N values in the vector X.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!
!!  Auxiliary Routines Required:
!!    SORT : A subroutine to do a sort on a REAL vector
!!
!!
!!  Variable Descriptions:
!!    Inputs
!!    ----------------------------------------------------
!!    X    : Vector of input values.
!!    N    : Number of values in the vector X.
!!    WORK : A work vector with the same dimension as X.
!!
!!    Outputs
!!    ----------------------------------------------------
!!    XMIN : Minimum value in X.
!!    XMAX : Maximum value in X.
!!    XAVG : Average (mean) value in X.
!!    XMED : Median of the values in X.
!!    XSTD : Standard deviation of the values in X.
!!    XCOV : Coefficient of variation of the values in X.
!!    XSKW : Skewness of the values in X.
!!    IERR : Error flag
!!             0 : Normal termination
!!             1 : N < 1, no data values and no calculations
!!             2 : Unexpected sort error
!!             3 : N = 1, no calculations for XSTD, XCOV or XSKW
!!             4 : XSTD <= 0.0, no calculations for XCOV or XSKW
!!             5 : XAVG = 0, no calculation for XCOV
!!             6 : N = 2, no calculation for XSKW
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: N
      REAL, DIMENSION(N) :: X
      REAL :: XMIN, XMAX, XAVG, XMED, XSTD, XCOV, XSKW
      REAL, DIMENSION(N) :: WORK
      INTEGER :: IERR
!
! *** Local variables
      REAL(KIND=8) :: SUMX, SUM2
      REAL :: SMALL
      INTEGER :: I, KFLAG
!
! *** The value SMALL is to prevent an underflow in the coefficient
!     of variation calculation
!
      DATA SMALL / 1.0E-30 /
!
!---- Executable code -------------------------------------------------------------
!
!----------------------------------------------------------------------C
!              Initialize and check for error on entry
!----------------------------------------------------------------------C
!
      IERR = 0
      XMIN = 0.0
      XMAX = 0.0
      XAVG = 0.0
      XSTD = 0.0
      XCOV = 0.0
      XSKW = 0.0
      IF( N .LT. 1 ) THEN
        IERR = 1
        RETURN
      END IF
!
!----------------------------------------------------------------------C
!            Sort the data to find the median efficiently
!               Use the WORK array for all calculations
!----------------------------------------------------------------------C
!
      DO I = 1, N
        WORK(I) = X(I)
      END DO
!
      KFLAG = 1
      CALL SORT( WORK, N, KFLAG, IERR )
      IF( IERR .NE. 0 ) THEN
        IERR = 2
        RETURN
      END IF
!
!----------------------------------------------------------------------C
!               Find the minimum, maximum, and median
!----------------------------------------------------------------------C
!
      XMIN = WORK(1)
      XMAX = WORK(N)
      IF( N .EQ. 1 ) THEN
        XMED = WORK(N)
      ELSE
        IF( MOD(N,2) .EQ. 1 ) THEN
          XMED = WORK(N/2+1)
        ELSE
          XMED = (WORK(N/2)+WORK(N/2+1)) / 2.0
        END IF
      END IF
!
!----------------------------------------------------------------------C
!                          Find the mean
!----------------------------------------------------------------------C
!
      SUMX = 0.0
      DO I = 1, N
        SUMX = SUMX + WORK(I)
      END DO
      XAVG =  SUMX / FLOAT(N)
!
!----------------------------------------------------------------------C
!                   Find the standard deviation
!              Also sum for the skewness calculation
!----------------------------------------------------------------------C
!
      SUMX = 0.0
      SUM2 = 0.0
      DO I = 1, N
        SUMX = SUMX + (WORK(I)-XAVG)**2
        SUM2 = SUM2 + (WORK(I)-XAVG)**3
      END DO
!
      IF( N .EQ. 1 ) THEN
        XSTD = 0.0
        IERR = 3
        RETURN
      ELSE
        XSTD = SQRT( SUMX/FLOAT(N) )
      END IF
!
      IF( XSTD .LE. 0.0 ) THEN
        IERR = 4
        RETURN
      END IF
!
!----------------------------------------------------------------------C
!                 Find the coefficient of variation
!----------------------------------------------------------------------C
!
      IF( ABS(XAVG) .GT. SMALL ) THEN
        XCOV = XSTD / ABS(XAVG)
      ELSE
        XCOV = 0.0
        IERR = 5
      END IF
!
!----------------------------------------------------------------------C
!                         Find the skewness
!----------------------------------------------------------------------C
!
      IF( N .EQ. 2 ) THEN
        IERR = 6
        RETURN
      END IF
      XSKW = (SUM2/FLOAT(N)) / XSTD**3
!
! *** Exit Normally
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE WRDATA( IERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!    This subroutine writes a data file of all computed data.
!!
!!
!!  History:
!!    Paul W. Eslinger : 01 Jul 1992 : Original Source
!!    Paul W. Eslinger : 16 Apr 2009 : Convert to Fortran 95
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Files_Mod
      USE Control_Mod
      USE Data_Mod
      USE Errors_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IERR ! Error flag variable
!
! *** Local variables
      CHARACTER(LEN=6) :: CALLER = 'WRDATA' ! Name of this routine
      INTEGER :: I, J, ISTOC
!
!---- Executable code -------------------------------------------------------------
!
      IERR = 0
!
! *** Header for the output file
!
      CALL PAGER( IDAT )
      WRITE(IDAT,1000,ERR=9999) TITLE
 1000 FORMAT(A)
      WRITE(IDAT,1010,ERR=9999) PTOT, N
 1010 FORMAT(2(1X,I0),' Number of variables and number of iterations.')
!
! *** Data output for all realizations and all variables
!
      ISTOC = 0
      DO J = 1, PTOT
!
! ***   Write out the variable name and the distribution index
        WRITE(IDAT,1020,ERR=9999) VNAME(J), UNIQUE(J), VTYPE(J), VDESC(J)
 1020   FORMAT(A9,1X,A5,1X,I0/A76)
!
        IF( VTYPE(J) .NE. 0 ) THEN
! ***     Write out a "stochastic" variable
          ISTOC = ISTOC + 1
          DO I = 1, N
            WRITE(IDAT,1030,ERR=9999) I, X(I,ISTOC)
 1030       FORMAT(1X,I6,1P,1X,E11.4)
          END DO
!
        ELSE
! ***     Write out a "constant" variable
          DO I = 1, N
            WRITE(IDAT,1030,ERR=9999) I, XOUT(J)
          END DO
!
        END IF
!
      END DO
!
! *** Normal return point
      RETURN
!
! *** Write error encountered
 9999 CONTINUE
      IERR = 1
      MESSAG(1) = 'Write error encountered in the output file'
      CALL PRTERR( IERR, CALLER, 1 )
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE RDBLK( IKEY, IPERR, TITLE, IRERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!   This module reads a block of data from a text input data file
!!   (logical unit IKEY).  The block includes a keyword card and all
!!   subsequent continuation cards, to the next keyword card.
!!
!!    Output from this set of subroutines includes:
!!
!!      A) The keyword from the keyword line (KNAME, Char*8),
!!      B) The data values from that card and subsequent continuation
!!         cards (VALUE,real),
!!      C) Secondary keywords on the input card block (CVALUE, Char*8).
!!      D) Strings enclosed in double quotes.  (QUOTE, Char*64)
!!
!!  Call Arguments:
!!
!!   IKEY  : Logical unit number for text data input.  (input)
!!   IPERR : Logical unit number for error messages.  (input)
!!   IRERR : Error message indicator (0=no error).  (output)
!!   TITLE : Keyword line text, with the keyword deleted.  This
!!           returns the contents of the keyword line after the
!!           first separator. (output)
!!
!!  Usage Considerations:
!!
!!    1. All output keywords are stored as upper case characters.  All
!!    numeric values are stored as real numbers (not integer).  Quote
!!    strings are remain literally as given in the input deck.
!!
!!    2. Strings which are enclosed in double quotes are treated
!!    separately, to allow literal text (or strings of multiple words)
!!    to be read into the system.  These are referred to as 'quoted
!!    strings', and were originally intended to be used for file names.
!!    The input string can be of any length, but is truncated at
!!    (LENQQQ) characters.
!!
!!    3. The routines RDBLK and UPCASE require alphabetic characters to
!!    be 'represented' contiguously in memory (a thru z, and A thru Z).
!!    In routine SEPCHR, a TAB character is a valid separator only if it
!!    converts to a character code of less than 10 decimal.  ASCII code
!!    is generally expected, but not necessary if these conditions
!!    prevail.
!!
!!    4. Logical function CEXIST is not called from these routines, but
!!    is provided as a utility routine for the user.
!!
!!    5.  On input to this routine, it is assumed the next line of the
!!    input data file is already present in array INFO, except on the
!!    first call (ILINE=0).  ILINE must be initialized to zero by the
!!    main routine (or block data).
!!
!!    6. Array INFO must not be modified between calls to RDBLK.  It
!!    saves the previous keyword line from the input file.
!!
!!    7. The subroutine PRTERR is not used with these routines because
!!    there is no guarantee that the report file has been opened when
!!    an error is encountered by the RDBLK routines.
!!
!! Variables:
!!
!!   CVALUE = Array of secondary keywords from the input data block.
!!            Only the first eight characters are saved.  (CHAR*8)
!!   QUOTE  = Character string entity in the input block which was
!!            enclosed in double quotes.  (CHAR*64)
!!   IKEY   = Logical unit number for data read.
!!   ILINE  = Current line number of the input data file.
!!   INFO   = Saves the contents of the next line of the input file.
!!            (Dimension LENCRD, Char*1)
!!   LSTLIN = Line number of last keyword.  Identifies current command.
!!   KNAME   = Keyword from the current data block.  (CHAR*8)
!!   NCVALU = Number of secondary keywords from data block.
!!   NDXCHR = Indices in the data block of keyword entities.
!!   NDXQQQ = Indices in the data block of quoted entities.
!!   NDXVAL = Indices in the data block of numeric entities.
!!   NVALUE = Number of data values from the data block.
!!   NTITY  = Number of keyword and value entities read from a data
!!            block.
!!   TITLE  = Returns the entire contents of the keyword line,
!!            following the first separator.
!!   VALUE  = Array of data values from the input data block.
!!            These are stored as real values (not integers).
!!
!!  Auxiliary Routines:
!!
!!    RDBLK   Controlling routine for decoding keywords
!!    COMCHR  Identifies comment characters.
!!    DCDSEP  Parses the input line into numeric and character data.
!!    FINSEP  Locates separators in the input line.
!!    SAVTTL  Moves the entire contents of a keyword line, following
!!            the first separator, into an output character array.
!!    SEPCHR  Determines if a character is a legal separator.
!!    UPCASE  Converts character strings to upper case.  Depends on
!!            contiguous 'ordering' of alphabetic characters.
!!    XFKEY   Transfers a keyword from a character string to a char*8
!!            variable.  Blank fills for strings less than length 8.
!!    CEXIST  Logical function to determine if an 8 character string
!!            is present among the secondary keywords (CVALUE).
!!    NXTVAL  Returns the index and value in the VALUE array of the
!!            first numeric entry after the last keyword identified
!!            by the most recent call to CEXIST.
!!    NXTQOT  Returns the index and text in the QUOTE array of the
!!            first quote string after the last keyword identified
!!            by the most recent call to CEXIST.
!!
!!  History:
!!
!!    Dave Langford    : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 16 Mar 2000 : Convert to Fortran 90
!!                                     Add IRERR error return
!!    Paul W. Eslinger : 13 Mar 2003 : Increase card length logic
!!    Paul W. Eslinger :  5 Jan 2006 : Extend keyword line length
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Global parameters, variables, and arrays
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: COMCHR, SEPCHR
!
! *** Call list variables
      INTEGER :: IKEY
      INTEGER :: IPERR
      INTEGER :: IRERR
      CHARACTER*(LENCRD) :: TITLE
!
! *** Local variables
      INTEGER, DIMENSION(LENCRD) :: LOC
      INTEGER :: I    ! Local looping control variable
      INTEGER :: IERR ! Error number from read statement
      INTEGER :: NUM  ! Local number for separator character
!
!---- Executable code ---------------------------------------------------
!
      IRERR = 0
!
!---- Read first line(s) of data file, to a keyword card
!
      IF( ILINE .LE. 0 ) THEN
  100   CONTINUE
        ILINE = ILINE+1
        READ(IKEY,500,END=470,ERR=480,IOSTAT=IERR) (INFO(I),I=1,LENCRD)
        IF( ((INFO(1).LT.'A').OR.(INFO(1).GT.'Z')).AND. &
          ((INFO(1).LT.'a').OR.(INFO(1).GT.'z')) ) GO TO 100
      END IF
      LSTLIN = ILINE
!
      NTITY  = 0
      NCVALU = 0
      NQUOTE = 0
      NVALUE = 0
      NEXIST = 0
!
!---- Move keyword into array KNAME, with blank filling.
!
!      CALL XFKEY( INFO(1), KNAME )
      CALL XFKEY( INFO, KNAME )
!
!---- Move line data into array TITLE
!
      CALL SAVTTL( TITLE )
!
!---- Return if END card encountered
!
      IF( KNAME.EQ.'END     ' ) RETURN
!
!---- Locate all separators in this line.  This is
!     the top of loop for reading continuation cards.
!
  110 CONTINUE
      CALL FINSEP( NUM, LOC )
!
!---- Move character data into array CVALUE, and
!     numeric data into array VALUE.
!
      IF( NUM .GT. 1 ) CALL DCDSEP( NUM, LOC, IPERR, IRERR )
      IF( IRERR .NE. 0 ) RETURN
!
!---- Read next line and reformat
!
  120 CONTINUE
      ILINE = ILINE+1
      READ(IKEY,500,END=130,ERR=480,IOSTAT=IERR) (INFO(I),I=1,LENCRD)
!
!---- Return if next keyword card was found
!
      IF( ((INFO(1).GE.'A').AND.(INFO(1).LE.'Z')).OR. &
          ((INFO(1).GE.'a').AND.(INFO(1).LE.'z')) ) RETURN
!
!---- If comment card, read the next line.
!
      IF( COMCHR(1,INFO(1)) ) GO TO 120
!
!---- Check for a continuation card.  Loop up to decode, if found.
!
      IF( .NOT.SEPCHR(INFO(1)) ) THEN
         WRITE(IPERR,510)
         GO TO 490
      END IF
      GO TO 110
!
!---- Artificial END card generated
!---- Change to error return for no END card
!
  130 CONTINUE
      WRITE(IPERR,520)
!      INFO(1)='E'
!      INFO(2)='N'
!      INFO(3)='D'
!      INFO(4)=' '
      IRERR = 1
      RETURN
!
!---- Error handing
!
  470 CONTINUE
      WRITE(IPERR,970)
      GO TO 490
  480 CONTINUE
      WRITE(IPERR,980)
  490 CONTINUE
      WRITE(IPERR,990) ILINE, IERR
      IRERR = 1
      RETURN
!
  500 FORMAT(2048A1)
  510 FORMAT(/' ** Error! A continuation card is expected, but the current'/ &
              '    line does not begin with a valid separator character.')
  520 FORMAT(/' ** Error! End-of-File encountered on the input file.'/ &
              '    No END card was found by RDBLK.')
  970 FORMAT(/' ** Error! End-of-File encountered in the input file'/ &
              '    prior to reading a keyword card.')
  980 FORMAT(/' ** Error reading data from the input file.')
  990 FORMAT(4X,'Reading line number ',I5/ &
             4X,'I/O Error Status is ',I5/ &
             4X,'Error encountered in routine RDBLK')
!
      END SUBROUTINE
!
!
      LOGICAL FUNCTION CEXIST( KEYWRD )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!   This function determines if an eight letter keyword is one of
!!   the secondary keywords of the current data block.
!!
!!  Call Argument:
!!
!!    KEYWRD : The character string for which to search the list of
!!             secondary keywords.  (Input)
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=*) :: KEYWRD
!
! *** Local variables
      INTEGER :: N ! Local looping control variable
!
!---- Executable code ---------------------------------------------------
!
      DO N = 1, NCVALU
        IF( CVALUE(N) .EQ. KEYWRD ) THEN
          NEXIST = N
          CEXIST = .TRUE.
          RETURN
        END IF
      END DO
!
      NEXIST = 0
      CEXIST = .FALSE.
!
      RETURN
      END FUNCTION
!
!
      LOGICAL FUNCTION COMCHR( IPLACE, CH )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This routine determines if the character CH is a comment
!!    identifying character.  Comment lines are ignored, as are
!!    characters after a comment identifier on one line.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!! Call Arguments:
!!
!!   CH     : Character to examine.  (input)
!!   IPLACE : Indicates the position of character CH in the input
!!            line.  Note: the '/' character indicates a comment
!!            only if it is the first character in a line. (input)
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: IPLACE
      CHARACTER(LEN=1) :: CH
!
!---- Executable code ---------------------------------------------------
!
      IF( (CH.EQ.'!').OR.(CH.EQ.'$') ) THEN
        COMCHR = .TRUE.
      ELSE
        IF( CH.EQ.'/' .AND. IPLACE.EQ.1 ) THEN
          COMCHR = .TRUE.
        ELSE
          COMCHR = .FALSE.
        END IF
      END IF
!
      RETURN
      END FUNCTION
!
!
      SUBROUTINE DCDSEP( NUM, LOC, IPERR, IRERR )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!   This subroutine examines character data in the input line (INFO),
!!   decodes character data into the array CVALUE, and decodes numeric
!!   data into array VALUE.
!!
!!  Call Arguments:
!!
!!    LOC   : Locates the field separators from the input file. (input)
!!    NUM   : Indicates the number of separators present. (input)
!!    IPERR : Logical unit number for error messages. (input)
!!    IRERR : Error message indicator (0=no error).  (output)
!!
!!  Comments:
!!
!!    1. Real numbers may be written in exponential format, but only
!!    an 'E' or an 'e' are recognized exponent indicators.  The double
!!    precision 'D' exponent indicator is not valid.
!!
!!    2.  A '*' character interior to a line is taken to indicate a
!!    repetition of the following value or character field.  It must
!!    be preceded by an unsigned integer value.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger :  5 May 2000 : Version 1.0 - Add IRERR logic
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: NUM
      INTEGER, DIMENSION(LENCRD) :: LOC
      INTEGER :: IPERR
      INTEGER :: IRERR
!
! *** Local variables
      CHARACTER(LEN=1) :: CH
      CHARACTER(LEN=1), DIMENSION(LENCRD) :: DUMY
!
      LOGICAL :: DECSET
      LOGICAL :: EXPSET
      LOGICAL :: MINUS
!
      INTEGER :: I ! Looping control variable
      INTEGER :: J ! Looping index variable
      INTEGER :: K ! Looping index variable
!
      INTEGER :: IEND   ! Local counting variable
      INTEGER :: IS     ! Local counting variable
      INTEGER :: ISETNO ! Local counting variable
      INTEGER :: ISTART ! Local counting variable
      INTEGER :: MULT   ! Local counting variable
      INTEGER :: NCHARS ! Local counting variable
      INTEGER :: NDIGIT ! Local counting variable
      INTEGER :: NUMB   ! Local counting variable
!
      REAL :: RDATA ! Local real variable
      REAL :: D     ! Local real variable
!
!---- Executable code ---------------------------------------------------
!
      IRERR = 0
!
!---- Loop on the number of separators, minus one.
!     Initialize the character count between adjacent separators.
!
      DO 220 I = 1,(NUM-1)
        J = I+1
        ISTART = LOC(I)+1
        NCHARS = LOC(J)-ISTART
        IF( NCHARS.LT.1 ) GO TO 220
!
!----   Look for string in double quotes.  An inefficient transfer loop
!       is used, since this algorithm is not expected to be used much.
!
        IF( INFO(LOC(I)).EQ.'"' ) THEN
          IF( NQUOTE.GE.MAXQQQ ) GO TO 460
          NQUOTE = NQUOTE+1
          ISTART = ISTART-1
          DO J = 1,LENQQQ
            IF( J.LE.NCHARS ) THEN
              K = ISTART+J
              QUOTE(NQUOTE)(J:J) = INFO(K)
            ELSE
              QUOTE(NQUOTE)(J:J) = ' '
            END IF
          END DO
          NTITY = NTITY+1
          NDXQQQ(NQUOTE) = NTITY
          GO TO 220
        END IF
!
!----   Initializations for decoding loop
!
        DECSET = .FALSE.
        EXPSET = .FALSE.
        IEND   = LOC(J)-1
        ISETNO = 0
        MULT   =-1
        NDIGIT = 0
        RDATA  = 0.0
!
        IS   = LOC(I)
        IEND = LOC(J)-1
!
!----   Check for sign of numeric result
!
  110   CONTINUE
        MINUS = .FALSE.
        IF( INFO(IS+1).EQ.'+' ) THEN
          IS = IS+1
          IF( IS.GE.IEND ) GO TO 190
        END IF
        IF( INFO(IS+1).EQ.'-' ) THEN
          MINUS = .TRUE.
          IS    = IS+1
          IF( IS.GE.IEND ) GO TO 190
        END IF
!
!----   Primary loop decodes positive integer values
!
  120   CONTINUE
        ISETNO = ISETNO+1
        NUMB   = 0
        NDIGIT = 0
  130   CONTINUE
        IS = IS+1
        CH = INFO(IS)
        IF( (CH.GE.'0').AND.(CH.LE.'9') ) THEN
          READ(CH,940) J
          NUMB   = NUMB*10+J
          NDIGIT = NDIGIT+1
          IF( NDIGIT .GT. 10 ) GO TO 450
          IF( IS.GE.IEND ) THEN
            IF( MINUS ) NUMB = -NUMB
            GO TO 150
          END IF
          GO TO 130
        END IF
        IF( MINUS ) NUMB = -NUMB
!
!----   Check for repetition factor
!
        IF( CH.EQ.'*' ) THEN
          IF( (MULT.GT.0) .OR.(NDIGIT.LT.1) ) GO TO 190
          IF( (IS.GE.IEND).OR.(ISETNO.GT.1) ) GO TO 190
          IF( MINUS ) GO TO 110
          MULT   = NUMB
          ISTART = IS+1
          NCHARS = LOC(I+1)-ISTART
          GO TO 110
        END IF
!
!----   Check for decimal point.  Note that a 'minus' is saved.
!
        IF( CH.EQ.'.' ) THEN
          IF( DECSET.OR.EXPSET ) GO TO 190
          DECSET = .TRUE.
          IF( NDIGIT.GT.0 ) RDATA = FLOAT(NUMB)
          IF( IS.GE.IEND ) GO TO 170
          GO TO 120
        END IF
!
!----   Check for exponent marker
!
        IF( (CH.EQ.'E').OR.(CH.EQ.'e') ) THEN
          IF( EXPSET ) GO TO 190
          IF( (ISETNO.LT.2).AND.(NDIGIT.LT.1) ) GO TO 190
          EXPSET = .TRUE.
          IF( NDIGIT.GT.0 ) THEN
            D = FLOAT(NUMB)
            IF( DECSET ) THEN
              DO J = 1,NDIGIT
                D = 0.1*D
              END DO
            END IF
            RDATA = RDATA+D
          END IF
          IF( IS.GE.IEND ) GO TO 170
          GO TO 110
        END IF
!
!----   Cannot decipher string as a number
!
        GOTO 190
!
!----   Finish decoding numeric value
!
  150   CONTINUE
        IF( EXPSET ) THEN
          IF( NUMB.NE.0) RDATA = RDATA*(10.0**NUMB)
        ELSEIF( DECSET ) THEN
          D = FLOAT(NUMB)
          DO J = 1,NDIGIT
            D = 0.1*D
          END DO
          RDATA = RDATA+D
        ELSE
          RDATA = FLOAT(NUMB)
        END IF
!
!----   Fill in numeric data.  Add multiple values, if requested.
!
  170   CONTINUE
        NVALUE = NVALUE+1
        IF( NVALUE.GT.NVALS ) GO TO 470
        VALUE(NVALUE) = RDATA
        IF( NVALUE.LE.NKEYS ) THEN
          NTITY = NTITY+1
          NDXVAL(NVALUE) = NTITY
        END IF
!
        IF( MULT.GT.1 ) THEN
          IF( (NVALUE+MULT-1).GT.NVALS ) GO TO 470
          DO J = 2,MULT
            NVALUE = NVALUE+1
            VALUE(NVALUE) = RDATA
            IF( NVALUE.LE.NKEYS ) NDXVAL(NVALUE) = NTITY
          END DO
        END IF
        GOTO 220
!
!----   Transfer string as a character variable
!
  190   CONTINUE
        NCVALU = NCVALU+1
        IF( NCVALU.GT.NKEYS ) GO TO 480
        K=0
        DO J=ISTART,IEND
          K=K+1
          DUMY(K) = INFO(J)
        END DO
        DUMY(K+1) = ' '
        CALL XFKEY( DUMY, CVALUE(NCVALU) )
        NTITY = NTITY+1
        NDXCHR(NCVALU) = NTITY
!
!----   Fill in multiple character values, if requested
!
        IF( MULT.GT.1 ) THEN
          IF( (NCVALU+MULT-1).GT.NKEYS ) GO TO 480
          K = NCVALU
          DO J = 2,MULT
            NCVALU = NCVALU+1
            CVALUE(NCVALU) = CVALUE(K)
            NDXCHR(NCVALU) = NTITY
          END DO
        END IF
!
  220 CONTINUE
!
      RETURN
!
!---- Error handling
!
  450 CONTINUE
      IRERR = 1
      WRITE(IPERR,950)
      GOTO 490
  460 CONTINUE
      IRERR = 2
      WRITE(IPERR,960) MAXQQQ
      GOTO 490
  470 CONTINUE
      IRERR = 3
      WRITE(IPERR,970) NVALS
      GOTO 490
  480 CONTINUE
      IRERR = 4
      WRITE(IPERR,980) NKEYS
  490 CONTINUE
      WRITE(IPERR,990) ILINE
      RETURN
!
  940 FORMAT(I1)
  950 FORMAT(/' Error found in input data deck.  A number was encountered that'/ &
              ' was more than 10 digits long.  The limit is 10 digits.')
  960 FORMAT(/' Error in input data deck.  Too many strings in double quotes'/ &
              ' were found within a single data block.  Limit is ',I5,'.')
  970 FORMAT(/' Error in input data deck.  Too many numeric values'/ &
              ' were found within a single data block.  Limit is ',I5,'.')
  980 FORMAT(/' Error in input data deck.  Too many character values'/ &
              ' were found within a single data block.  Limit is ',I5,'.')
  990 FORMAT(/' Error at line ',I5,' of the input data file.'/ &
              ' Error termination in routine DCDSEP.')
      END SUBROUTINE
!
!
      SUBROUTINE FINSEP( NUM, LOC )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine locates all separators in the current input line,
!!    contained in the array INFO.
!!
!!  Call Arguments:
!!
!!    NUM : The total number of separators found.  (output)
!!    LOC : The location of each separator in the current input string.
!!          (output)
!!
!!  Comments:
!!
!!    1.  A dummy separator is placed at position LENCRD+1, if the last
!!    character in array INFO is a relevant character (not a comment
!!    or separator).
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: COMCHR, SEPCHR
!
! *** Call list variables
      INTEGER :: NUM
      INTEGER, DIMENSION(LENCRD) :: LOC
!
! *** Local variables
      LOGICAL :: INQUOT
      INTEGER :: I ! Looping control variable
      INTEGER :: LENGTH ! Local index for length of line processed
!
!---- Executable code ---------------------------------------------------
!
!---- Initialize pointers, exit if comment line
!
      NUM    = 0
      LENGTH = 0
      IF( COMCHR(1,INFO(1)) ) RETURN
!
!---- Determine length of line
!
      DO I = 1, LENCRD
        IF( INFO(I) .NE. ' ' ) LENGTH = I
      END DO
      IF( LENGTH .LT. LENCRD ) LENGTH = LENGTH+1
      IF( LENGTH .EQ. 0 ) RETURN
!
!---- Loop on the characters of the input line.  Check first for a
!     string in double quotes, then a comment character, then for a
!     separator character.
!
      INQUOT = .FALSE.
      DO 110 I = 1, LENGTH
!
!---- Look first for anything in double quotes
!
        IF( INFO(I).EQ.'"' ) THEN
          NUM = NUM+1
          LOC(NUM) = I
          INQUOT = .NOT.INQUOT
          GO TO 110
        END IF
        IF( INQUOT ) GO TO 110
!
!---- Look for comment character.  Terminate line if found
!
        IF( COMCHR(I,INFO(I)) ) THEN
          IF( (I.GT.1).AND.(NUM.GT.0) ) THEN
            IF( LOC(NUM).LT.(I-1) ) THEN
              NUM = NUM+1
              LOC(NUM) = I
            END IF
          END IF
          RETURN
        END IF
!
!--- Look for separator character
!
        IF( SEPCHR(INFO(I)) ) THEN
          NUM = NUM+1
          LOC(NUM) = I
        END IF
!
  110 CONTINUE
!
!---- Add ending separator location if:
!      1) Still within a double quotation
!      2) End of line has been reached without hitting a separator
!
      IF( INQUOT .OR. ((LENGTH.EQ.LENCRD).AND.(.NOT.SEPCHR(INFO(LENCRD)))) ) THEN
        NUM = NUM+1
        LOC(NUM) = LENCRD+1
      END IF
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE SAVTTL( TITLE )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!   This subroutine saves the contents of the input line, following
!!   the first separator.
!!
!!  Call Argument:
!!
!!    TITLE : The text of the command line, with the leading keyword
!!            stripped off.  (output)
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford    : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger : 19 Sep 2002 : Increase card length logic
!!    Paul W. Eslinger :  5 Jan 2006 : Extend keyword line length
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: SEPCHR
!
! *** Call list variables
      CHARACTER(LEN=LENCRD) :: TITLE
!
! *** Local variables
      INTEGER :: IST ! Index of first quaote mark
      INTEGER :: I   ! Looping control variable
      INTEGER :: NB  ! Local index value for number of blanks
!
!---- Executable code ---------------------------------------------------
!
      IST=0
      DO I = 1, LENCRD
        IF( IST.EQ.0 ) THEN
          IF( SEPCHR(INFO(I)) ) IST = I
        ELSE
          IF( .NOT.SEPCHR(INFO(I)) ) THEN
             IST = I
             GO TO 110
          END IF
        END IF
      END DO
!
      WRITE(TITLE,500) (INFO(I),I=1,LENCRD)
  500 FORMAT(2048A1)
      RETURN
!
  110 CONTINUE
!
      NB = IST-1
      WRITE(TITLE,500) (INFO(I),I=IST,LENCRD),(' ',I=1,NB)
!
      RETURN
      END SUBROUTINE
!
!
      LOGICAL FUNCTION SEPCHR( CH )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This function determines if a character is a legal command
!!    line separator.
!!
!!  Call Argument:
!!
!!    CH : Character to examine for being a separator.  (input)
!!
!!  Comment:
!!
!!    The current conventions for RDBLK also use double quotes
!!    (") as a separator character.  It is used to indicate long
!!    text strings to be saved as literals.  However, they must
!!    be located in routines FINSEP and DCDSEP, rather than
!!    identified here.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      CHARACTER(LEN=1) :: CH
!
!---- Executable code ---------------------------------------------------
!
      IF( (ICHAR(CH).LT.10) .OR. &
        (CH.EQ.' ') .OR. (CH.EQ.',')  .OR. &
        (CH.EQ.'=') .OR. (CH.EQ.':')  .OR. &
        (CH.EQ.';') .OR. (CH.EQ.'''') .OR. &
        (CH.EQ.'(') .OR. (CH.EQ.')') ) THEN
        SEPCHR=.TRUE.
      ELSE
        SEPCHR=.FALSE.
      END IF
!
      RETURN
      END FUNCTION
!
!
      SUBROUTINE NXTQOT( KINDEX, FSTRNG )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!   This subroutine returns the index and value of the 'next' quoted
!!   string in the data block, after the last keyword identified by a
!!   call to the function CEXIST.
!!
!!
!!  Call Arguments:
!!
!!    KINDEX : Index of desired quoted string, in the array QUOTE.
!!             (output)
!!    FSTRNG : Desired quoted string.  (output)
!!
!!
!!  Comment:
!!
!!    1.  If no previous call was made to CEXIST, if the desired
!!    keyword was not found, or if no subsequent quoted string
!!    data is present in the data block, the index is returned
!!    as zero.
!!
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 16 March 2000 : Version 1.0 : Add check on NQUOTE
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: KINDEX
      CHARACTER(LEN=*) :: FSTRNG
!
! *** Local variables
      INTEGER :: I      ! Local looping control value
      INTEGER :: N      ! Local index value
      INTEGER :: NSERCH ! Local looping value
!
!---- Executable code ---------------------------------------------------
!
      KINDEX   = 0
      FSTRNG = ' '
!
!---- No quote strings were entered
!
      IF( NQUOTE .LT. 1 ) RETURN
!
!---- No 'previous' call to CEXIST, or the keyword was not found
!
      IF( (NEXIST.LT.1) .OR. (NEXIST.GT.NKEYS) ) RETURN
!
!---- Check to see if numeric data is present
!
      N = NDXCHR(NEXIST)
      IF( N .GE. NDXQQQ(NQUOTE) ) RETURN
!
!---- Locate index and text of the next item in the input list
!
      NSERCH = MIN( NQUOTE, MAXQQQ )
      DO I = 1, NSERCH
        IF( NDXQQQ(I) .GT. N ) THEN
          KINDEX = I
          FSTRNG = QUOTE(KINDEX)
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE NXTVAL( KINDEX, KVAL )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!    This subroutine returns the index and value of the 'next'
!!    numeric value in the the data block, after the last keyword
!!    identified by a call to the function CEXIST.
!!
!!  Call Arguments:
!!
!!    KINDEX : Index of desired value, in the data array VALUE. (output)
!!    KVAL   : Desired numeric value.  (output)
!!
!! Comment:
!!
!!   If no previous call was made to CEXIST, if the desired keyword
!!   was not found, or if no subsequent numeric data is present in
!!   the data block, the index and value are returned as zero.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 16 March 2000 : Version 1.0 : Add check on NVALUE
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Global variables
      USE Rdblk_Mod
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** Call list variables
      INTEGER :: KINDEX ! Index of desired value, in the data array VALUE
      REAL :: KVAL      ! Desired numeric value
!
! *** Local variables
      INTEGER :: N ! Local index value
      INTEGER :: I ! Local looping control value
!
!---- Executable code ---------------------------------------------------
!
      KINDEX = 0
      KVAL   = 0
!
!---- No 'previous' call to CEXIST, or the keyword was not found
!
      IF( (NEXIST.LT.1) .OR. (NEXIST.GT.NKEYS) ) RETURN
!
!---- No values were entered
!
      IF( NVALUE .LT. 1 ) RETURN
!
!---- Check to see if numeric data are present
!
      N = NDXCHR(NEXIST)
      IF( N .GE. NDXVAL(NVALUE) ) RETURN
!
!---- Locate index and value of the next item in the input list
!
      DO I = 1, NVALUE
        IF( NDXVAL(I).GT.N ) THEN
          KINDEX = I
          KVAL   = VALUE(KINDEX)
          RETURN
        END IF
      END DO
!
      RETURN
      END SUBROUTINE
!
!
      SUBROUTINE XFKEY( INFO, KEYWRD )
!!**************************************************************************************************
!!
!!  Purpose:
!!
!!   This subroutine examines the first eight characters in array
!!   INFO, in order to move that data into variable KEYWRD.  The
!!   keyword in KEYWRD is also converted to all upper case.
!!
!!  Call Arguments:
!!
!!    INFO   : The first eight characters of a keyword line.  (Input)
!!    KEYWRD : The keyword, as taken from the keyword line.  (Output)
!!
!! Comment:
!!
!!   If a separator is present, the relevant characters are
!!   transferred to variable KEYWRD, and the remainder of KEYWRD
!!   is blank filled.
!!
!!  Auxiliary Routines:
!!
!!    This routine is part of the RDBLK library.
!!
!!  History:
!!
!!    Dave Langford : 26 Jun 1992 : Original Version
!!    Paul W. Eslinger : 13 Mar 2000 : Version 1.0
!!    Paul W. Eslinger :  6 Jun 2007 : Udate comments
!!
!!**************************************************************************************************
!
! *** Force explicit typing of all variables and functions
      IMPLICIT NONE
!
! *** User defined functions
      LOGICAL, EXTERNAL :: SEPCHR
!
! *** Call list variables
      CHARACTER(LEN=1), DIMENSION(8) :: INFO
      CHARACTER(LEN=8) :: KEYWRD
!
! *** Local variables
      CHARACTER(LEN=1), DIMENSION(8) :: JNFO ! Local vector of same size as INFO
      INTEGER :: I ! Looping variable
      INTEGER :: J ! Looping variable
!
!---- Executable code ---------------------------------------------------
!
      DO I = 1, 8
        JNFO(I) = INFO(I)
        IF( SEPCHR(INFO(I)) ) THEN
          DO J = I, 8
            JNFO(J) = ' '
          END DO
          EXIT
        END IF
      END DO
!
      WRITE(KEYWRD,'(8A1)') (JNFO(I),I=1,8)
      CALL UPCASE( KEYWRD )
!
      RETURN
      END SUBROUTINE
