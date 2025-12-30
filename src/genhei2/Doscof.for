*-----------------------------------------------------------------------
*
      subroutine dosecof(nuke, f1, class, amad, ipth, jban)
*
*-----------------------------------------------------------------------
*     routine:  dosecof
*     author:   k.f. eckerman
*     data:     05/20/96
*     purpose:  assemble the decay chain and extract the dose coefficients
*               for the chain members.
*-----------------------------------------------------------------------
C
C     5 NOV 98   BAN    ADDED READ FOR WATER SUBMERSION FACTORS
C     6 Jun 01   BAN    Corrected reads for Inhalation Class
C     18 Nov 02  BAN    Allowed selection of alternate ingestion cases via jban
C
C-----------------------------------------------------------------------
*     input variables
*       nuke       parent of the chain in standard notation; e.g., Cs-137
*       f1o(mage)  f_1 value for oral intakes (defaults to highest effective
*                  dose
*       class      inhalation absorption type for parent.  decay products
*                  will be of same type, if possible, or type with highest
*                  effective dose.
*       f1i(mage)  f_1 value for inhalation.  coefficients are actually
*                  picked by class.
*       amad       amad for the aerosol in æm.
*       IPTH(i)   pathway logical flag of size 9. 1 inhalation, 2 ingestion,
*                  3 submersion, 4 ground surface, 5 infinite, 
*
*     output 
*     output is through the common blocks in the include file dcfpak.cmn
*     as shown below.
*
*      common /dfacts/ organ(morg), df(mspec, mfact, morg), f1inh(mspec),
*     :                f1orl(mspec), classo(mspec), iflag(mspec, mfact), 
*     :                nint, next
*       organ      character*9 array of organ names.
*       df         dose factors array by chain member, pathway, and organ
*       f1inh      array of f_1 values for inhalation by chain member
*       f1orl      array of f_1 values for ingestion by chain member
*       classo     array of class notation for inhalation by chain member
*       iflag      logical flags for dose factor by chain member and pathway
*       nint       length of chain for internal factors
*       next       length of chain for external factors
*
*      common /radat/ thalf(mspec), iu(mspec), nucnam(mspec), 
*     :               branch(mspec, mspec), lmr(mspec),
*     :               ibr(mspec,mspec), nbr(mspec), nspec
*       thalf      half-life of chain members
*       iu         units of the half-lives
*       nucnam     names of the chain members
*       branch     branching fraction
*       lmr        decay constant in 1/d
*       ibr        branch pointer
*       nbr        number of branches for chain members
*       nspec      length of chain
*
      include 'pakparm.cmn'
      include 'dcfpak.cmn'
      include 'batch.cmn'
      character*7 nuke, nuclide
      character*1 class, clasp, redclas
      integer ipt, ibinry
      logical IPTH(9)
      dimension dx(35), ifi(8)
      include 'iolist.cmn'
      data ifi /i30, i31, i32, i33, i34, i35, i36, i37/
*
C      if (.not. dbatch) call cls
*
*     zero out the dose factor array
*
      call zerom
*
*     assemble the decay chain
*
      call chain (nuke)
c      call pauseit
*
*     first do external coefficients for the next chain members
*
      if (IPTH(3)) then
        do ispec = 1, next
          iflag(ispec, 3) = .false.
          nuke = nucnam(ispec)
          ipt = ibinry( nuke )
          if (ipt .ne. 0) then

CCC  CHANGE TO FGR 13 CD FILE FORMAT FROM ECKERMAN'S ORIGINAL  BAN  22 NOV 2001
            read(idex,'(a7,a8,a2,a8,126x,i4,12x,i6)', rec=ipt)
     :        nuke, t, ix, mode, iex, iex1
            if (iex .ne. 0) then
               iflag(ispec, 3) = .true.
               do ip = 1, mext
                  read(ifi(ip+2),'(a7,27e9.0)', rec=iex+3) nuclide,
     :                (dx(j), j=1, 27)
                  do ir = 1, 24     
                     dfext(ispec,ip, ir)= dx(iextorg(ir))
                  end do 
               end do
                  READ (IDEXW,'(7X,1P23E9.0,9X,E9.0)',REC=IEX+3) 
     .                 (DFEXTW(ISPEC,J),J=1,24) 
              jrec = iex1 + 2      ! 3 head records
               do ip = 1, mext
                  do ican = 1, mcan
                     jrec = jrec + 1
c                     read(i35,'(a7,57x,e9.0,36x,e9.0)',rec=jrec) 
c     :                   nuclide, (rFextD(ispec,ican,ip,k), k=1,2) 
c
c  revised to read 5 age groups BAN 9 May 2005
c
                    read(i35,'(a7,21x,10e9.0)',rec=jrec) 
     :                   nuclide, 
     .               ((rFextD(iage,ispec,ican,ip,k),iage=1,5), k=1,2)      
                  end do
               end do
            end if
          end if
        end do                                   ! End species loop
      end if
*     call pauseit
*
*     now do the inhalation and ingestion coefficients
*
      do 200 ispec = 1, nint
         nuke = nucnam(ispec)
         ipt = ibinry( nuke )
         read(idex,'(a7,132x,2i6,i4,3i6)', rec = ipt) 
     :        nuke, ing, inh, iext, ingr, inhr, iextr
c
c        inhalation
c
         iflag(ispec, 1) = .false.
         if (IPTH(1).and. inh .ne. 0) then
           iflag(ispec, 1) = .true.
           read(i31,'(a7,22x,i2,1x,a1)',rec=inh+2)nuclide,nlets,let
           itype = 0
           irec = inh + 1        ! offset 2 for header
c           loop                  !  loop over types
           do iBAN = 1,10
             itype = itype + 1
             do iage = 1, 6
               irec = irec + 1
               read(i31,'(a7,i5,f5.0,1x,a1,2x,e8.0,4x,33e10.0)',
     :            rec=irec) nuclide, kage, amad, redclas,
     :            f1inh(ispec,iage),(dx(j), j = 1, 33)
               do ir = 1, 24
                  dfinh(iage, ispec, ir) = dx(indorg(ir))
               end do
               if (nlets .eq. 2) then
                  irec = irec + 1
                  read(i31,'(33x,33e10.0)',rec=irec)(dx(j), j=1,31)
                  do ir = 1, 23
                     dfinh(iage, ispec, ir) = dfinh(iage, ispec, ir) + 
     .				     20.0 * dx(indorg(ir))
                  end do 
               end if
             end do
	if (class .eq. redclas) go to 123
             read(i31,'(a7)',rec=irec+1)nuclide
             if (nuclide .ne. nuke) go to 123
C           end loop
           end do
  123      continue
           irec = inhr + 2       ! offset 3 for header records
           do itype = 1, itype
             do ican = 1, mcan
                irec = irec + 1
C                read (i37,'(a7,63x,e9.0,36x,e9.0)', rec=irec) nuclide,
C     :               (rfinhd(ispec,ican,k),k=1,2)
C
C  REVISED FOR 5 AGES  BAN 9 MAY 2005
C
                read (i37,'(a7,27x,10e9.0)', rec=irec) nuclide,
     :               ((rfinhd(IAGE,ispec,ican,k),IAGE=1,5),k=1,2)
             end do
           end do
         end if                               ! End of inhalation reads
c
c        ingestion
c
         iflag(ispec, 2) = .false.
         if (ipth(2) .and. ing .ne. 0) then
           iflag(ispec, 2) = .true.
           read(i30,'(a7,13x,i2,1x,a1)',rec=ing+2)nuclide,nlets,let
           itype = 0
           irec = ing + 1  ! offset 2 for header records
C           loop            !  loop over types
            do iban = 1, jban  ! this allows selection of later types, i.e. OBT
C             itype = itype + 1
             do iage = 1, 6
               irec = irec + 1 
               read(i30,'(a7,i5,e8.0,4x,33e10.0)',
     :            rec=irec) nuclide, kage, f1ing(ispec,iage),
     :            (dx(j), j = 1, 33)
               do ir = 1, 24
                 dfing(iage, ispec, ir) = dx(indorg(ir))
               end do   
               if (nlets .eq. 2) then
                  irec = irec + 1
                  read(i30,'(24x,33e10.0)',rec=irec)(dx(j), j = 1, 31)
                  do ir = 1, 23
                    dfing(iage, ispec, ir) = dfing(iage, ispec, ir) + 
     .				  20.0 * dx(indorg(ir))
                 end do   
               end if
             end do
             read(i30,'(a7)',rec=irec+1)nuclide
C             if (nuclide .ne. nuke) quit
C           end loop
            end do  ! iban loop
           irec = ingr + 2     ! offset 3 for header records
C           do itype = 1, itype
            do iban = 1, jban ! this allows selection of later types, i.e. OBT
             do ip = 1, 2
               do ican = 1, mcan
                irec = irec + 1
C                   read (i36,'(a7,62x,e9.0,36x,e9.0)', rec=irec) 
C     :                   nuclide, (rfingd(ispec, ican, ip, k),
C     :                   k = 1, 2)
C
C  REVISED FOR 5 AGE GROUPS  ban 9 MAY 2005
C
                   read (i36,'(a7,26x,10e9.0)', rec=irec) 
     :                   nuclide, ((rfingd(IAGE,ispec, ican, ip, k),
     :                   IAGE=1,5),k = 1, 2)
               end do
            end do
	      end do !  iban loop
C           end do  
C           call chemdat(2, ispec)
         end if  
  200 continue
      return
   60 write(*,*) 'Fatal error'
      Stop 1
      end
