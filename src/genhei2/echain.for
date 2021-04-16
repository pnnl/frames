*----------------------------------------------------------------------*
*                                                                      *
      subroutine chain(nuke)
*                                                                      *
*----------------------------------------------------------------------*
*   routine:  chain
*   author:   k. f. eckerman
*   date:     04/06/89 : 09/25/91 : 08/06/92: 02/28/95 :06/04/96
*   purpose:  assemble decay chain.  the name of the parent nuclide should
*             be passed to the routine as namen(1) in the common block
*             /chains/. upon return the chain members will be contained
*             in namen, and ndau is the length of the chain.
*
      include 'pakparm.cmn'
      include 'dcfpak.cmn'
      include 'batch.cmn'
* common block /chaind/.
      character*7 named, nuke
      real fhold
      integer iptb, iparb, ibrch, ipar, ipt, n
      logical eob, pob
      common/chaind/named(mspec), fhold(mspec), iptb(mspec),
     :              iparb(mspec), ipt, ibrch, ipar, eob, pob
      common/energy/ealpha(mspec), ebeta(mspec), egamma(mspec)
      dimension eat(mspec), ebt(mspec), egt(mspec)
*     local variables.
      character*78 text
      character*8 t12
      character*2 t12u
      double precision zln2, timest
      parameter(zln2=0.693147181d0, zero=0.0)
      include 'iolist.cmn'
*
*     initialize chain parameters
*
      ibrch = 0
      ipar = 1
      nspec = 1
      eob = .true.
      pob = .false.
      nucnam(1) = nuke
      do i = 1, mspec
         do j = 1, mspec
            branch(i, j) = zero
         end do
      end do
*
*     assign one unit of activity to the parent, rest are zero
*
      branch(1, 1) = 1.0
      lmr(1) = 0.0d0
*
   20 call frward
      if (nspec .le. 0) then
        i = len_trim(nucnam(1))
        write(*,'(1x,(a),'' is not in data base!'')') nucnam(1)(:i)
        return
      endif
      call recver
      if (.not. eob) goto 20
      nspec = nspec - 1
      if (pob) call order
      do i = 1, nspec
         if (nucnam(i)(:2) .eq. 'Sf') then
            lmr(i) = 0.0D0
         else
            lmr(i) = zln2 / timest(thalf(i), iu(i))
         end if
      end do
      t12 = thalf(1)
      t12u = iu(1)
      text = nucnam(1)(:len_trim(nucnam(1))) // ' Decay Chain:'
      text = text(:len_trim(text)) // ' Half-lives and Branching'
      text = text(:len_trim(text)) // ' Fractions'
C      if(.not. dbatch) then
C         write(*,*) text(:len_trim(text))
C         write(olog, '(/a)') text(:len_trim(text))
C         call printm
C         if (nspec .gt. 5) call cls
C      end if
      call path
      timess = 36525.0
*
C      if(.not. dbatch) then
C      text = ': Activity, Transformations, & Cumulative Energies (MeV) a
C     :t 100y'
C      write(*,*) nucnam(1)(:len_trim(nucnam(1))), text(:len_trim(text))
C      write(olog,*)nucnam(1)(:len_trim(nucnam(1))),text(:len_trim(text))
C      write(*,'(''     Nuclide  T1/2        A(t)/Ao   intA/Ao(d)   Ealph
C     .a    Ebeta    Egamma'')')
C      write(olog,'(''     Nuclide  T1/2        A(t)/Ao   intA/Ao(d)   Ea
C     .lpha    Ebeta    Egamma'')')
C      end if
*
      ea = zero
      eb = zero
      eg = zero
      do 50 ispec = 1, nspec
        if (nucnam(ispec)(:2) .eq. 'Sf') goto 50
        call birch(ispec, timess, rx1, rx2)
        ea = ea + rx2 * ealpha(ispec)
        eb = eb + rx2 * ebeta(ispec)
        eg = eg + rx2 * egamma(ispec)
*
        if (.not. dbatch) then
           write(*,'(i4, 1x, a7, 1x, a8, a2, 1p2d12.5,3e9.2)') ispec,
     :     nucnam(ispec), thalf(ispec), iu(ispec), rx1, rx2, ea, eb, eg
           write(olog,'(i4, 1x, a7, 1x, a8, a2, 1p2d12.5,3e9.2)') ispec,
     :     nucnam(ispec), thalf(ispec), iu(ispec), rx1, rx2, ea, eb, eg
        end if
*
        eat(ispec) = ea
        ebt(ispec) = eb
        egt(ispec) = eg
   50 continue
      if (nucnam(nspec)(:2) .eq. 'Sf') then
         nint = icutoff(eat, ebt, egt, 0, nspec-1)
         next = icutoff(eat, ebt, egt, 1, nspec-1)
      else
         nint = icutoff(eat, ebt, egt, 0, nspec)
         next = icutoff(eat, ebt, egt, 1, nspec)
      end if
      if (nspec .eq. 1) then
         nint = 1
         next = 1
      end if
C      if (.not. dbatch) call pauseit
      do jspec = 2, nspec
         n = 0
         do ispec = 1, nspec
            if (nucnam(ispec)(:2) .ne. 'Sf') then
               if (branch(ispec, jspec) .gt. 1.0E-6) then
                  n = n + 1
                  ibr(n, jspec) = ispec
               end if
            end if
         end do
         nbr(jspec) = n
      end do
*
      return
      end
