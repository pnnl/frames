*----------------------------------------------------------------------*
*                                                                      *
      subroutine frward
*                                                                      *
*----------------------------------------------------------------------*
*   routine:  frward
*   author:   k. f. eckerman
*   date:     01/14/92
*   purpose:  read down a branch of a decay chain.
*
      include 'pakparm.cmn'
      include 'dcfpak.cmn'
      character*7 named
      real fhold
      integer iptb, iparb, ibrch, ipar, ipt
      logical eob, pob
      common/chaind/named(mspec), fhold(mspec), iptb(mspec), 
     :              iparb(mspec), ipt, ibrch, ipar, eob, pob
      common/energy/ealpha(mspec), ebeta(mspec), egamma(mspec)
*
*     functions referenced.
*
      integer ibinry
      character*8 t, mode
      character*7 nuke, d1
      character*2 ix
      integer j
      include 'iolist.cmn'
*
*     get parent record.
*
      if (ipar .eq. 1) then
         nuke = nucnam(ipar)
         ipt = ibinry(nuke)
         if (ipt .eq. 0) then
            nspec = 0
            return
         endif
      endif
   10 if (ipt .lt. 999) then
C         read(idex,'(a7,a8,a2,a6,3(i4,e11.0),3f7.0)',
         read(idex,'(a7,a8,a2,a8,22x,3(i4,e11.0),e7.0,2f8.0)',
     :      rec=ipt) nuke, t, ix, mode, id1, f1, id2, f2, id3, f3, 
     :      ea, eb, eg
      else
        id1 = 0
        f1 = 0.0
        id2 = 0
        f2 = 0.0
        id3 = 0
        f3 = 0.0
        nuke = 'Sf'
        ea = 0.
        eb = 0.
        eg = 0.
        t = ' '
        ix = ' '
      end if
*
*     ids = 999 denotes "sf" which is not a daughter product, thus set
*     the ids to zero if "sf".
*
      if (id1 .eq. 999 .and. id2 .ne. 0) then
         if (id3 .eq. 0) then
            fhld = f1
            ihld = id1
            f1 = f2
            id1 = id2
            f2 = fhld
            id2 = ihld
         else
            fhld = f1
            ihld = id1
            f1 = f2
            id1 = id2
            f2 = f3
            id2 = id3
            f3 = fhld
            id3 = ihld
         end if
      end if
      if (id2 .eq. 999 .and. id3 .ne. 0) then
         ihld = id2
         fhld = f2
         id2 = id3
         f2 = f3
         id3 = ihld
         f3 = fhld
      end if
*
*     if processing a branch then check to see if d1 has already
*     been included in nucnam, if so only fix up branch(ipar,past) and
*     terminate chain, i.e., chain has converged.
*
      if (pob) then
         if (id1 .gt. 0 .and. id1 .lt. 999) then
            read(idex, '(a7)', rec = id1) d1
         elseif (id1 .eq. 999) then
            d1 = 'Sf'
         end if   
         do j = 1, nspec - 1
            if (d1 .eq. nucnam(j)) goto 16
         end do
         goto 17
*
*        have already handled this daughter; chain has converged.
*        set end of chain and return.
*
   16    branch(ipar, j) = f1
         nucnam(ipar) = nuke
         thalf(ipar) = t
         ealpha(ipar) = ea
         ebeta(ipar) = eb
         egamma(ipar) = eg
         iu(ipar) = ix
         nspec = nspec + 1
         return
      end if
*
*     need to treat this chain member.
*
   17 nucnam(ipar) = nuke
      thalf(ipar) = t
      ealpha(ipar) = ea
      ebeta(ipar) = eb
      egamma(ipar) = eg
      iu(ipar) = ix
      nspec = nspec + 1
      branch(ipar, nspec) = f1
*
*     no further daughters in chain - set end of chain
*
      if (id1 .ne. 0) then
*       further daughters, treat id1 and check for possible branches.
        ipt = id1
*
        if (id2 .ne. 0 ) then
*
*         set end of branch to false, increment branch counter, store 
*         pointer of parent, and record number of second or third daughter
*         while following current chain.  routine recver will direct 
*         recovery of branches.
*
          eob = .false.
          ibrch = ibrch + 1
          iptb(ibrch) = id2
          fhold(ibrch) = f2
          iparb(ibrch) = ipar
          if (id2 .ne. 999) then
             read(idex, '(a7)', rec = id2) named(ibrch)
          else
             named(ibrch) ='Sf'
          end if
        endif
*       third daughter, branch info held as above.
        if (id3 .ne. 0) then
          eob = .false.
          ibrch = ibrch + 1
          iptb(ibrch) = id3
          fhold(ibrch) = f3
          iparb(ibrch) = ipar
          if (id3 .ne. 999) then
             read(idex, '(a7)', rec = id3) named(ibrch)
          else
             named(ibrch) = 'Sf'
          end if
        endif
        ipar = nspec
        if (ipt .ne. 999) goto 10
      endif
      return
*
      end
