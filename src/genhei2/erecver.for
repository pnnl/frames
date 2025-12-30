*----------------------------------------------------------------------*
*                                                                      *
      subroutine recver
*                                                                      *
*----------------------------------------------------------------------*
*   routine:  recver
*   author:   k. f. eckerman
*   date:     04/06/89
*   purpose:  recover info on branches in the chain that were detected
*             by frward and direct the reading of the new branch.
*
      include 'pakparm.cmn'
      include 'dcfpak.cmn'
      character*7 named
      real fhold
      integer iptb, iparb, ibrch, ipar, ipt
      logical eob, pob
      common/chaind/named(mspec), fhold(mspec), iptb(mspec),
     :              iparb(mspec), ipt, ibrch, ipar, eob, pob
* local variables.
      character*7 nuke
      integer i
      include 'iolist.cmn'
*
*     no branches to treat, set end of branch to true and return.
*
    1 if (ibrch .eq. 0) then
         eob = .true.
*      elseif (iptb(ibrch) .eq. 999) then
*         eob = .true.
      else
*
*        consider remaining branches. recover parent's
*        index at branch (ipar) and daughter's record number (ipt).  
*        decrement branch counter and return.
*
         pob = .true.
         ipar = iparb(ibrch)
         ipt = iptb(ibrch)
         nuke = named(ibrch)
*
*        need to check to see of the daughter of the branch has already
*        a member of the chain.
*
         do i = 1, nspec - 1
            if (nuke .eq. nucnam(i)) goto 15
         end do
         nucnam(nspec) = nuke
         branch(ipar, nspec) = fhold(ibrch)
         ibrch = ibrch - 1
         ipar = nspec
         return
*
*        if already a chain member set r, decrement the branch counter 
*        and look for another branch to process.
*
  15     branch(ipar,i) = fhold(ibrch)
*
         ibrch = ibrch - 1
         go to 1
*
      endif
*
      return
      end
