*----------------------------------------------------------------------*
*                                                                      *
      subroutine order
*                                                                      *
*----------------------------------------------------------------------*
*   routine:  order
*   author:   k. f. eckerman
*   date:     04/06/89
*   purpose:  order the chain so daughter index > parents.
*
      include 'pakparm.cmn'
      include 'dcfpak.cmn'
      common/energy/ealpha(mspec), ebeta(mspec), egamma(mspec)
      character*8 thold
      character*7 nuke
      character*2 ix
      real rsave, csave
      integer i, j, ip, jp, ipass, move
      dimension rsave(mspec), csave(mspec)
      include 'iolist.cmn'
*
*     move # of elements to move
*
      ipass = 0
  100 move = 0
      ipass = ipass + 1
      if (ipass .gt. 4*nspec) then
         write(olog,'('' Failure in routine order: greater than'',i3,
     :   '' passes for '', a7, ''.'')') ipass, nucnam(1)
         write(*,'('' Failure in routine order: greater than'',i3,
     :   '' passes for '', a7, ''.'')') ipass, nucnam(1)
         stop 1
      endif
*
      do i = 1, nspec
         do j = 1, i-1
            if (branch(i, j) .ne. 0.) then
               ip = i
               jp = j
               move = 1
               go to 15
            endif
         end do
      end do
*
*     if no elements to move then return
*
  15  if (move .eq. 0) return
      nuke = nucnam(ip)
      thold = thalf(ip)
      ea = ealpha(ip)
      eb = ebeta(ip)
      eg = egamma(ip)
      ix = iu(ip)
      do j = 1, nspec
         rsave(j) = branch(ip, j)
      end do
      do i = ip - 1, jp, -1
         nucnam(i + 1) = nucnam(i)
         thalf(i + 1) = thalf(i)
         ealpha(i + 1) = ealpha(i)
         ebeta(i + 1) = ebeta(i)
         egamma(i + 1) = egamma(i)
         iu(i + 1) = iu(i)
         do j = 1, nspec
            branch(i + 1, j) = branch(i, j)
         end do
      end do
      nucnam(jp) = nuke
      thalf(jp) = thold
      iu(jp) = ix
      ealpha(jp) = ea
      ebeta(jp) = eb
      egamma(jp) = eg
      do j = 1, nspec
         branch(jp, j) = rsave(j)
      end do
      do i = 1, nspec
         csave(i) = branch(i, ip)
      end do
      do j = ip - 1, jp, -1
         do i = 1, nspec
            branch(i, j + 1) = branch(i, j)
         end do
      end do
      do i = 1, nspec
         branch(i, jp) = csave(i)
      end do
*
*     Yes we do have backward point gotos in FORTRAN, sorry!
*
      goto 100
*
      end
