*----------------------------------------------------------------------*
*                                                                      *
      subroutine path
*                                                                      *
*----------------------------------------------------------------------*
*     author:  K.F. Eckerman
*     date:    04/06/89
*     purpose: initialize mpath and max matrices
*              Adopted from A. Birchall, Health Phys. 50, 3, 389-397, 1986.
*
      include 'pakparm.cmn'
      include 'dcfpak.cmn'
      integer max, mpath
      common/calcul/ max(mspec), mpath(mspec, mspec)
*     initializes mpath and max matrics.
      do i = 1, nspec
        max(i) = 0
        do j = 1, nspec
           mpath(i,j) = 0
        end do
      end do
      do j = 2, nspec
         do i = 1, j - 1
           if (branch(i, j) .ne. 0.) then
              max(j) = max(j) + 1
              mpath(max(j), j) = i
           end if
         end do
      end do
      return
      end
