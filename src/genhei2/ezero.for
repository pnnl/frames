*-----------------------------------------------------------------------
*
      subroutine zerom
*
*-----------------------------------------------------------------------
*     routine:  zerom
*     author:   k.f. eckerman
*     date:     10/23/93
*     purpose:  set the dose coefficient arrays to zero
*
      include 'pakparm.cmn'
      include 'dcfpak.cmn'
*
      do ispec = 1, mspec
         do ifact = 1, mfact
            iflag(ispec, ifact) = .false.
         end do
      end do
      do iage = 1, mage
         do ispec = 1, mspec
            do iorg = 1, morg
               dfinh(iage, ispec, iorg) = 0.0
               dfing(iage, ispec, iorg) = 0.0
            end do
         end do
      end do
      do ispec = 1, mspec
         do ipath = 1, mext
            do iorg = 1, morg
               dfext(ispec, ipath, iorg) = 0.0
            end do
         end do
      end do
      DO IAGE = 1,5
	do ispec = 1, mspec
         do ican = 1, mcan
            do k= 1, 2
               rfinhd(IAGE,ispec, ican, k) = 0.0
               do ipath = 1, 2
                  rfingd(IAGE,ispec, ican, ipath, k) = 0.0
               end do
            end do
         end do
      end do
	END DO
	do iage = 1,5
      do ispec = 1, mspec
         do ican = 1, mcan
            do ipath = 1, mext
               do k = 1, 2
                  rfextd(iage,ispec, ican, ipath, k) = 0.0
               end do
            end do
         end do
      end do
	end do
      return
      end
