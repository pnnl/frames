
      SUBROUTINE PUFFR ( isrc, acute, PrgStat )

C-----------------------------------------------------------------------
c
c     PUFFR
c
c     Date:              March 14, 2000
C      Rev:              14 Dec 2001  BAN  dimension rg_, ng_, part_fract
c
c     Description:       Assigns initial attributes to puffs at release 
c                        time.
c
c     Required modules:
c
c          Subroutines:  None
c
c            Functions:  USTAR
c
C-----------------------------------------------------------------------

      IMPLICIT       NONE

      INCLUDE       'parm.inc'
      INCLUDE       'const.inc'
      INCLUDE       'depos.inc'
      INCLUDE       'met_data.inc'
      INCLUDE       'nuc_data.inc'
      INCLUDE       'puffs.inc'
      INCLUDE       'rel.inc'  
      
      REAL          CORRUSTR
      REAL          INVMOL
      REAL          FNPLMRS
      REAL          PROFILE
      
      REAL          mol, prise, rht_spd, ustr_src, z0src
      
      INTEGER       isrc, ix, iy, k, n

      CHARACTER*50  PrgStat

      LOGICAL       acute

      tpuffs = tpuffs + 1

c  Get the surface roughness of the source

      ix = INT(xsrc(isrc) + 0.5)   
      iy = INT(ysrc(isrc) + 0.5)
      
      z0src = Gz0(ix,iy)
      
c  Correct ustar if source and station sfc roughness different

      ustr_src = CORRUSTR( ustr, z0sta, z0src, stab, acute, PrgStat )
      IF( PrgStat .NE. ' ' ) RETURN

      mol = INVMOL( Stab, z0src )

      IF( zsourc(isrc) .GT. 12 ) THEN
         
         rht_spd = PROFILE( z0src, ustr_src, mol, stab, zsourc(isrc),
     &                      acute, PrgStat )
         IF( PrgStat.NE.' ' ) RETURN
         
      ELSE
      
         rht_spd = PROFILE( z0src, ustr_src, mol, stab, 10.0, acute, 
     &                      PrgStat )
         IF( PrgStat .NE. ' ' ) RETURN
      
      ENDIF
      
      ldepth = mixhgt
      
      IF ( dorise(isrc) ) THEN
         prise = FNPLMRS( stab, zsourc(isrc), rstack(isrc), 
     &                    sflow(isrc), rht_spd, stempc(isrc), 
     &                    temp, ldepth, wndmin )
      
      ELSE
         prise = 0.0
         
      ENDIF           

c  Define variables for the puff
      
      mf(tpuffs) = 1
      xp(tpuffs) = xsrc(isrc)
      yp(tpuffs) = ysrc(isrc)
      zp(tpuffs) = zsourc(isrc) + prise
      if ( zp(tpuffs).lt.0 ) zp(tpuffs) = 0.0

c  limit plume rise to top of the mixing-layer if stack height within the
c  mixing layer 

      IF ( (zp(tpuffs) .GT. ldepth) .AND. (zsourc(isrc) .lt. ldepth) )
     &     zp(tpuffs) = ldepth 

      sp(tpuffs) = isrc
      age(tpuffs) = 0

c     Building wake***Needs to be implemented
      hts(tpuffs) = 0.0
      vts(tpuffs) = 0.0
c      
c      IF( (do_bldgwake .EQ. 'Y') .AND. (BldgArea .GT. 0)  
c     &                           .AND. (rht_spd .GT. 0.4) ) THEN
c          IF ( PrgStat.NE.' ' ) RETURN
c          hts( tpuffs)  = SQRT( Float( BldgArea ) ) / ustr
c          vts(tpuffs) = hts(tpuffs) / ( 2 + (zp(tpuffs) * s_mol) )
c      ENDIF
     
      DO n = 1, nnucs
      
        qp(n,tpuffs,1)=relrate(isrc,n)*3600.0/FLOAT(nph) * ng_fract(n)
        qp(n,tpuffs,2)=relrate(isrc,n)*3600.0/FLOAT(nph) * rg_fract(n)
        qp(n,tpuffs,3)=relrate(isrc,n)*3600.0/FLOAT(nph) * part_fract(n)
         
      ENDDO
         
      DO k = 1, numpardis(isrc)

         qpf(k,tpuffs) = massfrac(isrc,k)

      ENDDO
      
      sigmaz(tpuffs) = SigZIn(isrc)
      sigmay(tpuffs) = SigYIn (isrc)
      sigyn(tpuffs) = SigYin(isrc)
      sigzn(tpuffs) = SigZin(isrc)

      RETURN 
      
      END