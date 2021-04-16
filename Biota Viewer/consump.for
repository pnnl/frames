      subroutine consump(nages,ndist,NDIR,pop,npoints,PATH,NVALS,food,
     . totprod, TOTCONS,pthon)
	include 'datarcp.cmn'
	dimension pop(nages,ndist,ndir), TOTCONS(90),food(8,ndist,ndir)
	dimension totprod(90)
	CHARACTER*22 PATH(90)
	LOGICAL SEQI, pthon(90)
	totprod = 0.0
	totcons = 0.0
	pthon = .false.
        DO IPT = 1, NPOINTS
          NDR = (IPT-1)/NDIST+1
          NDST = MOD(IPT,NDIST)
          IF(NDST .EQ. 0) NDST = npoints/ndir
          IF(NDST .EQ. 0) NDST = 1
C
		  DO IVAL = 1, NVALS
	        kpthC=0
	        kpthA=0
	        if(SEQI(path(ival),'leaf',4))kpthC=1
	        if(SEQI(path(ival),'root',4))kpthC=2
	        if(SEQI(path(ival),'frui',4))kpthC=3
	        if(SEQI(path(ival),'grai',4))kpthC=4
	        if(SEQI(path(ival),'meat',4))kpthA=1
	        if(SEQI(path(ival),'poul',4))kpthA=2
	        if(SEQI(path(ival),'milk',4))kpthA=3
	        if(SEQI(path(ival),'eggs',4))kpthA=4
	        IF (KPTHC .NE. 0)THEN
	          pthon(ival) = .true.
	          totprod(ival)=totprod(ival)+food(kpthC,ndst,ndr)
	          DO IAGE = 1,NAGES
	        totcons(ival) = totcons(ival)+
     .          POP(IAGE,NDR,NDST)*indconsC(kpthC,IAGE)
	          ENDDO
	        ELSE IF (KPTHA .NE.0) THEN
	          pthon(ival) = .true.
	          totprod(ival)=totprod(ival)+food(kpthA+4,ndst,ndr)
	          DO IAGE = 1,NAGES
	        totcons(ival) = totcons(ival)+
     .          POP(IAGE,NDR,NDST)*indconsA(kpthA,IAGE)
	          ENDDO
	        END IF
            END DO ! PATHWAYS (nvals)
	   END DO ! LOCATIONS
	 RETURN
	END

