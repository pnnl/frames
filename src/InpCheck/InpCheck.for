!=======================================================================
!=================== Start of Program InpCheck =========================
c --- AUTHOR:  MPelton and JRishel
c
c --- PURPOSE:
c
c --- INPUTS:
c
c --- OUTPUT:
c
!=======================================================================
      Program InpCheck

c --- Data declarations
      DATA ver/'25-Apr-07'/

c --- Function type declarations 
c --- trust me you need them
c --- you get very unpredicatble behavior
c --- unless the name starts with correct
c --- default type letter
      Logical CalcNumObs
      Logical CheckEnds
      Logical CheckLeap
      Real CalcWindDir

c --- File and temp variable declarations 
      Integer ieUnt
      Integer icUnt
      Integer isIUnt
      Integer isOUnt
      Integer i, j, k, n, nObs, nLev, bIdx, eIdx
      Integer kYr, kJd, kHr, yrLen, cnt
      Character ver
      Character*80 des

c --- Declarations for upper air checks and fill
      Real, ALLOCATABLE:: zsub(:),psub(:),sigma(:)
      Real m,b
      Real pmiss,zmiss,tmiss,dmiss,smiss
      integer nlevels,ncomplete,nglevels

c --- Bwiccalmet.inp info
      Character*250 path
      Character*80 run
      Real xSw
      Real ySw
      Integer uZone
      Real xySz
      Integer xNum
      Integer yNum
      Integer sYr, sMn, sDy, sJd, sHr
      Integer yr, jd, hr
      Integer TZ
      Integer sLen
      Character topo
      Character*80, ALLOCATABLE::datName(:) 

c --- Upper surface station info
      Integer sNum
      Character*80, ALLOCATABLE::sname(:) 
      Character*20, ALLOCATABLE::slat(:), slng(:) 
      Integer, ALLOCATABLE::sId(:), sTZ(:), anem(:) 

c --- Upper air station info
      Integer aNum
      Character*80, ALLOCATABLE::aname(:) 
      Character*20, ALLOCATABLE::alat(:), alng(:) 
      Integer, ALLOCATABLE::aId(:), aTZ(:), aht(:) 

c --- station .dat info
      Real pstop
      Logical Done, anErr
      Logical lht, ltemp, lwd, lws
      Integer ibYr, ibJd, ibHr, ieYr, ieJd, ieHr, imon, iday
      Integer jdat, ifmt, iTZ, nSta, tIdx, lastIdx
      Real, ALLOCATABLE::tempk(:,:), pres(:,:), ht(:,:)
      Real, ALLOCATABLE::u(:,:), v(:,:), ws(:,:), wd(:,:)
      Integer, ALLOCATABLE::iws(:,:), iwd(:,:), fill(:), newiStop(:)
      Integer, ALLOCATABLE::iId(:), iceil(:, :), icc(:,:), irh(:,:) 
      Integer, ALLOCATABLE::iYr(:), iJd(:), iHr(:), ipcode(:,:)
      Integer, ALLOCATABLE::itpdk(:), iMn(:), iDy(:), mLev(:), iStop(:)
      Character*6, ALLOCATABLE::isId(:)
      
c --- File unit assignments
      ieUnt = 1
      icUnt = 2
      isIUnt = 3
      isOUnt = 4      

c --- Open error log      
      Open(UNIT=ieUnt, FILE='inpCheck.log', STATUS='replace') 

c --- Open and Read bwiccalmet.inp      
      Open (UNIT=icUnt, FILE='BWICCALMET.INP', STATUS='OLD') 
      Read(icUnt,*) path, des
      Read(icUnt,*) run, des
      Read(icUnt,*) xSw, des
      Read(icUnt,*) ySw, des
      Read(icUnt,*) uZone, des
      Read(icUnt,*) xySz, des
      Read(icUnt,*) xNum, des
      Read(icUnt,*) yNum, des
      Read(icUnt,*) sYr, des
      Read(icUnt,*) sMn, des
      Read(icUnt,*) sDy, des
      Read(icUnt,*) sHr, des
      Read(icUnt,*) TZ, des
      Read(icUnt,*) sLen, des
      Read(icUnt,*) topo, des

c --- Read Surface station data
      Read(icUnt,*) sNum, des      
      Read(icUnt,70) des, des, des, des, des, des
      ALLOCATE(sname(sNum), slat(sNum), slng(sNum)) 
      ALLOCATE(sId(sNum), sTZ(sNum), anem(sNum)) 
      Do i = 1, sNum
        Read(icUnt,*) sname(i),sId(i),slat(i),slng(i),sTZ(i),anem(i) 
      End Do

c --- Read upper air station data      
      Read(icUnt,*) aNum, des
      Read(icUnt,70) des, des, des, des, des, des
      ALLOCATE(aname(aNum), alat(aNum), alng(aNum)) 
      ALLOCATE(aId(aNum), aTZ(aNum), aht(aNum)) 
      Do i = 1, aNum
        Read(icUnt,*) aname(i),aId(i),alat(i),alng(i),aTZ(i),aht(i)
      End Do

c --- Read file names     
      ALLOCATE(datName(aNum+1)) 
      Read(icUnt,*) datName(1) 
      Read(icUnt,70) des, des
      Do i = 2, aNum+1
        Read(icUnt,*) datName(i) 
      End Do
      Close(icUnt, STATUS='KEEP') 

c --- Calculate Ending time from .inp file
      call julday(ieUnt, sYr, sMn, sDy, sJd) 
      hr = MOD(sLen+sHr, 24) 
      jd = sJd + (sLen+sHr) /24
      If (CheckLeap(yr)) Then
        yr = sYr + jd/365
        jd = MOD(jd, 364) 
      Else
        yr = sYr + jd/364
        jd = MOD(jd, 364) 
      End If
cLst
      Write(ieUnt,*) 'Simulation start time', sYr, sJd, sHr
      Write(ieUnt,*) 'Simulation End time', yr, jd, hr
      Write(ieUnt,*) 'The simulation length ', sLen
      Write(ieUnt,*) 'Done Reading BWICCALMET.inp'
      Write(*,*) 'Done Reading BWICCALMET.inp'
c      Read(*,*) topo
cLst

c=======================================================================
c --- Open and Read formatted up(n).dat file     
c=======================================================================
      Do n=2, aNum+1
        anErr = .FALSE.
        Write(*,*) 'Reading ', datName(n)
        Open(UNIT=isIUnt, FILE=datName(n), STATUS='OLD')     
        Read(isIUnt,140) ibYr, ibJd, ibHr, ieYr,
     1                   ieJd, ieHr, pstop, jdat, ifmt
        if (k.NE.0) Then
	    enErr = .True.
	    write(ieUnt,*) 'Invalid header format format '
	    goto 2000
	  End if

        Read(isIUnt,150) lht, ltemp, lwd, lws
	  if (k.NE.0) Then
	    enErr = .True.
	    write(ieUnt,*) 'Invalid flag format '
	    goto 2000
	  End if

        anErr = CalcNumObs(ieUnt,ibYr,ibJd,ibHr,
     1                     ieYr,ieJd,ieHr,nObs)
        If (anErr) then
          write(ieUnt,*) 'CalcNumObs failed!'
          Goto 2000
        End If

c ---   Allocate space for processing for up to fifty levels 
        nLev = 50
        nObs = nObs/12 + 1
        i = nObs+1
        j = nLev
        ALLOCATE(itpdk(i), isId(i), iYr(i), iJd(i), iHr(i)) 
        ALLOCATE(iMn(i), iDy(i), mLev(i), iStop(i), fill(i)) 
        ALLOCATE(u(i,j), v(i,j), iws(i,j), iwd(i,j)) 
        ALLOCATE(tempk(i,j), pres(i,j), ht(i,j), newiStop(i)) 
        
        Do i=1,nObs
          fill(i)=.FALSE.
        End Do
        
        cnt = 0
        Do While (k.EQ.0 .AND. cnt.LE.nObs)
          Read(isIUnt,160,IOSTAT=k) 
     1         itpdk(i), isId(i), iYr(i), iMn(i), 
     2           iDy(i), iHr(i), mLev(i), iStop(i)

		if (k.NE.0 .and. k.NE.-1) Then
		  enErr = .True.
		  write(ieUnt,*) 'Invalid observation header format ',k
		  goto 2000
		End if

          If (k.EQ.0) Then       
c           find time in observation data
c           Start find later in time, file is supposed to be sequential
            tIdx = 1
            kHr = ibHr
            kJd = ibJd
            kYr = ibYr
            call julday(ieUnt, iYr(i), iMn(i), iDy(i), iJd(i))
            Do While (tIdx.LE.nObs)

c      Write(*,*) 
c     1 kYr,kJd,kHr, iYr(i),'-',iJd(i),'-',iHr(i),'-',k,tIdx,lastIdx
c      read(*,*) topo

              If (kYr.EQ.iYr(i).AND.
     1            kJd.EQ.iJd(i).AND.
     2            kHr.EQ.iHr(i)) Then     
                itpdk(tIdx) = itpdk(i)
                isId(tIdx) = isId(i)
                iYr(tIdx) = iYr(i)
                iMn(tIdx) = iMn(i)
                iDy(tIdx) = iDy(i)
                iJd(tIdx) = iJd(i)
                iHr(tIdx) = iHr(i)
                mLev(tIdx) = mLev(i)
                iStop(tIdx) = iStop(i)
                If (.NOT.fill(tIdx)) Then
                  cnt=cnt+1
                  fill(tIdx) = .TRUE.
                End If  
                Goto 2400
              End If
              call IncTime(kYr,kJd,kHr,12)
              tIdx=tIdx+1
            End Do

c           If tIdx > nObs values are read into extra obs space
2400        Read(isIUnt,170,IOSTAT=k)
     1        (pres(tIdx,j), ht(tIdx,j), tempk(tIdx,j),
     2         iwd(tIdx,j), iws(tIdx,j), j=1, iStop(i))
c            write(*,*) k, tIdx, cnt,
c     1        (pres(tIdx,j), ht(tIdx,j), tempk(tIdx,j),
c     2         iwd(tIdx,j), iws(tIdx,j), j=1, iStop(i))
            if (k.NE.0) Then
	        enErr = .True.
	        write(ieUnt,*) 'Invalid observation format '
	        goto 2000
	      End if
          End If
        End Do
        
cLst
        Write(ieUnt,*) 'Upper air data start time', ibYr, ibJd, ibHr
        Write(ieUnt,*) 'Upper air data End time', ibYr, ieJd, ieHr
        Write(ieUnt,*) 'Finished reading ', datName(n)
        Write(*,*) 'Finished reading ', datName(n) 
c        write(*,*) 'Got here2', cnt, k, nObs
c        Read(*,*) topo
cLst
c=======================================================================
        
c       Fill missing readings by finding the closest obs
        If (cnt.NE.nObs) Then
          If (cnt.NE.0) Then
            kHr = ibHr
            kJd = ibJd
            kYr = ibYr
            Do i=1,nObs
              If (.NOT. fill(i)) Then
                Do j=i-1,1,-1
                  If (fill(j)) then
c      write(*,*) 'Fill forward',cnt,j,i,khr,kjd,kyr
c      Read(*,*) topo
                    goto 2500  
                  End If
                End Do
                Do j=i+1,nObs
                  If (fill(j)) then
                    goto 2500  
c      write(*,*) 'Fill backward',cnt,j
c      Read(*,*) topo
                  End If
                End Do

2500             call grday(ieUnt,kYr,kJd,imon,iday)
c      write(*,*) 'Got to fill',cnt,j
c      Read(*,*) topo
                iYr(i) = kYr
                iMn(i) = imon
                iDy(i) = iday
                iHr(i) = kHr
                itpdk(i) = itpdk(j)
                isId(i) = isId(j)
                mLev(i) = mLev(j)
                iStop(i) = iStop(j)
                Do k=1, iStop(j)
                  pres(i,k) = pres(j,k)
                  ht(i,k) = ht(j,k)
                  tempk(i,k) = tempk(j,k)
                  iwd(i,k) = iwd(j,k)
                  iws(i,k) = iws(j,k)
                End Do
              End If
              call IncTime(kYr,kJd,kHr,12)
            End Do
          Else
            anErr = .true.
            Write(ieUnt,*) 'No valid data!'
            write(*,*) 'No valid data!',cnt
            Goto 2000
          End If  
        End If


c=======================================================================

c       CALMET missing value indicators
        pmiss = -99.0
        zmiss = 9999.0
        tmiss = 999.9
        dmiss = 999
        smiss = 999

      Do i=1,nObs
        ALLOCATE(zsub(nLev),psub(nLev),sigma(nLev))
        nglevels = 0
	  ncomplete = 0

c	zface top is the last domain height in the CALMET.inp file.
c	this should be read dynamically at some point, but currently hard-coded to value used in BWIC
	zfacelast = 6000.

c	(replace: upper air station height from BWICCALMET.inp file)
	zstation = aht(i)

c	set zabove as 10 meters above the last CALMET zface height. if last level in sounding is not
c	above zfacelast, it will be set to zabove to ensure high enough sounding
	zabove = zfacelast + 10.

c	number of levels used from sounding
c	(replace: this is ISTOP in each UP.dat sounding header)
	nlevels = iStop(i)
c
c	sounding is initally assumed to be good till checked
	gflag = .TRUE.
c
c	number of entirely complete levels
c	(note: TWO complete levels are required in sounding, else sounding must
c	be persisted from previous time
	ncomplete = 0
c
c	number of good levels for regression only
c	(note: determined by count of non-missing height/pressure levels)
	nglevels = 0

c	determine if sufficient number of levels in sounding to perform filling
c	(note: two COMPLETE levels are required).
        Do j=1,iStop(i)
          If ((ht(i,j).NE.zmiss) .AND. (pres(i,j).NE.pmiss) .AND.
     &       (tempk(i,j).NE.tmiss) .AND. (iwd(i,j).NE.dmiss) .AND.
     &       (iws(i,j).NE.smiss)) then
              ncomplete = ncomplete + 1
		end if
c		build subset array of good height/pressure values for linear regression purpose.
c		only include points where both xtab (height) and ytab (pressure) exist
		if ((ht(i,j).NE.zmiss).AND.(pres(i,j).NE.pmiss)) then
c			increment count for number of good levels
			nglevels = nglevels + 1
c			create sub array that contains all non-missing height.AND.pressure levels
c			these sub arrays are the arrays that will be filled
			zsub(nglevels) = ht(i,j)
c			(note: pressure must be log to be of form y=mx+b)
			psub(nglevels) = log10(pres(i,j))
c			uncertainty sigma(i) associated with psub(i); note: assume normal)
			sigma(nglevels) = 1.0
		end if
	  end do
	
	  if (ncomplete.LT.2) then
c		set inidicator that sounding is incomplete and can not be filled.
		fill(i) = .TRUE.
		write(6,*) 'ncomplete = ', ncomplete, '  levels - not enough
     &		data to build sounding		'
	  else
c		good sounding, so fill sounding.
		fill(i) = .FALSE.
		write(6,*) 'ncomplete = ', ncomplete, '  levels in sounding'
		write(6,*) 'nglevels = ', nglevels, ' for regression'
c
c		Step 1: use linear regression equation to get relationship between pressure and height using ordered pair from sub array
		call lineregress(m,b,sigma,zsub,psub,nglevels)
c
c		Step 2: FILL FIRST LEVEL VARIABLES
		!height
		if (ht(i,1).EQ.zmiss) then
c			first level missing a height which will cause CALMET to fail.
c			get height of upper air station from BWICCALMET.inp file
			ht(i,1) = zstation
c			verify this height is less than the next height in the sounding array
			do j = 2,nlevels
				if (ht(i,j).NE.zmiss) then
					if (ht(i,1).GE.ht(i,j)) then
c						1st level is higher then second level, so set 1st level equal to second level
						ht(i,1) = ht(i,j)
						goto 1001
					end if
				end if
			end do
		end if
1001		continue
		!pressure
		if (pres(i,1).EQ.pmiss) then
c			height z exists, so calculate pressure p
			pres(i,1)=10**(m*ht(i,1)+b)
c			verify that calculated pressure is greater than next level's pressure
			do j = 2,nlevels
				if (pres(i,j).NE.pmiss) then
					if (pres(i,1).LE.pres(i,j)) then
c						1st level is higher then second level, so set 1st level equal to second level
						pres(i,1) = pres(i,j)
						goto 1002
					end if
				end if
			end do
		end if
1002		continue		
c		!temperature
		if (tempk(i,1).EQ.tmiss) then
c			get next level that has a temperature
			do j = 2,nlevels
				if (tempk(i,j).NE.tmiss) then
c					make sure this level also has a height, if not calculate it from pressure
					if (ht(i,j).EQ.zmiss) then
						zcalc = (log10(pres(i,j))-b)/m
					else
						zcalc = ht(i,j)
					end if
c					now asssume a standard lapse rate and calculate temperature at level 1
					tempk(i,1) = tempk(i,j)+0.0065*(zcalc-ht(i,1))
					goto 1003
				end if
			end do
		end if
1003		continue
c		!wind speed and direction
		if ((iws(i,1).EQ.smiss).OR.(iwd(i,1).EQ.dmiss)) then
c			wind speed or direction is missing so get from next non-missing level
			do j = 2,nlevels
				if ((iws(i,j).NE.smiss).AND.(iwd(i,j).EQ.dmiss)) then
c					make sure this level also has a height, if not calculate it from pressure
					iws(i,1) = iws(i,j)
					iwd(i,1) = iwd(i,j)
					goto 1004
				end if
			end do
		end if
1004		continue

c		Step 3: FILL LAST LEVEL VARIABLES
		!height (find height at last sounding level acceptable to CALMET - newiStop(i))
		do j = nlevels,1,-1
			if ((ht(i,j).NE.zmiss).OR.(pres(i,j).NE.pmiss)) then
c			have at least pressure as vertical coordinate so can calculate height if not defined explicitly
				if (ht(i,j).EQ.zmiss) then
c					use regression results to calculate height
					ht(i,j) = (log10(pres(i,j))-b)/m
					do k=(j-1),1,-1
c						verify that this calculated level is higher than level below
						if (ht(i,k).NE.zmiss) then
							if (ht(i,k).GT.ht(i,j))	then
c								levels are close to one another, so add set last level height 10 meters above lower level
								ht(i,j) = ht(i,k) + 10
								goto 1005
							else
							goto 1005
							end if
						end if
					end do
				else
c					have actual height so exit loop
					goto 1005
				end if
			end if
		end do
1005		continue
c		now find sounding level index that meets CALMET domain-top criteria
c		must be less than 9998 meters and greater than zfacelast
		do j = nlevels,1,-1
c			find first no-missing level below 9998 meters
			if ((ht(i,j).NE.zmiss).AND.(ht(i,j).LT.9998.))then
c				calculate level's height above ground level for comparison to zfacelast
				zcalc = ht(i,j)-ht(i,1)
				if (zcalc.LT.zfacelast) then
c					must set last level above zabove plus height of first level for CALMET.  
c					the last sounding height will always be at least 10 meters higher than zfacelast
					ht(i,j) = zabove + ht(i,1)
				end if
c				capture index of last level to be used when writing to file and fixing number of used sounding levels in sounding header
				newiStop(i) = j
				goto 1006
			end if
		end do
1006		continue
		!pressure (verify pressure at newiStop(i))
		if (pres(i,newiStop(i)).EQ.pmiss) then
c			height z exists, so calculate pressure p
			pres(i,newiStop(i))=10**(m*ht(i,newiStop(i))+b)
c			verify that calculated pressure is less than next level's pressure
			do j = newiStop(i),1,-1
				if (pres(i,j).NE.pmiss) then
					if (pres(i,newiStop(i)).GT.pres(i,j)) then
c						1st level is higher then second level, so set 1st level equal to second level
						pres(i,newiStop(i)) = pres(i,j)
						goto 1007
					end if
				end if
			end do
		end if
1007		continue
		!temperature (verify temperature at newiStop(i))
		if (tempk(i,newiStop(i)).EQ.tmiss) then
c			get next level that has a temperature
			do j = newiStop(i),1,-1
				if (tempk(i,j).NE.tmiss) then
c					make sure this level also has a height, if not calculate it from pressure
					if (ht(i,j).EQ.zmiss) then
						zcalc = (log10(pres(i,j))-b)/m
					else
						zcalc = ht(i,j)
					end if
c					now asssume a standard lapse rate and calculate temperature at newiStop(i)
					tempk(i,newiStop(i)) = tempk(i,j)+
     &								0.0065*(zcalc-ht(i,newiStop(i)))
					goto 1008
				end if
			end do
		end if
1008		continue
c		!wind speed and direction
		if ((iws(i,newiStop(i)).EQ.smiss).OR.
     &		(iwd(i,newiStop(i)).EQ.dmiss)) then
c			wind speed or direction is missing so get from next non-missing level below
			do j = newiStop(i),1,-1
				if ((iws(i,j).NE.smiss).AND.(iwd(i,j).NE.dmiss)) then
					iws(i,newiStop(i)) = iws(i,j)
					iwd(i,newiStop(i)) = iwd(i,j)
					goto 1009
				end if
			end do
		end if
1009		continue
	  end if

	write(ieUnt,*) 1,ht(i,1),pres(i,1),
     & tempk(i,1),iws(i,1),iwd(i,1)

	write(ieUnt,*) newiStop(i),ht(i,newiStop(i)),pres(i,newiStop(i)),
     & tempk(i,newiStop(i)),iws(i,newiStop(i)),iwd(i,newiStop(i))
	
        DEALLOCATE(zsub,psub,sigma)
      End Do


c=======================================================================

c       Fill missing readings again by finding the closest obs
        If (cnt.NE.nObs) Then
          If (cnt.NE.0) Then
            kHr = ibHr
            kJd = ibJd
            kYr = ibYr
            Do i=1,nObs
              If (.NOT. fill(i)) Then
                Do j=i-1,1,-1
                  If (fill(j)) then
c      write(*,*) 'Fill forward',cnt,j,i,khr,kjd,kyr
c      Read(*,*) topo
                    goto 2900  
                  End If
                End Do
                Do j=i+1,nObs
                  If (fill(j)) then
                    goto 2900  
c      write(*,*) 'Fill backward',cnt,j
c      Read(*,*) topo
                  End If
                End Do

2900             call grday(ieUnt,kYr,kJd,imon,iday)
c      write(*,*) 'Got to fill',cnt,j
c      Read(*,*) topo
                iYr(i) = kYr
                iMn(i) = imon
                iDy(i) = iday
                iHr(i) = kHr
                itpdk(i) = itpdk(j)
                isId(i) = isId(j)
                mLev(i) = mLev(j)
                iStop(i) = iStop(j)
                Do k=1, iStop(j)
                  pres(i,k) = pres(j,k)
                  ht(i,k) = ht(j,k)
                  tempk(i,k) = tempk(j,k)
                  iwd(i,k) = iwd(j,k)
                  iws(i,k) = iws(j,k)
                End Do
              End If
              call IncTime(kYr,kJd,kHr,12)
            End Do
          Else
            anErr = .true.
            Write(ieUnt,*) 'No valid data!'
            write(*,*) 'No valid data!',cnt
            Goto 2000
          End If  
        End If


c=======================================================================


2000    Close(isIUnt,STATUS='KEEP')

cLst
        Write(ieUnt,*) 'Finished upper air observation checks'
        Write(*,*) 'Finished upper air observation checks'
c        Read(*,*) topo
cLst

c          write(*,*) 'Got before save', cnt
c          read(*,*) topo
c          Open(UNIT=isOUnt, FILE=datName(n), STATUS='replace') 
c          Write(isOUnt,140) ibYr, ibJd, ibHr, ieYr, ieJd, ieHr,
c     1                     pstop, jdat, ifmt
c          Write(isOUnt,150) lht, ltemp, lwd, lws
c          Do i=1, nObs
c            Write(isOUnt,160) itpdk(i), isId(i), iYr(i), iMn(i),
c     1                        iDy(i), iHr(i), mLev(i), iStop(i) 
c            Write(isOUnt,170) (pres(i,j), ht(i,j), tempk(i,j),
c     1                       iwd(i,j), iws(i,j), j=1, iStop(i))
c	    End Do 
c          Close(isOUnt)
c          write(*,*) 'Got after save', cnt,k
c          read(*,*) topo


        

c=======================================================================
c ---   Open and Write formatted up(n) .dat file     
c=======================================================================
        If (.NOT. anErr) Then
          Open(UNIT=isOUnt, FILE=datName(n), STATUS='replace') 
c ---     Set start time
          ibYr=sYr-2000
          ibJd=sJd
          ibHr=iHr(1)
          call IncTime(ibYr,ibJd,ibHr,-48)
c ---     Set End time
          ebYr=sYr
          ebJd=sJd
          ebHr=sHr
          call IncTime(ebYr,ebJd,ebHr,(sLen/24)*24 + 48)

          Write(isOUnt,140) ibYr, ibJd, ibHr, ieYr, ieJd, ieHr,
     1                     pstop, jdat, ifmt
          Write(isOUnt,150) lht, ltemp, lwd, lws

c         pad with persistance
          i = 1
          Do 
            call grday(ieUnt,ibYr,ibJd,imon,iday)
            Write(isOUnt,160) itpdk(i), isId(i), ibYr, imon,
     1                        iday, ibHr, mLev(i), newiStop(i) 
            Write(isOUnt,170) (pres(i,j), ht(i,j), tempk(i,j),
     1                       iwd(i,j), iws(i,j), j=1, newiStop(i))

c      write(*,*) 'Time', i,ibYr,ibJd,ibHr,iYr(i),iJd(i),iHr(i)
c      read(*,*) topo
            If (ibYr.EQ.iYr(i).AND.
     1          ibJd.EQ.iJd(i).AND.
     2          ibHr.EQ.iHr(i)) Then     
              if (i.LT.nObs) then 
			  i=i+1
              end if 
c      write(*,*) 'Inc i', i
c      read(*,*) topo
            End If
            if (ibYr.EQ.ieYr.AND.
     1          ibJd.EQ.ieJd.AND.
     2          ibHr.EQ.ieHr) Then  
c      write(*,*) 'Reached end'
c      read(*,*) topo
              Goto 2700 
            End if 
            call IncTime(ibYr,ibJd,ibHr,12)
c      write(*,*) 'Inc time', i,ibYr,ibJd,ibHr,ieYr,ieJd,ieHr
c      read(*,*) topo
	    End Do 
2700      Close(isOUnt)
cLst
          Write(ieUnt,*) 'Finished writing ', datName(n) 
          Write(*,*) 'Finished writing ', datName(n) 
c          Read(*,*) topo
cLst   
        End If

c ---   Deallocate space for processing for up to fifty levels 
        DEALLOCATE(itpdk, isId, iYr, iJd, iHr, STAT=k) 
        DEALLOCATE(iMn, iDy, mLev, iStop, fill, STAT=k)
        DEALLOCATE(u, v, iws, iwd, newiStop, STAT=k)
        DEALLOCATE(tempk, pres, ht, STAT=k)
      End Do
      
c --- Deallocate space for input processing
      DEALLOCATE(sname, slat, slng) 
      DEALLOCATE(sId, sTZ, anem) 
      DEALLOCATE(aname, alat, alng) 
      DEALLOCATE(aId, aTZ) 
      Write(*,*) 'Fin'
      
c --- inp line formats
 70   format(a,a,a,a,a,a,a) 
 80   format(a,i,a,a,i,i) 
c --- Surf.dat line formats
 90   format(i2.2,i4.2,i4.2,i4.2,i4.2,i4.2,i4.2,i4.2,i4.2) 
100   format(i6,i4,i4,i6,i4,i4,i5,i5) 
110   format(i8,i8,i8,i8,i8) 
120   format(i2.2,i4,i4) 
130   format(f9.3,f9.3,i5,i5,f9.3,i5,f9.3,i5) 
c --- up.dat line formats
140   format(1x,6i5,f5.0,2i5) 
150   format(5x,l1,4x,l1,4x,l1,4x,l1) 
160   format(3x,i4,5x,a5,5x,4i2,5x,i2,31x,i2) 
170   format(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))

      End
!=======================================================================
!=================== End of Program InpCheck ===========================
!=======================================================================

!=======================================================================
      Subroutine IncTime(kyr,kjd,khr,hr)
c
c --- PURPOSE: to increment/decrement the date by hr, hr 
c               
c --- INPUTS:
      Integer kyr,kjd,khr,hr
c --- OUTPUT:
c
!=======================================================================

      Integer yrLen, CheckLeap

        If (CheckLeap(kYr)) Then 
          yrLen = 365
        Else
          yrLen = 364
        End If 

        If (hr.GT.0) Then
          kJd = kJd + (kHr+hr)/24
          kHr = MOD(kHr+hr,24)
          Do while (kJd.GT.yrLen)
            kYr = kYr + 1
            kJd = kJd-yrLen
            If (CheckLeap(kYr)) Then 
              yrLen = 366
            Else
              yrLen = 365
            End If 
          End Do
        Else
          kJd = kJd + (kHr+hr)/24
          kHr = MOD(kHr+hr,24)
          Do while (kJd.LT.0)
            kYr = kYr - 1
            If (CheckLeap(kYr)) Then 
              yrLen = 366
            Else
              yrLen = 365
            End If 
            kJd = kJd+yrLen
          End Do
        End If
      End

!=======================================================================
      Logical Function CalcNumObs(ieUnt, ibYr, ibJd, ibHr,
     1                            ieYr, ieJd, ieHr, nObs)
c
c --- PURPOSE:
c
c --- INPUTS:
      Integer ieUnt,ibYr,ibJd,ibHr,ieYr,ieJd,ieHr,nObs
c --- OUTPUT:
c
!=======================================================================
      
      Integer i
      Logical anErr      

c ---   check to see start date occurs before End date
      anErr = .FALSE.
      If (ibYr.GT.ieYr .OR. ibJd.GT.365 .OR. ieJd.GT.365 .OR. 
     1    ibHr.GT.23 .OR. ieHr.GT.23) Then
        Write(ieUnt,*)  ieUnt,ibYr,ibJd,ibHr,ieYr,ieJd,ieHr,nObs,
     1 'Malformed surface data line 1, date info in error!'
        anErr = .TRUE.
      End If

c --- calculating the correct number of hours (nObs) for dynamic allocation
      nObs = 0
      If (ibYr.EQ.ibYr) Then 
        nObs = (ieJd-ibJd)*24 + (ieHr-ibHr) +1
      Else    
        nObs = (364-ibJd) * 24
        If (CheckLeap(ibYr).EQ..TRUE.) Then 
          nObs = nObs+24
        End If
        Do i=ibYr+1, ibYr-1
          nObs = nObs + (365*24) 
          If (CheckLeap(i).EQ..TRUE.) Then 
            nObs = nObs + 24
          End If
        End Do
        nObs = ieJd*24 + ieHr
      End If      
      CalcNumObs = anErr
      Return
      End

!=======================================================================
      Logical Function CheckEnds(ieUnt,sLen,nObs,sYr,sJd,sHr,yr,jd,hr,
     1                           iYr,iJd,iHr,bIdx,eIdx)
c
c --- PURPOSE:
c
c --- INPUTS:
c
c --- OUTPUT:
c
!=======================================================================
      Integer ieUnt,sYr,sJd,sHr,yr,jd,hr
      Integer iYr(nObs),iJd(nObs),iHr(nObs)
      Integer sLen,nObs,bIdx,eIdx
      
      Integer i
      Logical anErr

c --- Find start time index surf.dat file
      bIdx = 0
      Do i=1, nObs
        If (sYr.EQ.iYr(i)) Then
          If (sJd.EQ.iJd(i)) Then
            If (sHr.EQ.iHr(i)) Then
              bIdx = i 
            End If
          End If    
        End If
      End Do

c --- Find End time index surf.dat file
      eIdx = 0
      Do i=bIdx, nObs
        If (yr.EQ.iYr(i)) Then
          If (jd.EQ.iJd(i)) Then
            If (hr.EQ.iHr(i)) Then
              eIdx = i 
            End If
          End If    
        End If
      End Do
cLst
      Write(ieUnt,*) 'The calculated number of observations ', nObs
      Write(ieUnt,*) 'B&E observation indices', bIdx, eIdx
      Write(*,*) 'The calculated number of observations ', nObs
      Write(*,*) 'B&E observation indices', bIdx, eIdx
cLst

c --- Time seqence checks
c      anErr = .FALSE.
c      If (iYr(1).NE.ibYr .OR. iJd(1).NE.ibJd .OR. iHr(1).NE.ibHr) Then
c        Write(ieUnt,*) 
c     1  'Data file start time and first obeservation time Do not match!'
c        anErr = .TRUE.
c      End If

      If (sLen.GT.nObs+2) Then
        Write(ieUnt,*) 
     1  'Data file can not support simualtion length!'
        anErr = .TRUE.
      End If

      If (bIdx.LT.2) Then
        Write(ieUnt,*) 
     1  'Data file can not support simualtion start time!'
        anErr = .TRUE.
      End If

      If (eIdx.EQ.0 .OR. eIdx.EQ.nObs) Then
        Write(ieUnt,*) 
     1  'Data file can not support simualtion End time!'
        anErr = .TRUE.
      End If
      CheckEnds = anErr
      End

!=======================================================================
      Real Function CalcWindDir(u, v, ws) 
c
c --- PURPOSE:
c
c --- INPUTS:
c
c --- OUTPUT:
c
!=======================================================================
      Real u, v, ws
    
      Real wd
      Real rad2Deg
        
      rad2Deg = 57.29577951
      If (-1.0 * U .GE. 0.0) Then
        If (.Not. (ACOS(-V/ws).EQ.-99999999.0)) Then
          wd = ACOS(-V/ws) * rad2Deg
        Else
          If (V.LT.0) Then
            wd = 0.0
          Else
            wd = 180.0
          End If
        End If
      Else
        If (.Not.(ACOS(-V/ws).EQ.-99999999.0)) Then
          wd = 360 - ACOS(-V/ws) * rad2Deg
        Else
          If (V.LT.0) Then
            wd = 0.0
          Else
            wd = 180.0
          End If
        End If
      End If
      CalcWindDir = wd
      End

!=======================================================================
      Subroutine ifillvalues(nSta, nObs, bIdx, eIdx, val, default) 
c
c --- PURPOSE:
c
c --- INPUTS:
c
c --- OUTPUT:
c
!=======================================================================
      Integer nSta, nObs, bIdx, eIdx, default
      Integer val(nObs, nSta) 
        
      Integer i, j, k, n, cnt, fillCnt
      Real tot, m

c --- Fill missing End values - part 'a' in surf check Write up
c     Write(*,*) 'Input for fill', nSta, nObs, bIdx, eIdx, default
      Do j=1, nSta
        fillCnt=0
c ---   front End values
        Do i=bIdx-1, eIdx+1
          If (val(i,j).NE. 9999) Then
            goto 1000
          End If
        End Do
1000    If (i.LT.eIdx+1) Then
          Do k=bIdx-1, i-1
            val(k,j) = val(i,j) 
c       Write(*,*) 'front End fill', val(i,j), k, j
        fillCnt = fillCnt+1
          End Do
        Else
c ---     Fill missing values between Ends - part 'c' in surf check Write up 
          Do i=bIdx-1, eIdx+1
            cnt = 0
            tot = 0.0
            Do k=1, nSta
              If (val(i,k) .NE.9999) Then
                tot = tot + val(i,k) 
                cnt = cnt+1
              End If
            End Do
            If (cnt.NE.0) Then
              val(i,j) = tot/cnt
c       Write(*,*) 'average fill', val(i,j), i, j, tot, cnt
              fillCnt = fillCnt+1
            Else
c ---         Fill missing values with default - part 'd' in surf check Write up 
              val(i,j) = default
c       Write(*,*) 'default fill', val(i,j), i, j
              fillCnt = fillCnt+1
            End If
          End Do
        End If

c ---   back End values
        Do i=eIdx+1, bIdx-1, -1
          If (val(i,j).NE. 9999) Then
            goto 2000
          End If
        End Do
2000    If (i.GT.bIdx-1) Then
          Do k=i+1, eIdx-1
            val(k,j) = val(i,j) 
c       Write(*,*) 'back End fill', val(k,j), k, j
            fillCnt = fillCnt+1
          End Do
        End If
c ---   Fill missing End values between Ends - part 'b' in surf check Write up 
        Do i=bIdx-1, eIdx+1
          If (val(i,j).EQ.9999) Then
            k = i+1
            Do while (val(k,j).EQ.9999) 
              k=k+1
            End Do
c ---       calculate slope
            m = (val(k,j) -val(i-1,j)) / (k-(i-1)) 
c ---       fill in values
            Do n=i, k-1
              val(i,j) =val(i-1,j) + m*(n-(i-1)) 
c       Write(*,*) 'between fill', val(i,j), i, j, m
              fillCnt = fillCnt+1
            End Do
          End If
        End Do
        Write(*,*) 'Station Done->', j, ' Filled values->', fillCnt
      End Do
      End

!=======================================================================
      Subroutine rfillvalues(nSta, nObs, bIdx, eIdx, val, default) 
c
c --- PURPOSE:
c
c --- INPUTS:
c
c --- OUTPUT:
c
!=======================================================================
      Integer nSta, nObs, bIdx, eIdx
      Real val(nObs, nSta), default
        
      Integer i, j, k, n, cnt, fillCnt
      Real tot, m

c --- Fill missing End values - part 'a' in surf check Write up
c     Write(*,*) 'Inputs for rfill', nSta, nObs, bIdx, eIdx, default
      Do j=1, nSta
        fillCnt=0
c ---   fill front End values
        Do i=bIdx-1, eIdx+1
          If (val(i,j).NE. 9999.0) Then
            goto 1000
          End If
        End Do
1000    If (i.LT.eIdx+1) Then
          Do k=bIdx-1, i-1
            val(k,j) = val(i,j) 
c       Write(*,*) 'front End fill', val(i,j), k, j
            fillCnt = fillCnt+1
          End Do
        Else
c ---     Fill missing values between Ends - part 'c' in surf check Write up 
          Do i=bIdx-1, eIdx+1
            cnt = 0
            tot = 0.0
            Do k=1, nSta
              If (val(i,k) .NE.9999.0) Then
                tot = tot + val(i,k) 
                cnt = cnt+1
              End If
            End Do
            If (cnt.NE.0) Then
              val(i,j) = tot/cnt
c       Write(*,*) 'average fill', val(i,j), i, j, tot, cnt
              fillCnt = fillCnt+1
            Else
c ---         Fill missing values with default - part 'd' in surf check Write up 
              val(i,j) = default
c       Write(*,*) 'default fill', val(i,j), i, j
              fillCnt = fillCnt+1
            End If
          End Do
        End If

c ---   back End values
        Do i=eIdx+1, bIdx-1, -1
          If (val(i,j).NE. 9999.0) Then
            goto 2000
          End If
        End Do
2000    If (i.GT.bIdx-1) Then
          Do k=i+1, eIdx-1
            val(k, j) = val(i,j) 
c       Write(*,*) 'back End fill', val(k,j), k, j
            fillCnt = fillCnt+1
          End Do
        End If
c ---   Fill missing End values between Ends - part 'b' in surf check Write up 
        Do i=bIdx-1, eIdx+1
          If (val(i,j).EQ.9999.0) Then
            k = i+1
            Do while (val(k,j).EQ.9999) 
              k=k+1
            End Do
c ---       calculate slope
            m = (val(k,j) -val(i-1,j)) / (k-(i-1)) 
c ---       fill in values
            Do n=i, k-1
              val(i,j) =val(i-1, j) + m*(n-(i-1)) 
c       Write(*,*) 'between fill', val(i,j), i, j, m
              fillCnt = fillCnt+1
            End Do
          End If
        End Do
        Write(*,*) 'Station Done->', j, ' Filled values->', fillCnt
      End Do
      End

!=======================================================================
      Logical Function CheckLeap(year) 
c
c --- PURPOSE:
c
c --- INPUTS:
c
c --- OUTPUT:
c
!=======================================================================
      Integer year

      If ((mod(year, 4).EQ.0) .AND. ((mod(year, 100).NE.0) .OR.
     1    (mod(year, 400).EQ.0)) ) Then
        CheckLeap=.TRUE.
      Else
        CheckLeap=.FALSE.
      End If
      End  

!=======================================================================
      subroutine grday(inum,iyr,ijul,imo,iday)
!=======================================================================
c
c --- CALMET   Version: 5.2       Level: 901130                   GRDAY
c ---          J. Scire, SRC
c
c --- Compute the Gregorian date (month, day) from the Julian day
c
c --- INPUTS:
c           IYR - integer      - Year
c          IJUL - integer      - Julian day
c        Parameters: IO6
c
c --- OUTPUT:
c           IMO - integer      - Month
c          IDAY - integer      - Day
c
c --- GRDAY called by:  COMP
c --- GRDAY calls:      none
c----------------------------------------------------------------------
c
c
      integer kday(12,2),ileap
      data kday/31,59,90,120,151,181,212,243,273,304,334,365,
     1          31,60,91,121,152,182,213,244,274,305,335,366/
c
      ileap=1
      If(mod(iyr,4).eq.0)ileap=2
      If(ijul.lt.1.or.ijul.gt.kday(12,ileap))go to 11
c
      Do 10 i=1,12
      If(ijul.gt.kday(i,ileap))go to 10
      imo=i
      iday=ijul
      If(imo.ne.1)iday=ijul-kday(imo-1,ileap)
      return
10    continue
c
11    continue
      write(inum,12)iyr,ijul
12    format(//2x,'ERROR in SUBR. GRDAY -- invalid Julian day '//2x,
     1 'iyr = ',i5,3x,'ijul = ',i5)
      stop
      End


!=======================================================================
      subroutine julday(io, iYr, iMo, iDy, iJday) 
!=======================================================================
c
c --- CALUTILS   Version: 2.1      Level: 000602                 JULDAY
c ---            J. Scire, SRC
c
c --- PURPOSE:  Compute the Julian day number from the Gregorian
c               date (month, day) 
c
c --- UPDATE
c ---               000602  (DGS) : YYYY format for year
c
c --- INPUTS:
c            IO - Integer      - Unit number for list file output
c           iYr - Integer      - Year
c           IMo - Integer      - Month
c           iDy - Integer      - Day
c
c --- OUTPUT:
c          iJay - Integer      - Julian day
c
c --- JULDAY called by:  host subroutines
c --- JULDAY calls:      none
!=======================================================================
c
      Integer kday(12) 
      data kday/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
c
c --- Check for valid input data
      ierr=0
c --- Check for valid month
      If(imo.lt.1.OR.imo.gt.12) ierr=1
c --- Check for valid day in 30-day months
      If(imo.eq.4.OR.imo.eq.6.OR.imo.eq.9.OR.imo.eq.11) Then
         If(iDy.gt.30) ierr=1
      Else If(imo.eq.2) Then
         If(mod(iYr, 4).EQ.0) Then
c ---       February in a leap year
            If(iDy.gt.29) ierr=1
         Else
c ---       February in a non-leap year
            If(iDy.gt.28) ierr=1
         Endif
      Else
c ---    Check for valid day in 31-day months
         If(iDy.gt.31) ierr=1
      Endif
c
      If(ierr.eq.1) Then
         Write(io,*) 
         Write(io,*) 'ERROR in SUBR. JULDAY'
         Write(io,*) 'Invalid date - iYr = ', iYr, ' IMO = ', 
     1    imo, ' iDy = ', iDy
         Write(*,*) 
         Stop 'Halted in JULDAY -- see list file.'
      Endif
c
c --- Compute the Julian day
      iJday=kday(imo) +iDy
      If(imo.le.2) return
      If(mod(iYr, 4).EQ.0) iJday=iJday+1
c
      Return
      End

!=======================================================================
      subroutine lineregress(m,b,sigma,xtab,ytab,ntab)
!=======================================================================
c --- AUTHOR:  JRishel
c --- PURPOSE: Calculate the slope (m) and intercept (b) of a best fit
c       line to a group of points (xtab,ytab)
c
c       Formulations adapted from "Numerical Recipes, The Art of
c              Scientific Computing (Fortran Version)", 1990.
c
c --- INPUTS:
c         xtab - Real array  - x point values
c         ytab - Real array  - y point values
c               sigma - Real array  - uncertainity in y points
c         ntab - integer    - number of points
c
c --- OUTPUT:
c          m - Real        - slope of line
c                   b - Real        - intercept
!=======================================================================
c
      Real sigma(ntab),xtab(ntab),ytab(ntab)
      Real m,b
      integer ntab
c
c     perform summations for regression equation
      Do 20 i= 1,ntab
        s = s + (1/sigma(i)**2)
        sx = sx + (xtab(i)/sigma(i)**2)
        sy = sy + (ytab(i)/sigma(i)**2)
        sxx = sxx + ((xtab(i))**2/sigma(i)**2)
        sxy = sxy + ((xtab(i)*ytab(i))/sigma(i)**2)
20    continue

c     calculate coefficients for straight line
      delta=s*sxx-(sx)**2
c     slope
      m=(s*sxy-sx*sy)/delta
c     intercept
      b=(sxx*sy-sx*sxy)/delta

      return
      End

!=======================================================================
      subroutine linint(x,y,xtab,ytab,ntab)
!=======================================================================
c --- AUTHOR:  JRishel
c --- PURPOSE: Given a value of x return a value of y based on interpolation
c              within a table of y values (ytab) corresponding to a table of x
c              values (xtab).  The subroutine assumes that the values in xtab
c              increase monotonically.  Efficiency is increased by remembering
c              the table points used in the last call (ilast) xtab and ytab
c              are provided through the argument list as is their length ntab.
c
c --- INPUTS:
c            x - Real       - known value of x 
c         xtab - Real array - x point values
c         ytab - Real array - y point values
c         ntab - integer   - number of points
c
c --- OUTPUT:
c          y - Real       - interpolated value of y paired with known value of x
!=======================================================================
      Real xtab(ntab),ytab(ntab)
c
      save ilast
c
      data ilast/1/
c
c    Start the search from the last point of table use index
c
      If (x.le.xtab(ilast+1)) then
c
c    Search down the table from point of last use
c
          Do 20 i1=ilast,1,-1
              If(x.ge.xtab(i1)) go to 60
  20          continue
          write(6,*) 'x = ', x, '  is below the table range'  
          stop
      Else
c
c    Search up the table from point of last use
c
          Do 40 i1=ilast+1,ntab-1
              If(x.le.xtab(i1+1)) go to 60
  40          continue
          write(6,*) 'x = ', x, '  is above the table range'
          stop
      endif
c
c   Bounding points found, interpolate
c
  60  wx=(x-xtab(i1))/(xtab(i1+1)-xtab(i1))
      y=(1-wx)*ytab(i1)+wx*ytab(i1+1)
      ilast=i1
      return
      End
!=======================================================================