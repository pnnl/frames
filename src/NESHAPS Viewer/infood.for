      subroutine infood(npths,ndist,ndir,food)
	 include 'neshaps.cmn'
	dimension food(8,ndist,ndir)
	character*16 pathread(8)
	logical SEQI
c
      food=0.0
	do mp=1,npths
		read(nfod,*)pathread(mp)
	        if(SEQI(pathread(mp),'leaf',4))kpth=1
	        if(SEQI(pathread(mp),'root',4))kpth=2
	        if(SEQI(pathread(mp),'frui',4))kpth=3
	        if(SEQI(pathread(mp),'grai',4))kpth=4
	        if(SEQI(pathread(mp),'meat',4))kpth=5
	        if(SEQI(pathread(mp),'poul',4))kpth=6
	        if(SEQI(pathread(mp),'milk',4))kpth=7
	        if(SEQI(pathread(mp),'eggs',4))kpth=8
	 do j=1,ndir
	  read(nfod,*)(food(kpth,i,j),i=1,ndist)
	 end do
	end do
	return
	end
