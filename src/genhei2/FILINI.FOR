*------------------------------------------------------------------------------*
*
      subroutine fileini(fnpath, target, nlen, nulog, nuini, program)
*
*------------------------------------------------------------------------------*
*  Author: K. F. Eckerman
*  Opens the file 'program'.ini and finds the full file name, including path,
*  for the file target and its record length nlen.  See the ini file for
*  additional comments.
*  target = file name passed to this subprogram
*  fnstd = standard file name read from file 'program'.INI
*  fnpath = file name including path
*
      character*(*) fnpath, target, program
      character*12 fnini, fnstd, lcase
      integer nulog
      logical test
      parameter (maxfil = 40)
      fnini = program(1:len_trim(program)) // '.ini'
      inquire (file = fnini, exist = test)
      if (.not. test) goto 20
      open(nuIni, file=fnini, status='old')
      target = lcase(target)
      do i = 1, maxfil
        read(nuIni, *, end=15) fnstd, fnpath, nlen
        if (fnstd(:3) .eq. 'EOF') goto 15
        if(lcase(fnstd) .eq. target) goto 20
      end do

   15 write(nuLog,9110)target(:len_trim(target)),fnini(:len_trim(fnini))
      write( *   ,9110)target(:len_trim(target)),fnini(:len_trim(fnini))
      close(nuIni)
      stop 1
   20 close(nuIni)
      return
 9110 format(' **** FATAL ERROR in function FileIni: Unable to find the
     :file ', a, ';'/6x,'check ',a,' for proper assignments ****')
      end
