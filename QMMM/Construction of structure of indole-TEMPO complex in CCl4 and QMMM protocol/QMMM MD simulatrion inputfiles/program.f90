program p
implicit none
character*80 cmdstr,gpl
integer iii,system,ist,iatom,nstep
character*63 strtail,tmpstr
tmpstr='   '
nstep=0

open(60,file='spden.out')
do 

   cmdstr='tail -n 1 old.gau_job.log > filetail'

   iii=system(cmdstr)
   open(10,file='filetail')
   read(10,'(a63)')strtail
   close(10)
   if(tmpstr.ne.strtail)then 
      nstep=nstep+1
      open(30,file='old.gau_job.log')
      do
        read(30,'(a80)',iostat=ist)gpl
        if(ist.lt.0)exit
        if(gpl(1:37).eq.' Mulliken charges and spin densities:')then
          read(30,*)
          write(60,'("Step:  ",i8)')nstep
          do iatom=1,45
             read(30,'(a32)')gpl
             write(60,'(a32)')gpl
          enddo
          exit
        endif
      enddo
      close(30)
      tmpstr=strtail
   endif


!if(nstep.eq.20000)exit
enddo


end 
