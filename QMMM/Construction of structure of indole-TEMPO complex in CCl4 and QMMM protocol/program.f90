program t
implicit none
integer*8 i,ii,iii,iiii
real(kind=8)::rsc=5.500d0,dist,jd=3.50d0
character*3 atomna
character*80 pl
integer ist,n
real*8 x,y,z
real*8 cx,cy,cz
 
 type :: pdb
 character(len=6)::atm
 integer(kind=4)::atnum
 character(len=6)::atnam
 character(len=3)::resnam
 integer(kind=4)::resnum
 real(kind=4)::x
 real(kind=4)::y
 real(kind=4)::z
 end type pdb
 
 type(pdb)::inpdbl(45)  !allocatable::pdbl(:)
 type(pdb)::pdbl(2560)  !allocatable::pdbl(:)
 integer(kind=4)::delm(512)

do i=1,512
   delm(i)=0
enddo

open(10,file='TEMPOindole')
do i=1,45
   read(10,100)inpdbl(i)
   write(*,100)inpdbl(i)
enddo
close(10)
 
cx=0.00d0
cy=0.00d0
cz=0.00d0
do i=1,45
   cx=cx+inpdbl(i).x
   cy=cy+inpdbl(i).y
   cz=cz+inpdbl(i).z
!     write(*,100)inpdbl(i)
enddo
cx=cx/45.00d0
cy=cy/45.00d0
cz=cz/45.00d0
 
do i=1,45
   inpdbl(i).x=inpdbl(i).x-cx
   inpdbl(i).y=inpdbl(i).y-cy
   inpdbl(i).z=inpdbl(i).z-cz
   write(678,100)inpdbl(i)
enddo
   write(678,'("TER")')



open(10,file='CCl4')
i=0
do
   read(10,'(a80)',iostat=ist)pl
   if(ist.lt.0)exit
   if(pl(1:6).eq.'ATOM  ')then
!     backspace(10)
     i=i+1
     read(pl(1:54),100)pdbl(i)
   else

   endif
   write(*,100)pdbl(i)
enddo
close(10)
100 format (a6,1x,i4,a6,a3,3x,i3,4x,3(f8.3))
 
cx=0.00d0
cy=0.00d0
cz=0.00d0
do i=1,2560
   cx=cx+pdbl(i).x
   cy=cy+pdbl(i).y
   cz=cz+pdbl(i).z
!     write(*,100)pdbl(i)
enddo
cx=cx/2560.00d0
cy=cy/2560.00d0
cz=cz/2560.00d0
 
do i=1,2560
   pdbl(i).x=pdbl(i).x-cx
   pdbl(i).y=pdbl(i).y-cy
   pdbl(i).z=pdbl(i).z-cz
   write(688,100)pdbl(i)
   if(mod(i,5).eq.0)then 
     write(688,'("TER")')
   endif
enddo
     write(688,'("TER")')

do i=1,2560
  do ii=1,45
     dist=sqrt((pdbl(i).x-inpdbl(ii).x)**2+&
               (pdbl(i).y-inpdbl(ii).y)**2+&
               (pdbl(i).z-inpdbl(ii).z)**2)
     if(dist.lt.jd)then 
        delm(pdbl(i).resnum)=pdbl(i).resnum
        print*, delm(pdbl(i).resnum)
     endif
  enddo
enddo

do i=1,45
   write(78,100)inpdbl(i)
enddo
   write(78,'("TER")')
n=3
do i=1,2560
   if(delm(pdbl(i).resnum).ne.0) cycle
   pdbl(i).resnum=n
   write(78,100)pdbl(i)
   if(mod(i,5).eq.0)then
     write(78,'("TER")')
     n=n+1
   endif
enddo
     write(78,'("TER")')



 
end
