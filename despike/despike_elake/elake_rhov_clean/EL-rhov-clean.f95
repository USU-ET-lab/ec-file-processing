!  Program to Find Spikes in Water Vapor Density for Eagle Lake
!  Updated 14 December 2018
!

        implicit none

        character (len=25) infile,outfile1,outfile2,dfile
        character (len=1) response
        character(len=1), parameter :: c_esc = achar(27)
        character(len=*), parameter :: c_red = '[31m'
        character(len=*), parameter :: c_bold = '[1m'
        character(len=*), parameter :: c_reset = '[0m'
        

        integer, parameter :: dp=kind(1.0d0)
        integer, Parameter :: Medium=Selected_INT_Kind(6)
        integer, Parameter :: Long=Selected_INT_Kind(9)
        integer stat,iday,unr,ix1,ix2,nw1,nw2,nw,n,i,j,jj,day,year(80000),doy(80000),hrmin(80000),hr,dist
        integer(kind=Medium) ::  finish,oldhr,nrecord,del,kk
        integer(kind=long) :: d1(80000)
	
        real sdev,rn,nr,sec(80000),Ux(80000),Uy(80000),Uz(80000),Ts(80000),rhoc(80000),rhov(80000),P(80000),agc(80000)
        real del_rhovw(80000),drhov,nbeg,nend,diff,sdev_hi,sdev_low,del_hi,del_low

        real(kind=dp) ::  sumrhov,sumrhov2,rhovavg,stdevrhov,SD_rhov(80000)
       
        write (6,'(A22)') 'Name of the input file'
        read (5,*) infile
        open(1,file=infile)

        write (6,'(/,A53)') 'Name of the intermediate file made using first window'
        read (5,*) outfile1

        write (6,'(/,A22)') 'Name of the final file'
        read (5,*) outfile2

        open(2,file=outfile1,action='READWRITE')
        open(4,file=outfile2,action='WRITE')

        write(dfile,"(a8)")("diag.dat")
        open(3,file=dfile)

        write (6,'(/,A61)') 'Size of the 1st and 2nd Window & Critical Standard Deviation Value'
        read (5,*) nw1,nw2,sdev
	
!
!  Define Critical Values of the Difference of Instantaneous Value From the Window Average for Each Variable.
!  This is a Critical and Somewhat Subjective Step.
!


!  rhov in gm/m3
        drhov=5.0

!  Upper and lower coefficients for the 
        sdev_hi=1.3
        sdev_low=.6
        
        del_hi=1.6
        del_low=.3

        sumrhov=0.0
        sumrhov2=0.0

!
!  Read All the Data Into Arrays and Determine When End of the Hour is Reached
!

        i=1
        nrecord=0
        stat=0
        finish=0
        nw=nw1
        write(6,'(/,A15,2x,I4,/)') 'Running Window ',nw
        call sleep(2)

	
 10 	do while (stat==0)
 
         if(stat/=0) then
         go to 90
         end if

        if(finish==0) then

        unr=1
        read (1,*,iostat=stat,end=90) year(i),doy(i),hrmin(i),sec(i),ux(i),uy(i),uz(i),Ts(i),  &
                                    d1(i),rhoc(i),rhov(i),P(i),agc(i)

        else
        unr=2
        read (2,*,iostat=stat,end=90) year(i),doy(i),hrmin(i),sec(i),ux(i),uy(i),uz(i),Ts(i),  &
                                      d1(i),rhoc(i),rhov(i),P(i),agc(i)
        end if

        hr=INT(hrmin(i)/100)
        day=doy(i)
     
        if(i==1) then
        iday=doy(i)
        oldhr=hr
        end if

        nrecord=nrecord+1

!
!  Check if Entered New Hour. If So, Back Up One Record, So Next Record Read is the First of the New Hour.
!

        if((hr.NE.oldhr).or.(doy(i)>iday)) then
        backspace(unit=unr)
        oldhr=hr

        if((hrmin(i-1)==2359).and.(sec(i-1)==59.95)) then
        hr=24
        i=i+1
        go to 90
        end if

        go to 90
        end if

        oldhr=hr
        i=i+1
        go to 10

   90   n=i-1
        rn= float(n)
        hr=hr*100
        
        nrecord=nrecord-1
        write(6,'(/,A15,2x,I4)') 'Processing Hour',hr
        write(6,109) nrecord
 109    format(2x,I6,2x,'records read')

!
!  Sum the Variables and Products Required for Later Calculations.
!  Statistics Calculated for All Values in the Window Except the Given Value.
!
        if(finish==0) then
        nw=nw1
        else
        nw=nw2
        end if

        nr=float(nw)
        
        if(nw<100) then
        dist=40
        else
        dist=60
        end if
             
        do j=1,(n-nw),nw

        do i=j,(j+(nw-1)),1
        sumrhov=sumrhov+rhov(i)
        sumrhov2=sumrhov2+rhov(i)*rhov(i)
	    end do
	 
!
!  Calculate Averages and Standard Deviations in the Window
!

        rhovavg=sumrhov/nr
        stdevrhov=sqrt((sumrhov2-(sumrhov**2.0)/nr)/(nr-1))

        sumrhov=0.0
        sumrhov2=0.0

        do i=j,(j+(nw-1))

!
!  Calculate Difference From Instantaneous Values From Window Average
!

        del_rhovw(i)=ABS(rhov(i)-rhovavg)

!
!  Determine Number of Standard Deviations Away From Window Average
!

        SD_rhov(i)=ABS((rhov(i)-rhovavg)/stdevrhov)
                  
!
!  For Each Variable, Check if Values Exceed Standard Deviations and Absolute Difference Limits.
!  If They Exceed Criteria, Write 60 Earlier Values, Flagged Value, Then 60 Later Values
!
       
        if(((ABS(del_rhovw(i)).GT.drhov).and.(ABS(SD_rhov(i)).GT.(sdev*sdev_low))).or. &
          &((ABS(SD_rhov(i)).GT.sdev).and.(ABS(del_rhovw(i)).GT.(drhov*del_low))).or. &
          &(ABS(del_rhovw(i)).GT.(drhov*del_hi)).or. &
          &(ABS(SD_rhov(i)).GT.(sdev*sdev_hi))) then

        write(6,'(/)')
        do jj=dist,1,-1
        write(6,150) (-jj),hrmin(i-jj),sec(i-jj),rhov(i-jj)
   150  format(I3,2x,I4,2x,F5.2,4x,F6.2)
        end do

        write(6,'(A1)') ' '
        write(6,145) c_esc,c_red,c_esc,c_bold,c_esc,hrmin(i),sec(i),rhov(i),SD_rhov(i),del_rhovw(i),c_esc,c_reset
   145  format(a1,a4,a1,a3,a1,'   0',2x,I4,2x,F5.2,4x,F6.2,3x,'# Stdev = ',F5.1,2x,'drhov_win =  ',F5.1,2x,a1,a3)
        write(6,'(A1)') ' '

        do jj=1,dist
        write(6,150) (jj),hrmin(i+jj),sec(i+jj),rhov(i+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response

        if(response=='Y'.or.response=='y') then
        write(6,'(A1)') ' '
        write(6,'(A39)',advance="no") 'First and last index to be replaced    '
        read(5,*) ix1,ix2
        nbeg=rhov(i+ix1-1)
        nend=rhov(i+ix2+1)
        del=ix2-ix1
        diff=nend-nbeg
        kk=0

        do jj=ix1,ix2,1
        kk=kk+1
        write(3,408) hrmin(i+jj),sec(i+jj),rhov(i+jj)
   408  format('original'  I4,2x,F5.2,2x,'rhov= ',F5.2)

        if((ix1>0.and.ix2>0).or.(ix1<0.and.ix2<0)) then
        rhov(i+jj)=nbeg+kk*(diff/ABS(ix1+ix2))
        else
        rhov(i+jj)=nbeg+kk*(diff/(ABS(ix1)+ix2+2))
        end if

        write(3,409) hrmin(i+jj),sec(i+jj),rhov(i+jj)
   409  format('new'  I4,2x,F5.2,2x,'rhov= ',F5.2,/)

        end do
        
        end if
        
        end if
        
        end do
        
        end do

!
!  Write Out the New Data for the Hour
!

        do i=1,n,1
        if(finish==0) then
        write(2,200) year(i),doy(i),hrmin(i),sec(i),ux(i),uy(i),uz(i),Ts(i),  &
                    d1(i),rhoc(i),rhov(i),P(i),agc(i)
                
  200   format(I4,2x,I3,2x,I4,2x,F5.2,2x,3(F9.5,2x),F8.5,2x,I6,2x,F9.4,2x,F9.6,2x,F8.5,2x,F6.2)
        else
        write(4,200) year(i),doy(i),hrmin(i),sec(i),ux(i),uy(i),uz(i),Ts(i),  &
                     d1(i),rhoc(i),rhov(i),P(i),agc(i)
        end if
        end do
        
        if(hr==2400) then
        go to 92
        end if

        i=1
        nrecord=0
        go to 10
        
        end do
        
   92   finish=finish+1
        
        if(finish==1) then
        rewind(unit=2)
        stat=0
        nrecord=0
        i=1
        nw=nw2
        write(6,'(/,A15,2x,I4,/)') 'Running Window ',nw2
        call sleep(2)
        go to 10
        end if
        
        if(finish==2) then
        go to 99
        end if
        
  99    end

  
