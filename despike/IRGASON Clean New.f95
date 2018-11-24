!  Program to Find Spikes in the Time Series Data For BOR IRGASON System
!  Updated 9 November 2018
!

        implicit none

        character (len=25) infile,outfile1,outfile2,dfile
        character (len=6) site
        character (len=3) cdoy
        character (len=1) response

        integer, parameter :: dp=kind(1.0d0)
        integer, Parameter :: Medium=Selected_INT_Kind(6)
        integer(kind=Medium) :: stat,nw1,nw2,nw,i,j,k,l,n,jj,day,year(80000),doy(80000),hrmin(80000),hr,finish,oldhr
        integer(kind=Medium) ::  nrecord,ix1,ix2,del,kk,ik
        integer(kind=Medium) :: Ux_skip=0.0,Uy_skip=0.0,Uz_skip=0.0,Ts_skip=0.0,rhov_skip=0.0,rhoc_skip=0.0
        integer, Parameter :: Long=Selected_INT_Kind(9)
        Integer(Kind=Long) diag_sonic(80000),diag_irga(80000)
	
        real sdev,rn,nr,sec(80000),ux(80000),uy(80000),uz(80000)
        real Ts(80000),rhov(80000),rhoc(80000),P(80000),signalc(80000),signalv(80000)
        real T1_irga(80000),T2_irga(80000),rhoc_irga(80000),sdevUz
        real sdux(80000),sduy(80000),sduz(80000),sdTs(80000)
        real sdrhov(80000),sdrhoc(80000),delux(80000),deluy(80000),deluz(80000),delTs(80000)
        real delrhov(80000),delrhoc(80000)
        real duxw(80000),duyw(80000),duzw(80000),dTsw(80000),drhovw(80000),drhocw(80000)
        real dux,duy,duz,dTs,drhov,drhoc,nbeg,nend,diff

        real(kind=dp) ::  sumux(80000),sumuy(80000),sumuz(80000),sumTs(80000)
        real(kind=dp) ::  sumrhov(80000),sumrhoc(80000),sumux2(80000)
        real(kind=dp) ::  sumuy2(80000),sumuz2(80000),sumTs2(80000),sumrhov2(80000)
        real(kind=dp) ::  sumrhoc2(80000),uxavg(80000),uyavg(80000),uzavg(80000),Tsavg(80000),rhovavg(80000)
        real(kind=dp) ::  rhocavg(80000),stdevux(80000),stdevuy(80000)
        real(kind=dp) ::  stdevuz(80000),stdevTs(80000),stdevrhov(80000),stdevrhoc(80000)
       
        write (6,'(A12)') 'Site and DOY'
        read (5,*) site,day

        write (6,'(A22)') 'Name of the input file'
        read (5,*) infile
        open(1,file=infile)

        write (6,'(/,A53)') 'Name of the intermediate file made using first window'
        read (5,*) outfile1

        write (6,'(/,A22)') 'Name of the final file'
        read (5,*) outfile2
        open(2,file=outfile1,action='READWRITE')
        open(4,file=outfile2,action='WRITE')

        write(cdoy,"(I3)") day
        write(dfile,"(a20)")("d"//site//cdoy//".dat")
        open(3,file=dfile)

        write (6,'(/,A61)') 'Size of the 1st and 2nd Window & Critical Standard Deviation Value'
        read (5,*) nw1,nw2,sdev

       sdevUz=2*sdev
       nw=nw1
	
!
!  Define Critical Values of the Difference of Instantaneous Value From the Window Average for Each Variable.
!  This is a Critical and Somewhat Subjective Step.
!

!  Ux in m/s
	dux=4.0
	
!  Uy in m/s
	duy=4.0

!  Uz in m/s
	duz=3.0

!  Tsonic in C
	dTs=4.0

!  rhov in gm/m3
    drhov=4.0

!  rhoc in mg/m3
	drhoc=160.0

!
!  Zero Out the Summation Terms
!
         
        do i=1,rn,1
        sumux(i)=0.0
        sumuy(i)=0.0
        sumuz(i)=0.0
        sumTs(i)=0.0
        sumrhov(i)=0.0
        sumrhoc(i)=0.0
        sumux2(i)=0.0
        sumuy2(i)=0.0
        sumuz2(i)=0.0
        sumTs2(i)=0.0
        sumrhov2(i)=0.0
        sumrhoc2(i)=0.0
        end do

!
!  Read All the Data Into Arrays and Determine When End of the Hour is Reached
!

        i=1
        nrecord=0
        stat=0
        finish=0

	
 10 	do while (stat==0)

        if(finish==0) then

        read (1,*,iostat=stat,end=92) year(i),doy(i),hrmin(i),sec(i),ux(i),uy(i),uz(i),Ts(i),  &
                        diag_sonic(i),rhoc(i),rhov(i),diag_irga(i),rhoc_irga(i),T1_irga(i),T2_irga(i),  &
                        P(i),signalc(i),signalv(i)

        else

        read (2,*,iostat=stat,end=89) year(i),doy(i),hrmin(i),sec(i),ux(i),uy(i),uz(i),Ts(i),  &
                        diag_sonic(i),rhoc(i),rhov(i),diag_irga(i),rhoc_irga(i),T1_irga(i),T2_irga(i),  &
                        P(i),signalc(i),signalv(i)
        end if

        hr=INT(hrmin(i)/100)
     
        if(i==1) then
        oldhr=hr
        end if

        nrecord=nrecord+1

!
!  Crossed into New Hour. Back Up One Record, So Next Record Read is the First of the New Hour.
!
        if(hr.NE.oldhr) then
        backspace(1)
        oldhr=hr

        if(finish==1) then
        backspace(2)
        oldhr=hr
        end if

        if((hrmin(i-1)==2359).and.(sec(i-1)==59.95)) then
        hr=24
        i=i+1

!
!  After 1st Window Completed, Set Flag Denoting That Output File is to be Read.
!


        if(finish==1) then
        finish=2
        end if

        end if
        go to 90
        end if
        
        oldhr=hr
        i=i+1
        go to 10
  89    finish=finish+1                                  
  90    n=i-1
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
        if(finish==1) then
        nw=nw2
        end if

         nr=float(nw)
             
        do j=1,(n-nw),nw
         
        do l=j,(j+(nw-1)),1
	
        do i=j,(j+(nw-1)),1
        if(l/=i) then   
        sumux(l)=sumux(l)+ux(i)
        sumuy(l)=sumuy(l)+uy(i)
        sumuz(l)=sumuz(l)+uz(i)
        sumTs(l)=sumTs(l)+Ts(i)
        sumrhov(l)=sumrhov(l)+rhov(i)
        sumrhoc(l)=sumrhoc(l)+rhoc(i)

        sumux2(l)=sumux2(l)+ux(i)*ux(i)
        sumuy2(l)=sumuy2(l)+uy(i)*uy(i)
        sumuz2(l)=sumuz2(l)+uz(i)*uz(i)
        sumTs2(l)=sumTs2(l)+Ts(i)*Ts(i)
        sumrhov2(l)=sumrhov2(l)+rhov(i)*rhov(i)
        sumrhoc2(l)=sumrhoc2(l)+rhoc(i)*rhoc(i)

        end if
	    
        end do
	 
!
!  Calculate Standard Deviations in the Window
!
        uxavg(l)=sumux(l)/nr
        uyavg(l)=sumuy(l)/nr
        uzavg(l)=sumuz(l)/nr
        Tsavg(l)=sumTs(l)/nr
        rhovavg(l)=sumrhov(l)/nr
        rhocavg(l)=sumrhoc(l)/nr

        stdevux(l)=sqrt((sumux2(l)-(sumux(l)**2.0)/nr)/(nr-1))
        stdevuy(l)=sqrt((sumuy2(l)-(sumuy(l)**2.0)/nr)/(nr-1))
        stdevuz(l)=sqrt((sumuz2(l)-(sumuz(l)**2.0)/nr)/(nr-1))
        stdevTs(l)=sqrt((sumTs2(l)-(sumTs(l)**2.0)/nr)/(nr-1))
        stdevrhov(l)=sqrt((sumrhov2(l)-(sumrhov(l)**2.0)/nr)/(nr-1))
        stdevrhoc(l)=sqrt((sumrhoc2(l)-(sumrhoc(l)**2.0)/nr)/(nr-1))

        end do

        do ik=j,(j+(nw-1)),1

!
!  Determine Number of Standard Deviations Away From Window Average
!

        sdux(k)=(ux(k)-uxavg(k))/stdevux(k)
        sduy(k)=(uy(k)-uyavg(k))/stdevuy(k)
        sduz(k)=(uz(k)-uzavg(k))/stdevuz(k)
        sdTs(k)=(Ts(k)-Tsavg(k))/stdevTs(k)
        sdrhov(k)=(rhov(k)-rhovavg(k))/stdevrhov(k)
        sdrhoc(k)=(rhoc(k)-rhocavg(k))/stdevrhoc(k)
                  
!
!  Calculate Difference From Instantaneous Values From Window Average
!

        duxw(k)=ux(k)-Uxavg(k)
        duyw(k)=uy(k)-Uyavg(k)
        duzw(k)=uz(k)-Uzavg(k)
        dTsw(k)=Ts(k)-Tsavg(k)
        drhovw(k)=rhov(k)-rhovavg(k)
        drhocw(k)=rhoc(k)-rhocavg(k)

!
!  Calculate Difference of Successive Values
!
        if(k/=1) then
        delUx(k)=Ux(k)-Ux(k-1)
        delUy(k)=Uy(k)-Uy(k-1)
        delUz(k)=Uz(k)-Uz(k-1)
        delTs(k)=Ts(k)-Ts(k-1)
        delrhov(k)=rhov(k)-rhov(k-1)
        delrhoc(k)=rhoc(k)-rhoc(k-1)
        else
        delUx(k)=0.0
        delUy(k)=0.0
        delUz(k)=0.0
        delTs(k)=0.0
        delrhov(k)=0.0
        delrhoc(k)=0.0

        end if

!
!  For Each Variable, Check if Values Exceed Standard Deviations and Absolute Difference Limits
!  If They Exceed Criteria, Write 60 Earlier Values, Flagged Value, Then 60 Later Values
!

!
!  Procedure for Ux Values
!

        if(Ux_skip==0) then

!
!  If Have Not Already Approved the Next Set of Values, Check the Next Value in the Window
!

        if((ABS(sdux(k)).GT.sdev*2).or.(ABS(delux(k)).GT.dux).or.(ABS(duxw(k)).GT.dux)) then
        do jj=60,1,-1
        write(6,140) (-jj),hrmin(k-jj),sec(k-jj),ux(k-jj)
  140   format(I3,2x,I4,2x,F5.2,4x,F6.2)
        end do

        write(6,'(A1)') ' '
        write(6,'(A12)') '          Ux'
        write(6,141) hrmin(k),sec(k),ux(k),sdux(k),delux(k)
 141    format(' 0',4x,I4,2x,F5.2,4x,F6.2,4x,'SD = ',F6.1,2x,'Del=  ',F6.1)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,140) (jj),hrmin(k+jj),sec(k+jj),ux(k+jj)
        end do

!
!  User Decides if the Value is Valid
!

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response

!
!  When Bad Data identified, User Notes the Beginning and End of Data to Be Replaced
!

        if(response=='Y'.or.response=='y') then
        Ux_skip=0
        write(6,'(A39)') 'First and last index to be replaced    '
        read(5,*) ix1,ix2
        nbeg=ux(k+ix1-1)
        nend=ux(k+ix2+1)
        diff=nend-nbeg
        kk=0

        do jj=ix1,ix2,1
        kk=kk+1

        write(3,400) hrmin(k+jj),sec(k+jj),ux(k+jj)
400     format('original'  I4,2x,F5.2,2x,'Ux= ',F6.2)


!
!  Linear Interpolation Used to Replace Values
!

        if((ix1>0.and.ix2>0).or.(ix1<0.and.ix2<0)) then
        ux(k+jj)=nbeg+kk*(diff/ABS(ix1+ix2))
        else
        ux(k+jj)=nbeg+kk*(diff/(ABS(ix1)+ix2+2))
        end if

        write(3,401) hrmin(k+jj),sec(k+jj),ux(k+jj)
401     format('new'  I4,2x,F5.2,2x,'Ux= ',F6.2,/)

        end do

        else
        Ux_skip=1
        end if

        end if

!
!  If the Set of Displayed Values Was Good, Keep Skipping Until End of Future Diplay Reached
!

        else
        Ux_skip=Ux_skip+1
        if(Ux_skip>60) then
        Ux_skip=0
        end if

        end if

!
!  Procedure for Uy Values
!

        if(Uy_skip==0) then

        if((ABS(sduy(k)).GT.sdev*2).or.(ABS(deluy(k)).GT.duy).or.(ABS(duyw(k)).GT.duy)) then
        do jj=60,1,-1
        write(6,140) (-jj),hrmin(k-jj),sec(k-jj),uy(k-jj)
        end do

        write(6,'(A1)') ' '
        write(6,'(A12)') '          Uy'
        write(6,141) hrmin(k),sec(k),uy(k),sduy(k),deluy(k)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,140) (jj),hrmin(k+jj),sec(k+jj),uy(k+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response


        if(response=='Y'.or.response=='y') then
        Uy_skip=0
        write(6,'(A39)',advance="no") 'First and last index to be replaced    '
        read(5,*) ix1,ix2
        nbeg=uy(k+ix1-1)
        nend=uy(k+ix2+1)
        diff=nend-nbeg
        kk=0
        do jj=ix1,ix2,1
        kk=kk+1
        write(3,402) hrmin(k+jj),sec(k+jj),uy(k+jj)
   402  format('original'  I4,2x,F5.2,2x,'Uy= ',F6.2)


        if((ix1>0.and.ix2>0).or.(ix1<0.and.ix2<0)) then
        uy(k+jj)=nbeg+kk*(diff/ABS(ix1+ix2))
        else
        uy(k+jj)=nbeg+kk*(diff/(ABS(ix1)+ix2+2))
        end if

        write(3,403) hrmin(k+jj),sec(k+jj),uy(k+jj)
   403  format('new'  I4,2x,F5.2,2x,'Uy= ',F6.2,/)

        end do


        else
        Uy_skip=1
        end if

        end if

        else
        Uy_skip=Uy_skip+1
        if(Uy_skip>60) then
        Uy_skip=0
        end if

        end if

!
!  Same Procedure for Uz Values
!

        if(Uz_skip==0) then

        if((ABS(sduz(k)).GT.sdev*2).or.(ABS(deluz(k)).GT.duz).or.(ABS(duzw(k)).GT.duz)) then
        do jj=60,1,-1
        write(6,140) (-jj),hrmin(k-jj),sec(k-jj),uz(k-jj)
        end do

        write(6,'(A1)') ' '
        write(6,'(A12)') '          Uz'
        write(6,141) hrmin(k),sec(k),uz(k),sduz(k),deluz(k)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,140) (jj),hrmin(k+jj),sec(k+jj),uz(k+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response

        if(response=='Y'.or.response=='y') then
        Uz_skip=0
        write(6,'(A39)',advance="no") 'First and last index to be replaced    '
        read(5,*) ix1,ix2
        nbeg=uz(k+ix1-1)
        nend=uz(k+ix2+1)
        diff=nend-nbeg
        kk=0
        do jj=ix1,ix2,1
        kk=kk+1
        write(3,404) hrmin(k+jj),sec(k+jj),uz(k+jj)
   404  format('original'  I4,2x,F5.2,2x,'Uz= ',F6.2)

        if((ix1>0.and.ix2>0).or.(ix1<0.and.ix2<0)) then
        uz(k+jj)=nbeg+kk*(diff/ABS(ix1+ix2))
        else
        uz(k+jj)=nbeg+kk*(diff/(ABS(ix1)+ix2+2))
        end if

        write(3,405) hrmin(k+jj),sec(k+jj),uy(k+jj)
   405  format('new'  I4,2x,F5.2,2x,'Uz= ',F6.2,/)

        end do


        else
        Uz_skip=1
        end if

        end if

        else
        Uy_skip=Uy_skip+1
        if(Uy_skip>60) then
        Uy_skip=0
        end if

        end if

!
!  Same Procedure for Ts Values
!

        if(Ts_skip==0) then

        if((ABS(sdTs(k)).GT.sdev).or.(ABS(delTs(k)).GT.dTs).or.(ABS(dTsw(k)).GT.dTs)) then
        do jj=60,1,-1
        write(6,140) (-jj),hrmin(k-jj),sec(k-jj),Ts(k-jj)
        end do

        write(6,'(A1)') ' '
        write(6,'(A12)') '          Ts'
        write(6,141) hrmin(k),sec(k),Ts(k),sdTs(k),delTs(k)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,140) (jj),hrmin(k+jj),sec(k+jj),Ts(k+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response

        if(response=='Y'.or.response=='y') then
        Ts_skip=0
        write(6,'(A39)',advance="no") 'First and last index to be replaced    '
        read(5,*) ix1,ix2
        nbeg=Ts(k+ix1-1)
        nend=Ts(k+ix2+1)
        diff=nend-nbeg
        kk=0
        do jj=ix1,ix2,1
        kk=kk+1
        write(3,406) hrmin(k+jj),sec(k+jj),Ts(k+jj)
   406  format('original'  I4,2x,F5.2,2x,'Ts= ',F5.2)

        if((ix1>0.and.ix2>0).or.(ix1<0.and.ix2<0)) then
        Ts(k+jj)=nbeg+kk*(diff/ABS(ix1+ix2))
        else
        Ts(k+jj)=nbeg+kk*(diff/(ABS(ix1)+ix2+2))
        end if

        write(3,407) hrmin(k+jj),sec(k+jj),Ts(k+jj)
   407  format('new'  I4,2x,F5.2,2x,'Ts= ',F5.2,/)

        end do


        else
        Ts_skip=1
        end if

        end if

        else
        Ts_skip=Ts_skip+1
        if(Uy_skip>60) then
        Ts_skip=0
        end if

        end if


!
!  Same Procedure for rhov Values
!

        if(rhov_skip==0) then

        if((ABS(delrhov(k)).GT.drhov).or.(ABS(sdrhov(k)).GT.sdev)) then
        do jj=60,1,-1
        write(6,150) (-jj),hrmin(k-jj),sec(k-jj),rhov(k-jj)
  150   format(I3,2x,I4,2x,F5.2,4x,F6.2)
        end do

        write(6,'(A1)') ' '
        write(6,'(A14)') '          rhov'
        write(6,151) hrmin(k),sec(k),rhov(k),sdrhov(k),delrhov(k)
  151   format(' 0',4x,I4,2x,F5.2,4x,F6.2,4x,'SD = ',F6.1,2x,'Del=  ',F6.1)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,150) (jj),hrmin(k+jj),sec(k+jj),rhov(k+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response

        if(response=='Y'.or.response=='y') then
        rhov_skip=0
        write(6,'(A1)') ' '
        write(6,'(A39)',advance="no") 'First and last index to be replaced    '
        read(5,*) ix1,ix2
        nbeg=rhov(k+ix1-1)
        nend=rhov(k+ix2+1)
        del=ix2-ix1
        diff=nend-nbeg
        kk=0
        do jj=ix1,ix2,1
        kk=kk+1
        write(3,408) hrmin(k+jj),sec(k+jj),rhov(k+jj)
   408  format('original'  I4,2x,F5.2,2x,'rhov= ',F5.2)

        if((ix1>0.and.ix2>0).or.(ix1<0.and.ix2<0)) then
        rhov(k+jj)=nbeg+kk*(diff/ABS(ix1+ix2))
        else
        rhov(k+jj)=nbeg+kk*(diff/(ABS(ix1)+ix2+2))
        end if

        write(3,409) hrmin(k+jj),sec(k+jj),rhov(k+jj)
   409  format('new'  I4,2x,F5.2,2x,'rhov= ',F5.2,/)

        end do


        else
        rhov_skip=1
        end if

        end if

        else
        rhov_skip=rhov_skip+1
        if(rhov_skip>60) then
        rhov_skip=0
        end if

        end if

!
!  Same Procedure for rhoc Values
!

        if(rhoc_skip==0) then

        if((ABS(sdrhoc(k)).GT.sdev).or.(ABS(delrhoc(k)).GT.drhoc).or.(ABS(drhocw(k)).GT.drhoc)) then
        do jj=60,1,-1
        write(6,160) (-jj),hrmin(k-jj),sec(k-jj),rhoc(k-jj)
 160    format(I3,2x,I4,2x,F5.2,4x,F6.1)
        end do

        write(6,'(A1)') ' '
        write(6,'(A14)') '          rhoc'
        write(6,161) hrmin(k),sec(k),rhoc(k),sdrhoc(k),delrhoc(k)
 161    format(' 0',4x,I4,2x,F5.2,4x,F6.1,4x,'SD = ',F6.1,2x,'Del=  ',F6.1)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,160)(jj),hrmin(k+jj),sec(k+jj),rhoc(k+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response

        if(response=='Y'.or.response=='y') then
        rhoc_skip=0
        write(6,'(A39)',advance="no") 'First and last index to be replaced    '
        read(5,*) ix1,ix2
        nbeg=rhoc(k+ix1-1)
        nend=rhoc(k+ix2+1)
        diff=nend-nbeg
        kk=0
        do jj=ix1,ix2,1
        kk=kk+1

        write(3,410) hrmin(k+jj),sec(k+jj),rhoc(k+jj)
   410  format('original'  I4,2x,F5.2,2x,'rhoc= ',F8.3)

        if((ix1>0.and.ix2>0).or.(ix1<0.and.ix2<0)) then
        rhoc(k+jj)=nbeg+kk*(diff/ABS(ix1+ix2))
        else
        rhoc(k+jj)=nbeg+kk*(diff/(ABS(ix1)+ix2+2))
        end if

        write(3,411) hrmin(k+jj),sec(k+jj),rhov(k+jj)
   411  format('new'  I4,2x,F5.2,2x,'rhoc= ',F7.3)

        end do
        

        else
        rhoc_skip=1
        end if

        end if

        else
        rhoc_skip=rhoc_skip+1
        if(rhoc_skip>60) then
        rhoc_skip=0
        end if

        end if

        end do

!
!  Zero Out the Summation Terms in Preparation for the Next Hour
!

        do i=1,rn,1
        sumux(i)=0.0
        sumuy(i)=0.0
        sumuz(i)=0.0
        sumTs(i)=0.0
        sumrhov(i)=0.0
        sumrhoc(i)=0.0
        sumux2(i)=0.0
        sumuy2(i)=0.0
        sumuz2(i)=0.0
        sumTs2(i)=0.0
        sumrhov2(i)=0.0
        sumrhoc2(i)=0.0
        end do

        end do

      
!
!  Write Out the New Data for the Hour
!


        do i=1,n,1
        if(finish==0) then
        write(2,200) year(i),doy(i),hrmin(i),sec(i),ux(i),uy(i),uz(i),Ts(i),diag_sonic(i),  &
                        rhoc(i),rhov(i),diag_irga(i),rhoc_irga(i),T1_irga(i),T2_irga(i),  &
                        P(i),signalc(i),signalv(i)
  200   format(I4,2x,I3,2x,I4,2x,F5.2,2x,3(F9.5,2x),F8.5,2x,I6,2x,F9.4,2x,F9.6,2x,I6,2x,  &
                F9.4,2x,F10.4,2x,F8.4,2x,F8.5,2x,F6.4,2x,F6.4)
        else
        write(4,200) year(i),doy(i),hrmin(i),sec(i),ux(i),uy(i),uz(i),Ts(i),diag_sonic(i),  &
                        rhoc(i),rhov(i),diag_irga(i),rhoc_irga(i),T1_irga(i),T2_irga(i),  &
                        P(i),signalc(i),signalv(i)
        end if
        end do
        
        if(finish==2) then
        go to 99
        end if
                
        i=1
        nrecord=0
        go to 10

        end do

 92     finish=finish+1

        if(finish==1) then
        rewind(2)
        stat=0
        i=1
        go to 10
        end if

        if(finish==2) then
        go to 99
        end if
        
  99    end

  
