!  Program to Find Spikes in the Time Series Data for Eagle Lake System
!
!  Updated 18 January 2019 to add new spike detection parameters
!  Updated 6 Feb 2019 to significantly increase delta rhov

        implicit none

        character (len=25) dfile !outfile1,infile,outfile2
        character (len=6) site
        character (len=3) cdoy
        character (len=1) response
        character(len=1), parameter :: c_esc = achar(27)
        character(len=*), parameter :: c_red = '[31m'
        character(len=*), parameter :: c_bold = '[1m'
        character(len=*), parameter :: c_reset = '[0m'
        character(*), parameter :: indir='/Users/miksch/Thesis_Files/Processed/Eagle_Lake/EL_16/fort_v02/'
        character(*), parameter :: outdir='/Users/miksch/Thesis_Files/Processed/Eagle_Lake/EL_16/fort_v03/'
        character(*), parameter :: d_dir='/Users/miksch/Thesis_Files/Processed/Eagle_Lake/EL_16/despike_vals/'
        character(*), parameter :: tmpfile='temp.dat'
        

        integer, parameter :: dp=kind(1.0d0)
        integer, Parameter :: Medium=Selected_INT_Kind(6)
        integer, Parameter :: Long=Selected_INT_Kind(9)
        integer iday,unr       
        integer(kind=Medium) ::stat,nw1,nw2,nw,i,j,k,n,jj,day,year(80000),doy(80000),hrmin(80000),hr,finish,oldhr
        integer(kind=Medium) ::  nrecord,ix1,ix2,del,kk
        integer(Kind=Long) d1(80000)
	
        real sdev,rn,nr,sec(80000),ux(80000),uy(80000),uz(80000)
        real Ts(80000),rhov(80000),rhoc(80000),P(80000),agc(80000)
        real sdux(80000),sduy(80000),sduz(80000),sdTs(80000),sdrhov(80000),sdrhoc(80000)
        real duxw(80000),duyw(80000),duzw(80000),dTsw(80000),drhovw(80000),drhocw(80000)
        real dux,duy,duz,dTs,drhov,drhoc,nbeg,nend,diff,sdev_hi,sdev_low,del_hi,del_low

        real(kind=dp) ::  sumux,sumuy,sumuz,sumTs,sumrhov,sumrhoc
        real(kind=dp) ::  sumux2,sumuy2,sumuz2,sumTs2,sumrhov2,sumrhoc2
        real(kind=dp) ::  uxavg,uyavg,uzavg,Tsavg,rhovavg,rhocavg
        real(kind=dp) ::  stdevux,stdevuy,stdevuz,stdevTs,stdevrhov,stdevrhoc
       
        write (6,'(A12)') 'Site and DOY'
        read (5,*) site,day
        write(cdoy,"(I3)") day

        !write (6,'(A22)') 'Name of the input file'
        !read (5,*) infile
        !open(1,file=infile)

        !Open
        open(1,file=indir//"el16"//cdoy//"_v02.csv")

        !write (6,'(/,A53)') 'Name of the intermediate file made using first window'
        !read (5,*) outfile1

        !write (6,'(/,A22)') 'Name of the final file'
        !read (5,*) outfile2
        open(2,file=tmpfile,action='READWRITE')
        open(4,file=outdir//"el16"//cdoy//"_v03.csv",action='WRITE',status='unknown')


        write(dfile,*)("despike_"//cdoy//".dat")
        open(3,file=d_dir//dfile)

        write (6,'(/,A61)') 'Size of the 1st and 2nd Window & Critical Standard Deviation Value'
        read (5,*) nw1,nw2,sdev
	
!
!  Define Critical Values of the Difference of Instantaneous Value From the Window Average for Each Variable.
!  This is a Critical and Somewhat Subjective Step.
!

!  Ux in m/s
	dux=8.0
	
!  Uy in m/s
	duy=8.0

!  Uz in m/s
	duz=5.0

!  Tsonic in C
	dTs=5.0

!  rhov in gm/m3
  drhov=9.0

!  rhoc in mg/m3
	drhoc=160.0


!  Upper and lower coefficients for the if statements
        sdev_hi=1.5
        sdev_low=.6
        
        del_hi=1.6
        del_low=.3
        
!
!  Zero Out the Summation Terms
!
         

        sumux=0.0
        sumuy=0.0
        sumuz=0.0
        sumTs=0.0
        sumrhov=0.0
        sumrhoc=0.0
        sumux2=0.0
        sumuy2=0.0
        sumuz2=0.0
        sumTs2=0.0
        sumrhov2=0.0
        sumrhoc2=0.0

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
         go to 99
         end if

        if(finish==0) then
        
        unr=1
        read (1,*,iostat=stat,end=92) year(i),doy(i),hrmin(i),sec(i),ux(i),uy(i),uz(i),Ts(i),  &
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
!  Crossed into New Hour. Back Up One Record, So Next Record Read is the First of the New Hour.
!
        if((hr.NE.oldhr).or.(doy(i)>iday)) then
        backspace(unit=unr)
        oldhr=hr
        go to 90
        end if
        
        if((hrmin(i)==2359).and.(sec(i)==59.95)) then
        hr=24
        i=i+1
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
             
        do j=1,(n-nw),nw

        do i=j,(j+(nw-1)),1
  
        sumux=sumux+ux(i)
        sumuy=sumuy+uy(i)
        sumuz=sumuz+uz(i)
        sumTs=sumTs+Ts(i)
        sumrhov=sumrhov+rhov(i)
        sumrhoc=sumrhoc+rhoc(i)

        sumux2=sumux2+ux(i)*ux(i)
        sumuy2=sumuy2+uy(i)*uy(i)
        sumuz2=sumuz2+uz(i)*uz(i)
        sumTs2=sumTs2+Ts(i)*Ts(i)
        sumrhov2=sumrhov2+rhov(i)*rhov(i)
        sumrhoc2=sumrhoc2+rhoc(i)*rhoc(i)

	    
        end do
	 
!
!  Calculate Standard Deviations in the Window
!
        uxavg=sumux/nr
        uyavg=sumuy/nr
        uzavg=sumuz/nr
        Tsavg=sumTs/nr
        rhovavg=sumrhov/nr
        rhocavg=sumrhoc/nr

        stdevux=sqrt((sumux2-(sumux**2.0)/nr)/(nr-1))
        stdevuy=sqrt((sumuy2-(sumuy**2.0)/nr)/(nr-1))
        stdevuz=sqrt((sumuz2-(sumuz**2.0)/nr)/(nr-1))
        stdevTs=sqrt((sumTs2-(sumTs**2.0)/nr)/(nr-1))
        stdevrhov=sqrt((sumrhov2-(sumrhov**2.0)/nr)/(nr-1))
        stdevrhoc=sqrt((sumrhoc2-(sumrhoc**2.0)/nr)/(nr-1))

        do k=j,(j+(nw-1)),1

!
!  Determine Number of Standard Deviations Away From Window Average
!

        sdux(k)=ABS(ux(k)-uxavg)/stdevux
        sduy(k)=ABS(uy(k)-uyavg)/stdevuy
        sduz(k)=ABS(uz(k)-uzavg)/stdevuz
        sdTs(k)=ABS(Ts(k)-Tsavg)/stdevTs
        sdrhov(k)=ABS(rhov(k)-rhovavg)/stdevrhov
        sdrhoc(k)=ABS(rhoc(k)-rhocavg)/stdevrhoc
                  
!
!  Calculate Difference From Instantaneous Values From Window Average
!

        duxw(k)=ABS(ux(k)-Uxavg)
        duyw(k)=ABS(uy(k)-Uyavg)
        duzw(k)=ABS(uz(k)-Uzavg)
        dTsw(k)=ABS(Ts(k)-Tsavg)
        drhovw(k)=ABS(rhov(k)-rhovavg)
        drhocw(k)=ABS(rhoc(k)-rhocavg)

!
!  For Each Variable, Check if Values Exceed Standard Deviations and Absolute Difference Limits
!  If They Exceed Criteria, Write 60 Earlier Values, Flagged Value, Then 60 Later Values
!

!
!  Procedure for Ux Values
!


!
!  If Have Not Already Approved the Next Set of Values, Check the Next Value in the Window
!

        if(((ABS(duxw(k)).GT.dux).and.(ABS(sdux(k)).GT.(sdev*sdev_low))).or. &
          &((ABS(sdux(k)).GT.sdev).and.(ABS(duxw(k)).GT.(dux*del_low))).or. &
          &(ABS(duxw(k)).GT.(dux*del_hi)).or. &
          &(ABS(sdux(k)).GT.(sdev*sdev_hi))) then
          
        do jj=60,1,-1
        write(6,140) (-jj),hrmin(k-jj),sec(k-jj),ux(k-jj)
  140   format(I3,2x,I4,2x,F5.2,4x,F6.2)
        end do

        write(6,'(A1)') ' '
        write(6,141) c_esc,c_red,c_esc,c_bold,c_esc,hrmin(k),sec(k),ux(k),sdux(k),duxw(k),c_esc,c_reset
  141   format(a1,a4,a1,a3,a1,'   0',2x,I4,2x,F5.2,4x,F6.2,3x,'Ux',4x,'SD = ',F4.1,2x,'Del=  ',F4.1,a1,a3)
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

        write(6,'(A39)') 'First and last index to be replaced    '
        read(5,*) ix1,ix2
        nbeg=ux(k+ix1-1)
        nend=ux(k+ix2+1)
        diff=nend-nbeg
        kk=0

        do jj=ix1,ix2,1
        kk=kk+1

        write(3,400) hrmin(k+jj),sec(k+jj),ux(k+jj)
  400   format('original'  I4,2x,F5.2,2x,'Ux= ',F6.2)


!
!  Linear Interpolation Used to Replace Values
!

        if((ix1>0.and.ix2>0).or.(ix1<0.and.ix2<0)) then
        ux(k+jj)=nbeg+kk*(diff/ABS(ix1+ix2))
        else
        ux(k+jj)=nbeg+kk*(diff/(ABS(ix1)+ix2+2))
        end if

        write(3,401) hrmin(k+jj),sec(k+jj),ux(k+jj)
  401   format('new'  I4,2x,F5.2,2x,'Ux= ',F6.2,/)

        end do

!
!  If the Set of Displayed Values Was Good, Keep Skipping Until End of Future Diplay Reached
!

        else

        end if
        end if
        

        


!
!  Procedure for Uy Values
!


        if(((ABS(duyw(k)).GT.duy).and.(ABS(sduy(k)).GT.(sdev*sdev_low))).or. &
          &((ABS(sduy(k)).GT.sdev).and.(ABS(duyw(k)).GT.(duy*del_low))).or. &
          &(ABS(duyw(k)).GT.(duy*del_hi)).or. &
          &(ABS(sduy(k)).GT.(sdev*sdev_hi))) then
          
        do jj=60,1,-1
        write(6,140) (-jj),hrmin(k-jj),sec(k-jj),uy(k-jj)
        end do

        write(6,'(A1)') ' '
        write(6,142) c_esc,c_red,c_esc,c_bold,c_esc,hrmin(k),sec(k),uy(k),sduy(k),duyw(k),c_esc,c_reset
  142   format(a1,a4,a1,a3,a1,'   0',2x,I4,2x,F5.2,4x,F6.2,3x,'Uy',4x,'SD = ',F4.1,2x,'Del=  ',F4.1,a1,a3)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,140) (jj),hrmin(k+jj),sec(k+jj),uy(k+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response


        if(response=='Y'.or.response=='y') then

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


        end if
        
        end if
        
 

!
!  Same Procedure for Uz Values
!

        if(((ABS(duzw(k)).GT.duz).and.(ABS(sduz(k)).GT.(sdev*sdev_low))).or. &
          &((ABS(sduz(k)).GT.sdev).and.(ABS(duzw(k)).GT.(duz*del_low))).or. &
          &(ABS(duzw(k)).GT.(duz*del_hi)).or. &
          &(ABS(sduz(k)).GT.(sdev*sdev_hi))) then
          
        do jj=60,1,-1
        write(6,140) (-jj),hrmin(k-jj),sec(k-jj),uz(k-jj)
        end do

        write(6,'(A1)') ' '
        write(6,143) c_esc,c_red,c_esc,c_bold,c_esc,hrmin(k),sec(k),uz(k),sduz(k),duzw(k),c_esc,c_reset
  143   format(a1,a4,a1,a3,a1,'   0',2x,I4,2x,F5.2,4x,F6.2,3x,'Uz',4x,'SD = ',F4.1,2x,'Del=  ',F4.1,a1,a3)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,140) (jj),hrmin(k+jj),sec(k+jj),uz(k+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response

        if(response=='Y'.or.response=='y') then

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


        end if
        
        end if
        



!
!  Same Procedure for Ts Values
!
        
        if(((ABS(dTsw(k)).GT.dTs).and.(ABS(sdTs(k)).GT.(sdev*sdev_low))).or. &
          &((ABS(sdTs(k)).GT.sdev).and.(ABS(dTsw(k)).GT.(dTs*del_low))).or. &
          &(ABS(dTsw(k)).GT.(dTs*del_hi)).or. &
          &(ABS(sdTs(k)).GT.(sdev*sdev_hi))) then
          
        do jj=60,1,-1
        write(6,140) (-jj),hrmin(k-jj),sec(k-jj),Ts(k-jj)
        end do

        write(6,'(A1)') ' '
        write(6,144) c_esc,c_red,c_esc,c_bold,c_esc,hrmin(k),sec(k),Ts(k),sdTs(k),dTsw(k),c_esc,c_reset
   144  format(a1,a4,a1,a3,a1,'   0',2x,I4,2x,F5.2,4x,F6.2,3x,'Ts',4x,'SD = ',F4.1,2x,'Del=  ',F4.1,a1,a3)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,140) (jj),hrmin(k+jj),sec(k+jj),Ts(k+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response

        if(response=='Y'.or.response=='y') then

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


        end if
        
        end if
        


!
!  Same Procedure for rhov Values
!
        
        if(((ABS(drhovw(k)).GT.drhov).and.(ABS(sdrhov(k)).GT.(sdev*sdev_low))).or. &
          &((ABS(sdrhov(k)).GT.sdev).and.(ABS(drhovw(k)).GT.(drhov*del_low))).or. &
          &(ABS(drhovw(k)).GT.(drhov*del_hi)).or. &
          &(ABS(sdrhov(k)).GT.(sdev*sdev_hi))) then
          
        do jj=60,1,-1
        write(6,150) (-jj),hrmin(k-jj),sec(k-jj),rhov(k-jj)
   150  format(I3,2x,I4,2x,F5.2,4x,F6.2)
        end do

        write(6,'(A1)') ' '
        write(6,145) c_esc,c_red,c_esc,c_bold,c_esc,hrmin(k),sec(k),rhov(k),sdrhov(k),drhovw(k),c_esc,c_reset
   145  format(a1,a4,a1,a3,a1,'   0',2x,I4,2x,F5.2,4x,F6.2,3x,'rhov',4x,'SD = ',F4.1,2x,'Del=  ',F4.1,a1,a3)
        write(6,'(A1)') ' '

        do jj=1,60
        write(6,150) (jj),hrmin(k+jj),sec(k+jj),rhov(k+jj)
        end do

        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
        read(5,*) response

        if(response=='Y'.or.response=='y') then

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


        end if
        
        end if
        


!
!  Same Procedure for rhoc Values
!

!        if(rhoc_skip==0) then
!
!        if((ABS(sdrhoc(k)).GT.sdev).or.(ABS(delrhoc(k)).GT.drhoc).or.(ABS(drhocw(k)).GT.drhoc)) then
!        do jj=60,1,-1
!        write(6,160) (-jj),hrmin(k-jj),sec(k-jj),rhoc(k-jj)
!  160   format(I3,2x,I4,2x,F5.2,4x,F6.1)
!        end do
!
!        write(6,'(A1)') ' '
!        write(6,146) c_esc,c_red,c_esc,c_bold,c_esc,hrmin(k),sec(k),rhoc(k),sdrhoc(k),delrhoc(k),c_esc,c_reset
!  146   format(a1,a4,a1,a3,a1,'   0',2x,I4,2x,F5.2,4x,F6.2,3x,'rhoc',4x,'SD = ',F4.1,2x,'Del=  ',F4.1,a1,a3)
!        write(6,'(A1)') ' '
!
!        do jj=1,60
!        write(6,160)(jj),hrmin(k+jj),sec(k+jj),rhoc(k+jj)
!        end do
!
!        write(6,'(A30)', advance="no") 'Problem?  Enter Y or N    '
!        read(5,*) response
!
!        if(response=='Y'.or.response=='y') then
!        rhoc_skip=0
!        write(6,'(A39)',advance="no") 'First and last index to be replaced    '
!        read(5,*) ix1,ix2
!        nbeg=rhoc(k+ix1-1)
!        nend=rhoc(k+ix2+1)
!        diff=nend-nbeg
!        kk=0
!        do jj=ix1,ix2,1
!        kk=kk+1
!
!        write(3,410) hrmin(k+jj),sec(k+jj),rhoc(k+jj)
!   410  format('original'  I4,2x,F5.2,2x,'rhoc= ',F8.3)
!
!        if((ix1>0.and.ix2>0).or.(ix1<0.and.ix2<0)) then
!        rhoc(k+jj)=nbeg+kk*(diff/ABS(ix1+ix2))
!        else
!        rhoc(k+jj)=nbeg+kk*(diff/(ABS(ix1)+ix2+2))
!        end if
!
!        write(3,411) hrmin(k+jj),sec(k+jj),rhov(k+jj)
!   411  format('new'  I4,2x,F5.2,2x,'rhoc= ',F7.3)
!
!        end do
!
!        else
!        rhoc_skip=rhoc_skip+1
!        if(rhoc_skip>60) then
!        rhoc_skip=0
!        end if
!        end if
!        
!        end if
!        
!        end if

        end do

!
!  Zero Out the Summation Terms in Preparation for the Next Hour
!


        sumux=0.0
        sumuy=0.0
        sumuz=0.0
        sumTs=0.0
        sumrhov=0.0
        sumrhoc=0.0
        sumux2=0.0
        sumuy2=0.0
        sumuz2=0.0
        sumTs2=0.0
        sumrhov2=0.0
        sumrhoc2=0.0


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

  
