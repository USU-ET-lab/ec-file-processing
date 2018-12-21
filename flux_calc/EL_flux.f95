!  Program to Calculate Fluxes for Eagle Lake
!
!  Rotation Error Corrected  14 June 2017
!  Revised  5 Novemeber 2017 to include statistics on instantaneous uz*rhov flux
!  Revised  4 December 2018 into Fortran 95
!

    implicit none

	character(len=24) in,out,dfile
	character(len=4) site
	character(len=3) cdoy

        integer, parameter :: dp=kind(1.0d0)
        integer, Parameter :: Long=Selected_INT_Kind(9)
        integer lag,nf,uzTs_shift,uzrhov_shift,zT,zrhov,stat,i,j,n,year,doy,hrmin,hr,oldhr
	integer(kind=long) :: diag_sonic
	real sec,ux(80000),uy(80000),uz(80000),Ts(80000)
	real rhov(80000),rhoc(80000),q(80000),P(80000)
	real rn,rnt,b,dist,phi,path,D,agc
	real(kind=dp) :: Td,varTs,varTa
	real(kind=dp) :: sumux,sumuy,sumuz,sumT,sumT2,sumrhov,sump,sumuxn
	real(kind=dp) :: sumrhovn,sumuyn,sumuzn,sumx,sumy,sumz,sumv,sumTs,sumTsn,sumqn,sumTa,sumTa2
	real(kind=dp) :: sumx2,sumy2,sumz2,ux2bar,uy2bar,uz2bar,q2bar,sume,sumqm,sumq,sumq2,sumqd2,sumTs2,sumc2
	real(kind=dp) :: sumuxux,sumuxuy,sumuxuz,sumuyuy,sumuyuz,sumuzuz,sumuxTs,sumuyTs,sumuzTs,sumuxTsn,sumuyTsn
	real(kind=dp) :: sumuzTsn,sumuxrhov,sumuyrhov,sumuzrhov,sumuxrhovn,sumuyrhovn,sumuzrhovn
	real(kind=dp) :: sumuxq,sumuyq,sumuzq,sumuxqn,sumuyqn,sumuzqn
	real(kind=dp) :: q_Ts,covux_Ts(100),covuy_Ts(100),covuz_Ts(100),uxq(100)
	real(kind=dp) :: covux_Tsn(100),covuy_Tsn(100),covuz_Tsn(100),sumqTsn,sumqTs,covq_Tsn(100),covq_Ts(100),qTs(100)
	real(kind=dp) :: covux_rhovn(100),covuy_rhovn(100),covuz_rhovn(100)
	real(kind=dp) :: covux_rhov(100),covuy_rhov(100),covuz_rhov(100),uyq(100),uzq(100)
	real(kind=dp) :: covux_qn(100),covuy_qn(100),covuz_qn(100),covux_q(100),covuy_q(100),covuz_q(100)
	real(kind=dp) :: uxTs(100),uyTs(100),uzTs(100),uxrhov(100),uyrhov(100),uzrhov(100)
	real(kind=dp) :: ux_ux,ux_uy,ux_uz,uy_uy,uy_uz,uz_uz,ux_Ts,uy_Ts,uz_Ts,uz_Tsr,uz_Ta,ux_rhov,uy_rhov,uz_rhov
 	real(kind=dp) :: uz_rhovr,uz_Tan,ux_q,uy_q,uz_q,uz_Un,uxavg,uyavg,uzavg,Tsavg,rhovavg,Pavg
	real(kind=dp) :: Uavg,ustar,ustarn,cosnu,sinnu,sintheta,costheta,V,wind_dir,sonic_dir,wind_compass
	real(kind=dp) :: taob,taoeKH20,taoeTs,taoemomentum,rmomentum
	real(kind=dp) :: pmomentum,z,L,alfa,eta,pi,pTs,nx,fx,pKH20,rTs,rKH20
	real(kind=dp) :: sumuxr,sumuyr,sumuzr,uxr(80000),uyr(80000),uzr(80000)
	real(kind=dp) :: uxravg,uyravg,uzravg,sumuxr2,sumuyr2,sumuzr2
	real(kind=dp) :: stdTa,stdux,stduy,stduz,stduxr,stduyr,stduzr,stdq
	real(kind=dp) :: g0,g1,g2,g3,g4,g5,g6,g7,c0,c1,c2,c3,d0,d1,d2,d3,lnes,lne
	real(kind=dp) :: Runiv,Rd,Rv,R,e,Ta,Tsair,eavg,eair,es,qs,qavg,rhod,rho,cpd,cp
	real(kind=dp) :: Lv,Co,Mo,H,LE,zeta,Tq1,Tq2,qs1,qs2,stdTa1,s,gamma,Tavg,ET
	real(kind=dp) :: flux(80000),sumflux,avgflux,stdflux,varflux,skewflux,kurtflux,devs,sumdev,sqdev
	real(kind=dp) :: sumrhov1,sumuz1,avgrhov,avguz

       
	Runiv=8.314
	Rd=287.05
 	Rv=461.51
	cpd=1005.0
	pi=3.14159
	Co=0.21
	Mo=0.032
	stat=0
	ET=0.0

!
!  Input the Separation Distance in m Between the CSAT and IRGA in m
!

	dist=0.11

!
!  Specify the Direction of the CSAT and Height of Sensors
!
 
	sonic_dir=221
	z=2.78

!
!  Coefficients for Saturation Vapor Pressure Equation
!

	g0=-2.8365744E03
	g1=-6.028076559E03
	g2=1.954263612E01
	g3=-2.737830188E-02
	g4=1.6261698E-05
	g5=7.0229056E-10
	g6=-1.8680009E-13
	g7=2.7150305

!
!  Coefficients for Dew Point Equation
!

	c0=2.0798233E02
	c1=-2.0156028E01
	c2=4.6778925E-01
	c3=-9.2288067E-06

	d0=1.0
	d1=-1.3319669E-01
	d2=5.6577518E-03
	d3=-7.5172865E-05


!
!  Zero Out Variables For Sums
!

	sumx=0.0
	sumy=0.0
	sumz=0.0
	sumT=0.0
        sumTa=0.0
        sumTs=0.0
        sumv=0.0
	sumTa=0.0
	sumrhov=0.0
	sump=0.0
	sume=0.0
	sumqm=0.0
	sumx2=0.0
	sumy2=0.0
	sumz2=0.0
	sumq=0.0
	sumT2=0.0
	sumTs2=0.0
 	sumq2=0.0
	sumqd2=0.0
	sumc2=0.0

        sumux=0.0
	sumuy=0.0
	sumuz=0.0

	sumuxn=0.0
	sumuyn=0.0
	sumuzn=0.0
	sumTsn=0.0
        sumrhovn=0.0
	sumqn=0.0

	sumuxr=0.0
	sumuyr=0.0
	sumuzr=0.0
	sumuxr2=0.0
	sumuyr2=0.0
	sumuzr2=0.0

	sumuxux=0.0
	sumuxuy=0.0
	sumuxuz=0.0
	sumuyuy=0.0
	sumuyuz=0.0
	sumuzuz=0.0

	sumuxTsn=0.0
	sumuyTsn=0.0
	sumuzTsn=0.0
	sumuxrhovn=0.0
	sumuyrhovn=0.0
	sumuzrhov=0.0
	sumuxqn=0.0
	sumuyqn=0.0
	sumuzqn=0.0
	sumqTsn=0.0

	sumuxTs=0.0
	sumuyTs=0.0
	sumuzTs=0.0
	sumuxrhov=0.0
	sumuyrhov=0.0
	sumuzrhov=0.0

	sumuxq=0.0
	sumuyq=0.0
	sumuzq=0.0
	sumqTs=0.0

	write (6,'(/,A12)') 'Site and DOY '
	read (5,*) site,doy

	write (6,'(/,A22)') 'Name of the input file'
	read (5,*) in
	open(1,file=in)

	write (6,'(/,A23)') 'Name of the output file'
	read (5,*) out
	open(2,file=out)
	
	write(cdoy,"(I3)") doy
	write(dfile,"(a12)")("d"//site//cdoy//".dat")
	open(3,file=dfile)
	
	write (6,'(/,A39)') 'Number of Scans to Shift for Covariance'
	read (5,*) lag

	write(3,121) site,doy
 121	format(A4,2x,'DOY',1x,I3,/)

	write (2,100)
 100	format('YEAR',1x,'DOY',1x,'hrmin',3x,'Ta',5x,'Td',6x,'D',5x,'stdTa',4x,'Dir',5x,'V',3x,'ustar',5x, &
	'z/L',6x,'H',5x,'LE',6x,'rhov*Uz_Avg',3x,'rhov*Uz_Std',3x,'rhov*Uz_Var',3x,'rhov*Uz_Skew',2x,  &
        'rhov*Uz_Kurt',3x,'Records')

!
!  Read All the Data Into Arrays and Determine When End of the Hour is Reached
!

	oldhr=0
	i=1
	
	do while (stat==0)

  10	if(stat/=0) then
	go to 99
	end if

	read (1,*,iostat=stat,end=90) year,doy,hrmin,sec,ux(i),uy(i),uz(i),Ts(i),diag_sonic, &
	            rhoc(i),rhov(i),P(i),agc

 	hr=INT(hrmin/100.0)

        if(i==1) then
        oldhr=hr
        end if
	i=i+1

	if(hr/=oldhr) then
	i=i-1
	oldhr=hr
	hr=hr*100
	backspace(unit=1)
	go to 90
	end if
	
	if((hrmin==2359).and.(sec==59.95)) then
	hr=2400
        go to 90
        end if

	oldhr=hr
	go to 10
   
  90	n=i-1
	rn= float (n)

	write(3,158) hr
 158	format('Hour = ',I4)

	write(6,'(/,A15,2x,I4)') 'Processing Hour',hr

!
!  Sum the Variables and Products Required for Later Calculations
!

	do i=1,n,1

	Ts(i)=Ts(i)+273.16
	rhov(i)=rhov(i)/1000.0
	P(i)=P(i)*1000.0

	sumx=sumx+ux(i)
	sumy=sumy+uy(i)
	sumz=sumz+uz(i)
	sumT=sumT+Ts(i)
	sumv=sumv+rhov(i)
	sump=sump+P(i)
	sumT2=sumT2+Ts(i)**2.0

	sumuxux=sumuxux+ux(i)*ux(i)
	sumuxuy=sumuxuy+ux(i)*uy(i)
	sumuxuz=sumuxuz+ux(i)*uz(i)
	sumuyuy=sumuyuy+uy(i)*uy(i)
	sumuyuz=sumuyuz+uy(i)*uz(i)
	sumuzuz=sumuzuz+uz(i)*uz(i)


!
!  Humidity Values Determined 
!
	e=rhov(i)*Rv*Ts(i)
	q(i)=(0.622*e)/(P(i)-0.378*e)
	Ta=Ts(i)/(1+0.51*q(i))
	lnes=g0*Ta**(-2)+g1*Ta**(-1)+g2+g3*Ta+g4*Ta**2.0+g5*Ta**3.0+g6*Ta**4+g7*ALOG(Ta)
	es=EXP(lnes)
	qs=(0.622*es)/(P(i)-0.378*es)
	sumTa=sumTa+Ta
	sumTa2=sumTa2+(Ta**2.0)
	sume=sume+e
	sumqm=sumqm+q(i)
	sumq2=sumq2+q(i)**2.0

	end do

!
!  Calculate Averages and Standard Deviations for Velocities and Humidity  
!

	uxavg=sumx/rn
	uyavg=sumy/rn
	uzavg=sumz/rn
	Tsavg=sumT/rn
	Tavg=sumTa/rn
	Pavg=sump/rn
	eair=sume/rn
	eair=eair*1000.0
	rhovavg=sumv/rn
	eavg=sume/rn
	qavg=sumqm/rn
	R=qavg*Rv+(1-qavg)*Rd

	stdux=sqrt((sumuxux-(sumx**2.0)/rn)/(rn-1))
	stduy=sqrt((sumuyuy-(sumy**2.0)/rn)/(rn-1))
	stduz=sqrt((sumuzuz-(sumz**2.0)/rn)/(rn-1))
	stdTa=sqrt((sumTa2-(sumTa**2.0)/rn)/(rn-1))
	stdq=sqrt((sumq2-(sumqm**2.0)/rn)/(rn-1))

!
!  Uncorrected Variance of Sonic Temperature Which is Used Later
!

	varTs=(sumT2-(sumT**2.0)/rn)/(rn-1)

!
!  Find Average of Square of Deviations From the Mean for Velocity Components and Humidity
!

	do i=1,n,1

	sumx2=sumx2+(ux(i)-uxavg)**2
	sumy2=sumy2+(uy(i)-uyavg)**2
	sumz2=sumz2+(uz(i)-uzavg)**2
	sumqd2=sumqd2+(q(i)-qavg)**2

	end do

	ux2bar=sumx2/rn
	uy2bar=sumy2/rn
	uz2bar=sumz2/rn
	q2bar=sumqd2/rn

!
!  Calculate the Correct Average Values for Some Key Variables
!

	cp=cpd*(1.0+0.84*qavg)
	rhod=(Pavg-eavg)/(Rd*Ta)
	rho=rhod+rhovavg	

!
!  Shift the Wind and Scalar Arrays in Both Directions and Calculate Covariances for Each Lag
!

	do j=1,lag+1,1
	
	do i=1,n-j+1,1

	sumuxn=sumuxn+ux(i+j-1)
	sumuyn=sumuyn+uy(i+j-1)
	sumuzn=sumuzn+uz(i+j-1)
	sumTsn=sumTsn+Ts(i)
        sumrhovn=sumrhovn+rhov(i)
	sumqn=sumqn+q(i)
	
	sumux=sumux+ux(i)
	sumuy=sumuy+uy(i)
	sumuz=sumuz+uz(i)
	sumTs=sumTs+Ts(i+j-1)
        sumrhov=sumrhov+rhov(i+j-1)
	sumq=sumq+q(i+j-1)

	sumuxTsn=sumuxTsn+ux(i+j-1)*Ts(i)
	sumuyTsn=sumuyTsn+uy(i+j-1)*Ts(i)
	sumuzTsn=sumuzTsn+uz(i+j-1)*Ts(i)
	sumuxrhovn=sumuxrhovn+ux(i+j-1)*rhov(i)
	sumuyrhovn=sumuyrhovn+uy(i+j-1)*rhov(i)
	sumuzrhovn=sumuzrhovn+uz(i+j-1)*rhov(i)
	sumuxqn=sumuxqn+ux(i+j-1)*q(i)
	sumuyqn=sumuyqn+uy(i+j-1)*q(i)
	sumuzqn=sumuzqn+uz(i+j-1)*q(i)
	
	sumuxTs=sumuxTs+ux(i)*Ts(i+j-1)
	sumuyTs=sumuyTs+uy(i)*Ts(i+j-1)
	sumuzTs=sumuzTs+uz(i)*Ts(i+j-1)
	sumuxrhov=sumuxrhov+ux(i)*rhov(i+j-1)
	sumuyrhov=sumuyrhov+uy(i)*rhov(i+j-1)
	sumuzrhov=sumuzrhov+uz(i)*rhov(i+j-1)
	sumuxq=sumuxq+ux(i)*q(i+j-1)
	sumuyq=sumuyq+uy(i)*q(i+j-1)
	sumuzq=sumuzq+uz(i)*q(i+j-1)
	sumqTs=sumqTs+Ts(i)*q(i+j-1)
	end do

	rnt=rn-float(j)+1.0

	covux_Tsn(j)=(sumuxTsn-(sumuxn*sumTsn)/rnt)/(rnt-1)
	covuy_Tsn(j)=(sumuyTsn-(sumuyn*sumTsn)/rnt)/(rnt-1)
	covuz_Tsn(j)=(sumuzTsn-(sumuzn*sumTsn)/rnt)/(rnt-1)
	covux_rhovn(j)=(sumuxrhovn-(sumuxn*sumrhovn)/rnt)/(rnt-1)
	covuy_rhovn(j)=(sumuyrhovn-(sumuyn*sumrhovn)/rnt)/(rnt-1)
	covuz_rhovn(j)=(sumuzrhovn-(sumuzn*sumrhovn)/rnt)/(rnt-1)
	covux_qn(j)=(sumuxqn-(sumuxn*sumqn)/rnt)/(rnt-1)
	covuy_qn(j)=(sumuyqn-(sumuyn*sumqn)/rnt)/(rnt-1)
	covuz_qn(j)=(sumuzqn-(sumuzn*sumqn)/rnt)/(rnt-1)
	covq_Tsn(j)=(sumqTsn-(sumqn*sumTsn)/rnt)/(rnt-1)

	covux_Ts(j)=(sumuxTs-(sumux*sumTs)/rnt)/(rnt-1)
	covuy_Ts(j)=(sumuyTs-(sumuy*sumTs)/rnt)/(rnt-1)
	covuz_Ts(j)=(sumuzTs-(sumuz*sumTs)/rnt)/(rnt-1)
	covux_rhov(j)=(sumuxrhov-(sumux*sumrhov)/rnt)/(rnt-1)
	covuy_rhov(j)=(sumuyrhov-(sumuy*sumrhov)/rnt)/(rnt-1)
	covuz_rhov(j)=(sumuzrhov-(sumuz*sumrhov)/rnt)/(rnt-1)
	covux_q(j)=(sumuxq-(sumux*sumq)/rnt)/(rnt-1)
	covuy_q(j)=(sumuyq-(sumuy*sumq)/rnt)/(rnt-1)
	covuz_q(j)=(sumuzq-(sumuz*sumq)/rnt)/(rnt-1)
	covq_Ts(j)=(sumqTs-(sumq*sumTs)/rnt)/(rnt-1)

	sumuxn=0.0
	sumuyn=0.0
	sumuzn=0.0
	sumTsn=0.0
	sumrhovn=0.0
	sumqn=0.0

	sumux=0.0
	sumuy=0.0
	sumuz=0.0
	sumTs=0.0
	sumrhov=0.0
	sumq=0.0

	sumuxTsn=0.0
	sumuyTsn=0.0
	sumuzTsn=0.0
	sumuxrhovn=0.0
	sumuyrhovn=0.0
	sumuzrhovn=0.0
	sumuxqn=0.0
	sumuyqn=0.0
	sumuzqn=0.0
	sumqTsn=0.0
	
	sumuxTs=0.0
	sumuyTs=0.0
	sumuzTs=0.0
	sumuxrhov=0.0
	sumuyrhov=0.0
	sumuzrhov=0.0
	sumuxq=0.0
	sumuyq=0.0
	sumuzq=0.0
	sumqTs=0.0
	end do

!
!  Load Covariances From Lags in Each Direction Into Single Arrays
!

	do i=2,lag+1
 	uxTs(i)=covux_Tsn(i)
	uyTs(i)=covuy_Tsn(i)
	uzTs(i)=covuz_Tsn(i)
	uxrhov(i)=covux_rhovn(i)
	uyrhov(i)=covuy_rhovn(i)
	uzrhov(i)=covuz_rhovn(i)
	uxq(i)=covux_qn(i)
	uyq(i)=covuy_qn(i)
	uzq(i)=covuz_qn(i)
	qTs(i)=covq_Tsn(i)
	end do

	nf=2*lag+1

	do i=lag+2,nf
	uxTs(i)=covux_Ts(i-lag)
	uyTs(i)=covuy_Ts(i-lag)
	uzTs(i)=covuz_Ts(i-lag)
	uxrhov(i)=covux_rhov(i-lag)
	uyrhov(i)=covuy_rhov(i-lag)
	uzrhov(i)=covuz_rhov(i-lag)
	uxq(i)=covux_q(i-lag)
	uyq(i)=covuy_q(i-lag)
	uzq(i)=covuz_q(i-lag)
	qTs(i)=covq_Ts(i-lag)
	end do

!
!  Place the Original Covariances in the First Location of Each Array
!

	i=1

	uxTs(i)=covux_Ts(i)
	uyTs(i)=covuy_Ts(i)
	uzTs(i)=covuz_Ts(i)
	uxrhov(i)=covux_rhov(i)
	uyrhov(i)=covuy_rhov(i)
	uzrhov(i)=covuz_rhov(i)
	uxq(i)=covux_q(i)
	uyq(i)=covuy_q(i)
	uzq(i)=covuz_q(i)
	qTs(i)=covq_Ts(i)

	ux_Ts=uxTs(i)
	uy_Ts=uyTs(i)
	uz_Ts=uzTs(i)
	ux_rhov=uxrhov(i)
	uy_rhov=uyrhov(i)
	uz_rhov=uzrhov(i)
	ux_q=uxq(i)
	uy_q=uyq(i)
	uz_q=uzq(i)
	q_Ts=qTs(i)

	write(3,740) uz_Ts,uz_rhov
 740	format('Original Cov',10x,'Uz_Ts = ',F8.5,2x,'Uz_rhov = ',F11.8)
 	
!
!  Find the Maximum Covariance Values 
!

 	do i=1,nf

 	if (abs(uxTs(i))>abs(ux_Ts)) then
 	ux_Ts=uxTs(i)
 	end if

 	if (abs(uyTs(i))>abs(uy_Ts)) then
 	uy_Ts=uyTs(i)
 	end if

 	if (abs(uzTs(i))>abs(uz_Ts)) then
 	uz_Ts=uzTs(i)
 	uzTs_shift=i-1
 	end if

 	if (abs(uxrhov(i))>abs(ux_rhov)) then
 	ux_rhov=uxrhov(i)
 	end if

 	if (abs(uyrhov(i))>abs(uy_rhov)) then
 	uy_rhov=uyrhov(i)
 	end if
	
 	if (abs(uzrhov(i))>abs(uz_rhov)) then
 	uz_rhov=uzrhov(i)
 	uzrhov_shift=i-1
 	end if

	end do
	

!  Determine Shift Value for Each Maximum Vertical Covariance
!  Positive Means the Velocity Leads the Scalar
!  Negative Means the Scalar Leads the Velocity
!

	if(uzTs_shift>(lag+2)) then
	zT=i-(lag+2)
	else
	zT=-uzTs_shift
	end if
		
	if(uzrhov_shift>(lag+2)) then
	zrhov=i-(lag+2)
	else
	zrhov=-uzrhov_shift
	end if

	write(3,750) zT,zrhov,Uz_Ts,Uz_rhov
 750	format('Shift Values',1x,I3,1x,I3,2x,'Uz_Ts = ',F8.5,2x,'Uz_rhov = ',F11.8)
 
!
!  Calculate Covariances for Wind Components
!
		
	ux_ux=(sumuxux-(sumx*sumx)/rn)/(rn-1)
	ux_uy=(sumuxuy-(sumx*sumy)/rn)/(rn-1)
	ux_uz=(sumuxuz-(sumx*sumz)/rn)/(rn-1)
	uy_uy=(sumuyuy-(sumy*sumy)/rn)/(rn-1)
	uy_uz=(sumuyuz-(sumy*sumz)/rn)/(rn-1)
	uz_uz=(sumuzuz-(sumz*sumz)/rn)/(rn-1)

!
!  Traditional Coordinate Rotation
!

	eta=atan(uyavg/uxavg)
	cosnu=(uxavg/sqrt(uxavg**2.0+uyavg**2.0))
	sinnu=(uyavg/sqrt(uxavg**2.0+uyavg**2.0))
	sintheta=(uzavg/sqrt(uxavg**2.0+uyavg**2.0+uzavg**2.0))
	costheta=(sqrt(uxavg**2.0+uyavg**2.0)/sqrt(uxavg**2.0+uyavg**2.0+uzavg**2.0))

	sumuxr=0.0
	sumuyr=0.0
	sumuzr=0.0

!
!  Rotate the Velocity Values
!

	do i=1,n,1
	uxr(i)=ux(i)*costheta*cosnu+uy(i)*costheta*sinnu+uz(i)*sintheta
	uyr(i)=uy(i)*cosnu-ux(i)*sinnu
	uzr(i)=uz(i)*costheta-ux(i)*sintheta*cosnu-uy(i)*sintheta*sinnu
	sumuxr=sumuxr+uxr(i)
	sumuyr=sumuyr+uyr(i)
	sumuzr=sumuzr+uzr(i)
	sumuxr2=sumuxr2+uxr(i)**2
	sumuyr2=sumuyr2+uyr(i)**2.0
	sumuzr2=sumuzr2+uzr(i)**2.0
	end do

!
!  Calculate the Average and Standard Deviations of the Rotated Velocity Components
!
	uxravg=sumuxr/rn
	uyravg=sumuyr/rn
	uzravg=sumuzr/rn
	stduxr=sqrt((sumuxr2-(sumuxr**2.0)/rn)/(rn-1))
	stduyr=sqrt((sumuyr2-(sumuyr**2.0)/rn)/(rn-1))
	stduzr=sqrt((sumuzr2-(sumuzr**2.0)/rn)/(rn-1))

	Uavg=uxavg*costheta*cosnu+uyavg*costheta*sinnu+uzavg*sintheta

!
!  Correct Covariances for Coordinate Rotation
!

	uz_Tsr=uz_Ts*costheta-ux_Ts*sintheta*cosnu-uy_Ts*sintheta*sinnu

	if(abs(uz_Tsr)>=abs(uz_Ts)) then
	uz_Ts=uz_Tsr
	end if

        uz_rhovr=uz_rhov*costheta-ux_rhov*sintheta*cosnu-uy_rhov*sinnu*sintheta

	if(abs(uz_rhovr)>=abs(uz_rhov)) then
	uz_rhov=uz_rhovr
	end if

	ux_q=ux_q*costheta*cosnu+uy_q*costheta*sinnu+uz_q*sintheta
 	uy_q=uy_q*cosnu-ux_q*sinnu
	uz_q=uz_q*costheta-ux_q*sintheta*cosnu-uy_q*sinnu*sintheta

	ux_uz=ux_uz*cosnu*(costheta**2-sintheta**2)-2.0*ux_uy*sintheta*costheta*sinnu*cosnu+ &
	    uy_uz*sinnu*(costheta**2-sintheta**2)-ux2bar*sintheta*costheta*cosnu**2- &
	    uy2bar*sintheta*costheta*sinnu**2+uz2bar*sintheta*costheta
     
	uy_uz=uy_uz*costheta*cosnu-ux_uz*costheta*sinnu-ux_uy*sintheta*(cosnu**2-sinnu**2)+ &
	    ux2bar*sintheta*sinnu*cosnu-uy2bar*sintheta*sinnu*cosnu

	uz_Un=sqrt(ux_uz**2.0+uy_uz**2.0)
	ustar=sqrt(uz_Un)


	write(3,760) uz_Ts,uz_rhov
 760	format('After Rotation',8x,'Uz_Ts = ',F8.5,2x,'Uz_rhov = ',F11.8)


!
!  Calculate Value of Latent Heat of Vaporization
!

	Lv=(2500800.0-2366.8*(Tavg-273.16))

!
!  Calculate Variance of Air Temperature From Variance of Sonic Temperature
!

      
	varTa=varTs-1.02*Tsavg*q_Ts-(0.51**2.0)*q2bar*Tsavg**2.0
	stdTa1=SQRT(varTa)

	uz_Ta=uz_Ts-0.07*Lv*uz_rhov/(rho*cp)

!
!  Determine Saturation Vapor Pressure of the Air
!  Uses Highly Accurate Wexler's Equations Modified by Hardy
!

	lnes=g0*Tavg**(-2)+g1*Tavg**(-1)+g2+g3*Tavg+g4*Tavg**2.0+g5*Tavg**3.0+g6*Tavg**4+g7*ALOG(Tavg)
	es=EXP(lnes)
	lne=ALOG(eavg)
	Td=(c0+c1*lne+c2*lne**2.0+c3*lne**3.0)/(d0+d1*lne+d2*lne**2.0+d3*lne**3.0)
	D=es-eavg

	Tq1=Ta-1.0
	lnes=g0*Tq1**(-2)+g1*Tq1**(-1)+g2+g3*Tq1+g4*Tq1**2.0+g5*Tq1**3.0+g6*Tq1**4+g7*ALOG(Tq1)
	es=EXP(lnes)
	qs1=(0.622*es)/(Pavg-0.378*es) 
	Tq2=Ta+1.0 

	lnes=g0*Tq2**(-2)+g1*Tq2**(-1)+g2+g3*Tq2+g4*Tq2**2.0+g5*Tq2**3.0+g6*Tq2**4+g7*ALOG(Tq2)
	es=EXP(lnes)
	qs2=(0.622*es)/(Pavg-0.378*es)
	s=(qs2-qs1)/2.0
	 
!
!  Determine Wind Direction
!

	V=sqrt(uxavg**2+uyavg**2)
	wind_dir=atan(uyavg/uxavg)*180.0/pi
	if (uxavg < 0) then
	if (uyavg >= 0) then
	wind_dir=wind_dir+180.0
	else  
	wind_dir=wind_dir-180.0
	end if
	end if
	wind_compass=-1.0*wind_dir+sonic_dir
	if (wind_compass < 0 ) then
	wind_compass=wind_compass+360
	end if
	if (wind_compass > 360) then
        wind_compass=wind_compass-360
        end if

!
!  Calculate the Lateral Separation Distance Projected Into the Mean Wind Direction
!

	phi=(pi/180.0)*wind_compass
	path=dist*abs(sin(phi))

!
!  Frequency Response Corrections (Massman, 2000 & 2001)
!

	taob=(60*60)/2.8 

	taoeKH20=sqrt(((1.0/100)/(4.0*Uavg))**2+(path/(1.1*Uavg))**2.0)
	taoeTs=sqrt(((10.0/100)/(8.4*Uavg))**2)
	taoeMomentum=sqrt(((10.0/100)/(5.7*Uavg))**2+((10.0/100)/(2.8*Uavg))**2)

!
!  Calculate z/L and Correct Values of Ustar and uz_Ta
!

	L=-(ustar**3)*Tavg/(9.8*0.4*uz_Ta)
	if (z/L<=0.0) then
	alfa=0.925
	nx=0.085
	else
	alfa=1.0
	nx=2.0-1.915/(1.0+0.5*z/L)	
	end if

	fx=nx*Uavg/z
	b=2.0*pi*fx*taob
	pMomentum=2.0*pi*fx*taoeMomentum
	pTs=2.0*pi*fx*taoeTs
	pKH20=2.0*pi*fx*taoeKH20


	rMomentum=((b**alfa)/((b**alfa)+1.0))*((b**alfa)/(b**alfa+pMomentum**alfa))* &
	        (1.0/((pMomentum**alfa)+1.0))

	rTs=((b**alfa)/((b**alfa)+1.0))*((b**alfa)/(b**alfa+pTs**alfa))* &
	        (1.0/((pTs**alfa)+1.0))

	uz_Un=uz_Un/rMomentum
	ustarn=sqrt(uz_Un)
	uz_Tan=uz_Ta/rTs
	
!
!  Re-calculate L With New Ustar and Uz_Ta, and Calculate High Frequency Corrections
!

	L = -(ustarn**3)*Tavg/(9.8*0.4*uz_Tan)
	if (z/L<=0.0) then
	alfa=0.925
	nx=0.085
	else
	alfa=1.0
	nx=2.0-1.915/(1.0+0.5*z/L)
	end if

	rMomentum=((b**alfa)/((b**alfa)+1.0))*((b**alfa)/(b**alfa+pMomentum**alfa))* &
	        (1.0/((pMomentum**alfa)+1.0))

	rTs=((b**alfa)/((b**alfa)+1.0))*((b**alfa)/(b**alfa+pTs**alfa))* &
	        (1.0/((pTs**alfa)+1.0))

	rKH20=((b**alfa)/((b**alfa)+1.0))*((b**alfa)/(b**alfa+pkh20**alfa))* &
	        (1.0/((pkh20**alfa)+1.0))

!
!  Correct the Covariance Values for High Frequency Effects
!

	uz_Ta=uz_Ta/rTs
	uz_rhov=uz_rhov/rKH20
	uz_Un=uz_Un/rMomentum
	ustar=sqrt(uz_un)
	zeta=z/L
	gamma=cp/lv

	write(3,780) uz_Ta,uz_rhov
 780	format('After High Frequency',2x,'Uz_Ta = ',F8.5,2x,'Uz_rhov = ',F11.8)
  

!
!  Calculate New H and LE Values
!

	H=rho*cp*uz_Ta
	LE=Lv*uz_rhov

	write(3,800) LE
 800	format('LE Before WPL',9x,'LE = ',F6.1)
!	
!  Webb, Pearman and Leuning Correction
!

	LE=Lv*rho*cp*Tavg*(1.0+(1.0/0.622)*(rhovavg/rhod))*(uz_rhov+(rhovavg/Tavg)*uz_Ta)/(rho*cp*Tavg+ &
	        Lv*(1.0+(1/0.622)*(rhovavg/rhod))*rhovavg*0.07)

!
!  Calculate Variance, Skewness, and Kurtosis of instantaneous rhov flux 
!
        sumflux=0.0
        sumrhov1=0.0
        sumuz1=0.0
        
        do i=1,n,1
                sumrhov1=sumrhov1 + rhov(i)
                sumuz1=sumuz1 + uz(i)
        end do
        
        avgrhov=sumrhov1/rn
        avguz=sumuz1/rn
        
        do i=1,n,1
                flux(i) = (uz(i)-avguz)*(rhov(i)-avgrhov)
                sumflux=sumflux + flux(i)
        end do
        
        avgflux=sumflux/rn
        stdflux=0.0
        varflux=0.0
        skewflux=0.0
        kurtflux=0.0
        sumdev=0.0
        sqdev=0.0
        devs=0.0
        
        do i=1,n 
                devs=flux(i)-avgflux 
                sumdev=sumdev+devs                
                sqdev=devs*devs
                varflux=varflux+sqdev
                sqdev=sqdev*devs 
                skewflux=skewflux+sqdev 
                sqdev=sqdev*devs 
                kurtflux=kurtflux+sqdev
        enddo 
        
        varflux=(varflux-((sumdev**2)/rn))/(rn-1) 
        stdflux=sqrt(varflux)
        skewflux=skewflux/(rn*stdflux**3) 
        kurtflux=(kurtflux/(rn*varflux**2))-3
        
!
!  Change the Various T Values to C
!  Mean of All Individual Tair From Tsonic Values = Tavg
!  Average Air Temperature From Average Sonic Temperature = Tair_avg
!

	Tavg=Tavg-273.16
	Tsair=Tsavg/(1+0.51*qavg)
	Tsair=Tsair-273.16
	Td=Td-273.16

	write(3,810) H,LE
 810	format(23x,'H = ',F6.1,2x,'LE = ',F6.1,/,/)
        
        if(600<hr.and.hr<2300) then
	if(LE>0.0) then
	ET=ET+(LE*60*60)/Lv
	end if
	end if

!
!  Results Written For Each Hour
!
	hrmin=(hrmin+100)-59
	write (2,400) year,doy,hr,Tavg,Td,D,stdTa,wind_compass,V,ustar,zeta,H,LE,avgflux, &
        stdflux,varflux,skewflux,kurtflux,n
 400	format(I4,1x,I3,1x,I4,2x,F5.1,2x,F5.1,2x,F6.1,3x,F5.3,3x,F5.1,2x,F4.1,3x,F4.2,1x, &
        F8.3,2x,F5.1,2x,F5.1,5x,E11.4,3x,E11.4,3x,E11.4,5x,F7.3,7x,F7.3,7x,I5)

	i=1
	sumx=0.0
	sumy=0.0
	sumz=0.0
	sumT=0.0
	sumTa=0.0
	sumv=0.0
	sump=0.0
	sume=0.0
	sumx2=0.0
	sumy2=0.0
	sumz2=0.0
	sumTs=0.0

	sumqm=0.0
	sumT2=0.0
	sumTs2=0.0
	sumTa2=0.0
	sumq2=0.0
	sumqd2=0.0
	sumc2=0.0

	sumuxr=0.0
	sumuyr=0.0
	sumuzr=0.0
	sumuxr2=0.0
	sumuyr2=0.0
	sumuzr2=0.0

	sumuxux=0.0
	sumuxuy=0.0
	sumuxuz=0.0
	sumuyuy=0.0
	sumuyuz=0.0
	sumuzuz=0.0
 	sumqTs=0.0
 	
 	zT=0
 	zrhov=0

	if(hrmin==2400) then
        go to 99
        end if
        
	go to 10

	end do

  99	write(2,170) ET
 170	format(/,/,'Daily ET = 'F4.1,1x,'mm')
    
 	end
