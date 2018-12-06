! Program to Determine Standard Deviations of Lateral Velocities, Which is Uded in Kjlun Footprint Model 
! The Component of Horizontal Wind Normal to the Mean Wind Direction is Foud for Each Record
! The Standard Deviation of this Lateral Velocity is Then Reported for Each Hour
!
! Completed on 27 June 2018
!

                implicit none
                
                character(len=24) in,out
                integer stat,year,doy,doyinit,hrmin,i,n,hr,minute,oldminute,oldhr,endhr
                integer, parameter :: dp=kind(1.0d0)
                real wind_compass(80000),sec,Uz,rn,CSAT_dir,radians,degrees,dir_mean
                real Ts,diag_sonic,rhoc,rhov,diag_irga,rhoc_irga,T_irga,Tamb,P,sigc,sigv,wind_dir
                real(kind=dp) :: Ux(80000),Uy(80000),V(80000),Vn(80000),sumUx,sumUy,sumV
                real(kind=dp) :: sumdir,sumtheta,Uxbar,Uybar,Vbar,X_mean,Y_mean,X_normal
                real(kind=dp) :: Y_normal,theta_normal,dir_normal,sumVn,sumVn2,sdevVn
                real(kind=dp), parameter :: pi=2*ASIN(1.0)
                
                
                write (6,*) 'Name of the input file'
                read (5,*) in
                write (6,*) 'Name of the output file'
                read (5,*) out
                open(1,file=in)
                open(2,file=out)
                
!
!  Direction CSAT is Pointed Towards
!
                CSAT_dir=270.0


                radians=pi/180.0
                degrees=180.0/pi
                stat=0

                i=1
                sumUx=0.0
                sumUy=0.0
                sumV=0.0
                sumdir=0.0
                sumtheta=0.0
                sumVn=0.0
                sumVn2=0.0


                do while (stat==0)
                
  10	        if(stat/=0) then
	        go to 99
	        end if

                read (1,*,iostat=stat,end=90) year,doy,hrmin,sec,Ux(i),Uy(i),Uz,Ts, &
                                diag_sonic,rhoc,rhov,P,diag_irga

                hr=int(hrmin/100)
                minute=hrmin-(hr*100)

                if(i==1) then
                doyinit=doy
                oldhr=hr
                oldminute=minute
                end if
                
!
! Check to See if End of Hour Was Reached
!

                if((hr/=oldhr).or.(minute<oldminute)) then
                go to 90
                end if

                V(i)=SQRT(Ux(i)**2.0+Uy(i)**2.0)
                sumV=sumV+V(i)
                
!
! Determine True Wind Direction for Each Record
!

                	wind_dir=atan(Uy(i)/Ux(i))*180.0/pi
	        if (Ux(i) < 0) then
	        if (Uy(i) >= 0) then
	        wind_dir=wind_dir+180.0
	        else  
	        wind_dir=wind_dir-180.0
	        end if
	        end if
	        wind_compass(i)=-1.0*wind_dir+CSAT_dir
	        if (wind_compass(i) < 0 ) then
	        wind_compass(i)=wind_compass(i)+360
	        end if
	        if (wind_compass(i) > 360) then
                wind_compass(i)=wind_compass(i)-360
                end if
                sumdir=sumdir+wind_compass(i)

                
                sumUx=sumUx+Ux(i)
                sumUy=sumUy+Uy(i)
                i=i+1     
                go to 10

 90             n=i-1
                rn=float(n)

                oldhr=hr
                oldminute=minute
                if(doy>doyinit) then
                hr=oldhr
                end if
                backspace(1)
                
                Uxbar=sumUx/rn
                Uybar=sumUy/rn
                Vbar=sumV/rn
                dir_mean=sumdir/rn
                
!
!  Make X,Y Coordinate of Line for Mean Wind
!

                X_mean=Uxbar
                Y_mean=(Uybar/Uxbar)* X_mean               

!
!  Determine X,Y Coordinate for Line Normal to Mean Wind
!

                X_normal=(Uxbar)/(Uxbar/Uybar)
                Y_normal=-(Uxbar/Uybar)*X_normal
                theta_normal=ATAN(Y_normal/X_normal)*(180.0/pi)
                dir_normal=dir_mean+theta_normal
                
!
! Calculations for Each Individual Wind Data
!

                do i=1,n,1

!
!  Calculate Component of Wind in Normal Direction to Mean Wind
!  Inner Product of Individual and Normal Vectors Divided by Euclidean Norm of Vector Normal to Mean
!

                Vn(i)=ABS((Ux(i)*X_normal+Uy(i)*Y_normal)/SQRT(X_normal**2.0+Y_normal**2.0))
                
!
! Sum Terms for Standard Deviation
!

                sumVn=sumVn+Vn(i)
                sumvn2=sumVn2+Vn(i)**2.0
                                
                end do
                
                sdevVn=SQRT((sumVn2-(sumVn**2.0)/rn)/(rn-1))

                endhr=hr*100
                write(2,100) doy,endhr,Vbar,sdevVn
    100         format(I3,2x,I4,4x,F6.2,2x,F6.2)
    
!
! Check to See if Last Record in File is First Record of Next Day
!

                if(doy>doyinit) then
                go to 99
                end if
                               
                sumUx=0.0
                sumUy=0.0
                sumV=0.0
                sumdir=0.0
                sumtheta=0.0
                sumVn=0.0
                sumVn2=0.0

                i=1
                
                go to 10
                
                end do
                
                
     99         end
