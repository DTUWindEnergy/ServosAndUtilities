  subroutine init_servo_with_limits(array1,array2) bind(c,name="init_servo_with_limits")
  use servo_with_limits_data
  implicit none
!DEC$ IF .NOT. DEFINED(__LINUX__)
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_servo_with_limits'::init_servo_with_limits
!DEC$ END IF
  real*8 array1(10),array2(1)
! Input array1 must contain
!
!    1: Number of blades [-]
!    2: Frequency of 2nd order servo model of pitch system [Hz]
!    3: Damping ratio 2nd order servo model of pitch system [-]
!    4: Max. pitch speed [deg/s]
!    5: Max. pitch acceleration [deg/s^2]
!    6: Min. pitch angle [deg] 
!    7: Max. pitch angle [deg] 	  
!    8: Time for pitch runaway [s]
!    9: Time for blade 1 stuck [s]
!   10: Angle of blade 1 stuck [s]
!
! Output array2 contains nothing
!
  !DEC$ IF .NOT. DEFINED(__LINUX__)
  call write_textversion
  write(6,*) 'Pitch Servo ' //trim(adjustl(TextVersion))// ' loaded...'
  !DEC$ END IF
! Save parameters
  nblades  =int(array1(1))
  omega0   =array1(2)*2.d0*pi
  beta0    =array1(3)
  vmax     =array1(4)*pi/180.d0
  amax     =array1(5)*pi/180.d0
  theta_min=array1(6)*pi/180.d0
  theta_max=array1(7)*pi/180.d0
  time_runaway=array1(8)
  time_stuck=array1(9)
  stuck_angle=array1(10)*pi/180.d0
! Set initial conditions 
  ynew(1,1:nblades)=0.d0
  ynew(2,1:nblades)=0.d0
! Set oldtime
  stepno=0
  oldtime=0.d0
  array2=0.d0
  return
  end subroutine init_servo_with_limits
!***********************************************************************
  subroutine update_servo_with_limits(array1,array2) bind(c,name="update_servo_with_limits")
  use servo_with_limits_data
!  use imsl
  implicit none
!DEC$ IF .NOT. DEFINED(__LINUX__)
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_servo_with_limits'::update_servo_with_limits
!DEC$ END IF
  real*8 array1(5),array2(9)
! Input array1 must contain
!
!    1: Time                                  [s]
!    2: Pitch1 demand angle                   [rad] 
!    3: Pitch2 demand angle                   [rad] 
!    4: Pitch3 demand angle                   [rad] 
!    5: Emergency stop flag                   [0/1]
!
! Output array2 contains
!
!            1: Pitch1 angle                          [rad] 
!            2: Pitch2 angle                          [rad] 
!            3: Pitch3 angle                          [rad] 
!    nblades+1: Pitch1 velocity                       [rad/s] 
!    nblades+2: Pitch2 velocity                       [rad/s] 
!    nblades+3: Pitch3 velocity                       [rad/s] 
!  2*nblades+1: Pitch1 acceleration                   [rad/s^2] 
!  2*nblades+2: Pitch2 acceleration                   [rad/s^2] 
!  2*nblades+3: Pitch3 acceleration                   [rad/s^2] 
!
! Local variables
!  integer*4 i,j,ido
  integer*4 i,j
  !real*8 tol,param(50),y(2),t,tend,timestep
  real*8 tol,y(2),t,tend,timestep
  !parameter(tol=1.d-5)
  real*8 theta
  real*8 work(15),relerr,abserr
  parameter(abserr=1.d-8)
  integer*4 iflag,iwork(5)
  external ode
! Initial call values
  relerr=1.d-6
  iflag=1
! Check if the time has changed
  timestep=array1(1)-oldtime
  if (timestep.gt.0.d0) then
    stepno=stepno+1
    ! Handle inputs from other DLLs
    if (stepno.eq.1) array1(5) = 0.d0
    oldtime=array1(1)
    yold=ynew
!   Loop for all blades
    do i=1,nblades
!     Initial conditions for pitch angle and velocity
      t=0.d0
      y(1:2)=yold(1:2,i)
!     Actual and reference position and velocity of pitch angle
      theta_ref=array1(i+1)
!     Compute pitch angle and velocity at next step 
      tend=timestep
      if ((array1(1).gt.time_stuck).and.(time_stuck.gt.0.d0).and.(array1(5).lt.1.d0).and.(i.eq.1)) then
        y(1)=stuck_angle
        y(2)=0.d0
      elseif ((array1(1).gt.time_runaway).and.(time_runaway.gt.0.d0).and.(array1(5).lt.1.d0)) then
        y(1)=y(1)+y(2)*timestep
        y(2)=max(-vmax,y(2)-amax)
      elseif (array1(5).gt.0.d0) then
        y(1)=y(1)+y(2)*timestep
        y(2)=min(vmax,y(2)+amax)
      else
        call rkf45(ode,2,y,t,tend,relerr,abserr,iflag,work,iwork)
      endif
!     Apply hard limits on angles
      if (y(1).lt.theta_min) then
        y(1)=theta_min
        y(2)=0.d0
      endif
      if (y(1).gt.theta_max) then
        y(1)=theta_max
        y(2)=0.d0
      endif
!     Save results
      ynew(1:2,i)=y(1:2)
!     Fill output array2
      oldarray2(i)=y(1)
      oldarray2(nblades+i)=y(2)
      oldarray2(2*nblades+i)=(y(2)-yold(2,i))/timestep
    enddo
  endif
! Insert output
  array2(1:3*nblades)=oldarray2(1:3*nblades)
  return
  end subroutine update_servo_with_limits
!***********************************************************************
  subroutine ode(t,y,yprime)
  use servo_with_limits_data
  implicit none
  real*8 t,y(2),yprime(2)
! ODEs
  yprime(1) = y(2)
  yprime(2) = -amax/vmax*y(2)&
              +amax*dtanh(omega0**2*(theta_ref-y(1))/amax-&
                          (2.d0*beta0*omega0-amax/vmax)*y(2)/amax)
  return
  end subroutine ode
!***********************************************************************
