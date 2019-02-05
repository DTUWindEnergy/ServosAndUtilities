  subroutine init_mech_brake(array1,array2) bind(c,name="init_mech_brake")
  use mech_brake_fcns
  !DEC$ IF .NOT. DEFINED(__LINUX__)
  use write_version_mod
  !DEC$ END IF
  implicit none
!DEC$ IF .NOT. DEFINED(__LINUX__)
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_mech_brake'::init_mech_brake
!DEC$ END IF
  real*8 array1(100),array2(1)
! Input array1 must contain
!
!    1: constant 1 ; Fully deployed maximum brake torque [Nm]
!    2: constant 2 ; alpha, used in Q = tanh(omega*alpha), typically 1e2/Omega_nom
!    3: constant 3 ; Delay time for before brake starts to deploy [s]
!    4: constant 4 ; Time for brake to become fully deployed [s]
!
! Output array2 contains nothing
!
  !DEC$ IF .NOT. DEFINED(__LINUX__)
  call write_textversion
  write(6,*) 'Mech brake ' //trim(adjustl(TextVersion))// ' loaded...'
  !DEC$ END IF
! Save parameters
  Qmax     = array1(1)
  alpha    = array1(2)
  t_deploy = array1(3)
  t_delay  = array1(4)
! Initiate the dynamic variables
! Zero output
  array2=0.d0
  return
  end subroutine init_mech_brake
!***********************************************************************
  subroutine update_mech_brake(array1,array2) bind(c,name="update_mech_brake")
  use mech_brake_fcns
!  use imsl
  implicit none
!DEC$ IF .NOT. DEFINED(__LINUX__)
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_mech_brake'::update_mech_brake
!DEC$ END IF
  real*8 array1(100),array2(100)
! Input array1 must contain
!
!    1: general time                           ; Time [s]
!    2: constraint bearing1 shaft_rot 1 only 2 ; Generator LSS speed [rad/s]
!    3: dll inp 1 25                           ; Command to deploy mechanical disc brake [0,1]
!
! Output array2 contains
!
!    1: Mech. brake LSS torque [Nm]
!
! Local vars
  real*8 time,omega
! Time step
  time_old=time
  time=array1(1)
  dt=time-time_old
  if (dt.gt.0.d0) then
    stepno=stepno+1
  endif
! Handle inputs from other DLLs
  if (stepno.eq.1) array1(3) = 0.d0
! Save input
  omega=array1(2)
  BrakeCommand=min(BrakeCommand + dt*array1(3)/t_deploy,1.d0)
! Output
  array2(1)=tanh(omega*alpha)*BrakeCommand*Qmax
  return
  end subroutine update_mech_brake
!***********************************************************************
