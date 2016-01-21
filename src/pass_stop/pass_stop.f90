  subroutine init_pass_stop(array1,array2)
  use pass_stop_data
  implicit none
!DEC$ IF .NOT. DEFINED(__LINUX__)
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_pass_stop'::init_pass_stop
!DEC$ END IF
  real*8 array1(1000),array2(1)
! Input array1 must contain
!
!    1: Time for stop
!    2: U1(end) 90 deg
!    3: dU1/dt 1 deg/s
!    4: U2(end)
!    5: dU2/dt
!    6: U3(end)
!    7: dU3/dt
!    8: U4(end)
!    9: dU4/dt

!
! Output array2 contains nothing

  Tstop   =array1(1)
  uend(1) =array1(2)
  uend(2) =array1(4)
  uend(3) =array1(6)
  uend(4) =array1(8)

  dudt(1) =array1(3)
  dudt(2) =array1(5)
  dudt(3) =array1(7)
  dudt(4) =array1(9)


! Set oldtime
!  stepno=0
  oldtime=0.d0
  array2=0.d0
  return
  end subroutine init_pass_stop
!***********************************************************************
  subroutine update_pass_stop(array1,array2)
  use pass_stop_data
!  use imsl
  implicit none
!DEC$ IF .NOT. DEFINED(__LINUX__)
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_pass_stop'::update_pass_stop
!DEC$ END IF
  real*8 array1(1000),array2(100)
! Input array1 must contain
!
!    1: Time                                  [s]
!    2: u1
!    3: u2
!    4: u3
!    5: u4
!
! Output array2 contains
!    1 u1
!    2 u2
!    3 u3
!    4 u4
!
! Local variables


  real*8 uold(4),t,tend,timestep
  integer*4 i
  t = array1(1)

  timestep=array1(1)-oldtime

  if (t.gt.Tstop) then
    do i=1,4
      uold(i) = dudt(i)*timestep + uold(i)
      if (dudt(i).gt.0) then
        uold(i) = min(uold(i),uend(i))
      else
        uold(i) = max(uold(i),uend(i))
      endif
   enddo
  else
    uold(1:4) = array1(2:5)
  endif

  array2(1:4) = uold(1:4)
  return
  end subroutine update_pass_stop
!***********************************************************************
