module flap_servo_mod
   !
   !   Control Dll of type 2:
   ! Dll for accounting for flap servo actions, delay given by first order low pass filter (for now)
   ! Could be used either following cyclic control signal, or to assign steps
   !
   ! v.01, 31/10/2014, leob
   !
   use misc_mod 
   implicit none
   type(Tfirstordervar) LP1_act_fl1, LP1_act_fl2, LP1_act_fl3
   real(mk) defl_max, defl_min, time_old, deltat
   integer :: stepno = 0
   contains
!**************************************************************************************************
   subroutine init_flap_servo(array1, array2) bind(c,name="init_flap_servo")
      ! Initialize Cyclic Flap Controller
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_flap_servo'::init_flap_servo
      !DEC$ END IF
      real(mk) array1(3), array2(1)
      ! Input parameters
      !   1: constant   1  ; Low-pass filter time constant [s]
      !
      ! OBS: More general way to do it, for arbitrary number of inputs: define array type of LP1 variables
      !
      ! -- Read in parameters -- !
      LP1_act_fl1%tau  = array1(1)    ! Filter time constant [s]
      defl_max = array1(2)
      defl_min = array1(3)
      ! . Copy
      LP1_act_fl2 = LP1_act_fl1
      LP1_act_fl3 = LP1_act_fl1
      
      ! -- Dummy Output -- !
      array2(1) = 1.0_mk
      return
   end subroutine init_flap_servo
!**************************************************************************************************
   subroutine update_flap_servo(array1, array2) bind(c,name="update_flap_servo")
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_flap_servo'::update_flap_servo
      !DEC$ END IF
      real(mk) array1(4), array2(3)
      ! Input array1 must contains
      !
      !    1: general time [s]
      !    2..4: Flap signals to filter, either from ctrl or step [deg]
      !
      ! Output array2 contains
      !
      !   1..3:   Filtered flap signal [deg]
      !
      
      ! -- Internal Variables -- !
      integer i
      real(mk) time, fl1, fl2, fl3
      ! --------------------------------------------------------------- !
      ! Main Body
      ! --------------------------------------------------------------- !
      time = array1(1)
      ! Increment time step (may actually not be necessary in type2 DLLs)
      if (time - time_old .gt. 1.d-6) then
         deltat = time - time_old
         time_old = time
         stepno = stepno + 1
      endif
      ! -- Limit the inputs -- !
      fl1 = MAX(MIN(array1(2), defl_max), defl_min)
      fl2 = MAX(MIN(array1(3), defl_max), defl_min)
      fl3 = MAX(MIN(array1(4), defl_max), defl_min)
      ! -- Call filter for the number of channels -- !
      array2(1) = lowpass1orderfilt(deltat, stepno, LP1_act_fl1, fl1)
      array2(2) = lowpass1orderfilt(deltat, stepno, LP1_act_fl2, fl2)
      array2(3) = lowpass1orderfilt(deltat, stepno, LP1_act_fl3, fl3)
      return
   end subroutine update_flap_servo
!**************************************************************************************************
end module flap_servo_mod
