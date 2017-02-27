module generator_servo_mod
   use generator_servo_fcns
   contains
!**************************************************************************************************
   subroutine init_generator_servo(array1,array2)
      use write_version_mod
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_generator_servo'::init_generator_servo
      !DEC$ END IF
        real(mk) array1(100), array2(1)
      ! Input array1 must contain
      !
      !    1: constant 1 ; Frequency of 2nd order servo model of generator-converter system [Hz]
      !    2: constant 2 ; Damping ratio 2nd order servo model of generator-converter system [-]
      !    3: constant 3 ; Maximum allowable LSS torque (pull-out torque) [Nm]
      !    4: constant 4 ; Generator efficiency [-]
      !    5: constant 5 ; Gearratio [-]
      !    6: constant 6 ; Time for half value in softstart of torque [s]
      !    7: constant 7 ; Time for grid loss [s]
      !
      ! Output array2 contains nothing
      !
      call write_textversion
      write(6, *) 'Gen. torque Servo ' //trim(adjustl(TextVersion))// ' loaded...'
      ! Save parameters
      lowpass2ordergen%f0 = array1(1)*2.0_mk*pi
      lowpass2ordergen%zeta = array1(2)
      generatorvar%max_lss_torque = array1(3)
      generatorvar%n_eta = 1
      allocate(generatorvar%p_eta(1))
      generatorvar%p_eta = array1(4)
      generatorvar%nom_eta_x = 1.0_mk
      generatorvar%gearratio = array1(5)
      TimeGridLoss = array1(7)
      ! Initiate the dynamic variables
      generatorvar%stepno = 0
      generatorvar%time_old = 0.0_mk
      ! Zero output
      array2 = 0.0_mk
   end subroutine init_generator_servo
!**************************************************************************************************
   subroutine init_generator_servo_var_eta(array1,array2)
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_generator_servo_var_eta'::init_generator_servo_var_eta
      !DEC$ END IF
      real*8 array1(100), array2(1)
      !
      ! Input array1 must contain
      !
      !    1: constant  1 ; Frequency of genertor 2nd order control model [Hz]   
      !    2: constant  2 ; Damping ratio of genertor 2nd order control model [-]
      !    3: constant  3 ; Maximum allowable LSS torque (pull-out torque) [Nm]
      !    4: constant  4 ; N_eta - No. of efficiency factors [-]
      !    5: constant  5 ; Gearratio [-]
      !    6: constant  6 ; Time for half value in softstart of torque [s]
      !    7: constant  7 ; Time for grid loss [s]
      !    8: constant  8 ; unit conversion factor
      !    9: constant  9 ; Efficiency dependance: (1) LSS speed, (2) LSS mech torque, (3) mech. power
      !   10: constant 10 ; Nominal LSS speed/LSS torque/mech. power [rad/s / Nm / W]
      !   11: constant 11 ; Efficiency at 0/N_eta percent nominal speed/torque/power [-]
      !   12: constant 12 ; Efficiency at 1/N_eta percent nominal speed/torque/power [-]
      !   13: constant 13 ; Efficiency at 2/N_eta percent nominal speed/torque/power [-]
      !
      ! Output array2 contains nothing
      !
      ! Local vars
      real(mk), allocatable :: y_eta(:)
      integer i, j, n_eta
      ! Save parameters
      call init_generator_servo(array1, array2)
      n_eta = array1(4)
      generatorvar%n_eta = n_eta
      allocate(y_eta(n_eta))
      deallocate(generatorvar%p_eta)
      allocate(generatorvar%p_eta(n_eta))
      generatorvar%eta_dependance = array1(9)
      generatorvar%nom_eta_x = array1(10)
      do i = 1, n_eta
          y_eta(i) = array1(10 + i)
      enddo
      
      select case (n_eta)
         case (1)
            generatorvar%p_eta(1) = y_eta(1)
         case (2)
            generatorvar%p_eta(1) = y_eta(1)
            generatorvar%p_eta(2) = y_eta(2) - y_eta(1)
         case (3)
            generatorvar%p_eta(1) = y_eta(1)
            generatorvar%p_eta(3) = (y_eta(3) - 2.0_mk*y_eta(2) + y_eta(1))*2.0_mk
            generatorvar%p_eta(2) = (y_eta(2) - generatorvar%p_eta(3)/4.0_mk - y_eta(1))*2.0_mk
         case (4)
            generatorvar%p_eta(1) = y_eta(1)
            generatorvar%p_eta(3) = y_eta(3) - y_eta(1)
            generatorvar%p_eta(2) = 1.0_mk/sqrt(0.5_mk)*atanh((y_eta(2) - y_eta(1))/generatorvar%p_eta(3))
         case default
            write (0,*) 'Error: n_eta should be between 1,2 or 3'
      end select
      ! Initiate the dynamic variables
      generatorvar%stepno = 0
      generatorvar%time_old = 0.0_mk
      ! Zero output
      array2 = 0.0_mk
      return
   end subroutine init_generator_servo_var_eta
!**************************************************************************************************
   subroutine update_generator_servo(array1, array2)
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_generator_servo'::update_generator_servo
      !DEC$ END IF
        real*8 array1(100), array2(100)
      ! Input array1 must contain
      !
      !    1: general time                           ; Time [s]     
      !    2: dll inpvec 1 1                         ; Electrical torque reference [Nm]
      !    3: constraint bearing1 shaft_rot 1 only 2 ; Generator LSS speed [rad/s]   
      !....4: mbdy momentvec shaft 1 1 shaft # only 3;
      !
      ! Output array2 contains
      !
      !    1: Generator LSS torque [Nm]
      !    2: Electrical generator power [W]
      !    3: Gearbox reaction tower top LSS [Nm]
      !    4: Generator reaction tower top HSS  [Nm]
      !    5: Mechanical generator power [kW]
      !    6: mbdy moment_ext towertop 2 3 shaft
      !    7: Resulting efficiency
      !    8: Grid flag [1=no grid,0=grid]
      !
      ! Local vars
      real(mk) time, omegagen, Qgref, mech_Qgref, mech_Qg, Qg, softstart_torque
      real(mk) Qshaft
      real(mk) Qgdummy(2)
      integer i
      real(mk) eta_x
      ! New step?
      time = array1(1)
      if (time .gt. generatorvar%time_old) then
         generatorvar%deltat = time - generatorvar%time_old
         generatorvar%time_old = time
         generatorvar%stepno = generatorvar%stepno + 1
      endif
      ! Save input
      Qgref = array1(2)
      Qshaft = array1(4)
      omegagen = array1(3)
      ! Reference mech. torque
      mech_Qgref = min(Qgref/generatorvar%eta, generatorvar%max_lss_torque)
      ! Low-pass filter generator speed (LSS)
      Qgdummy = lowpass2orderfilt(generatorvar%deltat, generatorvar%stepno, lowpass2ordergen, mech_Qgref)
      mech_Qg = Qgdummy(1)
      ! Loss
      select case (generatorvar%eta_dependance)
         case (1)
            eta_x = omegagen
         case (2)
            eta_x = mech_Qg
         case (3)
            eta_x = mech_Qg*omegagen
      end select
    
      eta_x = max(min(eta_x/generatorvar%nom_eta_x, 1.0_mk), 0.0_mk)
    
      select case(generatorvar%n_eta)
         case (1)
            generatorvar%eta = generatorvar%p_eta(1)
         case (2)
            generatorvar%eta = generatorvar%p_eta(2)*eta_x + generatorvar%p_eta(1)
         case (3)
            generatorvar%eta = generatorvar%p_eta(3)*eta_x**2 + generatorvar%p_eta(2)*eta_x + generatorvar%p_eta(1)
         case (4)
            generatorvar%eta = generatorvar%p_eta(3)*tanh(sqrt(eta_x)*generatorvar%p_eta(2)) + generatorvar%p_eta(1)
      end select
    ! Output
      if ((time.gt.TimeGridLoss).and.(TimeGridLoss.gt.0.d0)) then
         array2 = 0.d0
         array2(8) = 1.d0
      else
         array2(1) = -mech_Qg
         array2(2) =  mech_Qg*omegagen*generatorvar%eta
         array2(3) = -mech_Qg + mech_Qg/generatorvar%gearratio
         array2(4) = -mech_Qg/generatorvar%gearratio
         array2(5) =  mech_Qg*omegagen*1.e-3_mk
         array2(6) = -Qshaft*1.e3_mk
         array2(8) = 0.0_mk
      endif
      array2(7) =  generatorvar%eta
      return
   end subroutine update_generator_servo
!**************************************************************************************************
end module generator_servo_mod
