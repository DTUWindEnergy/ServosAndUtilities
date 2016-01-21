module filters_mod
   use filters_data_mod
   implicit none
   contains
!**************************************************************************************************
   subroutine init_filters(array1,array2)
      ! Applies perturbation to control signals
      implicit none
      !DEC$ IF .NOT. DEFINED (__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_filters'::init_filters
      !DEC$ END IF
      real(mk) array1(1000),array2(1)
      ! Input parameters
      !   1: constant   1  ; Filter type [1=first order low pass,2=second order low pass,3=second order notch,4=second order bandpass]
      !   2: constant   2  ; Filter frequency, or 1/time constant of first order filter [Hz] 
      !   3: constant   3  ; Damping ratio for second order lowpass filter, or 3dB (relative) bandwidth of Notch and bandpass filter [Hz]
      !   4: constant   4  ; not used for lowpass filters, reduction ratio for Notch filter [-], or time lead constant for bandpass filter [s]
      filttype=int(array1(1))
      select case(filttype)
         case(1)
            LP1_var%tau   = 1._mk/array1(2)
         case(2)
            LP2_var%f0   = array1(2)
            LP2_var%zeta = array1(3)
         case(3)
            N_var%f0   = array1(2)
            N_var%zeta1 = 0.5_mk*array1(3)/array1(2)
            N_var%zeta2 = array1(4)*N_var%zeta1
         case(4)
            BP_var%f0   = array1(2)
            BP_var%zeta = 0.5_mk*array1(3)/array1(2)
            BP_var%tau = array1(4)
         case default
            write(6,'(a, i1, a)') 'ERROR in filters.dll: Filter type ',filttype,' unknown'
            stop
      end select
      return
   end subroutine init_filters
!**************************************************************************************************
   subroutine update_filters(array1,array2)
      implicit none
      !DEC$ IF .NOT. DEFINED (__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_filters'::update_filters
      !DEC$ END IF
      real(mk) array1(1000),array2(100)
      ! Input array1 must contains
      !
      !    1: general time [s]     
      !    2: Input [unit]
      !
      ! Output array2 contains
      !
      !    1: Filtered signal [unit]
      !
      ! Local variables
      real(mk) time, y(2)
      ! Input
      time=array1(1)
      ! Increment time step (may actually not be necessary in type2 DLLs)
      if (time-time_old .gt. 1.d-6) then
         deltat = time-time_old
         time_old = time
         stepno = stepno+1
      endif
      ! Notch filter
      select case(filttype)
         case(1)
            array2(1) = lowpass1orderfilt(deltat, stepno, LP1_var, array1(2))
         case(2)
            y = lowpass2orderfilt(deltat, stepno, LP2_var, array1(2))
            array2(1) = y(1)
         case(3)
            array2(1) = notch2orderfilt(deltat, stepno, N_var, array1(2))
         case(4)
            array2(1) = bandpassfilt(deltat, stepno, BP_var, array1(2))
      end select
      
      return
   end subroutine update_filters
!**************************************************************************************************
end module filters_mod
