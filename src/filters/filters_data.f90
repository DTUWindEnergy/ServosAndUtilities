module filters_data_mod
   use misc_mod
   ! Simulations variables
   integer :: stepno = 0
   real(mk) :: time_old = 0.0_mk
   real(mk) :: deltat = 0.02_mk
   ! Filters
   integer filttype
   type(Tfirstordervar) LP1_var
   type(Tlowpass2order) LP2_var
   type(Tnotch2order) N_var
   type(Tbandpassfilt) BP_var
end module filters_data_mod