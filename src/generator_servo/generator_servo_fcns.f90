module generator_servo_fcns
use misc_mod
! Types
type Tgenerator
  real*8,allocatable::p_eta(:)
  real*8 nom_eta_x
  real*8 eta,gearratio,mgenwa1,mgenera1,max_lss_torque
  integer*4 stepno,n_eta,eta_dependance
  real*8 deltat,outputvektor_old(15),time_old
end type Tgenerator
! Variables
type(Tgenerator) generatorvar
type(Tlowpass2order) lowpass2ordergen
real*8 TimeGridLoss
!*****************************************************************************************
contains
!*****************************************************************************************
end module generator_servo_fcns