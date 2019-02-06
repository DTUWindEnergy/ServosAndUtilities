module servo_with_limits_data
use misc_mod
integer*4 nblades,stepno
real*8 omega0,beta0,vmax,amax,theta_min,theta_max,time_runaway,time_stuck,stuck_angle
real*8 oldtime,theta_ref,yold(2,3),ynew(2,3)
real*8 oldarray2(100)
end module servo_with_limits_data
