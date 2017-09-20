module servo_with_limits_data
   use misc_mod
   integer nblades,stepno,stuck_type
   real(mk) omega0,beta0,vmax,amax,theta_min,theta_max,time_runaway,time_stuck,stuck_angle
   real(mk) oldtime,theta_ref,yold(2,3),ynew(2,3)
   real(mk) oldarray2(100)
end module servo_with_limits_data