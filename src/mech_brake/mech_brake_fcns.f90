module mech_brake_fcns
integer*4 :: stepno=0
real*8 pi,degrad,raddeg
parameter(pi=3.14159265358979,degrad=0.0174532925,raddeg=57.2957795131)
real*8 Qmax,Qbrake,t_deploy,t_delay,alpha,BrakeCommand
real*8 time_old,dt
end module mech_brake_fcns