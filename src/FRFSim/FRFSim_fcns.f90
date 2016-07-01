module frfsim_fcns_mod
! Parameters:
integer*4 looptype !   1: constant   1  ; Type: (1) Open-loop (2) Closed-loop
integer*4 nin      !   2: constant   2  ; Number of actuator signals [-]
integer*4 iperturb !   3: constant   3  ; Index of actuator signals to perturb [-]
real*8    f0       !   4: constant   4  ; First frequency [Hz]
real*8    df       !   5: constant   5  ; Step in frequency [Hz]
real*8    excamp   !   6: constant   6  ; Amplitude of perturbations [-]
integer*4 t0       !   7: constant   7  ; Time at which the perturbations starts [s]
integer*4 ncycmax  !   8: constant   8  ; Maximum number of cycles at one frequency [s]  
integer*4 nout     !   9: constant   9  ; Number of output signals to monitor [-]
real*8    relerr   !  10: constant  10  ; Percentage of standard deviation of the error relative to peak-to-peak range [-]
integer*4 ncyc     !  11: constant  11  ; Number of cycles in fit for error estimation [-]
integer*4 next     !  12: constant  12  ; Number 'nnn' for the output file extension 'frf.nnn'
! Old references and time
real*8 ref_old(100)
real*8::told=0.d0
! Frequency
real*8 f
! Angular variable and its history
integer*4 npsi,ipsi
integer*4,allocatable::converged(:)
real*8::psi=0.d0
real*8,allocatable::xvec(:),fvec(:,:),yvec(:,:),amp(:),pha(:),mean(:),resi(:)
! Counter
integer*4::ncycles=0
! File extension
character text8*8
! Constants
real*8 pi,degrad,raddeg
parameter(pi=3.14159265358979,degrad=0.0174532925,raddeg=57.2957795131)
end module frfsim_fcns_mod