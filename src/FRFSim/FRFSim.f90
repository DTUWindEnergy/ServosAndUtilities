module FRFSim_mod
use FRFSim_fcns_mod
implicit none
contains
!**************************************************************************************************
subroutine init_frfsim(array1,array2) bind(c,name='init_frfsim')
! Applies perturbation to control signals
implicit none
!DEC$ IF .NOT. DEFINED(__MAKEFILE__)
!DEC$ ATTRIBUTES DLLEXPORT::init_frfsim
!DEC$ END IF
real*8, dimension(100), intent(inout) :: array1
real*8, dimension(1), intent(inout)   :: array2
integer*4 j 
! Input parameters
!   1: constant   1  ; Type: (1) Open-loop (2) Closed-loop
!   2: constant   2  ; Number of actuator signals [-]
!   3: constant   3  ; Index of actuator signals to perturb [-] (if zero then an additional harmonic variating signal is provided)
!   4: constant   4  ; First frequency [Hz]
!   5: constant   5  ; Step in frequency [Hz]
!   6: constant   6  ; Amplitude of perturbations [-]
!   7: constant   7  ; Time at which the perturbations starts [s]
!   8: constant   8  ; Maximum number of cycles at one frequency [s]  
!   9: constant   9  ; Number of output signals to monitor [-]
!  10: constant  10  ; Percentage of standard deviation of the error relative to peak-to-peak range [-]
!  11: constant  11  ; Number of cycles in fit for error estimation [-]
!  12: constant  12  ; Number 'nnn' for the output file extension 'frf.nnn'
looptype = int(array1(1))
nin      = int(array1(2))
iperturb = int(array1(3))
f0       = array1(4)*2.d0*pi
df       = array1(5)*2.d0*pi
excamp   = array1(6)
t0       = array1(7)
ncycmax  = int(array1(8))
nout     = int(array1(9))
relerr   = array1(10)
ncyc     = int(array1(11))
next     = int(array1(12))
write(text8,'(i8)') next
!open(1000,file='frf.'//trim(adjustl(text8)))
!write(1000,'(a16,<nout>(a1)') '# Freq [Hz]',(('Amp [-]','Phase [deg]'),j=1,nout)
!close(1000)
allocate(mean(nout),amp(nout),pha(nout),converged(nout),resi(nout))
mean=0.d0
amp=0.d0
pha=0.d0
converged=0
resi=0.d0
return
end subroutine init_frfsim
!**************************************************************************************************
subroutine update_frfsim(array1,array2) bind(c,name='update_frfsim')
!use risoe_controller_fcns
implicit none
!DEC$ IF .NOT. DEFINED(__MAKEFILE__)
!DEC$ ATTRIBUTES DLLEXPORT::update_frfsim
!DEC$ END IF
 real*8, dimension(1000), intent(inout) :: array1
 real*8, dimension(100),  intent(inout) :: array2
! Input array1 must contains
!
!    1: general time                     [s]     
!    2: actuator signal                  [unit]
!    3: actuator signal                  [unit]
!    etc.
!
!    2+nin: response signal 1 to monitor [unit]
!    3+nin: response signal 2 to monitor [unit]
!    4+nin: response signal 3 to monitor [unit]
!    etc.
!                    
! Outputs array2 are:
!    1: actuator signal                  [unit]
!    2: actuator signal                  [unit]
!    etc. 
! last: Additional harmonic signal if init input #3 is zero
!                    
! Local variables
integer*4 i,j,fullconverged
real*8 time,dt,excsig,c(3),a(3,3),b(3),d,r,aa
! Read in time
time=array1(1)
! Get time step
if (time.gt.told) then
  dt=time-told
endif
! Save the current values of the reference signals for open-loop perturbation
if (time.lt.t0) then
  ref_old(1:nin) = array1(2:1+nin)
  array2(1:nin)  = array1(2:1+nin)
  array2(nin+1:nin+2+3*nout) = 0.d0
  f=f0
  told=time
  return
endif
! Compute perturbation signal
psi=psi+dt*f
excsig = excamp*sin(psi)
! Save angular varaible history
if (.not.allocated(xvec)) then
  npsi=floor(dfloat(ncyc)*2.d0*pi/f/dt)
  allocate(xvec(npsi),fvec(npsi,3),yvec(npsi,nout))
  ncycles=0
  converged=0
  ipsi=0
endif
! Increment steps and save
ipsi=ipsi+1
xvec(1:npsi-1)=xvec(2:npsi)
xvec(npsi)=psi
do i=1,nout
  yvec(1:npsi-1,i)=yvec(2:npsi,i)
  yvec(npsi,i)=array1(1+nin+i)
enddo
! Check for steady state for every 'ncyc' cycles
if (mod(ipsi,npsi).eq.0) then
  ncycles=ncycles+1
  ! Fit c(1)*cos(xvec)+c(2)*sin(xvec) to yvec for each output
  fullconverged=1
  do i=1,nout
    if (converged(i).eq.0) then
      ! Least square
      do j=1,npsi
        fvec(j,1)=dcos(xvec(j))
        fvec(j,2)=dsin(xvec(j))
        fvec(j,3)=1.d0
      enddo
      a=matmul(transpose(fvec),fvec)
      b=matmul(transpose(fvec),yvec(:,i))
      d=a(1,1)*a(2,2)*a(3,3)-a(1,1)*a(2,3)**2-a(1,2)**2*a(3,3)+2*a(1,3)*a(1,2)*a(2,3)-a(1,3)**2*a(2,2)
      c(1)= (a(1,2)*a(2,3)*b(3)-a(1,2)*a(3,3)*b(2)-a(1,3)*a(2,2)*b(3)+a(1,3)*a(2,3)*b(2)+a(2,2)*a(3,3)*b(1)-a(2,3)**2*b(1))/d
      c(2)=-(a(1,1)*a(2,3)*b(3)-a(1,1)*a(3,3)*b(2)-a(1,2)*a(1,3)*b(3)+a(1,2)*a(3,3)*b(1)+a(1,3)**2*b(2)-a(1,3)*a(2,3)*b(1))/d
      c(3)= (a(1,1)*a(2,2)*b(3)-a(1,1)*a(2,3)*b(2)-a(1,2)**2*b(3)+a(1,2)*a(1,3)*b(2)+a(1,2)*a(2,3)*b(1)-a(1,3)*a(2,2)*b(1))/d
      aa=c(1)**2+c(2)**2
      mean(i)=c(3)
      ! FRF
      amp(i)=dsqrt(aa)/excamp
      pha(i)=datan2(c(1),c(2)) ! Signs must be checked
      ! Squared error
      open(1000,file='frflog.'//trim(adjustl(text8)))
      r=0.d0
      do j=1,npsi
        r=r+(fvec(j,1)*c(1)+fvec(j,2)*c(2)+c(3)-yvec(j,i))**2/dfloat(npsi)
        write(1000,'(16e16.7)') xvec(j),fvec(j,1)*c(1)+fvec(j,2)*c(2)+c(3),yvec(j,i),aa,relerr,r,d
      enddo
      resi(i)=r
      close(1000)
      ! Check convergence and compute amplitude and phase
      if (r/aa.lt.relerr) converged(i)=1
    endif
    fullconverged=fullconverged*converged(i)
  enddo
  ! Write out FRF point
  if ((fullconverged.eq.1).or.(ncycles.gt.ncycmax)) then
    open(1000,file='frf.'//trim(adjustl(text8)),POSITION='APPEND')
    write(1000,'(<1+2*nout>e16.7,<nout>i4)') f,((amp(j),pha(j)),j=1,nout),(converged(j),j=1,nout)
    close(1000)
    deallocate(xvec,fvec,yvec)
    f=f+df
  endif
endif
! Add the perturbation
if (looptype.eq.1) then
  array2(1:nin) = ref_old(1:nin)
else
  array2(1:nin) = array1(2:1+nin)
endif
! Add the harmonic variation 
if (iperturb.gt.0) then
  array2(iperturb) = array2(iperturb) + excsig
else
  array2(nin+1) = excsig
endif
! Send back the amplitudes and convergence flags
array2(nin+2:nin+1+nout)=amp(1:nout)*excamp+mean(1:nout)
array2(nin+2+nout:nin+1+2*nout)=dfloat(converged(1:nout))
array2(nin+2+2*nout:nin+1+3*nout)=resi(1:nout)
! Save time
told=time
return
end subroutine update_frfsim
!**************************************************************************************************
end module FRFSim_mod
