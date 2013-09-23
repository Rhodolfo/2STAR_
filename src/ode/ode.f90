  module ode

  integer :: ode_bs_k        = 13
  integer, parameter :: kmax = 15

  real :: ode_tolerance       = 1.
  real :: ode_norm_truncation = 1.
  real :: ode_dt_suggested    = 1.
  real :: ode_order           = 4.      
  real :: ode_scalar          = 1.
  real, dimension(:), allocatable :: ode_ref_scale

  logical :: ode_error_control = .true.
  logical :: ode_reject_step   = .false.

  contains





! *********************************
! *** RHO'S ODE SOLVING PACKAGE ***
! *********************************





! This is a set of subroutines that focuses on solving the ODE:

!         du
!         -- = Source Function (u,t)
!         dt

! Note that the source function may be non-linear.

! The source function is incorporated into the subroutines by an interface.
! Do note that I allow the function u to be a vector.




! ROUTINE LIST

! So far, I've incorporated the following routines to the solver:

! - ode_euler_step(t,dt,u_old,u_new,source_function)
!   Takes a time step from time "t" to time "dt" using the naive Euler first  
!   order method. Returns u_new that is calculated from u_old through the procedure
!   "source_function."

! - ode_leapfrog_step(t,dt,u_old,u_mid,u_new,source_function)
!   Takes a time step from time "t" to time "dt" using the Leapfrog second  
!   order method. Returns u_new that is calculated from u_old and u_mid through the procedure
!   "source_function."

! - ode_rg4_step(t,dt,u_old,u_new,source_function):
!   Takes a time step from time "t" to time "dt" using the Runge-Kutta fourth
!   order method. Returns u_new that is calculated from u_old through the procedure
!   "source_function."

! - ode_bs_step(t,dt,u_old,u_new,source_function,nmax):
!   Takes a time step from time "t" to time "dt" using the Brulirsch-Stuer
!   order method. Note that the additional variable nmax determines how many 
!   iterations of the recursive formula for the BS method is used. 
!   Returns u_new that is calculated from u_old through the procedure
!   "source_function."





! IMPORTANT NOTES

! Some caveats that come with this code

! 1. The source function and the solution MUST be Fortran vectors. 
!    If in your case it is scalar, define the solution vector as a dimension(1) vector.
!    The function must always take an assumed shape array (see note 3 on how to
!    do this) and hence must be free to take any sized array as an argument.

! 2. The routines assume that the source function takes the solution vector and
!    time as arguments. Even if your source function is time independent, it
!    must be declared to have time as an argument (all one has to do is do
!    nothing with the time variable).

! 3. The source function must use assumed shape arrays for the u argument. 
!    Meaning that your source function has to be coded as such:

! 4. So far the only procedures that has implemented error control 
!    are the Bulirsch-Stuer and RG4 integrators.
 
!    function example(input_array,time_variable)
!    implicit none
!    real                               :: time_variable
!    real, dimension(:)                 :: input_array
!    real, dimension(size(input_array)) :: example
!    (Other variable declarations)
!    (An axample would be:)
!    real :: two
!    (Procedure to get the function value)
!    (Example procedure)
!    two     = 2.
!    example = input_array**two
!    return
!    end function example

!    Of course there is no reason to name your function "example",
!    Do whatever you like.








  subroutine ode_euler_step(time_value,time_step,solution_old,solution_new,source_function) 
  implicit none
  real, intent(in) :: time_value,time_step
  real, dimension(:), intent(in)  :: solution_old
  real, dimension(:), intent(out) :: solution_new
  interface 
    function source_function(vector,time)
    real              :: time
    real, dimension(:) :: vector
    real, dimension(size(vector)) :: source_function
    end function source_function
  end interface
  solution_new = solution_old + time_step * source_function(solution_old,time_value)
  return
  end subroutine ode_euler_step










  subroutine ode_leapfrog_step(time_value,time_step,solution_old,solution_mid,solution_new,source_function) 
  implicit none
  real, intent(in) :: time_value,time_step
  real, dimension(:), intent(in)  :: solution_old
  real, dimension(:), intent(in)  :: solution_mid
  real, dimension(:), intent(out) :: solution_new
  interface 
    function source_function(vector,time)
    real              :: time
    real, dimension(:) :: vector
    real, dimension(size(vector)) :: source_function
    end function source_function
  end interface
  solution_new = solution_old + 2.0 * time_step * source_function(solution_mid,time_value)
  return
  end subroutine ode_leapfrog_step










  subroutine ode_rg4_step(time_value,time_step,solution_old,solution_new,source_function) 
  implicit none
  real, intent(in) :: time_value,time_step
  real             :: dt,err
  real, dimension(:), intent(in)  :: solution_old
  real, dimension(:), intent(out) :: solution_new
  real, dimension(size(solution_old)) :: k1,k2,k3,k4,solution_ref,delta
  interface 
    function source_function(vector,time)
    real              :: time
    real, dimension(:) :: vector
    real, dimension(size(vector)) :: source_function
    end function source_function
  end interface
  dt = time_step
  k1 = source_function(solution_old            , time_value  )
  k2 = source_function(solution_old + 0.5*dt*k1, time_value + 0.5*dt)
  k3 = source_function(solution_old + 0.5*dt*k2, time_value + 0.5*dt)
  k4 = source_function(solution_old +     dt*k3, time_value +     dt)
  solution_new = solution_old + dt * ( (k1+2.0*k2+2.0*k3+k4) / 6.0 )
  if (ode_error_control) then 
  ! Refining step
    dt = time_step/2.0
    k1 = source_function(solution_old            , time_value         )
    k2 = source_function(solution_old + 0.5*dt*k1, time_value + 0.5*dt)
    k3 = source_function(solution_old + 0.5*dt*k2, time_value + 0.5*dt)
    k4 = source_function(solution_old +     dt*k3, time_value +     dt)
    solution_ref = solution_old + dt * ( (k1+2.0*k2+2.0*k3+k4) / 6.0 )
  ! Calculate truncation error and then the suggested time step
    delta   = abs(solution_ref-solution_new)
    if (allocated(ode_ref_scale)) then 
      delta = (delta/ode_ref_scale)**2
    else
      delta = delta**2
    end if
    err                 = abs(sum(delta)) / sqrt(real(size(delta)))
    ode_dt_suggested    = time_step*((ode_tolerance/err)**(1.0/4.0))
    if (err.ge.ode_tolerance) then 
      ode_reject_step = .true.
    else
      ode_reject_step = .false.
    end if
    solution_new = solution_ref 
  end if 
  return
  end subroutine ode_rg4_step










  subroutine ode_mm_step(time_value,time_step,solution_old,solution_new,source_function,nmax) 
  implicit none
  real, intent(in) :: time_value,time_step
  real, dimension(:), intent(in)  :: solution_old
  real, dimension(:), intent(out) :: solution_new
  integer, intent(in)             :: nmax
  real    :: h
  integer :: j,k
  real, dimension(size(solution_old),0:nmax) :: z
  interface 
    function source_function(vector,time)
    real              :: time
    real, dimension(:) :: vector
    real, dimension(size(vector)) :: source_function
    end function source_function
  end interface
! Let's define the first few iterations to get the iterative algorithm going
  h    = time_step/nmax
  z(:,0) = solution_old(:)
  z(:,1) = z(:,0) + h*source_function(z(:,0),time_value)
! Now use the recurtion formula to get all the z's
  do j = 1,nmax-1
    z(:,j+1) = z(:,j-1) + 2.*h*source_function(z(:,j),time_value+j*h)
  end do
! Now that we have all the z's, we are ready to take the full time step
  solution_new(:) = z(:,nmax)/2. + (z(:,nmax-1)+h*source_function(z(:,nmax),time_value+time_step))/2.
  return
  end subroutine ode_mm_step










  subroutine ode_bs_step(time_value,time_step,solution_old,solution_new,source_function)
  implicit none
  real, intent(in) :: time_value,time_step
  real, dimension(:), intent(in)  :: solution_old
  real, dimension(:), intent(out) :: solution_new
  interface 
    function source_function(vector,time)
    real              :: time
    real, dimension(:) :: vector
    real, dimension(size(vector)) :: source_function
    end function source_function
  end interface
! Integers for the BS algorithm
  integer                    :: ii,jj
  integer, dimension(1:kmax) :: n 
! Here I define some dummy array, the truncation  and the diagonal array 
! we're going to use for the Richardson extrapolation (see Shapiro Teukolsky pg 924)
  real, dimension(size(solution_old))               :: solution_dum
  real, dimension(size(solution_old),1:kmax,1:kmax) :: T
! Error tolerance variables
  logical                        :: done
  integer                        :: kk,knew,kdone
  real                           :: S1,S2,S3,S4,F,hnew
  real, dimension(1:kmax)        :: E,Hk,Wk,Ak
  real, dimension(size(solution_old)) :: delta,square
  real, dimension(size(solution_old)) :: S,NE

! Error control
  kk = ode_bs_k
  S1 = 0.05
  S2 = 0.05
  S3 = 0.02
  S4 = 4.0
  F  = S3**(1./(2.*real(kk)+1))
  done            = .false.
  ode_reject_step = .false.
  if (ode_error_control) then
    if (allocated(ode_ref_scale)) then
    S = ode_ref_scale 
    else 
    S = 1.
    end if
  else
  end if

! Defining the sequence of integers to pass to the mm method
  do ii = 1,kmax
    n(ii) = 2*ii
  end do

! Calculating the number of function evaluations for error control
  if (ode_error_control) then
  Ak(1) = n(1)
  do ii = 2,kmax
    Ak(ii) = Ak(ii-1) + n(ii)
  end do
  end if

! On to the BS method:
! Doing the first integration
  call ode_mm_step(time_value,time_step,solution_old,solution_dum,source_function,n(1))
  T(:,1,1) = solution_dum(:)
! Let's complete the tableau
  do ii = 1,kmax
    call ode_mm_step(time_value,time_step,solution_old,solution_dum,source_function,n(ii))
    T(:,ii,1) = solution_dum(:) 
    do jj = 2,ii
      T(:,ii,jj) = T(:,ii,jj-1) + (T(:,ii,jj-1)-T(:,ii-1,jj-1))/((real(n(ii))/(real(n(ii-jj+1))))**2-1)
    end do
  ! Some error control stuff
    if (ode_error_control) then
      if (ii.eq.1) delta(1) = 0.
      if (ii.gt.1) delta(:) = T(:,ii,ii)-T(:,ii,ii-1)
      if (ii.eq.2) delta(1) = delta(2)
      delta    = delta/S
      square   = delta**2 
      E(ii)    = sqrt(sum(square)/size(square))
      E(ii)    = max(E(ii),tiny(E(ii)))
      Hk(ii)   = time_step*S1*((S2/E(ii))**(1./(2*real(ii)+1.)))
      Wk(ii)   = Ak(ii)/Hk(ii)
    end if
  ! Testing for convergence
    if (ode_error_control) then
    ! Testing for convergence in the kk-1 row
      if (ii.eq.kk-1) then
      ! Time step and k recipe
!       write(*,*) "[ode]",ii,kk-1,knew
        if (Wk(kk-2).lt.0.8*Wk(kk-1)) then
          knew = kk-2
          hnew = Hk(knew)
        else if (Wk(kk-1).lt.0.9*Wk(kk-2)) then
          knew = kk
          hnew = Hk(kk-1)*Ak(kk)/Ak(kk-1)
        else
          knew = kk-1
          hnew = Hk(knew) 
        end if
!       write(*,*) "[ode]",ii,kk-1,knew 
      ! Done
      ! Convergence testing
        if (E(kk-1).lt.1.) then 
          done             = .true.
          kdone            = kk-1
          ode_bs_k         = knew
          ode_dt_suggested = hnew
          solution_new(:)  = T(:,kk-1,kk-1)
        else 
          if (E(kk-1).gt.((n(kk)/n(1))**2)*((n(kk+1)/n(1))**2)) then
            ode_reject_step  = .true.
            ode_bs_k         = knew
            ode_dt_suggested = hnew
          end if 
        end if
      ! Done
    ! Testing convergence for row kk
      else if (ii.eq.kk) then
      ! Time step and k recipe
!       write(*,*) "[ode]",ii,kk,knew
        if (Wk(kk-1).lt.0.8*Wk(kk)) then
          knew = kk-1
          hnew = Hk(knew)
        else if (Wk(kk).lt.0.9*Wk(kk-1)) then
          knew = kk+1
          hnew = Hk(kk)*Ak(kk+1)/Ak(kk)
        else
          knew = kk
          hnew = Hk(knew) 
        end if
!       write(*,*) "[ode]",ii,kk,knew 
      ! Done
      ! Convergence testing
        if (E(kk).lt.1.) then
          done             = .true.
          kdone            = kk
          ode_bs_k         = knew
          ode_dt_suggested = hnew
          solution_new(:)  = T(:,kk,kk)
        else
          if (E(kk).gt.(n(kk+1)/n(1))**2) then
            ode_reject_step  = .true.
            ode_bs_k         = knew
            ode_dt_suggested = hnew
          end if 
        end if
    ! Testing convergence for row kk+1
      else if (ii.eq.kk+1) then
      ! Time step recipe
!       write(*,*) "[ode]",ii,kk+1,knew  
        if (Wk(kk-1).lt.0.8*Wk(kk)) then
          knew = kk-1
          hnew = Hk(knew)
        else if (Wk(kk).lt.0.9*Wk(kk-1)) then
          knew = kk+1
          hnew = Hk(kk)*Ak(kk+1)/Ak(kk)
        else
          knew = kk
          hnew = Hk(knew) 
        end if
!       write(*,*) "[ode]",ii,kk+1,knew  
      ! Convergence testing
        if (E(kk+1).le.1.) then
          done             = .true.
          kdone            = kk+1
          ode_bs_k         = knew
          ode_dt_suggested = hnew
          solution_new(:)  = T(:,kk+1,kk+1)
        else 
          ode_reject_step  = .true.
          ode_bs_k         = knew
          ode_dt_suggested = hnew
        end if
      end if
    end if
  ! Error control is done
  ! Now some other refinements
    if (ode_error_control) then 
      ode_bs_k = min(ode_bs_k,kmax-1)
      ode_bs_k = max(3,knew)
      if (hnew/time_step.ge.1./F) then
        hnew             = time_step / F
        ode_dt_suggested = hnew
      else if (hnew/time_step.le.F/S4) then
        hnew             = time_step*F/S4
        ode_dt_suggested = hnew
      end if
      if (ode_reject_step) then
        ode_dt_suggested = min(time_step,hnew)
        ode_bs_k         = min(knew,kk)
      end if
      ode_bs_k = min(ode_bs_k,kmax-1)
      ode_bs_k = max(3,knew) 
    end if
    if (done) exit
    if (ode_reject_step) then 
      ode_dt_suggested = 0.85*ode_dt_suggested
      exit
    end if 
  end do
! Saving solution if kmax is correct
! write(*,*) "[ode]",time_step,hnew,kk,ode_bs_k,ode_reject_step
  if (.not.ode_error_control) solution_new(:) = T(:,kmax,kmax)
  end subroutine ode_bs_step





  end module ode
