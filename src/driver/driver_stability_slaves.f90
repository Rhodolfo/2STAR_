  function dr_critical_mass(acc_mass,don_mass_lo,don_mass_hi,criterion,error)
  use ph_vars, only: ph_msun,ph_year
  use dr_vars, only: dr_mode_contact,dr_mode_mdot,dr_integration_mode,dr_dif_mass_don
  use cp_vars, only: cp_mdot_eq,cp_mdot_edd,cp_mass_ratio,cp_min_radius,&
                     cp_acc_radius,cp_don_zeta,cp_zeta_roche,cp_q_a,cp_q_stable
  use io_vars, only: IO_save,IO_verb
  use cp_interface, only: cp_init
  implicit none
! Input and output
  integer :: criterion
  real    :: acc_mass,don_mass_lo,don_mass_hi,error
  real    :: dr_critical_mass
! Auxiliaries
  integer :: n
  integer :: mode             = dr_mode_contact
  integer :: T_T_CRITERION    = 0 
  integer :: EDD_CRITERION    = 1
  integer :: DIS_CRITERION    = 2
  integer :: UNS_CRITERION    = 3
  real    :: low_d,mid_d,hig_d,dm
  real    :: low_sign,mid_sign,hig_sign
  real    :: low_crit_variable,low_crit_target
  real    :: mid_crit_variable,mid_crit_variable_old,mid_crit_target
  real    :: hig_crit_variable,hig_crit_target
  logical :: exit_condition,save_temp
  real    :: factor,tolerance

! Flags for the module
  tolerance           = 1.e-3
  dr_integration_mode = dr_mode_mdot
  save_temp           = IO_save
  IO_save             = .false.
  IO_verb             = .false.

  n      = 0
  factor = 1.0
  low_d = don_mass_lo
  hig_d = don_mass_hi
 
! Things specific for Eddington
  if (criterion.eq.EDD_CRITERION) then 
    factor = ph_year/ph_msun
    hig_d  = don_mass_hi
    dm     = dr_dif_mass_don / 100.
    call cp_init(mode,hig_d*ph_msun,acc_mass*ph_msun,0.,0.,0.,0.) 
    do while (cp_mdot_eq.ge.0.)
      hig_d = hig_d - dm 
      call cp_init(mode,hig_d*ph_msun,acc_mass*ph_msun,0.,0.,0.,0.)
    end do  
    if (abs(cp_mdot_eq).lt.cp_mdot_edd) then
      dr_critical_mass = hig_d
      IO_save          = save_temp
      return
    end if
  end if

! Moving on to a midpoint method
  exit_condition = .false.
  do while (.not.exit_condition)

! Low mass first
    call cp_init(mode,low_d*ph_msun,acc_mass*ph_msun,0.,0.,0.,0.)
    if      (criterion.eq.T_T_CRITERION) then
      low_crit_variable = cp_mass_ratio
      low_crit_target   = cp_q_stable
    else if (criterion.eq.EDD_CRITERION) then
      low_crit_variable = cp_mdot_eq
      low_crit_target   = - cp_mdot_edd
    else if (criterion.eq.DIS_CRITERION) then
      low_crit_variable = cp_acc_radius
      low_crit_target   = cp_min_radius
    else if (criterion.eq.UNS_CRITERION) then
      low_crit_variable = cp_don_zeta - cp_zeta_roche
      low_crit_target   = 0.
    end if
    low_sign = sign(1.0,low_crit_variable-low_crit_target)

! High mass second 
    call cp_init(mode,hig_d*ph_msun,acc_mass*ph_msun,0.,0.,0.,0.)
    if (criterion.eq.T_T_CRITERION) then
      hig_crit_variable = cp_mass_ratio
      hig_crit_target   = cp_q_stable
    else if (criterion.eq.EDD_CRITERION) then
      hig_crit_variable = cp_mdot_eq
      hig_crit_target   = - cp_mdot_edd
    else if (criterion.eq.DIS_CRITERION) then
      hig_crit_variable = cp_acc_radius
      hig_crit_target   = cp_min_radius
    else if (criterion.eq.UNS_CRITERION) then
      hig_crit_variable = cp_don_zeta - cp_zeta_roche
      hig_crit_target   = 0. 
    end if
    hig_sign = sign(1.0,hig_crit_variable-hig_crit_target)

! Midpoint follows
    mid_d         = (low_d+hig_d)/2.0
    call cp_init(mode,mid_d*ph_msun,acc_mass*ph_msun,0.,0.,0.,0.)
    if (criterion.eq.T_T_CRITERION) then
      mid_crit_variable = cp_mass_ratio
      mid_crit_target   = cp_q_stable
    else if (criterion.eq.EDD_CRITERION) then
      mid_crit_variable = cp_mdot_eq
      mid_crit_target   = - cp_mdot_edd
    else if (criterion.eq.DIS_CRITERION) then
      mid_crit_variable = cp_acc_radius
      mid_crit_target   = cp_min_radius
    else if (criterion.eq.UNS_CRITERION) then
      mid_crit_variable = cp_don_zeta - cp_zeta_roche
      mid_crit_target   = 0. 
    end if
    mid_sign = sign(1.0,mid_crit_variable-mid_crit_target)

! Let's see if we reach an exit condition
    if (criterion.eq.T_T_CRITERION) then
      if (abs(cp_q_stable-cp_mass_ratio).lt.tolerance) exit_condition = .true. ! We can be very precise here
    else if (criterion.eq.EDD_CRITERION) then
      if (abs(cp_mdot_eq+cp_mdot_edd)/cp_mdot_edd.lt.tolerance) exit_condition = .true.
    else if (criterion.eq.DIS_CRITERION) then
      if (abs(cp_min_radius-cp_acc_radius)/cp_acc_radius.lt.tolerance) exit_condition = .true.
    else if (criterion.eq.UNS_CRITERION) then
      if (cp_mdot_eq.lt.0..and.cp_mdot_eq.lt.-cp_mdot_edd/tolerance) exit_condition = .true.
    end if
    n = n + 1
    if (n.gt.300) exit_condition = .true.
 
! Now the midpoint scheme chooses a good value
    if (mid_sign.eq.low_sign) low_d = mid_d
    if (mid_sign.eq.hig_sign) hig_d = mid_d 
!   write(*,*) n,mid_d,mid_crit_target*factor,mid_crit_variable*factor
    if (mid_crit_variable-mid_crit_variable_old.lt.tolerance.and.n.gt.10) exit_condition = .true.
    mid_crit_variable_old = mid_crit_variable

  end do

  dr_critical_mass = mid_d
  IO_save          = save_temp
  error            = mid_crit_variable - mid_crit_target

  if      (criterion.eq.T_T_CRITERION) then 
!   write(*,*) "Target   q      =",cp_q_stable  ," cp_mdot_eq  =",cp_mdot_eq*ph_year/ph_msun
!   write(*,*) "Obtained q      =",cp_mass_ratio," cp_mdot_edd =",-cp_mdot_edd*ph_year/ph_msun
!   write(*,*) "         cp_q_a    =",cp_q_a," r_acc =",cp_acc_radius/binary_sepa,"r_cir =",cir_radius/binary_sepa
!   write(*,*) "         cp_q_a_nd =",1.+donor_mass*(j_2-sqrt(G*accretor_mass*cir_radius))/j_orb 
!   write(*,*) "         cp_q_a_di =",1.+donor_mass*(j_2-sqrt(G*accretor_mass*cp_acc_radius))/j_orb 
  else if (criterion.eq.EDD_CRITERION) then
    error = error*ph_year/ph_msun
!   write(*,*) "Target   cp_mdot =",error(2)
!   write(*,*) "Obtained cp_mdot =",error(1)
  else if (criterion.eq.DIS_CRITERION) then
!   write(*,*) "Target   radius =",cp_min_radius
!   write(*,*) "Obtained radius =",cp_acc_radius
  else if (criterion.eq.UNS_CRITERION) then
!   write(*,*) "Target   zeta   =",0.,"  cp_mdot_eq =",cp_mdot_eq*ph_year/ph_msun
!   write(*,*) "Obtained zeta   =",cp_don_zeta - cp_zeta_roche
  end if
!   write(*,*) "Mass            =",don_mass 
  return
  end function dr_critical_mass
