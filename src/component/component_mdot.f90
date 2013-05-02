  subroutine cp_get_mdot
  use component, only: cp_get_mdot_donor,cp_get_mdot_accretor
  implicit none
  call cp_get_mdot_donor
  call cp_get_mdot_accretor
  return
  end subroutine cp_get_mdot





  subroutine cp_get_mdot_donor
  use   physics, only: G,h,pi,electron_mass,nucleon_mass,&
                       ph_eggleton_formula,ph_norm
  use    driver, only: dr_mdot_flags
  use component, only: cp_donor_mass,cp_overflow_par,cp_donor_position,&
                       cp_L1_position,cp_binary_separation,cp_roche_radius,&
                       cp_mean_nucleons_per_electron,cp_mdot_0,cp_mdot_donor,&
                       cp_donor_mass,cp_accretor_mass,cp_binary_period,&
                       cp_donor_radius,cp_accretor_radius,cp_mass_ratio,&
                       cp_mdot_donor_max
  implicit none
  real :: x_L1,mu,a_2
  real :: factor_1,factor_2,factor_3,marsh_function

! All this for one measly numerical factor
  mu   = cp_donor_mass / (cp_donor_mass+cp_accretor_mass)
  x_L1 = ph_norm(cp_donor_position-cp_L1_position)
  x_L1 = x_L1 / cp_binary_separation
  a_2  = ( mu / (x_L1**3.0) ) + &
         ( (1.0-mu) / ( (1.0-x_L1)**3.0 ) ) 
  if (dr_mdot_flags) then
    write(*,*) "Monitoring Flags for M_dot are .true."
    write(*,*) "mu   = ",mu
    write(*,*) "x_L1 = ",x_L1
    write(*,*) "a_2  = ",a_2
  else
  end if
  factor_1 = 8.0*(pi**3.0) / 9.0
  if (factor_1.le.0.0) write(*,*) "Factor 1 has been FLAGGED on first pass"
  factor_2 = 5.0*G*electron_mass
  factor_2 = factor_2**(3.0/2.0)
  factor_2 = factor_2 / (h**3.0)
  if (factor_2.le.0.0) write(*,*) "Factor 2 has been FLAGGED on first pass"
  factor_3 = ( cp_mean_nucleons_per_electron*nucleon_mass )**(5.0/2.0)
  if (factor_3.le.0.0) write(*,*) "Factor 3 has been FLAGGED on first pass" 
  marsh_function = factor_1*factor_2*factor_3
  if (dr_mdot_flags) then
    write(*,*) "Monitoring First Pass"
    write(*,*) "f1   = ",factor_1
    write(*,*) "f2   = ",factor_2
    write(*,*) "f3   = ",factor_3
    write(*,*) "Prod = ",marsh_function
  else
  end if
! Sanity check
  if (marsh_function.le.0.0) then
    write(*,*) "The Marsh function is NULL on first flag, STOP"
    stop
  end if
  factor_1 = 1.0 / cp_binary_period
  if (factor_1.le.0.0) then 
  write(*,*) "Factor 1 has been FLAGGED on second pass"
  write(*,*) "Binary period = ",cp_binary_period
  end if
  factor_2 = (3.*mu*cp_donor_mass/(5.*ph_eggleton_formula(cp_mass_ratio)*cp_donor_radius))**(3./2.)
  if (factor_2.le.0.0) then 
  write(*,*) "Factor 2 has been FLAGGED on second pass"
  write(*,*) "Donor mass / total mass =",mu
  write(*,*) "Roche radius            =",cp_roche_radius
  end if
  factor_3 = ( abs(a_2*(a_2-1.0)) )**(-1.0/2.0)
  if (factor_3.le.0.0) then 
  write(*,*) "Factor 3 has been FLAGGED on second pass"
  write(*,*) "Lubow's X_L1 =",x_L1
  write(*,*) "Lubow's A_2  =",a_2
  end if
  marsh_function = marsh_function*factor_1*factor_2*factor_3
  if (dr_mdot_flags) then
    write(*,*) "Monitoring Second Pass"
    write(*,*) "f1   = ",factor_1
    write(*,*) "f2   = ",factor_2
    write(*,*) "f3   = ",factor_3
    write(*,*) "Prod = ",marsh_function
  else
  end if
! Sanity check
  if (marsh_function.le.0.0) then
    write(*,*) "The Marsh function is NULL on second flag, STOP"
    write(*,*) "STOP criteria is enforced at the routine get_mdot"
    stop
  end if
 
! Measly numerical factor calculated, on to M_dot
  cp_mdot_0     =   marsh_function * (cp_donor_radius**3.0)
  if (marsh_function.eq.0) write(*,*) "Marsh proportionality constant is null!"
! If the overflow is negative, there is no mass transfer
  if (cp_overflow_par.gt.0.0) then
  cp_mdot_donor = - marsh_function * (cp_overflow_par**3.0)
  else
  cp_mdot_donor = 0.0
  end if

  return
  end subroutine cp_get_mdot_donor





  subroutine cp_get_mdot_accretor
  use    driver, only: dr_eddington,dr_hyper_eddington,&
                       dr_accretion_flow,dr_is_sub_eddington,&
                       dr_is_super_eddington,dr_is_hyper_eddington
  use component, only: cp_mdot_donor,cp_mdot_accretor,cp_accretion_efficiency,&
                       cp_mdot_hyper_eddington
  implicit none
! Eddington flow
  if (dr_eddington) then
    call cp_super_eddington
    cp_mdot_accretor = - cp_accretion_efficiency * cp_mdot_donor
! Hyper Eddington flow
    if (dr_hyper_eddington) then 
      stop ":  HYPER EDDINGTON IS NOT FUNCTIONAL YET!"
      call cp_hyper_eddington ! This currently does nothing!
      if (cp_mdot_accretor.gt.cp_mdot_hyper_eddington) then 
        dr_accretion_flow = dr_is_hyper_eddington
      else
      end if
    else
    end if
! End of hyper Eddington flow
  else
! end of Eddington flow
! If Eddington is disabled
    dr_accretion_flow       = dr_is_sub_eddington
    call cp_super_eddington
    cp_accretion_efficiency = 1.0
    cp_mdot_accretor        = - cp_mdot_donor
  end if 
  return
  end subroutine cp_get_mdot_accretor
