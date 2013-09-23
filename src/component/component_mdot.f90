  subroutine cp_get_mdot
  use cp_interface, only: cp_get_mdot_donor,cp_get_mdot_accretor
  implicit none
  call cp_get_mdot_donor
  call cp_get_mdot_accretor
  return
  end subroutine cp_get_mdot





  subroutine cp_get_mdot_donor
  use ph_vars, only: ph_G,ph_h,ph_pi,ph_me,ph_amu
  use dr_vars, only: dr_mdot_flags
  use cp_vars, only: cp_don_mass,cp_overflow_par,cp_don_pos,&
                     cp_L1_pos,cp_bin_sepa,cp_roche_radius,&
                     cp_stream_mu_e,cp_mdot_0,cp_don_mdot,&
                     cp_don_mass,cp_acc_mass,cp_bin_peri,&
                     cp_don_radius,cp_acc_radius,cp_mass_ratio,&
                     cp_mdot_max
  use ph_interface, only: ph_eggleton_formula,ph_norm
  implicit none
  real :: x_L1,mu,a_2
  real :: factor_1,factor_2,factor_3,marsh_function
! Exiting if not filling lobe
  if (cp_roche_radius.gt.cp_don_radius) then 
    cp_don_mdot = 0.
    return
  end if
! All this for one measly numerical factor
  mu   = cp_don_mass / (cp_don_mass+cp_acc_mass)
  x_L1 = ph_norm(cp_don_pos-cp_L1_pos)
  x_L1 = x_L1 / cp_bin_sepa
  a_2  = ( mu / (x_L1**3.0) ) + &
         ( (1.0-mu) / ( (1.0-x_L1)**3.0 ) ) 
  dr_mdot_flags = .false.
  if (dr_mdot_flags) then
    write(*,*) "Monitoring Flags for M_dot are .true."
    write(*,*) "mu   = ",mu
    write(*,*) "x_L1 = ",x_L1
    write(*,*) "a_2  = ",a_2
  else
  end if
  factor_1 = 8.0*(ph_pi**3.0) / 9.0
  if (factor_1.le.0.0) write(*,*) "Factor 1 has been flagged on first pass"
  factor_2 = 5.0*ph_G*ph_me
  factor_2 = factor_2**(3.0/2.0)
  factor_2 = factor_2 / (ph_h**3.0)
  if (factor_2.le.0.0) write(*,*) "Factor 2 has been flagged on first pass"
  factor_3 = ( cp_stream_mu_e*ph_amu )**(5.0/2.0)
  if (factor_3.le.0.0) write(*,*) "Factor 3 has been flagged on first pass" 
  marsh_function = factor_1*factor_2*factor_3
  if (dr_mdot_flags) then
    write(*,*) "Monitoring First Pass"
    write(*,*) "8*pi^3/9         = ",factor_1
    write(*,*) "(5*G*me/h^2)^3/2 = ",factor_2
    write(*,*) "mu_e*amu         = ",factor_3
    write(*,*) "Prod             = ",marsh_function
  else
  end if
! Sanity check
  if (marsh_function.le.0.0) then
    write(*,*) "The Marsh function is NULL on first flag, STOP"
    stop
  end if
  factor_1 = 1.0 / cp_bin_peri
  if (factor_1.le.0.0) then 
  write(*,*) "Factor 1 has been flagged on second pass"
  write(*,*) "Binary peri = ",cp_bin_peri
  end if
  factor_2 = (3.*mu*cp_don_mass/(5.*ph_eggleton_formula(cp_mass_ratio)*cp_don_radius))**(3./2.)
  if (factor_2.le.0.0) then 
  write(*,*) "Factor 2 has been flagged on second pass"
  write(*,*) "Donor mass / total mass =",mu
  write(*,*) "Roche radius            =",cp_roche_radius
  end if
  factor_3 = ( abs(a_2*(a_2-1.0)) )**(-1.0/2.0)
  if (factor_3.le.0.0) then 
  write(*,*) "Factor 3 has been flagged on second pass"
  write(*,*) "Lubow's X_L1 =",x_L1
  write(*,*) "Lubow's A_2  =",a_2
  end if
  marsh_function = marsh_function*factor_1*factor_2*factor_3
  if (dr_mdot_flags) then
    write(*,*) "Monitoring Second Pass"
    write(*,*) "1/P                   = ",factor_1
    write(*,*) "(3*mu*M2/5*rL*R2)^3/2 = ",factor_2
    write(*,*) "1/sqrt(a2*(a2-1))     = ",factor_3
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
  cp_mdot_0     =   marsh_function * (cp_don_radius**3.0)
  if (marsh_function.eq.0) write(*,*) "Marsh proportionality constant is null!"
! If the overflow is negative, there is no mass transfer
  if (cp_overflow_par.gt.0.0) then
  cp_don_mdot = - marsh_function * (cp_overflow_par**3.0)
  else
  cp_don_mdot = 0.0
  end if

  return
  end subroutine cp_get_mdot_donor





  subroutine cp_get_mdot_accretor
  use dr_vars, only: dr_eddington,dr_hyper_eddington,&
                     dr_accretion_flow,dr_is_sub_eddington,&
                     dr_is_super_eddington,dr_is_hyper_eddington
  use cp_vars, only: cp_don_mdot,cp_acc_mdot,cp_accretion_eff,&
                     cp_mdot_hyper_edd
  implicit none
! Eddington flow
  if (dr_eddington) then
    call cp_super_eddington
    cp_acc_mdot = - cp_accretion_eff * cp_don_mdot
! Hyper Eddington flow
    if (dr_hyper_eddington) then 
      stop ":  HYPER EDDINGTON IS NOT FUNCTIONAL YET!"
      call cp_hyper_edd ! This currently does nothing!
      if (cp_acc_mdot.gt.cp_mdot_hyper_edd) then 
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
    cp_accretion_eff = 1.0
    cp_acc_mdot        = - cp_don_mdot
  end if 
  return
  end subroutine cp_get_mdot_accretor
