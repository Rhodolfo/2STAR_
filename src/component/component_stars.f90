  subroutine cp_star_parameters
  use ph_vars, only: ph_pi
  use cp_vars, only: cp_don_mass, cp_acc_mass,&
                     cp_don_radius, cp_acc_radius,&
                     cp_don_zeta, cp_acc_zeta, &
                     cp_don_k_factor,cp_acc_k_factor,&
                     cp_don_k_zeta, cp_acc_k_zeta, &
                     cp_don_dens, cp_acc_dens, &
                     cp_roche_limit,cp_contact_limit,cp_mass_ratio
  use dr_vars, only: dr_donor_mode, dr_accretor_mode,dr_exit_trigger,&
                     dr_mode_he_white_dwarf, dr_mode_co_white_dwarf, dr_mode_neutron_star
  use ph_interface, only: ph_wd_radius,ph_wd_zeta,ph_wd_k_factor,ph_wd_k_zeta,&
                          ph_ns_radius,ph_ns_zeta,ph_ns_k_factor,ph_ns_k_zeta,&
                          ph_eggleton_formula
  use dr_interface, only: dr_abort
  use io_interface, only: io_log,io_2string
  implicit none
  character(len=2) :: comp
  if ( (dr_donor_mode.eq.dr_mode_he_white_dwarf).or.(dr_donor_mode.eq.dr_mode_co_white_dwarf) ) then
    if (dr_donor_mode.eq.dr_mode_he_white_dwarf) comp = "he"  
    if (dr_donor_mode.eq.dr_mode_co_white_dwarf) comp = "co"
    cp_don_radius   = ph_wd_radius(cp_don_mass,comp)
    cp_don_zeta     = ph_wd_zeta(cp_don_mass,comp)
    cp_don_k_factor = ph_wd_k_factor(cp_don_mass) 
    cp_don_k_zeta   = ph_wd_k_zeta(cp_don_mass)
  else if (dr_donor_mode.eq.dr_mode_neutron_star) then 
    cp_don_radius   = ph_ns_radius(cp_don_mass)
    cp_don_zeta     = ph_ns_zeta(cp_don_mass)
    cp_don_k_factor = ph_ns_k_factor(cp_don_mass) 
    cp_don_k_zeta   = ph_ns_k_zeta(cp_don_mass) 
  else
    call dr_abort("cp_star_parameters","Invalid mode for donor star")
  end if
  cp_don_dens = cp_don_mass / ( (4.0*ph_pi/3.0)*(cp_don_radius**3) )
! Sanity check
  if (cp_don_radius.le.0.) then
    call io_log("[cp_star] Warning: Donor radius is less than or equal to zero")
  end if
  
! Now let's calculate some parameters pertaining to the NS
  if ( (dr_accretor_mode.eq.dr_mode_he_white_dwarf).or.(dr_accretor_mode.eq.dr_mode_co_white_dwarf) ) then
    if (dr_donor_mode.eq.dr_mode_he_white_dwarf) comp = "he"  
    if (dr_donor_mode.eq.dr_mode_co_white_dwarf) comp = "co"
    cp_acc_radius   = ph_wd_radius(cp_acc_mass,comp)
    cp_acc_zeta     = ph_wd_zeta(cp_acc_mass,comp)
    cp_acc_k_factor = ph_wd_k_factor(cp_acc_mass) 
    cp_acc_k_zeta   = ph_wd_k_zeta(cp_acc_mass)
  else if (dr_accretor_mode.eq.dr_mode_neutron_star) then 
    cp_acc_radius   = ph_ns_radius(cp_acc_mass)
    cp_acc_zeta     = ph_ns_zeta(cp_acc_mass)
    cp_acc_k_factor = ph_ns_k_factor(cp_acc_mass) 
    cp_acc_k_zeta   = ph_ns_k_zeta(cp_acc_mass) 
  else
    call dr_abort("cp_star","Invalid mode for accretor star") 
  end if
  cp_acc_dens = cp_acc_mass / ( (4.0*ph_pi/3.0)*(cp_acc_radius**3) )
! Sanity check
  if (cp_acc_radius.le.0.) then
    call io_log("[cp_star] Warning: Accretor radius is less than or equal to zero")
  end if
 
! Setting the mass ratio and other stuff
  cp_mass_ratio        = cp_don_mass / cp_acc_mass
  cp_roche_limit       = 2.0*(cp_mass_ratio**(1.0/3.0))*cp_don_radius
  cp_contact_limit     = cp_don_radius / ph_eggleton_formula(cp_mass_ratio)

  return
  end subroutine cp_star_parameters
