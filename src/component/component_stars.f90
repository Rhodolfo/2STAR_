subroutine cp_star_parameters
  use   physics, only: ph_wd_radius,ph_wd_zeta,ph_wd_k_factor,ph_wd_k_zeta,&
                       ph_ns_radius,ph_ns_zeta,ph_ns_k_factor,ph_ns_k_zeta,&
                       ph_eggleton_formula,pi
  use component, only: cp_donor_mass, cp_accretor_mass,&
                       cp_donor_radius, cp_accretor_radius,&
                       cp_donor_zeta, cp_accretor_zeta, &
                       cp_donor_k_factor,cp_accretor_k_factor,&
                       cp_donor_k_zeta, cp_accretor_k_zeta, &
                       cp_donor_density, cp_accretor_density, &
                       cp_roche_limit,cp_contact_limit,cp_mass_ratio
  use    driver, only: dr_donor_mode, dr_accretor_mode,dr_exit_trigger,dr_abort,&
                       dr_mode_he_white_dwarf, dr_mode_co_white_dwarf, dr_mode_neutron_star
  use        IO, only: IO_log,IO_2string
  implicit none
  character(len=2) :: comp
  if ( (dr_donor_mode.eq.dr_mode_he_white_dwarf).or.(dr_donor_mode.eq.dr_mode_co_white_dwarf) ) then
    if (dr_donor_mode.eq.dr_mode_he_white_dwarf) comp = "he"  
    if (dr_donor_mode.eq.dr_mode_co_white_dwarf) comp = "co"
    cp_donor_radius   = ph_wd_radius(cp_donor_mass,comp)
    cp_donor_zeta     = ph_wd_zeta(cp_donor_mass,comp)
    cp_donor_k_factor = ph_wd_k_factor(cp_donor_mass) 
    cp_donor_k_zeta   = ph_wd_k_zeta(cp_donor_mass)
  else if (dr_donor_mode.eq.dr_mode_neutron_star) then 
    cp_donor_radius   = ph_ns_radius(cp_donor_mass)
    cp_donor_zeta     = ph_ns_zeta(cp_donor_mass)
    cp_donor_k_factor = ph_ns_k_factor(cp_donor_mass) 
    cp_donor_k_zeta   = ph_ns_k_zeta(cp_donor_mass) 
  else
    call dr_abort("cp_star_parameters","Invalid mode for donor star")
  end if
  cp_donor_density = cp_donor_mass / ( (4.0*pi/3.0)*(cp_donor_radius**3) )
! Sanity check
  if (cp_donor_radius.le.0.) then
    call dr_abort("cp_star_parameters","Donor radius is less than or equal to zero")
  end if
  
! Now let's calculate some parameters pertaining to the NS
  if ( (dr_accretor_mode.eq.dr_mode_he_white_dwarf).or.(dr_accretor_mode.eq.dr_mode_co_white_dwarf) ) then
    if (dr_donor_mode.eq.dr_mode_he_white_dwarf) comp = "he"  
    if (dr_donor_mode.eq.dr_mode_co_white_dwarf) comp = "co"
    cp_accretor_radius   = ph_wd_radius(cp_accretor_mass,comp)
    cp_accretor_zeta     = ph_wd_zeta(cp_accretor_mass,comp)
    cp_accretor_k_factor = ph_wd_k_factor(cp_accretor_mass) 
    cp_accretor_k_zeta   = ph_wd_k_zeta(cp_accretor_mass)
  else if (dr_accretor_mode.eq.dr_mode_neutron_star) then 
    cp_accretor_radius   = ph_ns_radius(cp_accretor_mass)
    cp_accretor_zeta     = ph_ns_zeta(cp_accretor_mass)
    cp_accretor_k_factor = ph_ns_k_factor(cp_accretor_mass) 
    cp_accretor_k_zeta   = ph_ns_k_zeta(cp_accretor_mass) 
  else
    call dr_abort("cp_star_parameters","Invalid mode for accretor star") 
  end if
  cp_accretor_density = cp_accretor_mass / ( (4.0*pi/3.0)*(cp_accretor_radius**3) )
! Sanity check
  if (cp_accretor_radius.le.0.) then
    call dr_abort("cp_star_parameters","Accretor radius is less than or equal to zero")
  end if
 
! Setting the mass ratio and other stuff
  cp_mass_ratio        = cp_donor_mass / cp_accretor_mass
  cp_roche_limit       = 2.0*(cp_mass_ratio**(1.0/3.0))*cp_donor_radius
  cp_contact_limit     = cp_donor_radius / ph_eggleton_formula(cp_mass_ratio)

return
end subroutine cp_star_parameters
