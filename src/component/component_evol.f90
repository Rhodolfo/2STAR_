  subroutine cp_evolution_coefficients
  use component, only: cp_driver_terms,cp_zeta_terms,cp_timescales
  implicit none
  call cp_driver_terms
  call cp_zeta_terms
  call cp_timescales
  return
  end subroutine cp_evolution_coefficients





  subroutine cp_driver_terms
  use physics  , only: ph_kepler_jorb,ph_grw_jdot
  use  driver  , only: dr_include_tides
  use component, only: cp_binary_separation,cp_binary_frequency,cp_donor_mass,cp_accretor_mass,&
                       cp_donor_radius,cp_accretor_radius,cp_donor_freq,cp_accretor_freq,&
                       cp_donor_k_factor,cp_accretor_k_factor,cp_donor_sync_freq,cp_accretor_sync_freq,&
                       cp_driver_separation,cp_driver_roche,cp_driver_donor_radius,cp_driver_donor_freq,cp_driver_accretor_freq
  implicit none
  real :: j_orb,j_don,j_acc,j_sys,jdot_grw,jdot_sys
  real :: jdot_tid_don,jdot_tid_acc,jdot_tid

! Driver terms are those that enter into the evolution equations 
! for the separation and the spins regardless of the mass transfer rate

! Angular momentum of components and system
  j_orb = ph_kepler_jorb(cp_donor_mass,cp_accretor_mass,cp_binary_separation)
  j_don = cp_donor_k_factor*cp_donor_mass*(cp_donor_radius**2)*cp_donor_freq
  j_acc = cp_accretor_k_factor*cp_accretor_mass*(cp_accretor_radius**2)*cp_accretor_freq 
  j_sys = j_orb + j_acc + j_don
! Gravitational wave angular momentum loss
  jdot_grw     = ph_grw_jdot(cp_donor_mass,cp_accretor_mass,cp_binary_separation)
  jdot_sys     = jdot_grw ! Mass loss is incorporated in the zeta terms, since it goes as mdot_donor
! Handle tidal terms
  if (dr_include_tides) then 
    jdot_tid_don = cp_donor_sync_freq*cp_donor_k_factor*&
                   cp_donor_mass*(cp_donor_radius**2)*(cp_binary_frequency-cp_donor_freq)
    jdot_tid_acc = cp_accretor_sync_freq*cp_accretor_k_factor*&
                   cp_accretor_mass*(cp_accretor_radius**2)*(cp_binary_frequency-cp_accretor_freq)
    jdot_tid     = jdot_tid_don + jdot_tid_acc
  else 
    jdot_tid_don = 0.
    jdot_tid_acc = 0.
    jdot_tid     = 0.
  end if  
  cp_driver_separation = 2.0*(jdot_sys-jdot_tid)/j_orb

! Roche lobe driver
  cp_driver_roche = cp_driver_separation

! Donor driver
  cp_driver_donor_radius = 0.0

! Driver for the spins
  cp_driver_donor_freq    = (cp_binary_frequency-cp_donor_freq)*cp_donor_sync_freq
  cp_driver_accretor_freq = (cp_binary_frequency-cp_accretor_freq)*cp_accretor_sync_freq

  return
  end subroutine cp_driver_terms





  subroutine cp_zeta_terms
  use   physics, only: ph_norm,ph_kepler_jorb,ph_eggleton_formula_zeta,G
  use    driver, only: dr_accretion_flow, dr_is_sub_eddington,dr_is_super_eddington,&
                       dr_ignore_donor_terms,dr_eddington,dr_include_tides,&
                       dr_advection_mode,dr_mode_direct_impact,dr_mode_disk_accretion,dr_mode_adaptative
  use component, only: cp_binary_separation,cp_binary_frequency,&
                       cp_ejection_efficiency,cp_accretion_efficiency,cp_disk_efficiency,&
                       cp_donor_position,cp_initial_position,&
                       cp_mass_ratio,cp_donor_mass,cp_accretor_mass,&
                       cp_donor_radius,cp_accretor_radius,&
                       cp_ang_radius,cp_circularization_radius,cp_min_radius,&
                       cp_donor_freq,cp_accretor_freq,cp_q_stable,cp_q_a,&
                       cp_donor_zeta,cp_donor_k_factor,cp_donor_k_zeta,&
                       cp_accretor_zeta,cp_accretor_k_factor,cp_accretor_k_zeta,&
                       cp_zeta_separation,cp_zeta_roche,cp_zeta_donor_freq,cp_zeta_accretor_freq
  implicit none
  real :: dum,factor_1,factor_2,advection_term,lambda
  real :: j_2,j_2_D,j_1_D,j_1,j_eje,j_orb

  j_orb = ph_kepler_jorb(cp_donor_mass,cp_accretor_mass,cp_binary_separation)
! Let's calculate the contribution to q_a due simply to mass ejection 
! (it's 1 if there is none)
  if ( (dr_accretion_flow.eq.dr_is_sub_eddington).or.(dr_eddington.eqv..false.) ) then
    factor_1 = 1.0
  else if ( (dr_accretion_flow.eq.dr_is_super_eddington).and.dr_eddington ) then
! My prescription
    factor_1 = 1.0 + (1.0-cp_accretion_efficiency)*cp_mass_ratio &
             - cp_ejection_efficiency*cp_mass_ratio / (2.0*(1.0+cp_mass_ratio)) 
  else
    write(*,*) "Invalid value for dr_accretion_flow"
    stop
  end if

! Now to calculate the form of the angular momenta, depending on what kind of
! angular momentum transfer flow we are envisioning
! The angular momentum of the stream as it leaves the donor j_2 = r x v
  dum = ph_norm(cp_donor_position-cp_initial_position)
  j_2 = (cp_donor_radius**2)*cp_binary_frequency
  if (dr_ignore_donor_terms) j_2 = 0.0
! Here's where things start to get tricky, we need to distinguish between
! direct impact and disk accretion
  if (cp_accretor_radius.gt.cp_min_radius) cp_ang_radius = cp_circularization_radius
  if (cp_accretor_radius.le.cp_min_radius) cp_ang_radius = cp_accretor_radius  
! In direct impact accretion, the stream will have the angular momentum
! corresponding to the circularization radius due to conservation laws.
! Since there is no disk, I set these angular momentum contributions to zero
  if       (dr_advection_mode.eq.dr_mode_direct_impact) then
! Then, the matter as it hits the disk: j_2_D
! Then, the matter as it leaves the disk: j_1_D
! Lastly, the matter as it hits the accretor: j_1 
    j_2_D = 0.0
    j_1_D = 0.0
    j_1   = sqrt(G*cp_accretor_mass*cp_circularization_radius)
! I will assume that the stream will carry the angular momentum corresponding to
! the circularization radius
  else if (dr_advection_mode.eq.dr_mode_disk_accretion) then
! Then, the matter as it hits the disk: j_2_D
! Then, the matter as it leaves the disk: j_1_D
! Lastly, the matter as it hits the accretor: j_1
    j_2_D = 0.0 !qrt(G*cp_accretor_mass*circularization_radius)
    j_1_D = 0.0 !qrt(G*cp_accretor_mass*accretion_radius)
    j_1   = sqrt(G*cp_accretor_mass*cp_accretor_radius)
  else if (dr_advection_mode.eq.dr_mode_adaptative) then
    j_2_D = 0.0 !qrt(G*cp_accretor_mass*circularization_radius)
    j_1_D = 0.0 !qrt(G*cp_accretor_mass*accretion_radius)
    j_1   = sqrt(G*cp_accretor_mass*cp_ang_radius)
! We are done
  else
    write(*,*) "Invalid value for dr_advection_mode"
    stop
  end if
! What about the angular momentum of the ejected matter?
  j_eje = j_1
! Let's add all the advection terms
  if ( (dr_accretion_flow.eq.dr_is_sub_eddington).or.(dr_eddington.eqv..false.) ) then
  advection_term = j_2 - j_2_D + j_1_D - j_1 
  else if ( (dr_accretion_flow.eq.dr_is_super_eddington).and.dr_eddington ) then
  advection_term = j_2 - j_2_D &
                 + (1.0-cp_disk_efficiency)*j_1_D - cp_accretion_efficiency*j_1 &
                 - cp_ejection_efficiency*j_eje
  else
    write(*,*) "Invalid value for dr_accretion_flow"
    stop
  end if
! Putting it all together
  factor_2 = cp_donor_mass*advection_term/j_orb

! Put it all in the q_a
! Basically q_a = 1 + advection_term*cp_donor_mass / j_orb
  cp_q_a = factor_1 + factor_2 
! q_a = 1.0 - sqrt(r_h*(1.0+mass_ratio))  
  cp_zeta_separation = - 2.0*(cp_q_a - cp_mass_ratio)

! Roche lobe zeta
  cp_zeta_roche = cp_zeta_separation + ph_eggleton_formula_zeta(cp_mass_ratio,cp_accretion_efficiency)

! Donor and accretor logarithmic derivatives have already been calculated
! cp_donor_zeta = already calculated

! Calculating zeta terms for the spins
  if (dr_include_tides) then 
    lambda = 1. + cp_donor_k_zeta + 2.*cp_donor_zeta
    cp_zeta_donor_freq    = (j_2/cp_donor_k_factor*(cp_donor_radius*2))-lambda*cp_donor_freq
    lambda = 1. + cp_accretor_k_zeta + 2.*cp_accretor_zeta 
    cp_zeta_accretor_freq = (j_1/cp_accretor_k_factor*(cp_accretor_radius*2))-lambda*cp_accretor_freq
  else
    cp_zeta_donor_freq    = 0.
    cp_zeta_accretor_freq = 0.
  end if

! Calculating q_stable
  cp_q_stable = cp_q_a + 0.5*(cp_donor_zeta-ph_eggleton_formula_zeta(cp_mass_ratio,cp_accretion_efficiency))

  return
  end subroutine cp_zeta_terms
