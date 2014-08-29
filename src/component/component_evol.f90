  subroutine cp_evolution_coefficients
  use cp_interface, only: cp_driver_terms,cp_zeta_terms,cp_timescales
  implicit none
  call cp_driver_terms
  call cp_zeta_terms
  call cp_timescales
  call cp_get_pdots
  return
  end subroutine cp_evolution_coefficients





  subroutine cp_driver_terms
  use ph_vars, only: ph_G,ph_pi
  use dr_vars, only: dr_include_tides
  use cp_vars, only: cp_bin_sepa,cp_bin_freq,cp_don_mass,cp_acc_mass,&
                     cp_don_radius,cp_acc_radius,cp_don_freq,cp_acc_freq,&
                     cp_don_k_factor,cp_acc_k_factor,cp_don_sync_time,cp_acc_sync_time,&
                     cp_driver_sepa,cp_driver_roche,cp_driver_don_radius,&
                     cp_driver_don_freq,cp_driver_acc_freq,&
                     cp_driver_sepa_grw,cp_driver_sepa_dontid,cp_driver_sepa_acctid,&
                     cp_env_mass,cp_env_radius,cp_driver_drag,cp_driver_reso,&
                     cp_driver_drag_norm,cp_driver_reso_norm,&
                     cp_driver_nodrag,cp_drag_vel,cp_drag_dens,cp_drag_jorb,cp_drag_area
  use ph_interface, only: ph_kepler_jorb,ph_grw_jdot,ph_eggleton_L1
  implicit none
  real :: j_orb,j_don,j_acc,j_sys,jdot_grw,jdot_sys
  real :: jdot_tid_don,jdot_tid_acc,jdot_tid
  real :: rhoave,vorb,venv,area,q_d,xL1,torque,adot_drag,angmom,mu1,mu2,coeff,adot_reso

! Driver terms are those that enter into the evolution equations 
! for the sepa and the spins regardless of the mass transfer rate

! Angular momentum of components and system
  j_orb = ph_kepler_jorb(cp_don_mass,cp_acc_mass,cp_bin_sepa)
  j_don = cp_don_k_factor*cp_don_mass*(cp_don_radius**2)*cp_don_freq
  j_acc = cp_acc_k_factor*cp_acc_mass*(cp_acc_radius**2)*cp_acc_freq 
  j_sys = j_orb + j_acc + j_don
! Gravitational wave angular momentum loss
  jdot_grw     = ph_grw_jdot(cp_don_mass,cp_acc_mass,cp_bin_sepa)
  jdot_sys     = jdot_grw ! Mass loss is incorporated in the zeta terms, since it goes as don_mdot
! Handle tidal terms
  if (dr_include_tides) then 
    jdot_tid_don = cp_don_k_factor*&
                   cp_don_mass*(cp_don_radius**2)*(cp_bin_freq-cp_don_freq)/&
                   cp_don_sync_time
    jdot_tid_acc = cp_acc_k_factor*&
                   cp_acc_mass*(cp_acc_radius**2)*(cp_bin_freq-cp_acc_freq)/&
                   cp_acc_sync_time
    jdot_tid     = jdot_tid_don + jdot_tid_acc
  else 
    jdot_tid_don = 0.
    jdot_tid_acc = 0.
    jdot_tid     = 0.
  end if  
  cp_driver_sepa = 2.0*(jdot_sys-jdot_tid)/j_orb


! Roche lobe driver
  cp_driver_roche = cp_driver_sepa

! Donor driver
  cp_driver_don_radius = 0.0

! Driver for the spins
  cp_driver_don_freq = (cp_bin_freq-cp_don_freq)/cp_don_sync_time
  cp_driver_acc_freq = (cp_bin_freq-cp_acc_freq)/cp_acc_sync_time

! Separating out just for data output
  cp_driver_sepa_grw    =   2.0*jdot_sys    /j_orb
  cp_driver_sepa_dontid = - 2.0*jdot_tid_don/j_orb
  cp_driver_sepa_acctid = - 2.0*jdot_tid_acc/j_orb

! Initialize these guys
  cp_driver_drag = 0.
  cp_driver_reso = 0.

! Drag term
  q_d         = cp_don_mass / cp_acc_mass
  vorb        = cp_bin_freq* q_d     *cp_bin_sepa
  venv        = cp_bin_freq*(1.- q_d)*cp_bin_sepa
  torque      = 0.
  area        = ph_pi*(cp_don_radius**2)
  if ((cp_env_mass.gt.0.).and.(cp_env_radius.gt.cp_acc_radius)) then 
    rhoave    = (cp_env_mass/(4.0*ph_pi*log(cp_env_radius/cp_acc_radius)))/(cp_bin_sepa**3)
    torque    = cp_bin_sepa*area*rhoave*((abs(vorb-venv))**2)
  else
    rhoave    = 0.
  end if
  cp_drag_vel    = abs(vorb-venv)
  cp_drag_dens   = rhoave 
  cp_drag_area   = area
  cp_drag_jorb   = j_orb
  cp_driver_drag = abs(torque/(2.0*j_orb)) 

! Resonant term
  mu1       = cp_acc_mass / (cp_acc_mass + cp_don_mass)
  mu2       = cp_don_mass / (cp_acc_mass + cp_don_mass)
  q_d       = (cp_env_mass)*(log(cp_bin_sepa/cp_acc_radius)/log(cp_env_radius/cp_acc_radius))
  if (q_d.le.0..and.cp_env_mass.le.0.) then
    q_d     = 0.
    coeff   = 0.
  else 
    q_d   = q_d / (cp_acc_mass + cp_don_mass)
    coeff   = (2.d0*1.d0/2.d0) & ! 2*L/M 
            * (1.d0/(mu1*mu2)) & ! alpha / mu1 mu2
            * ((3d-1)**2)      & ! (H/R)**2
            * (cp_bin_sepa/cp_env_radius) 
  end if
  adot_reso = coeff*q_d*cp_bin_freq
  cp_driver_reso = abs(adot_reso)

! Include drag
  cp_driver_nodrag = cp_driver_sepa
  cp_driver_sepa   =  cp_driver_sepa &
                   - (cp_driver_drag_norm*cp_driver_drag) &
                   - (cp_driver_reso_norm*cp_driver_reso) 

  return
  end subroutine cp_driver_terms





  subroutine cp_zeta_terms
  use ph_vars, only: ph_G
  use dr_vars, only: dr_accretion_flow, dr_is_sub_eddington,dr_is_super_eddington,&
                     dr_ignore_donor_terms,dr_eddington,dr_include_tides,&
                     dr_advection_mode,dr_mode_direct_impact,dr_mode_disk_accretion,dr_mode_adaptative
  use cp_vars, only: cp_bin_sepa,cp_bin_freq,&
                     cp_ejection_eff,cp_accretion_eff,cp_disk_eff,&
                     cp_don_pos,cp_initial_pos,&
                     cp_mass_ratio,cp_don_mass,cp_acc_mass,&
                     cp_don_radius,cp_acc_radius,&
                     cp_ang_radius,cp_cir_radius,cp_min_radius,&
                     cp_don_freq,cp_acc_freq,cp_q_stable,cp_q_a,&
                     cp_don_zeta,cp_don_k_factor,cp_don_k_zeta,&
                     cp_acc_zeta,cp_acc_k_factor,cp_acc_k_zeta,&
                     cp_zeta_sepa,cp_zeta_roche,cp_zeta_don_freq,cp_zeta_acc_freq
  use ph_interface, only: ph_norm,ph_kepler_jorb,ph_eggleton_formula_zeta
  implicit none
  real :: dum,factor_1,factor_2,advection_term,lambda
  real :: j_2,j_2_D,j_1_D,j_1,j_eje,j_orb

  j_orb = ph_kepler_jorb(cp_don_mass,cp_acc_mass,cp_bin_sepa)
! Let's calculate the contribution to q_a due simply to mass ejection 
! (it's 1 if there is none)
  if ( (dr_accretion_flow.eq.dr_is_sub_eddington).or.(dr_eddington.eqv..false.) ) then
    factor_1 = 1.0
  else if ( (dr_accretion_flow.eq.dr_is_super_eddington).and.dr_eddington ) then
! My prescription
    factor_1 = 1.0 + (1.0-cp_accretion_eff)*cp_mass_ratio &
             - cp_ejection_eff*cp_mass_ratio / (2.0*(1.0+cp_mass_ratio)) 
  else
    write(*,*) "Invalid value for dr_accretion_flow"
    stop
  end if

! Now to calculate the form of the angular momenta, depending on what kind of
! angular momentum transfer flow we are envisioning
! The angular momentum of the stream as it leaves the donor j_2 = r x v
  dum = ph_norm(cp_don_pos-cp_initial_pos)
  j_2 = (cp_don_radius**2)*cp_bin_freq
  if (dr_ignore_donor_terms) j_2 = 0.0
! Here's where things start to get tricky, we need to distinguish between
! direct impact and disk accretion
  if (cp_acc_radius.gt.cp_min_radius) cp_ang_radius = cp_cir_radius
  if (cp_acc_radius.le.cp_min_radius) cp_ang_radius = cp_acc_radius  
! In direct impact accretion, the stream will have the angular momentum
! corresponding to the circularization radius due to conservation laws.
! Since there is no disk, I set these angular momentum contributions to zero
  if       (dr_advection_mode.eq.dr_mode_direct_impact) then
! Then, the matter as it hits the disk: j_2_D
! Then, the matter as it leaves the disk: j_1_D
! Lastly, the matter as it hits the accretor: j_1 
    j_2_D = 0.0
    j_1_D = 0.0
    j_1   = sqrt(ph_G*cp_acc_mass*cp_cir_radius)
! I will assume that the stream will carry the angular momentum corresponding to
! the circularization radius
  else if (dr_advection_mode.eq.dr_mode_disk_accretion) then
! Then, the matter as it hits the disk: j_2_D
! Then, the matter as it leaves the disk: j_1_D
! Lastly, the matter as it hits the accretor: j_1
    j_2_D = 0.0 !qrt(G*cp_acc_mass*cir_radius)
    j_1_D = 0.0 !qrt(G*cp_acc_mass*accretion_radius)
    j_1   = sqrt(ph_G*cp_acc_mass*cp_acc_radius)
  else if (dr_advection_mode.eq.dr_mode_adaptative) then
    j_2_D = 0.0 !qrt(G*cp_acc_mass*cir_radius)
    j_1_D = 0.0 !qrt(G*cp_acc_mass*accretion_radius)
    j_1   = sqrt(ph_G*cp_acc_mass*cp_ang_radius)
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
                 + (1.0-cp_disk_eff)*j_1_D - cp_accretion_eff*j_1 &
                 - cp_ejection_eff*j_eje
  else
    write(*,*) "Invalid value for dr_accretion_flow"
    stop
  end if
! Putting it all together
  factor_2 = cp_don_mass*advection_term/j_orb

! Put it all in the q_a
! Basically q_a = 1 + advection_term*cp_don_mass / j_orb
  cp_q_a = factor_1 + factor_2 
! q_a = 1.0 - sqrt(r_h*(1.0+mass_ratio))  
  cp_zeta_sepa = - 2.0*(cp_q_a - cp_mass_ratio)

! Roche lobe zeta
  cp_zeta_roche = cp_zeta_sepa + ph_eggleton_formula_zeta(cp_mass_ratio,cp_accretion_eff)

! Donor and accretor logarithmic derivatives have already been calculated
! cp_don_zeta = already calculated

! Calculating zeta terms for the spins
  if (dr_include_tides) then 
  ! donor
    lambda           = 1. + cp_don_k_zeta + 2.*cp_don_zeta
    j_2              = j_2 / ((cp_don_k_factor*(cp_don_radius**2))) 
    cp_zeta_don_freq = j_2 - lambda*cp_don_freq 
  ! accretor
    lambda           = 1. + cp_acc_k_zeta + 2.*cp_acc_zeta 
    j_1              = j_1 / ((cp_acc_k_factor*(cp_acc_radius**2)))
    cp_zeta_acc_freq = j_1 - lambda*cp_acc_freq
  else
    cp_zeta_don_freq = 0.
    cp_zeta_acc_freq = 0.
  end if
 
! Calculating q_stable
  cp_q_stable = cp_q_a + 0.5*(cp_don_zeta-ph_eggleton_formula_zeta(cp_mass_ratio,cp_accretion_eff))

  return
  end subroutine cp_zeta_terms


  
  subroutine cp_get_pdots
  use cp_vars
  implicit none
  real :: mdot_sys,expon
! Conversion factors to transform from adots to pdots
  expon          = (3.0/2.0)*cp_bin_peri
  mdot_sys       = cp_ejection_eff*cp_don_mdot
! Now do it
  cp_pdot_mdot   = - (mdot_sys / cp_tot_mass)/2.0*cp_bin_peri
  cp_pdot_grw    = expon*cp_driver_sepa_grw
  cp_pdot_dontid = expon*cp_driver_sepa_dontid
  cp_pdot_acctid = expon*cp_driver_sepa_acctid
  cp_pdot_massfl = expon*cp_zeta_sepa*cp_don_mdot/cp_don_mass
  cp_pdot_total  = cp_pdot_mdot + cp_pdot_grw + cp_pdot_dontid &
                 + cp_pdot_acctid + cp_pdot_massfl
  end subroutine cp_get_pdots
