  subroutine cp_super_eddington
  use        IO, only: IO_rfrm,IO_rlen,IO_log
  use   physics, only: pi,G,c,k_B,ph_roche_pot,solar_mass,solar_radius,year
  use    driver, only: dr_eddington,dr_eddington_prescription,dr_abort,&
                       dr_use_naive,dr_use_infinity,dr_use_han,&
                       dr_accretion_flow,dr_is_sub_eddington,dr_is_super_eddington,&
                       dr_eddington_switch,dr_eddington_exit_switch,&
                       dr_time,dr_time_eddington,dr_time_eddington_exit,dr_abort
  use component, only: cp_donor_mass,cp_accretor_mass,cp_donor_radius,cp_accretor_radius,&
                       cp_binary_frequency,cp_binary_separation,cp_mass_ratio,&
                       cp_L_eddington,cp_L_eddington_infty,cp_opacity,cp_binding_to_rad_eff,&
                       cp_accretion_radius,cp_r_isco,&
                       cp_donor_position,cp_accretor_position,cp_L1_position,&
                       cp_potential_L1,cp_potential_Ra,&
                       cp_accretion_efficiency,cp_disk_efficiency,cp_ejection_efficiency,&
                       cp_mdot_donor,cp_mdot_eddington,cp_mdot_eddington_infty
  implicit none
  real                   :: some_mu,average_gravity
  real                   :: mdot_eddington_han,potential_han,L_edd_han
  real                   :: mdot_eddington_naive,potential_naive,L_edd_naive,L_edd_infty
  real, dimension(2)     :: dummy_vector
  character(len=IO_rlen) :: c0,c1,c2,c3,c4,c5,c6,c7,c8,c9
! The potential at L1 is meeded a lot of times
  cp_potential_L1 = ph_roche_pot(cp_L1_position,cp_donor_position,cp_accretor_position,&
                                 cp_donor_mass,cp_accretor_mass,cp_binary_frequency)
! Assuming accretion irradiates at the ISCO or star surface
! This usually doesn't matter for a WD, but it can matter for a NS and is
! crucial to BH
  cp_r_isco           = 6.*G*cp_accretor_mass / (c**2.)
  cp_accretion_radius = max(cp_r_isco,cp_accretor_radius)
! Standard L eddington and M eddington from infinity
  L_edd_infty             = 4.*pi*G*c*cp_accretor_mass / cp_opacity
  cp_L_eddington_infty    = L_edd_infty
  cp_mdot_eddington_infty = 4.*pi*c*cp_accretor_radius / &
                            ( cp_binding_to_rad_eff*cp_opacity )

! Naive approach, from L1 without averaging
  dummy_vector    = (/ -cp_accretion_radius , 0. /)
  dummy_vector    = cp_accretor_position + dummy_vector
  potential_naive = ph_roche_pot(dummy_vector,cp_donor_position,cp_accretor_position,&
                                 cp_donor_mass,cp_accretor_mass,cp_binary_frequency)
  mdot_eddington_naive = 4.0*pi*G*c*cp_accretor_mass / &
                         ( (cp_binding_to_rad_eff*cp_opacity)*(cp_potential_L1-potential_naive) )
  L_edd_naive          = L_edd_infty



! Approach from Han and Webbink (1999), it is the one I use now
  some_mu            = cp_accretor_mass / (cp_accretor_mass + cp_donor_mass)
  average_gravity    = G*cp_accretor_mass/(cp_accretion_radius**2.) &
                     - (2./3.)*cp_accretion_radius*G*(cp_accretor_mass+cp_donor_mass)/(cp_binary_separation**3.)
  potential_han      = - G*cp_accretor_mass/cp_accretion_radius - G*cp_donor_mass/cp_binary_separation &
                     - G*(cp_accretor_mass+cp_donor_mass)/(2.*(cp_binary_separation**3.))&
                     *((2./3.)*(cp_accretion_radius**2.)+((cp_binary_separation-some_mu*cp_binary_separation)**2.))
  mdot_eddington_han = 4.*pi*c*(cp_accretion_radius**2.)*average_gravity/ &
                       (cp_opacity*(cp_potential_L1-potential_han))
  mdot_eddington_han = abs(mdot_eddington_han)
  L_edd_han          = 4.0*pi*(cp_accretion_radius**2.)*c*average_gravity/cp_opacity



! Picking a prescription
! select case (dr_eddington_prescription)
! case (dr_use_naive) 
!   cp_mdot_eddington = mdot_eddington_naive
!   cp_L_eddington    = L_edd_naive
!   cp_potential_Ra   = potential_naive
! case (dr_use_infinity) 
!   cp_mdot_eddington = cp_mdot_eddington_infty
!   cp_L_eddington    = L_edd_infty
!   cp_potential_Ra   = -G*cp_accretor_mass/cp_accretion_radius
! case (dr_use_han)
    cp_mdot_eddington   = mdot_eddington_han
    cp_potential_Ra     = potential_han
    cp_L_eddington      = L_edd_han
! case default
!   call dr_abort("cp_super_eddington","Invalid mode for dr_eddington precription")
! end select



! Accetion efficiency, determines mass lost from the system
  if (dr_eddington) then
    if        (abs(cp_mdot_donor).lt.cp_mdot_eddington) then
      cp_accretion_efficiency = 1.0
      cp_ejection_efficiency  = 0.0
      cp_disk_efficiency      = 0.0
      dr_accretion_flow       = dr_is_sub_eddington
      if (dr_eddington_switch.and.dr_eddington_exit_switch) then 
        dr_time_eddington_exit   = dr_time
        dr_eddington_exit_switch = .false.
      end if
    else if   (abs(cp_mdot_donor).ge.cp_mdot_eddington) then
      cp_accretion_efficiency = ( cp_L_eddington  / (cp_potential_Ra*cp_mdot_donor) ) + &
                           ( cp_potential_L1 / cp_potential_Ra )
      cp_disk_efficiency      = 0.0
      cp_ejection_efficiency  = 1.0 - cp_accretion_efficiency - cp_disk_efficiency
      dr_accretion_flow       = dr_is_super_eddington
      if (.not.dr_eddington_switch)      dr_time_eddington        = dr_time
      if (.not.dr_eddington_exit_switch) dr_eddington_exit_switch = .true.
      dr_eddington_switch     = .true.
    else  
      call IO_log("Error at cp_super_eddington")
      write(c0,IO_rfrm) average_gravity
      write(c1,IO_rfrm) cp_potential_L1
      write(c2,IO_rfrm) cp_potential_Ra
      write(c3,IO_rfrm) cp_mdot_donor*year/solar_mass
      write(c4,IO_rfrm) cp_mdot_eddington*year/solar_mass
      write(c5,IO_rfrm) cp_accretion_radius
      write(c6,IO_rfrm) cp_accretor_radius
      write(c7,IO_rfrm) cp_accretor_mass
      write(c8,IO_rfrm) cp_donor_mass
      write(c9,IO_rfrm) some_mu
      call IO_log("g      = "//trim(adjustr(c0)))
      call IO_log("phi_L1 = "//trim(adjustr(c1)))
      call IO_log("phi_Ra = "//trim(adjustr(c2)))
      call IO_log("mdot   = "//trim(adjustr(c3)))
      call IO_log("mdot_e = "//trim(adjustr(c4)))
      call IO_log("r_acc  = "//trim(adjustr(c5)))
      call IO_log("r_acc  = "//trim(adjustr(c6)))
      call IO_log("m_acc  = "//trim(adjustr(c7)))
      call IO_log("m_don  = "//trim(adjustr(c8)))
      call IO_log("mu     = "//trim(adjustr(c9)))
      call dr_abort("cp_super_eddington","Something went wrong in this routine")
    end if
  else
! If Eddington is disabled, do nothing to accretion efficiencies and always flag
! accretion rates as sub Eddington
    cp_accretion_efficiency = 1.0
    cp_ejection_efficiency  = 0.0
    cp_disk_efficiency      = 0.0
    dr_accretion_flow       = dr_is_sub_eddington
  end if 
  return
  end subroutine cp_super_eddington





  subroutine cp_hyper_eddington
  implicit none
  return
  end subroutine cp_hyper_eddington
