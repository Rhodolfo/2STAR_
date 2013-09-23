  subroutine cp_super_eddington
  use io_vars, only: io_rfrm,io_rlen
  use ph_vars, only: ph_pi,ph_G,ph_c,ph_k_B,ph_msun,ph_rsun,ph_year
  use dr_vars, only: dr_eddington,dr_eddington_prescription,&
                     dr_use_naive,dr_use_infinity,dr_use_han,&
                     dr_accretion_flow,dr_is_sub_eddington,dr_is_super_eddington,&
                     dr_eddington_switch,dr_eddington_exit_switch,&
                     dr_time,dr_time_eddington,dr_time_eddington_exit
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_don_radius,cp_acc_radius,&
                     cp_bin_freq,cp_bin_sepa,cp_mass_ratio,&
                     cp_L_edd,cp_L_edd_infty,cp_opacity,cp_binding_to_rad_eff,&
                     cp_accretion_radius,cp_r_isco,&
                     cp_don_pos,cp_acc_pos,cp_L1_pos,&
                     cp_pot_L1,cp_pot_Ra,&
                     cp_accretion_eff,cp_disk_eff,cp_ejection_eff,&
                     cp_don_mdot,cp_mdot_edd,cp_mdot_edd_infty
  use ph_interface, only: ph_roche_pot
  use io_interface, only: io_log
  use dr_interface, only: dr_abort
  implicit none
  real                   :: some_mu,average_gravity
  real                   :: mdot_eddington_han,pot_han,L_eddington_han
  real                   :: mdot_eddington_naive,pot_naive,L_eddington_naive,L_eddington_infty
  real, dimension(2)     :: dummy_vector
  character(len=io_rlen) :: c0,c1,c2,c3,c4,c5,c6,c7,c8,c9
! The pot at L1 is meeded a lot of times
  cp_pot_L1 = ph_roche_pot(cp_L1_pos,cp_don_pos ,cp_acc_pos ,&
                                     cp_don_mass,cp_acc_mass,cp_bin_freq)
! Assuming accretion irradiates at the ISCO or star surface
! This usually doesn't matter for a WD, but it can matter for a NS and is
! crucial to BH
  cp_r_isco           = 6.*ph_G*cp_acc_mass / (ph_c**2.)
  cp_accretion_radius = max(cp_r_isco,cp_acc_radius)
! Standard L edd and M edd from infinity
  L_eddington_infty = 4.*ph_pi*ph_G*ph_c*cp_acc_mass / cp_opacity
  cp_L_edd_infty    = L_eddington_infty
  cp_mdot_edd_infty = 4.*ph_pi*ph_c*cp_acc_radius / &
                   ( cp_binding_to_rad_eff*cp_opacity )

! Naive approach, from L1 without averaging
  dummy_vector    = (/ -cp_accretion_radius , 0. /)
  dummy_vector    = cp_acc_pos + dummy_vector
  pot_naive = ph_roche_pot(dummy_vector,cp_don_pos,cp_acc_pos,&
                                 cp_don_mass,cp_acc_mass,cp_bin_freq)
  mdot_eddington_naive = 4.0*ph_pi*ph_G*ph_c*cp_acc_mass / &
                         ( (cp_binding_to_rad_eff*cp_opacity)*(cp_pot_L1-pot_naive) )
  L_eddington_naive    = L_eddington_infty



! Approach from Han and Webbink (1999), it is the one I use now
  some_mu            = cp_acc_mass / (cp_acc_mass + cp_don_mass)
  average_gravity    = ph_G*cp_acc_mass/(cp_accretion_radius**2.) &
                     - (2./3.)*cp_accretion_radius*ph_G*(cp_acc_mass+cp_don_mass)/(cp_bin_sepa**3.)
  pot_han      = - ph_G*cp_acc_mass/cp_accretion_radius - ph_G*cp_don_mass/cp_bin_sepa &
                     - ph_G*(cp_acc_mass+cp_don_mass)/(2.*(cp_bin_sepa**3.))&
                     *((2./3.)*(cp_accretion_radius**2.)+((cp_bin_sepa-some_mu*cp_bin_sepa)**2.))
  mdot_eddington_han = 4.*ph_pi*ph_c*(cp_accretion_radius**2.)*average_gravity/ &
                       (cp_opacity*(cp_pot_L1-pot_han))
  mdot_eddington_han = abs(mdot_eddington_han)
  L_eddington_han    = 4.0*ph_pi*(cp_accretion_radius**2.)*ph_c*average_gravity/cp_opacity



! Picking a prescription
! select case (dr_eddington_prescription)
! case (dr_use_naive) 
!   cp_mdot_edd = mdot_eddington_naive
!   cp_L_edd    = L_eddington_naive
!   cp_pot_Ra   = pot_naive
! case (dr_use_infinity) 
!   cp_mdot_edd = cp_mdot_edd_infty
!   cp_L_edd    = L_eddington_infty
!   cp_pot_Ra   = -ph_G*cp_acc_mass/cp_accretion_radius
! case (dr_use_han)
    cp_mdot_edd   = mdot_eddington_han
    cp_pot_Ra     = pot_han
    cp_L_edd      = L_eddington_han
! case default
!   call dr_abort("cp_super_eddington","Invalid mode for dr_eddington precription")
! end select



! Accetion eff, determines mass lost from the system
  if (dr_eddington) then
    if        (abs(cp_don_mdot).lt.cp_mdot_edd) then
      cp_accretion_eff = 1.0
      cp_ejection_eff  = 0.0
      cp_disk_eff      = 0.0
      dr_accretion_flow       = dr_is_sub_eddington
      if (dr_eddington_switch.and.dr_eddington_exit_switch) then 
        dr_time_eddington_exit   = dr_time
        dr_eddington_exit_switch = .false.
      end if
    else if   (abs(cp_don_mdot).ge.cp_mdot_edd) then
      cp_accretion_eff = ( cp_L_edd  / (cp_pot_Ra*cp_don_mdot) ) + &
                           ( cp_pot_L1 / cp_pot_Ra )
      cp_disk_eff      = 0.0
      cp_ejection_eff  = 1.0 - cp_accretion_eff - cp_disk_eff
      dr_accretion_flow       = dr_is_super_eddington
      if (.not.dr_eddington_switch)      dr_time_eddington        = dr_time
      if (.not.dr_eddington_exit_switch) dr_eddington_exit_switch = .true.
      dr_eddington_switch     = .true.
    else  
      call io_log("Error at cp_super_eddington")
      write(c0,io_rfrm) average_gravity
      write(c1,io_rfrm) cp_pot_L1
      write(c2,io_rfrm) cp_pot_Ra
      write(c3,io_rfrm) cp_don_mdot*ph_year/ph_msun
      write(c4,io_rfrm) cp_mdot_edd*ph_year/ph_msun
      write(c5,io_rfrm) cp_accretion_radius
      write(c6,io_rfrm) cp_acc_radius
      write(c7,io_rfrm) cp_acc_mass
      write(c8,io_rfrm) cp_don_mass
      write(c9,io_rfrm) some_mu
      call io_log("[cp_eddington] Something went wrong in this routine.")
      call io_log("[cp_eddington] The code will probably retry this step, spitting:")
      call io_log("g      = "//trim(adjustr(c0)))
      call io_log("phi_L1 = "//trim(adjustr(c1)))
      call io_log("phi_Ra = "//trim(adjustr(c2)))
      call io_log("mdot   = "//trim(adjustr(c3)))
      call io_log("mdot_e = "//trim(adjustr(c4)))
      call io_log("r_acc  = "//trim(adjustr(c5)))
      call io_log("r_acc  = "//trim(adjustr(c6)))
      call io_log("m_acc  = "//trim(adjustr(c7)))
      call io_log("m_don  = "//trim(adjustr(c8)))
      call io_log("mu     = "//trim(adjustr(c9)))
    end if
  else
! If Eddington is disabled, do nothing to accretion efficiencies and always flag
! accretion rates as sub Eddington
    cp_accretion_eff  = 1.0
    cp_ejection_eff   = 0.0
    cp_disk_eff       = 0.0
    dr_accretion_flow = dr_is_sub_eddington
  end if 
  return
  end subroutine cp_super_eddington





  subroutine cp_hyper_eddington
  implicit none
  return
  end subroutine cp_hyper_eddington
