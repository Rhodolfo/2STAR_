  subroutine dr_update_mdot
  use ode, only: ode_reject_step,ode_dt_suggested
  use ph_vars, only: ph_mchandra,ph_year,ph_msun
  use dr_vars, only: dr_hybrid,dr_include_tides,dr_accretion_flow,&
                     dr_is_sub_eddington,dr_is_super_eddington,&
                     dr_time,dr_time_step,dr_mode_separation,dr_time_peak,&
                     dr_mdot_vector_old,dr_mdot_vector_new,dr_exit_trigger,&
                     dr_time_eddington_elapsed,dr_mdot_new,dr_mdot_ref,&
                     dr_accretor_mode,dr_mode_he_white_dwarf,dr_mode_co_white_dwarf,&
                     sepa_var,macc_var,mdon_var,menv_var,facc_var,fdon_var,&
                     dr_period_ref,dr_period_new,dr_mdot_old
  use cp_vars, only: cp_don_mdot,cp_bin_sepa,cp_driver_sepa,cp_don_radius,cp_mass_ratio,&
                     cp_mdot_max,cp_virtemp,cp_mintemp,cp_maxtemp,cp_bin_peri,&
                     cp_tot_mass,cp_tot_mass_initial,cp_eje_mass,cp_eje_mass_peak,&
                     cp_vesc,cp_vesc_peak,cp_don_mass
  use ph_interface, only: ph_eggleton_formula
  use dr_interface, only: dr_set_time_step
  use cp_interface, only: cp_init
  use io_interface, only: io_log
  implicit none
  real :: donmass,accmass,invar,donfreq,accfreq,envmass,acrit,adot,dt
  logical :: nullmass,superchandra,accwd
! Advancing time and shit
  if (ode_reject_step) then 
    dr_time_step = min(0.9*ode_dt_suggested,0.5*dr_time_step)
    call io_log("[update] Step was rejected, retrying")
    return
  else 
  end if
! More treatment for the detached case
  if (cp_don_mass.le.0.) then 
    call io_log("[update] Donor mass is negative, retrying")
    dr_time_step = 0.1*dr_time_step
    ode_reject_step = .true.
    return
  end if
  if (cp_bin_sepa.le.0.) then 
    call io_log("[update] Separation is negative, retrying")
    dr_time_step = 0.1*dr_time_step
    ode_reject_step = .true. 
    return
  end if
  if ((abs(dr_mdot_old).eq.0).and.(abs(cp_don_mdot).ge.1e-9*ph_msun/ph_year)) then 
    call io_log("[update] Mass transfer rate jump between steps is too big, retrying")
    dr_time_step = 0.1*dr_time_step
    ode_reject_step = .true. 
    return
  end if
! if ((abs(cp_bin_peri-dr_period_ref).ge.(0.01*abs(dr_period_ref)))&
!     .and.(abs(dr_mdot_ref).eq.0.)) then 
!   call io_log("[update] Adjusting for a large change in period")
!   dr_time_step = 0.1*dr_time_step
!   return
! end if
! if (abs(dr_mdot_ref).gt.0.) then 
!   dr_period_ref = cp_bin_peri
! end if
! if (abs(dr_mdot_ref).eq.0..and.abs(cp_don_mdot*ph_year/ph_msun).ge.1e-10) then 
!   dr_time_step = 0.1*dr_time_step
!   call io_log("[update] Step was rejected due to a huge jump in mdot, retrying")
!   return
! end if
! Advancing time and storing data that needs to be saved
  dr_time            = dr_time + dr_time_step
  dr_mdot_vector_old = dr_mdot_vector_new
  cp_eje_mass    = abs(cp_tot_mass - cp_tot_mass_initial)
! Extracting data from the driver data vector
  invar   = dr_mdot_vector_old(sepa_var)
  donmass = dr_mdot_vector_old(mdon_var)
  accmass = dr_mdot_vector_old(macc_var)
  envmass = dr_mdot_vector_old(menv_var)
  if (dr_include_tides) donfreq = dr_mdot_vector_old(fdon_var)
  if (dr_include_tides) accfreq = dr_mdot_vector_old(facc_var) 
  if (dr_accretion_flow.eq.dr_is_super_eddington) then
    dr_time_eddington_elapsed = dr_time_eddington_elapsed + dr_time_step
  end if
! Dealing with total mass transfer or super chandrashekar things
  accwd = (dr_accretor_mode.eq.dr_mode_he_white_dwarf)&
      .or.(dr_accretor_mode.eq.dr_mode_co_white_dwarf)
  superchandra = (accmass.ge.0.98*ph_mchandra).and.accwd
  nullmass     = donmass.le.0.
  if (superchandra.or.nullmass) then 
    dr_exit_trigger = .true.
    return 
  else
    call cp_init(dr_mode_separation,donmass,accmass,envmass,invar,donfreq,accfreq)
    call dr_set_time_step
  end if
! Saving some data
  if (abs(cp_don_mdot).ge.abs(cp_mdot_max)) then 
    dr_time_peak     = dr_time
    cp_vesc_peak     = cp_vesc
    cp_eje_mass_peak = cp_eje_mass
    cp_mdot_max      = cp_don_mdot
  end if
! Hybrid mode things
  if (dr_hybrid) then
    cp_mintemp = min(cp_mintemp,cp_virtemp)
    cp_maxtemp = max(cp_maxtemp,cp_virtemp)
  end if
! Reference mdot
  dr_mdot_old   = cp_don_mdot
  dr_mdot_new   = cp_don_mdot
  dr_period_new = cp_bin_peri
  return
  end subroutine dr_update_mdot
