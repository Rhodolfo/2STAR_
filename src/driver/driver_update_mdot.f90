  subroutine dr_update_mdot
  use ode, only: ode_reject_step,ode_dt_suggested
  use ph_vars, only: ph_mchandra,ph_year,ph_msun
  use dr_vars, only: dr_include_tides,dr_accretion_flow,&
                     dr_is_sub_eddington,dr_is_super_eddington,&
                     dr_time,dr_time_step,dr_mode_separation,dr_time_peak,&
                     dr_mdot_vector_old,dr_mdot_vector_new,dr_exit_trigger,&
                     dr_time_eddington_elapsed,dr_mdot_new,dr_mdot_ref,&
                     dr_accretor_mode,dr_mode_he_white_dwarf,dr_mode_co_white_dwarf,&
                     sepa_var,macc_var,mdon_var,menv_var,facc_var,fdon_var,&
                     dr_period_ref,dr_period_new,dr_step_counter,dr_mdotdot,dr_mdot_old,&
                     dr_mdot_max,dr_mdotdot_env,dr_mdot_old_env,dr_mdot_new_env,&
                     dr_time_contact,dr_res_factor
  use cp_vars, only: cp_don_mdot,cp_bin_sepa,cp_driver_sepa,cp_don_radius,cp_mass_ratio,&
                     cp_mdot_max,cp_virtemp,cp_mintemp,cp_maxtemp,cp_bin_peri,&
                     cp_tot_mass,cp_tot_mass_initial,cp_eje_mass,cp_eje_mass_peak,&
                     cp_vesc,cp_vesc_peak,cp_don_mass,cp_env_mdot
  use ph_interface, only: ph_eggleton_formula
  use dr_interface, only: dr_set_time_step
  use cp_interface, only: cp_init
  use io_interface, only: io_log,io_2string
  implicit none
  real :: donmass,accmass,invar,donfreq,accfreq,envmass,acrit,adot,dt,tfall
  logical :: nullmass,superchandra,accwd

! Extracting data from solution vector
  invar   = dr_mdot_vector_new(sepa_var)
  donmass = dr_mdot_vector_new(mdon_var)
  accmass = dr_mdot_vector_new(macc_var)
  envmass = dr_mdot_vector_new(menv_var)

! Checking for a rejected time step from the ODE integrator
  if (ode_reject_step) then 
    dr_time_step = min(0.6*ode_dt_suggested,0.5*dr_time_step)
    call io_log("[update] Step was rejected,&
               & retrying step = "//io_2string(dr_step_counter))
    return
  else 
  end if

! Making time step bigger if enough time has elapsed after peak
  tfall = dr_time_contact + 2.0*(dr_time_peak-dr_time_contact)
  if (dr_time.ge.tfall.and.dr_time.ge.1e4.and.abs(cp_don_mdot).ge.1e-10) then 
    dr_res_factor    = 0.02
  end if

! Rejecting step if the donor loses too much mass
  if (donmass.le.0.) then 
    call io_log("[update] Donor mass is negative,&
               & retrying step = "//io_2string(dr_step_counter))
    dr_time_step    = 0.5*dr_time_step
    ode_reject_step = .true.
    return
  end if

! Rejecting step if the separation gets negative
  if (invar.le.0.) then 
    call io_log("[update] Separation is negative,&
               & retrying step = "//io_2string(dr_step_counter))
    dr_time_step    = 0.5*dr_time_step
    ode_reject_step = .true. 
    return
  end if

! Rejecting step if the accretion rate jumps too much
  if ((abs(dr_mdot_new).eq.0).and.(abs(cp_don_mdot).ge.1e-9*ph_msun/ph_year)) then 
    call io_log("[update] Mass transfer rate jump between steps is too big,&
               & retrying step = "//io_2string(dr_step_counter))
    dr_time_step    = 0.5*dr_time_step
    ode_reject_step = .true. 
    return
  end if

! Rejecting step if the enevelope mass is negative 
   if (envmass.lt.0.0) then 
    call io_log("[update] Mass of the envelope is negative,&
               & retrying step = "//io_2string(dr_step_counter))
    dr_time_step    = 0.5*dr_time_step
    ode_reject_step = .true. 
    return
  end if 

! Keeping track of when mass transfer first begins
  if (abs(cp_don_mdot).ge.1e-12) then 
    if (dr_time_contact.le.0.) then  
      dr_time_contact = dr_time
    end if
  end if

! Sanity checks are done, advancing if sane

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

! Reference mdot
  dr_mdot_max   = max(abs(dr_mdot_max),abs(dr_mdot_new))
  dr_mdot_old   = dr_mdot_new
  dr_mdot_new   = cp_don_mdot
  dr_period_new = cp_bin_peri
  dr_mdotdot    = abs(dr_mdot_old-dr_mdot_new)/dr_time_step

! For the envelope
  dr_mdot_old_env = dr_mdot_new_env
  dr_mdot_new_env = cp_env_mdot
  dr_mdotdot_env  = abs(dr_mdot_old_env-dr_mdot_new_env)/dr_time_step

  return
  end subroutine dr_update_mdot
