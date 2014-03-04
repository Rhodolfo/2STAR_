  subroutine dr_set_time_step
  use ode, only: ode_dt_suggested,ode_reject_step
  use dr_vars, only: dr_res_factor,dr_time,dr_time_step,&
                     dr_time_step_old,dr_time_step_tolerance,&
                     dr_accretion_flow,dr_is_super_eddington,&
                     dr_time_tolerance,dr_include_tides,dr_mdot_vector_new,&
                     dr_time,dr_step_counter,dr_mdot_ref,&
                     dr_time_peak,dr_time_contact,dr_time_step_max
  use cp_vars, only: cp_min_tscale,cp_don_radius,cp_roche_radius,&
                     cp_don_mdot,cp_mdot_edd,cp_bin_peri,cp_mass_ratio,&
                     cp_driver_sepa,cp_bin_sepa,cp_gravitational_tscale
  use dr_interface, only: dr_abort
  use io_interface, only: io_log
  implicit none
  real :: dt,pow,res,fac,acrit,adot,tfall

! Initial time step 
  if (dr_step_counter.eq.0) then 
    call io_log("[driver] Setting initial time step to the binary period")
    dr_time_step     = 10.0*cp_bin_peri
    dr_time_step_old = dr_time_step
    return
  else 
  end if

! Set tstep
  dr_time_step_old = dr_time_step 
  dr_time_step     = ode_dt_suggested

! Recaling when super Eddington
  if (abs(cp_don_mdot/cp_mdot_edd).ge.1e0) then
    dt           = dr_res_factor*cp_min_tscale
    fac          = ((1.+log10(cp_don_mdot/cp_mdot_edd))**4.0)
    dr_time_step = min(dr_time_step,dt/fac)
  end if

! Don't let the time step change too much 
  if (abs(dr_time_step - dr_time_step_old)/dr_time_step_old.ge.0.001) then 
    if (dr_time_step.gt.dr_time_step_old) dr_time_step = 1.001*dr_time_step_old
    if (dr_time_step.le.dr_time_step_old) dr_time_step = 0.999*dr_time_step_old
  end if

! Sanity checks
  if (dr_time_step.lt.0) then 
    call dr_abort("[driver]","Null or negative time step encountered")
  else if (dr_time_step.eq.0) then 
    call dr_abort("[driver]","Null time step encountered")
  end if
  
  dr_time_step_max = max(dr_time_step_max,dr_time_step)

  tfall = dr_time_contact + 2.0*(dr_time_peak-dr_time_contact)
  if (dr_time.ge.tfall.and.dr_time.ge.1e4.and.abs(cp_don_mdot).ge.1e-10) then 
    dr_time_step = max(dr_time_step,dr_time_step_max)
  end if

  return
  end subroutine dr_set_time_step
