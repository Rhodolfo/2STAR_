  subroutine dr_set_time_step
  use ode, only: ode_dt_suggested,ode_reject_step
  use dr_vars, only: dr_res_factor,dr_time,dr_time_step,&
                     dr_time_step_old,dr_time_step_tolerance,&
                     dr_accretion_flow,dr_is_super_eddington,&
                     dr_time_tolerance,dr_include_tides,dr_mdot_vector_new,&
                     dr_time,dr_step_counter,dr_mdot_ref
  use cp_vars, only: cp_min_tscale,cp_don_radius,cp_roche_radius,&
                     cp_don_mdot,cp_mdot_edd,cp_bin_peri,cp_mass_ratio,&
                     cp_driver_sepa,cp_bin_sepa,cp_gravitational_tscale
  use dr_interface, only: dr_abort
  use io_interface, only: io_log
  implicit none
  real :: dt,pow,res,fac,acrit,adot
  if (dr_step_counter.eq.0) then 
    call io_log("[driver] Setting initial time step to the binary period")
    dr_time_step     = 5.*cp_bin_peri
    dr_time_step_old = dr_time_step
    return
  end if
! Linear dr_time step refinement for mdot evolution
  dr_time_step_old = dr_time_step 
  dr_time_step     = ode_dt_suggested
  if (abs(cp_don_mdot/cp_mdot_edd).ge.1e0) then
    dt           = dr_res_factor*cp_min_tscale
    fac          = ((1.+log10(cp_don_mdot/cp_mdot_edd))**4.0)
    dr_time_step = min(dr_time_step,dt/fac)
  end if
! Don't let the time step change too much 
! if (abs(dr_time_step - dr_time_step_old)/dr_time_step_old.ge.0.01) then 
!   if (dr_time_step.gt.dr_time_step_old) dr_time_step = 1.01*dr_time_step_old
!   if (dr_time_step.le.dr_time_step_old) dr_time_step = 0.99*dr_time_step_old
! end if
! Sanity checks
  if (dr_time_step.lt.0) then 
    call dr_abort("[driver]","Null or negative time step encountered")
  else if (dr_time_step.eq.0) then 
    if (dr_time.ge.dr_time_tolerance) return
    call dr_abort("[driver]","Null time step encountered")
  end if
  if (dr_time_step.ge.0.01*dr_time_tolerance) then 
    dr_time_step = min(dr_time_step,0.01*dr_time_tolerance)
    call io_log("[driver] Time step larger than time remaining, shortening")
  end if
  return
  end subroutine dr_set_time_step
