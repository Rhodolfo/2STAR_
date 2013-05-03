  subroutine dr_set_time_step
        use ode, only: ode_dt_suggested,ode_reject_step
  use   physics
  use    driver, only: dr_res_factor,dr_time,dr_time_step,&
                       dr_time_step_old,dr_time_step_tolerance,&
                       dr_accretion_flow,dr_is_super_eddington,&
                       dr_time_tolerance,mdot_source_function,&
                       dr_include_tides,dr_mdot_vector_new,dr_time,dr_abort
  use component, only: cp_min_timescale,cp_donor_radius,cp_roche_radius,&
                       cp_mdot_donor,cp_accretion_efficiency,cp_mdot_donor_old,&
                       cp_mdot_eddington,cp_binary_period,cp_mdot_donor_max
  use        IO, only: IO_log
  implicit none
  real :: dt,pow,res,fac
  if (dr_time.le.0.) then 
    call IO_log("[driver] Setting initial time step to the binary period")
    dr_time_step = 5.*cp_binary_period
    dr_time_step_old = dr_time_step
    return
  end if
! Linear dr_time step refinement for mdot evolution
  dr_time_step_old = dr_time_step 
  dr_time_step     = ode_dt_suggested
  if (abs(cp_mdot_donor/cp_mdot_eddington).ge.1e0) then
    dt           = dr_res_factor*cp_min_timescale
    fac          = ((1.+log10(cp_mdot_donor/cp_mdot_eddington))**4.0)
    dr_time_step = min(dr_time_step,dt/fac)
  end if
  if (dr_time_step.lt.0) then 
    call dr_abort("dr_set_time_step","Null or negative time step encountered")
  else if (dr_time_step.eq.0) then 
    if (dr_time.ge.dr_time_tolerance) return
    call dr_abort("dr+set_time_step","Null time step encountered")
  end if
  if (dr_time_step.ge.0.01*dr_time_tolerance) then 
    dr_time_step = min(dr_time_step,0.01*dr_time_tolerance)
    write(*,*) "FLAG",dr_time_step,dr_time_tolerance
  end if
  return
  end subroutine dr_set_time_step
