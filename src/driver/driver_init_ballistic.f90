  subroutine dr_init_ballistic(setup_mode,donmass,accmass,invar,donfreq,accfreq)
  use   physics, only: ph_norm
  use    driver, only: dr_hit_switch,dr_circularization_switch,dr_accretor_surface_switch,&
                       dr_maximum_radius,dr_abort,dr_line_tolerance,dr_time,dr_time_tolerance,&
                       dr_time_step_tolerance,dr_time_step,dr_phase_point_old,dr_phase_point_new,&
                       dr_hybrid,dr_res_factor
  use component, only: cp_init,cp_binary_period,cp_binary_separation,cp_distance,&
                       cp_initial_position,cp_initial_velocity,cp_accretor_position,cp_speed,&
                       cp_hit_radius,cp_accretor_radius,cp_min_radius,cp_circularization_radius
  use      IO,   only: IO_write_header,IO_open
  implicit none
  integer, intent(in) :: setup_mode
  real   , intent(in) :: donmass,accmass,invar,donfreq,accfreq 
  if (.not.dr_hybrid) call cp_init(setup_mode,donmass,accmass,invar,donfreq,accfreq)
  dr_hit_switch              = .false.
  dr_circularization_switch  = .false.
  dr_accretor_surface_switch = .false.
  cp_distance                = ph_norm(cp_initial_position-cp_accretor_position)
  dr_maximum_radius          = 2.0*cp_binary_separation
  cp_speed                   = ph_norm(cp_initial_velocity)
  if (cp_accretor_radius.lt.cp_min_radius) then
    cp_hit_radius           = cp_circularization_radius
    dr_circularization_switch  = .true.
    dr_accretor_surface_switch = .false.
  else if (cp_accretor_radius.ge.cp_min_radius) then
    cp_hit_radius              = cp_accretor_radius
    dr_circularization_switch  = .true.
    dr_accretor_surface_switch = .true.
  else
    call dr_abort("dr_init","Something has gone horribly wrong")
  end if
  dr_line_tolerance          = dr_res_factor*cp_hit_radius
  dr_time                    = 0.0
  dr_time_tolerance          = cp_binary_period
  dr_time_step_tolerance     = dr_res_factor*dr_time_tolerance
  cp_speed                   = ph_norm(cp_initial_velocity)
  dr_time_step               = dr_line_tolerance/cp_speed
  dr_time_step               = min(dr_time_step_tolerance,dr_time_step)
  dr_phase_point_old   = (/ cp_initial_position(1:2), cp_initial_velocity(1:2) /) 
  dr_phase_point_new   = dr_phase_point_old
  end subroutine dr_init_ballistic
