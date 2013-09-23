  subroutine dr_init_ballistic(setup_mode,donmass,accmass,envmass,invar,donfreq,accfreq)
  use dr_vars, only: dr_hit_switch,dr_circularization_switch,dr_accretor_surface_switch,&
                     dr_maximum_radius,dr_line_tolerance,dr_time,dr_time_tolerance,&
                     dr_time_step_tolerance,dr_time_step,dr_phase_point_old,dr_phase_point_new,&
                     dr_hybrid,dr_res_factor
  use cp_vars, only: cp_bin_peri,cp_bin_sepa,cp_distance,&
                     cp_initial_pos,cp_initial_vel,cp_acc_pos,cp_speed,&
                     cp_hit_radius,cp_acc_radius,cp_min_radius,cp_cir_radius
  use dr_interface, only: dr_abort
  use cp_interface, only: cp_init
  use io_interface, only: io_write_header,io_open
  use ph_interface, only: ph_norm
  implicit none
  integer, intent(in) :: setup_mode
  real   , intent(in) :: donmass,accmass,envmass,invar,donfreq,accfreq 
  if (.not.dr_hybrid) call cp_init(setup_mode,donmass,accmass,envmass,invar,donfreq,accfreq)
  dr_hit_switch              = .false.
  dr_circularization_switch  = .false.
  dr_accretor_surface_switch = .false.
  cp_distance                = ph_norm(cp_initial_pos-cp_acc_pos)
  dr_maximum_radius          = 2.0*cp_bin_sepa
  cp_speed                   = ph_norm(cp_initial_vel)
  if (cp_acc_radius.lt.cp_min_radius) then
    cp_hit_radius              = cp_cir_radius
    dr_circularization_switch  = .true.
    dr_accretor_surface_switch = .false.
  else if (cp_acc_radius.ge.cp_min_radius) then
    cp_hit_radius              = cp_acc_radius
    dr_circularization_switch  = .true.
    dr_accretor_surface_switch = .true.
  else
    call dr_abort("dr_init","Something has gone horribly wrong")
  end if
  dr_line_tolerance          = dr_res_factor*cp_hit_radius
  dr_time                    = 0.0
  dr_time_tolerance          = cp_bin_peri
  dr_time_step_tolerance     = dr_res_factor*dr_time_tolerance
  cp_speed                   = ph_norm(cp_initial_vel)
  dr_time_step               = dr_line_tolerance/cp_speed
  dr_time_step               = min(dr_time_step_tolerance,dr_time_step)
  dr_phase_point_old   = (/ cp_initial_pos(1:2), cp_initial_vel(1:2) /) 
  dr_phase_point_new   = dr_phase_point_old
  end subroutine dr_init_ballistic
