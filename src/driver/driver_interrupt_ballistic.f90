  subroutine dr_interrupt_ballistic
  use dr_vars, only: dr_exit_trigger,dr_line_tolerance,dr_res_factor,&
                     dr_maximum_radius,dr_exit_trigger,dr_force_write,&
                     dr_file_counter,dr_screen_counter,&
                     dr_stop_on_hit,dr_hit_switch,&
                     dr_interrupting,dr_time,dr_time_step,&
                     dr_time_tolerance
  use cp_vars, only: cp_arc_distance,cp_distance,cp_hit_radius
  use dr_interface, only: dr_store_ballistic_data
  use io_interface, only: io_log,io_2string
  implicit none
! Checking if it's time to end the simulation
  if (.not.dr_exit_trigger) then
    if (dr_hit_switch.and.dr_stop_on_hit) then 
      dr_exit_trigger = .true. 
      call IO_log("Test particle has impacted the disk or accretor")
    else if (cp_distance.gt.dr_maximum_radius) then 
      dr_exit_trigger = .true. 
      call IO_log("Test particle has been unbound from the system")
    end if
  end if
! Checking if it's time to force a file write
  if (cp_arc_distance.ge.dr_line_tolerance/dr_res_factor) then
    dr_force_write  = .true.
    cp_arc_distance = 0.
  end if
  if (dr_exit_trigger) dr_force_write = .true.
! Checking if it is time to print to the screen
  if (dr_file_counter.eq.0.) call IO_log("t/T       dt/T      r/R_i")
  if (((dr_time/dr_time_tolerance).gt.(0.05*dr_screen_counter)).or.dr_interrupting) then
    call IO_log(IO_2string(dr_time/dr_time_tolerance)//" "//&
                IO_2string(dr_time_step/dr_time_tolerance)//" "//&
                IO_2string(cp_distance/cp_hit_radius))
    dr_screen_counter = dr_screen_counter + 1
  end if
! Write when time has elapsed
  if (((dr_time/dr_time_tolerance).ge.(0.0001*dr_file_counter)).or.dr_force_write) then
    call dr_store_ballistic_data
    if (.not.dr_force_write) dr_file_counter = dr_file_counter + 1
  end if
  end subroutine dr_interrupt_ballistic
