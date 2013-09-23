  subroutine dr_perform_stability_analysis
  use dr_vars, only: dr_phase,dr_integration_mode,dr_mode_stability
  use dr_interface, only: dr_perform_phase_1,dr_perform_phase_2,dr_perform_phase_3
  use io_interface, only: io_cli_options,io_log
  implicit none
  if (dr_phase.eq.0) then 
    call io_log("Performing main stability phases in one go")
    call dr_perform_phase_1
    call dr_perform_phase_2
  end if
  if (dr_phase.eq.1) call dr_perform_phase_1
  if (dr_phase.eq.2) call dr_perform_phase_2
  if (dr_phase.eq.3) call dr_perform_phase_3
  call io_cli_options
  dr_integration_mode = dr_mode_stability
  return
  end subroutine dr_perform_stability_analysis
