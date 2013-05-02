  subroutine dr_perform_stability_analysis
  use driver, only: dr_phase,dr_integration_mode,dr_mode_stability,&
                    dr_perform_phase_1,dr_perform_phase_2,dr_perform_phase_3
  use     IO, only: IO_read_command_line_options,IO_log
  implicit none
  if (dr_phase.eq.0) then 
    call IO_log("Performing main stability phases in one go")
    call dr_perform_phase_1
    call dr_perform_phase_2
  end if
  if (dr_phase.eq.1) call dr_perform_phase_1
  if (dr_phase.eq.2) call dr_perform_phase_2
  if (dr_phase.eq.3) call dr_perform_phase_3
  call IO_read_command_line_options
  dr_integration_mode = dr_mode_stability
  return
  end subroutine dr_perform_stability_analysis
