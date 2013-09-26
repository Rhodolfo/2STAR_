  subroutine dr_perform
  use      dr_vars, only: dr_integration_mode,dr_mode_ballistic,&
                          dr_mode_mdot,dr_mode_post,dr_mode_stability,&
                          dr_mode_eos_tables
  use dr_interface, only: dr_perform_ballistic_evolution,dr_perform_mdot_evolution,&
                          dr_perform_mdot_postprocessing,dr_perform_stability_analysis,&
                          dr_stop 
  use io_interface, only: io_log
  use eos_vars, only: eos_has_read_table
  use eos_root_table, only: eos_create_root_table,eos_create_pres_brackets
  implicit none
! Tells the EOS to read table once and save results
  eos_has_read_table = .false.
  select case (dr_integration_mode)
  case (dr_mode_ballistic)
    call IO_log("[driver] Calling ballistic solver")
    call dr_perform_ballistic_evolution
  case (dr_mode_mdot)
    call IO_log("[driver] Calling mass transfer history solver")
    call dr_perform_mdot_evolution
  case (dr_mode_post)
    call IO_log("[driver] Calling postprocessing routine")
    call dr_perform_mdot_postprocessing
  case (dr_mode_stability)
    call IO_log("[driver] Calling stability analysis routine")
    call dr_perform_stability_analysis
  case (dr_mode_eos_tables)
    call IO_log("[driver] Initializing EOS and writing to files")
    call eos_init
    call eos_create_root_table
    call eos_create_pres_brackets
  case default
    call dr_abort("dr_perform","Invalid driver mode")
  end select
  call dr_stop
  return
  end subroutine dr_perform
