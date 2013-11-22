  subroutine dr_reset
  use dr_vars
! reset all module variables to default values
  implicit none
! printing modes
  dr_interrupting       = .false.
  dr_force_write        = .false.
  dr_mdot_flags         = .false.
  dr_null_flags         = .false.
  dr_stop_on_eddington  = .false.
  dr_ignore_donor_terms = .false.
  dr_exit_trigger       = .false.
  dr_include_tides      = .false.
! setup routine modes
  dr_setup_mode         = dr_mode_contact
!usage modes
  dr_integration_mode   = dr_mode_mdot
! evolution routine modes
  dr_donor_mode         = dr_mode_he_white_dwarf
  dr_accretor_mode      = dr_mode_he_white_dwarf
  dr_evo_mode           = dr_mode_self_consistent
  dr_advection_mode     = dr_mode_adaptative
! some integers that are useful for the interrupt subroutines 
  dr_step_tolerance = 10000000
  dr_screen_counter = 0
  dr_force_counter  = 0
  dr_step_counter   = 0
  dr_file_counter   = 0

! orbital parameters
  dr_res_factor             = 1.0e-2
  dr_time                   = 0.
  dr_time_step              = 0.
  dr_time_step_old          = 0.
  dr_time_tolerance         = 0.
  dr_time_step_tolerance    = 0.
  dr_line_tolerance         = 0.
  dr_maximum_radius         = 0.

! vectors that store values for solving differential equations
! for an mdot evolution
  if (allocated(dr_mdot_vector_old)) deallocate(dr_mdot_vector_old)
  if (allocated(dr_mdot_vector_new)) deallocate(dr_mdot_vector_new)
! for ballistic evolution
  dr_phase_point_old       = 0.
  dr_phase_point_new       = 0.

! some variables relevant to stability analysis
  dr_phase        = 0
  dr_low_mass     = 0.1
  dr_hig_mass     = 1.4
  dr_dif_mass_don = 0.001
  dr_dif_mass_acc = 0.001

! switches for stream evolution
  dr_hit_switch              = .false.
  dr_stop_on_hit             = .false.
  dr_circularization_switch  = .false.
  dr_accretor_surface_switch = .false.


! super eddington flow modes
  dr_eddington              = .false.
  dr_hyper_eddington        = .false.
  dr_accretion_flow         = dr_is_super_eddington
  dr_eddington_prescription = dr_use_han

! super eddington flow parameters
  dr_eddington_switch       = .false. ! switches if super eddington
  dr_eddington_exit_switch  = .false. ! switches when exiting eddington
  dr_time_eddington         = 0.0     ! onse of eddington accretion
  dr_time_eddington_exit    = 0.0     ! exiting the super eddington accretion
  dr_time_eddington_elapsed = 0.0     ! duration of eddington accretion

  end subroutine dr_reset
