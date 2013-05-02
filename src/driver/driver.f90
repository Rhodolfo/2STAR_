module driver
  implicit none

! setup modes
  integer, parameter :: dr_mode_contact       = 0
  integer, parameter :: dr_mode_period        = 1
  integer, parameter :: dr_mode_separation    = 2
  integer, parameter :: dr_mode_mdot          = 3
  integer, parameter :: dr_mode_overflow_eq   = 4
  integer, parameter :: dr_mode_separation_eq = 5
  integer, parameter :: dr_mode_roche_limit   = 6

! perform modes
  integer, parameter :: dr_mode_stability   = 0
  integer, parameter :: dr_mode_ballistic   = 1
  integer, parameter :: dr_mode_post        = 2
! integer, parameter :: dr_mode_mdot        = 3 is already declared, but is an option for integration_mode

! identifying constants
  integer, parameter :: dr_data_id              = 42
  integer, parameter :: dr_donor                = 1
  integer, parameter :: dr_accretor             = 0
  integer, parameter :: dr_mode_neutron_star    = 0
  integer, parameter :: dr_mode_he_white_dwarf  = 1
  integer, parameter :: dr_mode_co_white_dwarf  = 2
  integer, parameter :: dr_mode_inconsistent    = 0
  integer, parameter :: dr_mode_self_consistent = 1
  integer, parameter :: dr_mode_direct_impact   = 0
  integer, parameter :: dr_mode_disk_accretion  = 1
  integer, parameter :: dr_mode_adaptative      = 2

! flow modes
  integer, parameter :: dr_is_sub_eddington   = 0
  integer, parameter :: dr_is_super_eddington = 1
  integer, parameter :: dr_is_hyper_eddington = 2

! psossible eddington prescriptions
  integer, parameter :: dr_use_naive    = 0      ! more physical, uses the lagrange point as reference, but does not average Roche potential at accretion radius
  integer, parameter :: dr_use_infinity = 1      ! unphysical, uses the usual bondi prescription
  integer, parameter :: dr_use_han      = 2      ! uses the hann and webbink prescription from 1999, it uses L1 and averages the Roche potential at accretion radius

! printing modes
  logical :: dr_interrupting       = .false.
  logical :: dr_force_write        = .false.
  logical :: dr_mdot_flags         = .false.
  logical :: dr_null_flags         = .false.
  logical :: dr_stop_on_eddington  = .false.
  logical :: dr_ignore_donor_terms = .false.
  logical :: dr_exit_trigger       = .false.
  logical :: dr_hybrid             = .false.
  logical :: dr_include_tides      = .false.
! setup routine modes
  integer            :: dr_setup_mode       = dr_mode_contact
!usage modes
  integer            :: dr_integration_mode = dr_mode_mdot
! evolution routine modes
  integer            :: dr_donor_mode           = dr_mode_he_white_dwarf
  integer            :: dr_accretor_mode        = dr_mode_he_white_dwarf
  integer            :: dr_evo_mode             = dr_mode_self_consistent
  integer            :: dr_advection_mode       = dr_mode_adaptative
! some integers that are useful for the interrupt subroutines 
  integer :: dr_step_tolerance = 10000000
  integer :: dr_screen_counter = 0
  integer :: dr_force_counter  = 0
  integer :: dr_step_counter   = 0
  integer :: dr_file_counter   = 0

! orbital parameters
  real               :: dr_res_factor             = 1.0e-2
  real               :: dr_time                   = 0.
  real               :: dr_time_step              = 0.
  real               :: dr_time_step_old          = 0.
  real               :: dr_time_tolerance         = 0.
  real               :: dr_time_step_tolerance    = 0.
  real               :: dr_line_tolerance         = 0.
  real               :: dr_maximum_radius         = 0.

! vectors that store values for solving differential equations
! for an mdot evolution
  real, dimension(:), allocatable :: dr_mdot_vector_old 
  real, dimension(:), allocatable :: dr_mdot_vector_new 
! for ballistic evolution
  real, dimension(4) :: dr_phase_point_old       = 0.
  real, dimension(4) :: dr_phase_point_new       = 0.

! some variables relevant to stability analysis
  integer :: dr_phase    = 0
  real    :: dr_low_mass = 0.1
  real    :: dr_hig_mass = 1.4
  real    :: dr_dif_mass_don = 0.001
  real    :: dr_dif_mass_acc = 0.001

! switches for stream evolution
  logical :: dr_hit_switch                 = .false.
  logical :: dr_stop_on_hit                = .false.
  logical :: dr_circularization_switch     = .false.
  logical :: dr_accretor_surface_switch    = .false.

! super eddington flow modes
  logical            :: dr_eddington          = .false.
  logical            :: dr_hyper_eddington    = .false.
  integer            :: dr_accretion_flow     = dr_is_sub_eddington
  integer       :: dr_eddington_prescription  = dr_use_han

! super eddington flow parameters
  logical :: dr_eddington_switch       = .false. ! switches if super eddington
  logical :: dr_eddington_exit_switch  = .false. ! switches when exiting eddington
  real    :: dr_time_eddington         = 0.0     ! onset of eddington accretion
  real    :: dr_time_eddington_exit    = 0.0     ! exiting the super eddington accretion
  real    :: dr_time_eddington_elapsed = 0.0     ! duration of eddington accretion
  real    :: dr_time_peak              = 0.0     ! time at which mdot peaks



! slave subroutines
  interface



! resets all variables to default values
   
    subroutine dr_restore_defaults
    implicit none
    end subroutine dr_restore_defaults

    subroutine dr_reset
    implicit none
    end subroutine dr_reset

    subroutine dr_abort(routine,message)
    implicit none
    character*(*) :: routine,message
    end subroutine dr_abort



! main performance routines
    subroutine dr_perform
    implicit none
    end subroutine dr_perform

    subroutine dr_perform_mdot_evolution(setmode,dm,am,invar,donfreq,accfreq)
    implicit none
    integer, optional :: setmode
    real,    optional :: dm,am,invar,donfreq,accfreq
    end subroutine dr_perform_mdot_evolution

    subroutine dr_perform_mdot_postprocessing
    implicit none
    end subroutine dr_perform_mdot_postprocessing

    subroutine dr_perform_ballistic_evolution(setmode,dm,am,invar,donfreq,accfreq)
    implicit none
    integer, optional :: setmode
    real,    optional :: dm,am,invar,donfreq,accfreq
    end subroutine dr_perform_ballistic_evolution

    subroutine dr_perform_stability_analysis
    implicit none
    end subroutine dr_perform_stability_analysis



! control procedures for main program

    subroutine dr_init(mode,md,ma,vr,fd,fa)
    implicit none
    integer, intent(in) :: mode
    real, intent(in)    :: md,ma,vr,fd,fa
    end subroutine dr_init

    subroutine dr_init_ballistic(mode,md,ma,vr,fd,fa)
    implicit none
    integer, intent(in) :: mode
    real, intent(in)    :: md,ma,vr,fd,fa
    end subroutine dr_init_ballistic

    subroutine dr_init_mdot(mode,md,ma,vr,fd,fa)
    implicit none
    integer, intent(in) :: mode
    real, intent(in)    :: md,ma,vr,fd,fa
    end subroutine dr_init_mdot

    subroutine dr_update
    implicit none
    end subroutine dr_update

    subroutine dr_interrupt
    implicit none
    end subroutine dr_interrupt

    subroutine dr_stop
    implicit none
    end subroutine dr_stop



! control procedures for mdot integration

    subroutine dr_update_mdot
    implicit none
    end subroutine dr_update_mdot

    subroutine dr_interrupt_mdot
    implicit none
    end subroutine dr_interrupt_mdot

    subroutine dr_set_time_step
    implicit none
    end subroutine dr_set_time_step

    subroutine dr_store_mdot_data
    implicit none
    end subroutine dr_store_mdot_data

    subroutine dr_store_full_mdot_data
    implicit none
    end subroutine dr_store_full_mdot_data



! control procedures for ballistic integration

    subroutine dr_update_ballistic
    implicit none
    end subroutine dr_update_ballistic

    subroutine dr_interrupt_ballistic
    implicit none
    end subroutine dr_interrupt_ballistic

    subroutine dr_store_ballistic_data
    implicit none
    end subroutine dr_store_ballistic_data



! routines for stability analysis

    subroutine dr_store_sweep_data
    implicit none
    end subroutine dr_store_sweep_data

    function dr_critical_mass(am,dl,dh,cr,er)
    implicit none 
    integer :: cr
    real    :: am,dl,dh,er,dr_critical_mass
    end function dr_critical_mass

    subroutine dr_perform_phase_1
    implicit none
    end subroutine dr_perform_phase_1

    subroutine dr_perform_phase_2
    implicit none
    end subroutine dr_perform_phase_2

    subroutine dr_perform_phase_3
    implicit none
    end subroutine dr_perform_phase_3





! source functions

    function stream_source_function(v,t)
    implicit none
    real :: t
    real, dimension(:) :: v
    real, dimension(size(v)) :: stream_source_function
    end function stream_source_function

    function mdot_source_function(v,t)
    implicit none
    real :: t
    real, dimension(:) :: v
    real, dimension(size(v)) :: mdot_source_function
    end function mdot_source_function

  end interface

end module driver
