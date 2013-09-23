  module dr_interface
  implicit none
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

    subroutine dr_init(mode,md,ma,me,vr,fd,fa)
    implicit none
    integer, intent(in) :: mode
    real, intent(in)    :: md,ma,me,vr,fd,fa
    end subroutine dr_init

    subroutine dr_init_ballistic(mode,md,ma,me,vr,fd,fa)
    implicit none
    integer, intent(in) :: mode
    real, intent(in)    :: md,ma,me,vr,fd,fa
    end subroutine dr_init_ballistic

    subroutine dr_init_mdot(mode,md,ma,me,vr,fd,fa)
    implicit none
    integer, intent(in) :: mode
    real, intent(in)    :: md,ma,me,vr,fd,fa
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

    subroutine dr_store_envelope
    implicit none
    end subroutine dr_store_envelope

    subroutine dr_store_pdots
    implicit none
    end subroutine dr_store_pdots





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

  end module dr_interface
