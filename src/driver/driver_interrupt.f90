  subroutine dr_interrupt
  use dr_vars, only: dr_accretion_flow,dr_is_super_eddington,&
                     dr_force_write,dr_force_counter,dr_file_counter,&
                     dr_step_counter,dr_integration_mode,&
                     dr_mode_ballistic,dr_mode_mdot,&
                     dr_res_factor
  use dr_interface, only: dr_interrupt_ballistic,dr_interrupt_mdot,dr_abort 
  use ode
  implicit none
  integer           :: dumb = 0
  integer           :: forcing_step
! Do not call if step was rejected by integrator
  if (ode_reject_step) return
! Force a write after a certain number of steps
  forcing_step  = floor(1./dr_res_factor)
  if (dr_force_counter.ge.forcing_step) then 
    dr_force_write   = .true.
    dr_force_counter = 0
  end if
  dr_step_counter  = dr_step_counter  + 1
  dr_force_counter = dr_force_counter + 1
  dumb             = dumb             + 1
! Interrupting routines are different with each mode
  select case (dr_integration_mode) 
  case (dr_mode_ballistic)
    call dr_interrupt_ballistic
  case (dr_mode_mdot)
    call dr_interrupt_mdot
    if (dr_accretion_flow.eq.dr_is_super_eddington) forcing_step = forcing_step**2
  case default
    call dr_abort("dr_interrupt","Invalid mode for dr_interrupt")
  end select
  dr_force_write = .false.
  return
  end subroutine dr_interrupt
