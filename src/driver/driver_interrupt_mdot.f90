subroutine dr_interrupt_mdot
  use rho_ode_solver
  use   physics, only: G,solar_mass,year
  use component, only: cp_binary_separation,cp_binary_period,cp_donor_radius,&
                       cp_mdot_donor,cp_mdot_eddington,cp_mdot_eq,cp_donor_mass,cp_total_mass,&
                       cp_donor_density,cp_min_timescale,cp_potential_ra,cp_potential_l1
  use    driver, only: dr_time,dr_time_tolerance,dr_time_step,dr_time_step_old,&
                       dr_exit_trigger,dr_interrupting,dr_stop_on_eddington,&
                       dr_force_write,dr_accretion_flow,dr_is_super_eddington,&
                       dr_screen_counter,dr_file_counter,dr_hybrid,& 
                       dr_store_full_mdot_data,dr_step_counter,dr_res_factor
  use        IO, only: IO_save,IO_log,IO_2string,IO_verb
  implicit none
  logical           :: dumverb
  character(len=40) :: forcing_message
  dumverb = IO_verb
  IO_verb = .true.
! Some sanity checks
  if (.not.dr_exit_trigger.and.dr_step_counter.gt.10) then
    if (dr_time_step.le.0.0) then 
      call IO_log("[driver] Anomalous time step encountered, exiting with time step = "//IO_2string(dr_time_step))
      dr_exit_trigger = .true. 
    else if (cp_binary_separation.lt.cp_donor_radius) then
      call IO_log("[driver] System has merged, exiting.&
     &  a = "//trim(adjustl(IO_2string(cp_binary_separation)))//&
     &" r = "//trim(adjustl(IO_2string(cp_donor_radius))))
      dr_exit_trigger = .true.
    else if (dr_time_step/dr_res_factor.lt.10.*cp_binary_period) then
      call IO_log("Timescales are of the order of a binary period, done")
      dr_exit_trigger = .true.
    else if ((cp_min_timescale.lt.100.*cp_binary_period)&
        .and.(abs(cp_mdot_donor).ge.cp_mdot_eddington)) then
      call IO_log("[driver] Averaged orbit equations are no longer valid")
      dr_exit_trigger = .true. 
    else if (abs(cp_mdot_donor).gt.cp_mdot_eddington.and.dr_stop_on_eddington) then
      call IO_log("[driver] Mass transfer has reached super Eddington rates")
      dr_exit_trigger = .true.
    else if (abs(cp_mdot_donor).gt.1e2*solar_mass/year) then !cp_total_mass/year) then
      call IO_log("[driver] Mass transfer threshhold reached")
      dr_exit_trigger = .true.
    else
    end if
  end if
  if (dr_exit_trigger) dr_force_write = .true.
! Checks done, exiting if insane
  forcing_message = "" 
  if (dr_file_counter.eq.0) call IO_log("t/T       t         dt        dt_ode    mdot      period    tscale")
  if (((dr_time/dr_time_tolerance).gt.(0.05*dr_screen_counter)).or.dr_interrupting.or.dr_force_write) then
   if (dr_force_write) forcing_message = "(Forced)"
   if (dr_accretion_flow.eq.dr_is_super_eddington) forcing_message = trim(adjustl(forcing_message))//"(Super Eddington)"
   call IO_log(IO_2string(dr_time/dr_time_tolerance)//" "//&
               IO_2string(dr_time)//" "//&
               IO_2string(dr_time_step_old)//" "//&
               IO_2string(rho_dt_suggested)//" "//&
               IO_2string(abs(cp_mdot_donor)*year/solar_mass)//" "//&
               IO_2string(cp_binary_period)//" "//&
               IO_2string(cp_min_timescale)//" "//&
               trim(adjustl(forcing_message)))
    if (dr_force_write.eqv..false.) dr_screen_counter = dr_screen_counter + 1
  end if
! Write when time has elapsed
  if (((dr_time/dr_time_tolerance).ge.(0.0001*dr_file_counter)).or.dr_force_write) then
    call dr_store_mdot_data
    if (dr_hybrid) call dr_store_full_mdot_data
    if (.not.dr_force_write) dr_file_counter = dr_file_counter + 1
  end if
  IO_verb = dumverb
end subroutine dr_interrupt_mdot
