  subroutine dr_interrupt_mdot
  use     ode, only: ode_dt_suggested
  use ph_vars, only: ph_G,ph_msun,ph_year
  use cp_vars, only: cp_bin_sepa,cp_bin_peri,cp_don_radius,&
                     cp_don_mdot,cp_mdot_edd,cp_mdot_eq,cp_don_mass,cp_tot_mass,&
                     cp_min_tscale,cp_pot_ra,cp_pot_l1
  use dr_vars, only: dr_time,dr_time_tolerance,dr_time_step,dr_time_step_old,&
                     dr_exit_trigger,dr_interrupting,dr_stop_on_eddington,&
                     dr_force_write,dr_accretion_flow,dr_is_super_eddington,&
                     dr_screen_counter,dr_file_counter,dr_hybrid,& 
                     dr_step_counter,dr_res_factor,dr_mdot_new,dr_mdot_ref,&
                     dr_head_counter,dr_period_new,dr_period_ref,&
                     dr_threshhold_reached,dr_mdot_old
  use io_vars, only: io_save,io_verb,io_path
  use dr_interface, only: dr_store_mdot_data,dr_store_full_mdot_data,&
                          dr_store_envelope, dr_store_pdots
  use io_interface, only: io_2string,io_log,io_quick_write
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
    else if (cp_bin_sepa.lt.cp_don_radius) then
      call IO_log("[driver] System has merged, exiting.&
     &  a = "//trim(adjustl(IO_2string(cp_bin_sepa)))//&
     &" r = "//trim(adjustl(IO_2string(cp_don_radius))))
      dr_exit_trigger = .true.
    else if (dr_time_step/dr_res_factor.lt.10.*cp_bin_peri.and.abs(dr_mdot_ref).gt.0.) then
      call IO_log("[driver] Timescales are of the order of a binary period, done")
      dr_exit_trigger = .true.
    else if ((cp_min_tscale.lt.100.*cp_bin_peri)&
        .and.(abs(cp_don_mdot).ge.cp_mdot_edd)) then
      call IO_log("[driver] Averaged orbit equations are no longer valid")
      dr_exit_trigger = .true. 
    else if (abs(cp_don_mdot).gt.cp_mdot_edd.and.dr_stop_on_eddington) then
      call IO_log("[driver] Mass transfer has reached super Eddington rates")
      dr_exit_trigger = .true.
    else if (abs(cp_don_mdot).gt.1e2*ph_msun/ph_year) then !cp_tot_mass/year) then
      if (dr_threshhold_reached) then 
      dr_exit_trigger = .true.
      call IO_log("[driver] Mass transfer threshhold reached")
      end if
      dr_threshhold_reached = .true. 
      if (.not.dr_exit_trigger) then 
      call IO_log("[driver] Simulation flagged for reaching thresshold") 
      end if
    else
    end if
  end if

! Makes sure to write data appropriate intervals
  if (dr_exit_trigger) dr_force_write = .true.
  if ((abs(dr_mdot_new-dr_mdot_ref).ge.(0.1*abs(dr_mdot_ref))).and.&
      (abs(dr_mdot_new).ge.1e-8*ph_msun/ph_year)) then 
    dr_mdot_ref    = dr_mdot_new
    dr_force_write = .true.
  end if
  if ((abs(dr_period_new-dr_period_ref).ge.(0.05*abs(dr_period_ref)))) then 
    dr_period_ref    = dr_period_new
    dr_force_write = .true.
  end if
  if ((abs(cp_don_mdot).eq.0.).and.(dr_file_counter.ge.1)) then 
    dr_force_write = .true.
  end if

! Checks done, exiting if insane
  forcing_message = "" 
  if (dr_file_counter.eq.0) then 
    call IO_log("[driver] Writting headers")
    call IO_log("t/T       t         dt        dt_ode    mdot      mdot_old  period    sepa      tscale") 
    call IO_quick_write(io_path,"env.dat"  ,"# t dt mdot env_mdot env_mass env_radius env_vesc eje_eff")
    call IO_quick_write(io_path,"pdots.dat","# t dt mdot pdt_tot  pdt_grw  pft_dunno") 
  end if  
  if (((dr_time/dr_time_tolerance).gt.(0.05*dr_screen_counter)).or.dr_interrupting.or.dr_force_write) then
   if (dr_force_write) forcing_message = "(Forced write)"
   if (dr_accretion_flow.eq.dr_is_super_eddington) forcing_message = trim(adjustl(forcing_message))//"(Eddington)"
   call IO_log(IO_2string(dr_time/dr_time_tolerance)//" "//&
               IO_2string(dr_time)//" "//&
               IO_2string(dr_time_step_old)//" "//&
               IO_2string(ode_dt_suggested)//" "//&
               IO_2string(abs(cp_don_mdot)*ph_year/ph_msun)//" "//&
               IO_2string(abs(dr_mdot_old)*ph_year/ph_msun)//" "//& 
               IO_2string(cp_bin_peri)//" "//&
               IO_2string(cp_bin_sepa)//" "//&
               IO_2string(cp_min_tscale)//" "//&
               trim(adjustl(forcing_message)))
    dr_head_counter = dr_head_counter + 1 
    if (dr_force_write.eqv..false.) dr_screen_counter = dr_screen_counter + 1
  end if
! Write when time has elapsed
  if (((dr_time/dr_time_tolerance).ge.(0.0001*dr_file_counter)).or.dr_force_write.or.dr_interrupting) then
    call dr_store_mdot_data
    call dr_store_envelope
    call dr_store_pdots
    if (dr_head_counter.ge.15) then 
      call IO_log("t/T       t         dt        dt_ode    mdot      mdot_old  period    sepa      tscale") 
      dr_head_counter = 0
    end if 
    if (dr_hybrid) call dr_store_full_mdot_data
    if (.not.dr_force_write) dr_file_counter = dr_file_counter + 1
  end if
  IO_verb = dumverb
  end subroutine dr_interrupt_mdot
