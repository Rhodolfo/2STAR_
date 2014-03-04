  subroutine dr_interrupt_mdot
  use     ode, only: ode_dt_suggested
  use ph_vars, only: ph_G,ph_msun,ph_year
  use cp_vars, only: cp_bin_sepa,cp_bin_peri,cp_don_radius,cp_don_mass,&
                     cp_don_mdot,cp_mdot_edd,cp_mdot_eq,cp_don_mass,cp_tot_mass,&
                     cp_min_tscale,cp_pot_ra,cp_pot_l1,cp_env_mass
  use dr_vars, only: dr_time,dr_time_tolerance,dr_time_step,dr_time_step_old,&
                     dr_exit_trigger,dr_interrupting,dr_stop_on_eddington,&
                     dr_force_write,dr_accretion_flow,dr_is_super_eddington,&
                     dr_screen_counter,dr_file_counter,& 
                     dr_step_counter,dr_res_factor,dr_mdot_new,dr_mdot_ref,&
                     dr_head_counter,dr_period_new,dr_period_ref,&
                     dr_threshhold_reached,dr_mdotdot,dr_mdot_new,&
                     dr_mdot_max,dr_mdot_old,dr_mdotdot,dr_quotchk,dr_tmdon,dr_tmdot,&
                     dr_time_contact,dr_time_peak
  use io_vars, only: io_save,io_verb,io_path,io_unit
  use dr_interface, only: dr_store_mdot_data,dr_store_full_mdot_data,&
                          dr_store_envelope, dr_store_pdots,dr_store_drag,&
                          dr_header_env,dr_header_pdots
  use io_interface, only: io_2string,io_log,io_quick_write
  implicit none
  logical           :: dumverb
  character(len=40) :: forcing_message
  dumverb = io_verb
  io_verb = .true.

! Some sanity checks
  if (.not.dr_exit_trigger.and.dr_step_counter.gt.10) then
    if (dr_time_step.le.0.0) then 
      call io_log("[driver] Anomalous time step encountered,&
                          & exiting with time step = "//io_2string(dr_time_step))
      dr_exit_trigger = .true. 
    else if (cp_bin_sepa.lt.cp_don_radius) then
      call io_log("[driver] System has merged, exiting.&
     &  a = "//trim(adjustl(io_2string(cp_bin_sepa)))//&
     &" r = "//trim(adjustl(io_2string(cp_don_radius))))
      dr_exit_trigger = .true.
    else if (dr_time_step/dr_res_factor.lt.10.*cp_bin_peri.and.abs(dr_mdot_ref).gt.0.) then
      call io_log("[driver] Timescales are of the order of a binary period, done")
      dr_exit_trigger = .true.
    else if ((cp_min_tscale.lt.100.*cp_bin_peri)&
        .and.(abs(cp_don_mdot).ge.cp_mdot_edd)) then
      call io_log("[driver] Averaged orbit equations are no longer valid")
      dr_exit_trigger = .true. 
    else if (abs(cp_don_mdot).gt.cp_mdot_edd.and.dr_stop_on_eddington) then
      call io_log("[driver] Mass transfer has reached super Eddington rates")
      dr_exit_trigger = .true.
    else if (abs(cp_don_mdot).gt.1e2*ph_msun/ph_year) then !cp_tot_mass/year) then
      if (dr_threshhold_reached) then 
      dr_exit_trigger = .true.
      call io_log("[driver] Mass transfer threshhold reached")
      end if
      dr_threshhold_reached = .true. 
      if (.not.dr_exit_trigger) then 
      call io_log("[driver] Simulation flagged for reaching thresshold") 
      end if
    else if (abs(dr_mdot_new/dr_mdotdot).ge.0.3*abs(cp_don_mass/dr_mdot_new)) then 
      if (dr_time.gt.dr_time_tolerance) then 
      dr_exit_trigger = .true.
      call io_log("[driver] Andrea's max criterion reached")
      write(*,*) "t,tco,tpe,tsto",dr_time,dr_time_contact,dr_time_peak,dr_time_contact+2.0*(dr_time_peak-dr_time_contact)
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

! If the time is greater than the analytical evolution timescale, 
! then just write when the t_mdot / t_mdotdot has changed by a certain ammount
! if (dr_time.ge.dr_time_tolerance) then 
!   dr_force_write = .false.
!   dr_tmdon       = abs(cp_don_mass/cp_don_mdot)
!   dr_tmdot       = abs(cp_don_mdot/dr_mdotdot)
!   if (dr_quotchk.le.0.1) then 
!     call io_log("[driver] Time tolerance has been reached, the mass transfer history &
!                 &is probably flattening out, writting out only when tmdot/tmdon has chaged by 0.1")
!   end if
!   if (dr_tmdot/dr_tmdon.ge.dr_quotchk) then 
!     dr_force_write = .true.
!     dr_quotchk     = dr_quotchk + 0.1
!   end if  
! else
!   dr_quotchk     = 0.1
! end if


! Checks done, exiting if insane
  forcing_message = "" 
  if (dr_file_counter.eq.0) then 
    call io_log("[driver] Writting headers")
    call io_log("t/T       t         dt        dt_ode    mdot      env_mass  period    sepa      tscale") 
    call dr_header_env(io_unit,io_path,"env.dat")
    call dr_header_env(io_unit,io_path,"env_late.dat") 
    call dr_header_pdots(io_unit,io_path,"pdots.dat")
  end if  
  if (((dr_time/dr_time_tolerance).gt.(0.05*dr_screen_counter)).or.dr_interrupting.or.dr_force_write) then
   if (dr_force_write) forcing_message = ""
   if (dr_accretion_flow.eq.dr_is_super_eddington) forcing_message = "(Super Eddington)"
   call io_log(io_2string(dr_time/dr_time_tolerance)//" "//&
               io_2string(dr_time)//" "//&
               io_2string(dr_time_step_old)//" "//&
               io_2string(ode_dt_suggested)//" "//&
               io_2string(abs(cp_don_mdot)*ph_year/ph_msun)//" "//&
               io_2string(abs(cp_env_mass)/ph_msun)//" "//& 
               io_2string(cp_bin_peri)//" "//&
               io_2string(cp_bin_sepa)//" "//&
               io_2string(cp_min_tscale)//" "//&
               trim(adjustl(forcing_message)))
    dr_head_counter = dr_head_counter + 1 
    if (dr_force_write.eqv..false.) dr_screen_counter = dr_screen_counter + 1
  end if
! Write when time has elapsed
  if (((dr_time/dr_time_tolerance).ge.(0.0001*dr_file_counter)).or.dr_force_write.or.dr_interrupting) then
    call dr_store_mdot_data
    if (cp_env_mass.gt.0) then 
    if (dr_accretion_flow.eq.dr_is_super_eddington) then
    call dr_store_envelope
    end if
    end if
    call dr_store_drag
    call dr_store_pdots
    if (dr_head_counter.ge.15) then 
      call io_log("t/T       t         dt        dt_ode    mdot      env_mass  period    sepa      tscale") 
      dr_head_counter = 0
    end if 
    if (.not.dr_force_write) dr_file_counter = dr_file_counter + 1
  end if
  io_verb = dumverb
  end subroutine dr_interrupt_mdot
