  subroutine dr_perform_phase_3
  use ph_vars, only: ph_msun,ph_year,ph_mchandra
  use dr_vars, only: dr_setup_mode,dr_mode_period,dr_mode_contact,&
                     dr_integration_mode,dr_mode_mdot,dr_mode_ballistic,&
                     dr_dif_mass_don,dr_low_mass,dr_hig_mass,dr_step_counter,&
                     dr_time,dr_time_eddington_elapsed,dr_hybrid
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_don_freq,cp_acc_freq,cp_initial_peri,&
                     cp_don_mdot,cp_bin_sepa,cp_virtemp,cp_impact_dens,cp_bin_peri,&
                     cp_env_mass
  use io_vars, only: io_save,io_unit,io_first_pass,&
                     io_path,io_file,io_verb,io_logf,io_data
  use dr_interface, only: dr_perform_mdot_evolution,dr_perform_ballistic_evolution,dr_reset
  use io_interface, only: io_allocate_data,io_deallocate_data,io_save_data,&
                          io_cli_options,io_newline,io_open
  use ph_interface, only: ph_triple_alpha_timescale
  implicit none
  integer :: stat
  logical :: run
  real :: m_stable,m_disk,m_edd,ma,md
  real :: sep,donfreq,accfreq,mdot,virtemp,nuctime,donmass,accmass,impdens
  real :: meje,timesim,timeedd,perif,perii
  character(len=100)            :: donmass_char,accmass_char,dumpath
  character(len=6)  , parameter :: f = "(f6.4)"
  dumpath = trim(adjustl(io_path))
  call io_log("[driver] Phase 3 of stability analysis has been called")
  call io_log("[driver] Performing mdot runs over parameter space")
! This file holds the original guesses for the critical masses
  open(unit=io_unit+2,file=trim(adjustl(io_path))//"/stability_2.dat",status="old")
  call io_open(io_path,"stability_3.dat") 
  io_file = "stability_3.dat"
  open(unit=io_unit+3,file=trim(adjustl(io_path))//"/stability_3.dat",status="old",position="append")
  write(io_unit+3,*) "# 1 macc-ini"
  write(io_unit+3,*) "# 2 mdon-ini"  
  write(io_unit+3,*) "# 3 macc-fin" 
  write(io_unit+3,*) "# 4 mdon_fin"
  write(io_unit+3,*) "# 5 mdot"  
  write(io_unit+3,*) "# 6 dens" 
  write(io_unit+3,*) "# 7 temp"  
  write(io_unit+3,*) "# 8 tnuc" 
  write(io_unit+3,*) "# 9 don-freq"
  write(io_unit+3,*) "# 10 acc-freq"
  write(io_unit+3,*) "# 11 total simtime"
  write(io_unit+3,*) "# 12 timeedd"
  write(io_unit+3,*) "# 13 meje"
  write(io_unit+3,*) "# 14 peri-ini"
  write(io_unit+3,*) "# 15 peri-fin"
  close(io_unit+3) 
  stat = 0
  do while (stat.eq.0)
    read(io_unit+2,*,iostat=stat) ma,m_stable,m_disk,m_edd
    md      = min(ma,ph_mchandra/ph_msun)-2.*dr_dif_mass_don
    run     = .false.
    if (ma.ge.dr_low_mass.and.ma.le.dr_hig_mass) run = .true.
    write(accmass_char,f) ma
  ! Let's do a batch of runs 
    do while (md.ge.m_edd.and.run)
    ! Doing an mdot evolution run and saving some data from it
      donmass             = md*ph_msun
      accmass             = ma*ph_msun
      write(donmass_char,f) md
      call dr_reset
      call io_cli_options
      dr_integration_mode = dr_mode_mdot
      dr_hybrid           = .false.
      io_verb             = .true.
      io_save             = .true.
      io_path             = trim(adjustl(dumpath))&
                     //"/"//trim(adjustl(accmass_char))&
                     //"/"//trim(adjustl(donmass_char))
      call io_open(io_path,io_logf) 
      call io_log("[driver] Performing mdot evolution")
      io_file             = "mdot.dat"
      call dr_perform_mdot_evolution(dr_mode_contact,&
                                     donmass,accmass,0.,0.,0.)
      timesim = dr_time
      timeedd = dr_time_eddington_elapsed
      call io_log("[driver] Done with mdot evolution")
      call io_log("[driver] Performing ballistic evolution")
      dr_integration_mode = dr_mode_ballistic
      io_file             = "ballistic_trayectory.dat" 
      call dr_perform_ballistic_evolution(dr_mode_period,&
                                          cp_don_mass,cp_acc_mass,cp_bin_peri,&
                                          cp_don_freq,cp_acc_freq)
      call io_log("[driver] Done with ballistic evolution")
      call io_log("[driver] Calling plotting routine")
      dr_integration_mode = dr_mode_mdot
      dr_hybrid = .true.
      call io_plot
      call io_log("[driver] Done with plotting routine")
      io_path = dumpath
      donmass = cp_don_mass
      accmass = cp_acc_mass
      donfreq = cp_don_freq
      accfreq = cp_acc_freq
      mdot    = cp_don_mdot
      sep     = cp_bin_sepa
      perii   = cp_initial_peri
      perif   = cp_bin_peri
      meje    = ma+md-(accmass/ph_msun+donmass/ph_msun)
      virtemp = cp_virtemp
      impdens = cp_impact_dens
      nuctime = ph_triple_alpha_timescale(virtemp,impdens)
    ! Now to saving that data
      call io_allocate_data(15)
      if (nuctime.ge.Huge(nuctime)) nuctime = Huge(nuctime)
      io_data = (/ ma, &
                 & md, &
                 & accmass/ph_msun,  &
                 & donmass/ph_msun,  &
                 & mdot*ph_year/ph_msun,&
                 & impdens, &
                 & virtemp, &
                 & nuctime, &
                 & donfreq, &
                 & accfreq, &
                 & timesim, &
                 & timeedd, &
                 & meje,    &
                 & perii,   &
                 & perif /)
    ! Do we keep this data or not? 
      call io_log("[driver] Saving data")
      if (abs(mdot)*ph_year/ph_msun.lt.10e-10) then 
        io_save = .false.
        call io_log("[driver] Simulation ignored")
      else 
        io_save = .true.
      end if
      call io_save_data(io_path,"stability_3.dat")
      call io_deallocate_data
      call io_log("[driver] Done saving")
    ! Advancing donor mass
      md = md - dr_dif_mass_don
    end do
    if (run) call io_newline(io_path,"stability_3.dat")
  end do
  close(io_unit+2)
  end subroutine dr_perform_phase_3
