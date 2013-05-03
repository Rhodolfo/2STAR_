  subroutine dr_perform_phase_3
  use   physics, only: solar_mass,year,ph_triple_alpha_timescale,chandra_mass
  use    driver, only: dr_setup_mode,dr_mode_period,dr_mode_contact,&
                       dr_integration_mode,dr_mode_mdot,dr_mode_ballistic,&
                       dr_perform_mdot_evolution,dr_perform_mdot_postprocessing,&
                       dr_reset,dr_dif_mass_don,dr_low_mass,dr_hig_mass,dr_step_counter,&
                       dr_time,dr_time_eddington_elapsed,dr_hybrid
  use component, only: cp_donor_mass,cp_accretor_mass,cp_donor_freq,cp_accretor_freq,cp_initial_period,&
                       cp_mdot_donor,cp_binary_separation,cp_virial_temperature,cp_impact_density,cp_binary_period
  use        IO, only: IO_save,IO_unit,IO_data,IO_allocate_data,IO_deallocate_data,IO_first_pass,IO_plot_now,&
                       IO_save_data,IO_open,IO_read_command_line_options,IO_newline,IO_path,IO_file,IO_verb,IO_logf
  implicit none
  integer :: stat
  logical :: run
  real :: m_stable,m_disk,m_edd,ma,md
  real :: sep,donfreq,accfreq,mdot,virtemp,nuctime,donmass,accmass,impdens
  real :: meje,timesim,timeedd,periodf,periodi
  character(len=100)            :: donmass_char,accmass_char,dumpath
  character(len=6)  , parameter :: f = "(f6.4)"
  dumpath = trim(adjustl(IO_path))
  call IO_log("[driver] Phase 3 of stability analysis has been called")
  call IO_log("[driver] Performing mdot runs over parameter space")
! This file holds the original guesses for the critical masses
  open(unit=IO_unit+2,file=trim(adjustl(IO_path))//"/stability_2.dat",status="old")
  call IO_open(IO_path,"stability_3.dat") 
  IO_file = "stability_3.dat"
  open(unit=IO_unit+3,file=trim(adjustl(IO_path))//"/stability_3.dat",status="old",position="append")
  write(IO_unit+3,*) "# 1 macc-ini"
  write(IO_unit+3,*) "# 2 mdon-ini"  
  write(IO_unit+3,*) "# 3 macc-fin" 
  write(IO_unit+3,*) "# 4 mdon_fin"
  write(IO_unit+3,*) "# 5 mdot"  
  write(IO_unit+3,*) "# 6 dens" 
  write(IO_unit+3,*) "# 7 temp"  
  write(IO_unit+3,*) "# 8 tnuc" 
  write(IO_unit+3,*) "# 9 don-freq"
  write(IO_unit+3,*) "# 10 acc-freq"
  write(IO_unit+3,*) "# 11 total simtime"
  write(IO_unit+3,*) "# 12 timeedd"
  write(IO_unit+3,*) "# 13 meje"
  write(IO_unit+3,*) "# 14 period-ini"
  write(IO_unit+3,*) "# 15 period-fin"
  close(IO_unit+3) 
  IO_plot_now = .false.
  stat = 0
  do while (stat.eq.0)
    read(IO_unit+2,*,iostat=stat) ma,m_stable,m_disk,m_edd
    md      = min(ma,chandra_mass/solar_mass)-2.*dr_dif_mass_don
    run     = .false.
    if (ma.ge.dr_low_mass.and.ma.le.dr_hig_mass) run = .true.
    write(accmass_char,f) ma
  ! Let's do a batch of runs 
    do while (md.ge.m_edd.and.run)
    ! Doing an mdot evolution run and saving some data from it
      donmass             = md*solar_mass
      accmass             = ma*solar_mass
      write(donmass_char,f) md
      call dr_reset
      call IO_read_command_line_options
      dr_integration_mode = dr_mode_mdot
      dr_hybrid           = .false.
      IO_verb             = .true.
      IO_save             = .true.
      IO_path             = trim(adjustl(dumpath))&
                     //"/"//trim(adjustl(accmass_char))&
                     //"/"//trim(adjustl(donmass_char))
      call IO_open(IO_path,IO_logf) 
      call IO_log("[driver] Performing mdot evolution")
      IO_file             = "mdot.dat"
      call dr_perform_mdot_evolution(dr_mode_contact,&
                                     donmass,accmass,0.,0.,0.)
      timesim = dr_time
      timeedd = dr_time_eddington_elapsed
      call IO_log("[driver] Done with mdot evolution")
      call IO_log("[driver] Performing ballistic evolution")
      dr_integration_mode = dr_mode_ballistic
      IO_file             = "ballistic_trayectory.dat" 
      call dr_perform_ballistic_evolution(dr_mode_period,&
                                          cp_donor_mass,cp_accretor_mass,cp_binary_period,&
                                          cp_donor_freq,cp_accretor_freq)
      call IO_log("[driver] Done with ballistic evolution")
      call IO_log("[driver] Calling plotting routine")
      dr_integration_mode = dr_mode_mdot
      dr_hybrid = .true.
      call IO_plot
      call IO_log("[driver] Done with plotting routine")
      IO_path = dumpath
      donmass = cp_donor_mass
      accmass = cp_accretor_mass
      donfreq = cp_donor_freq
      accfreq = cp_accretor_freq
      mdot    = cp_mdot_donor
      sep     = cp_binary_separation
      periodi = cp_initial_period
      periodf = cp_binary_period
      meje    = ma+md-(accmass/solar_mass+donmass/solar_mass)
      virtemp = cp_virial_temperature
      impdens = cp_impact_density
      nuctime = ph_triple_alpha_timescale(virtemp,impdens)
    ! Now to saving that data
      call IO_allocate_data(15)
      if (nuctime.ge.Huge(nuctime)) nuctime = Huge(nuctime)
      IO_data = (/ ma, &
                 & md, &
                 & accmass/solar_mass,  &
                 & donmass/solar_mass,  &
                 & mdot*year/solar_mass,&
                 & impdens, &
                 & virtemp, &
                 & nuctime, &
                 & donfreq, &
                 & accfreq, &
                 & timesim, &
                 & timeedd, &
                 & meje,    &
                 & periodi, &
                 & periodf /)
    ! Do we keep this data or not? 
      call IO_log("[driver] Saving data")
      if (abs(mdot)*year/solar_mass.lt.10e-10) then 
        IO_save = .false.
        call IO_log("[driver] Simulation ignored")
      else 
        IO_save = .true.
      end if
      call IO_save_data(IO_path,"stability_3.dat")
      call IO_deallocate_data
      call IO_log("[driver] Done saving")
    ! Advancing donor mass
      md = md - dr_dif_mass_don
    end do
    if (run) call IO_newline(IO_path,"stability_3.dat")
  end do
  IO_plot_now = .true.
  close(IO_unit+2)
  end subroutine dr_perform_phase_3
