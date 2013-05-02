  subroutine dr_perform_phase_1
  use   physics, only: solar_mass
  use    driver, only: dr_mode_contact,&
                       dr_low_mass,dr_hig_mass,dr_dif_mass_don,dr_dif_mass_acc
  use component, only: cp_init,cp_mass_ratio,cp_q_stable,cp_accretor_radius,cp_min_radius,&
                       cp_mdot_eq,cp_mdot_eddington
  use        IO, only: IO_save,IO_data,IO_allocate_data,IO_deallocate_data,IO_log,IO_2string,&
                       IO_save_data,IO_write_header_sweep,IO_newline,IO_open,IO_path,IO_verb
  implicit none
  real    :: accmass,donmass,donmass_stable,donmass_disk,donmass_stable_low,donmass_disk_low
  real    :: donmass_eddington,donmass_eddington_low
  logical :: flag_stable,flag_disk,flag_low,flag_eddington
  call IO_log("Phase 1 of stability analysis has been called")
  call IO_log("Performing a quick sweep over parameter space")
  call IO_log("Sweeping accretor mass values from "//&
               trim(adjustl(IO_2string(dr_low_mass)))//&
              " to "//&
               trim(adjustl(IO_2string(dr_hig_mass)))//&
              " Solar masses")
  IO_verb    = .false.
  IO_save    = .true.
  accmass    = dr_low_mass
  donmass    = dr_dif_mass_don
! Writing headers for the sweep
  call IO_open(IO_path,"stability_1.dat")
  call IO_open(IO_path,"stability_sweep.dat")
  call IO_write_header_sweep(IO_path,"stability_sweep.dat")
! On to the sweep
  do while (accmass.le.dr_hig_mass)
    flag_stable    = .false.
    flag_disk      = .false.
    flag_eddington = .false.
    flag_low       = .false.
    donmass_stable    = dr_dif_mass_don
    donmass_disk      = dr_dif_mass_don
    donmass_eddington = dr_dif_mass_don
    IO_verb = .true.
    call IO_log("[dr_perform_phase_1] m_acc = "//trim(adjustl(IO_2string(accmass)))//" msun")
    IO_verb = .false.
    do while  (donmass.le.accmass.and.donmass.le.1.4)
      call cp_init(dr_mode_contact,donmass*solar_mass,accmass*solar_mass,0.,0.,0.)
      if (cp_mass_ratio.ge.cp_q_stable.and..not.flag_stable) then 
        donmass_stable = donmass
        if (.not.flag_low) donmass_stable_low = dr_dif_mass_don
        if (flag_low)      donmass_stable_low = donmass_stable - dr_dif_mass_don
        donmass_stable_low = max(dr_dif_mass_don,donmass_stable_low)
        flag_stable = .true.
      end if
      if (cp_accretor_radius.ge.cp_min_radius.and..not.flag_disk) then
        donmass_disk   = donmass
        if (.not.flag_low) donmass_disk_low = dr_dif_mass_don
        if (flag_low)      donmass_disk_low = donmass_disk - dr_dif_mass_don 
        donmass_disk_low = max(dr_dif_mass_don,donmass_disk_low)
        flag_disk = .true.
      end if 
      if ( (abs(cp_mdot_eq).ge.cp_mdot_eddington.or.&
            cp_mdot_eq.gt.0.).and..not.flag_eddington) then
        donmass_eddington = donmass
        if (.not.flag_low) donmass_eddington_low = dr_dif_mass_don
        if (flag_low)      donmass_eddington_low = donmass_eddington - dr_dif_mass_don 
        donmass_eddington_low = max(dr_dif_mass_don,donmass_eddington_low)
        flag_eddington = .true.
      end if     
      flag_low = .true.
    ! Writing data from the sweep
      call dr_store_sweep_data
      donmass  = donmass + dr_dif_mass_don
    end do
    donmass_stable_low = max(dr_dif_mass_don,donmass_stable_low)
    donmass_disk_low   = max(dr_dif_mass_don,donmass_disk_low)
    if (donmass_stable-donmass_stable_low.lt.dr_dif_mass_don) donmass_stable_low = donmass_stable / 2.
    if (donmass_disk-donmass_disk_low    .lt.dr_dif_mass_don) donmass_disk_low   = donmass_disk   / 2. 
    if (donmass_eddington-donmass_eddington_low.lt.dr_dif_mass_don) &
        donmass_eddington_low   = donmass_eddington / 2. 
  ! Writing data for critical masses
    call IO_allocate_data(7)
    IO_data(1:7) =(/accmass,donmass_stable,donmass_disk,donmass_eddington,&
                            donmass_stable_low,donmass_disk_low,donmass_eddington_low/)
    call IO_save_data(IO_path,"stability_1.dat")
    call IO_deallocate_data
  ! Writing a new line into the sweep data, so that gnuplot can handle it
    call IO_newline(IO_path,"stability_sweep.dat")
    donmass    = dr_dif_mass_don
    accmass    = accmass + dr_dif_mass_acc
  end do
  IO_verb = .true.
  call IO_log("Phase 1 of stability analysis is done")
  return
  end subroutine dr_perform_phase_1
