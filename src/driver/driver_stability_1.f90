  subroutine dr_perform_phase_1
  use ph_vars, only: ph_msun
  use dr_vars, only: dr_mode_contact,&
                     dr_low_mass,dr_hig_mass,dr_dif_mass_don,dr_dif_mass_acc
  use cp_vars, only: cp_mass_ratio,cp_q_stable,cp_acc_radius,cp_min_radius,&
                     cp_mdot_eq,cp_mdot_edd
  use io_vars, only: io_save,io_data,io_path,io_verb
  use cp_interface, only: cp_init
  use io_interface, only: io_log,io_allocate_data,io_deallocate_data,io_2string,io_save_data,&
                          io_write_header_sweep,io_newline,io_open
  implicit none
  real    :: accmass,donmass,donmass_stable,donmass_disk,donmass_stable_low,donmass_disk_low
  real    :: donmass_eddington,donmass_eddington_low
  logical :: flag_stable,flag_disk,flag_low,flag_eddington
  call io_log("Phase 1 of stability analysis has been called")
  call io_log("Performing a quick sweep over parameter space")
  call io_log("Sweeping accretor mass values from "//&
               trim(adjustl(io_2string(dr_low_mass)))//&
              " to "//&
               trim(adjustl(io_2string(dr_hig_mass)))//&
              " Solar masses")
  io_verb    = .false.
  io_save    = .true.
  accmass    = dr_low_mass
  donmass    = dr_dif_mass_don
! Writing headers for the sweep
  call io_open(io_path,"stability_1.dat")
  call io_open(io_path,"stability_sweep.dat")
  call io_write_header_sweep(io_path,"stability_sweep.dat")
! On to the sweep
  do while (accmass.le.dr_hig_mass)
    flag_stable    = .false.
    flag_disk      = .false.
    flag_eddington = .false.
    flag_low       = .false.
    donmass_stable    = dr_dif_mass_don
    donmass_disk      = dr_dif_mass_don
    donmass_eddington = dr_dif_mass_don
    io_verb = .true.
    call io_log("[dr_perform_phase_1] m_acc = "//trim(adjustl(io_2string(accmass)))//" msun")
    io_verb = .false.
    do while  (donmass.le.accmass.and.donmass.le.1.4)
      call cp_init(dr_mode_contact,donmass*ph_msun,accmass*ph_msun,0.,0.,0.,0.)
      if (cp_mass_ratio.ge.cp_q_stable.and..not.flag_stable) then 
        donmass_stable = donmass
        if (.not.flag_low) donmass_stable_low = dr_dif_mass_don
        if (flag_low)      donmass_stable_low = donmass_stable - dr_dif_mass_don
        donmass_stable_low = max(dr_dif_mass_don,donmass_stable_low)
        flag_stable = .true.
      end if
      if (cp_acc_radius.ge.cp_min_radius.and..not.flag_disk) then
        donmass_disk   = donmass
        if (.not.flag_low) donmass_disk_low = dr_dif_mass_don
        if (flag_low)      donmass_disk_low = donmass_disk - dr_dif_mass_don 
        donmass_disk_low = max(dr_dif_mass_don,donmass_disk_low)
        flag_disk = .true.
      end if 
      if ( (abs(cp_mdot_eq).ge.cp_mdot_edd.or.&
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
    call io_allocate_data(7)
    io_data(1:7) =(/accmass,donmass_stable,donmass_disk,donmass_eddington,&
                            donmass_stable_low,donmass_disk_low,donmass_eddington_low/)
    call io_save_data(io_path,"stability_1.dat")
    call io_deallocate_data
  ! Writing a new line into the sweep data, so that gnuplot can handle it
    call io_newline(io_path,"stability_sweep.dat")
    donmass    = dr_dif_mass_don
    accmass    = accmass + dr_dif_mass_acc
  end do
  io_verb = .true.
  call io_log("Phase 1 of stability analysis is done")
  return
  end subroutine dr_perform_phase_1
