  subroutine cp_binary_tides
  use dr_vars, only: dr_initial_mdot_tstep,dr_include_tides,dr_time
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_don_radius,cp_acc_radius,&
                     cp_don_sync_time,cp_acc_sync_time,&
                     cp_don_sync_0,cp_acc_sync_0,& 
                     cp_bin_freq,cp_min_radius,cp_cir_radius,cp_roche_radius,&
                     cp_mass_ratio,cp_bin_sepa,cp_bin_peri, &
                     cp_acc_freq,cp_don_freq
  use dr_interface, only: dr_abort
  use io_interface, only: io_log
  implicit none

! Initial conditions
  if (dr_initial_mdot_tstep) then 
    call IO_log("[component] Forcing donor and accretor frequencies")
    cp_don_freq = cp_bin_freq
    cp_acc_freq = cp_bin_freq
    call IO_log("[component] Saving initial syncronization timescales and relevant data")
    cp_don_sync_0 = cp_don_sync_0/((cp_mass_ratio**2)*((cp_bin_sepa/cp_don_radius)**6))
    cp_acc_sync_0 = cp_acc_sync_0/(((1./cp_mass_ratio)**2)*((cp_bin_sepa/cp_acc_radius)**6))
  end if
  
! Evolving the syncronization timescales
  cp_don_sync_time = cp_don_sync_0*(cp_mass_ratio**2)*((cp_bin_sepa/cp_don_radius)**6)
  cp_acc_sync_time = cp_acc_sync_0*((1./cp_mass_ratio)**2)*((cp_bin_sepa/cp_acc_radius)**6)
  if (.not.dr_include_tides) then
    cp_don_sync_time = 1e12
    cp_acc_sync_time = 1e12
  end if

  return
  end subroutine cp_binary_tides
