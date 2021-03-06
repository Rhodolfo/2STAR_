  subroutine dr_store_mdot_data
  use ph_vars, only: ph_year,ph_msun
  use dr_vars, only: dr_time,dr_time_step
  use cp_vars, only: cp_bin_sepa,cp_bin_peri,&
                     cp_don_mdot,cp_don_mass,cp_don_radius,cp_roche_radius,&
                     cp_acc_mdot,cp_acc_mass,cp_acc_radius,&
                     cp_cir_radius,cp_min_radius,cp_accretion_radius,&
                     cp_mdot_eq,cp_mdot_edd,cp_accretion_eff,&
                     cp_q_a,cp_q_stable,cp_gravitational_tscale,cp_total_tscale,&
                     cp_overflow_tscale,cp_mass_transfer_tscale,&
                     cp_mass_transfer_change_tscale,cp_tau_star,&
                     cp_env_mdot,cp_env_mass,cp_env_radius,cp_env_teff,cp_wind_mdot,&
                     cp_acc_freq,cp_don_freq
  use IO_vars, only: IO_data,IO_path,IO_data,IO_save
  use IO_interface, only: IO_allocate_data,IO_deallocate_data,IO_save_data
  implicit none
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  call IO_allocate_data(32)
  IO_data(1:32) = (/ &
        dr_time/ph_year,&                              !  1
        dr_time_step/ph_year,&                         !  2
        cp_bin_sepa,&                                  !  3
        cp_bin_peri,&                                  !  4
        cp_don_mdot*ph_year/ph_msun,&                  !  5
        cp_don_mass/ph_msun,&                          !  6
        cp_don_radius,&                                !  7
        cp_don_freq,&                                  !  8 
        cp_roche_radius,&                              !  9
        cp_acc_mdot*ph_year/ph_msun,&                  ! 10
        cp_acc_mass/ph_msun,&                          ! 11
        cp_acc_radius,&                                ! 12
        cp_acc_freq,&                                  ! 13
        cp_cir_radius,&                                ! 14
        cp_min_radius,&                                ! 15
        cp_accretion_radius,&                          ! 16
        cp_mdot_eq*ph_year/ph_msun,&                   ! 17
        cp_mdot_edd*ph_year/ph_msun,&                  ! 18
        cp_accretion_eff,&                             ! 19
        cp_q_a,&                                       ! 20
        cp_q_stable,&                                  ! 21
        cp_gravitational_tscale/ph_year,&              ! 22
        cp_total_tscale/ph_year,&                      ! 23
        cp_overflow_tscale/ph_year,&                   ! 24
        cp_mass_transfer_tscale/ph_year,&              ! 25
        cp_mass_transfer_change_tscale/ph_year,&       ! 26
        cp_tau_star/ph_year,&                          ! 27
        cp_env_mdot,&                                  ! 28
        cp_env_mass,&                                  ! 29
        cp_env_radius,&                                ! 30
        cp_env_teff,&                                  ! 31
        cp_wind_mdot&                                  ! 32
        /)
    call IO_save_data(IO_path,"mdot.dat")
    call IO_deallocate_data
  end subroutine dr_store_mdot_data





  subroutine dr_store_ballistic_data
  use dr_vars, only: dr_time,dr_time_step,dr_phase_point_old
  use IO_vars, only: IO_data,IO_path,IO_data,IO_save
  use IO_interface, only: IO_allocate_data,IO_deallocate_data,IO_save_data 
  implicit none
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  call IO_allocate_data(6)
  IO_data(1:6) = (/ dr_time,dr_time_step,dr_phase_point_old /)
  call IO_save_data(IO_path,"ballistic_trayectory.dat")
  call IO_deallocate_data
  end subroutine dr_store_ballistic_data





  subroutine dr_store_full_mdot_data
  use ph_vars, only: ph_year,ph_msun,ph_G
  use dr_vars, only: dr_time,dr_time_step
  use cp_vars, only: cp_bin_sepa,cp_bin_peri,&
                     cp_don_mdot,cp_don_mass,cp_don_radius,cp_roche_radius,&
                     cp_acc_mdot,cp_acc_mass,cp_acc_radius,&
                     cp_cir_radius,cp_min_radius,cp_accretion_radius,&
                     cp_mdot_eq,cp_mdot_edd,cp_accretion_eff,&
                     cp_q_a,cp_q_stable,cp_gravitational_tscale,cp_total_tscale,&
                     cp_overflow_tscale,cp_mass_transfer_tscale,&
                     cp_mass_transfer_change_tscale,cp_tau_star,&
                     cp_env_mdot,cp_env_mass,cp_env_radius,cp_env_teff,cp_wind_mdot,&
                     cp_impact_dens,cp_initial_dens,cp_virtemp,cp_ekin,&
                     cp_acc_freq,cp_don_freq
  use IO_vars, only: IO_data,IO_path,IO_data,IO_save
  use IO_interface, only: IO_allocate_data,IO_deallocate_data,IO_save_data
  use ph_interface, only: ph_triple_alpha_timescale
  implicit none
  real :: tburn,tdyn
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  tburn = ph_triple_alpha_timescale(cp_virtemp,cp_impact_dens)
  tdyn  = sqrt(2.*(cp_acc_radius**3.)/(ph_G*cp_acc_mass)) 
  call IO_allocate_data(38)
  IO_data(1:38) = (/ &
        dr_time/ph_year,&                              !  1
        dr_time_step/ph_year,&                         !  2
        cp_bin_sepa,&                                  !  3
        cp_bin_peri,&                                  !  4
        cp_don_mdot*ph_year/ph_msun,&                  !  5
        cp_don_mass/ph_msun,&                          !  6
        cp_don_radius,&                                !  7
        cp_don_freq,&                                  !  8 
        cp_roche_radius,&                              !  8
        cp_acc_mdot*ph_year/ph_msun,&                  ! 10
        cp_acc_mass/ph_msun,&                          ! 11
        cp_acc_radius,&                                ! 12
        cp_acc_freq,&                                  ! 13
        cp_cir_radius,&                                ! 14
        cp_min_radius,&                                ! 15
        cp_accretion_radius,&                          ! 16
        cp_mdot_eq*ph_year/ph_msun,&                   ! 17
        cp_mdot_edd*ph_year/ph_msun,&                  ! 18
        cp_accretion_eff, &                            ! 19
        cp_q_a, &                                      ! 20
        cp_q_stable,&                                  ! 21
        cp_gravitational_tscale/ph_year,&              ! 22
        cp_total_tscale/ph_year,&                      ! 23
        cp_overflow_tscale/ph_year,&                   ! 24
        cp_mass_transfer_tscale/ph_year,&              ! 25
        cp_mass_transfer_change_tscale/ph_year,&       ! 26
        cp_tau_star/ph_year,&                          ! 27
        cp_env_mdot,&                                  ! 28
        cp_env_mass,&                                  ! 29
        cp_env_radius,&                                ! 30
        cp_env_teff,&                                  ! 31
        cp_wind_mdot,&                                 ! 32 
        cp_initial_dens,&                              ! 33
        cp_impact_dens,&                               ! 34
        cp_virtemp,&                                   ! 35
        cp_ekin,&                                      ! 36
        tburn,&                                        ! 37
        tdyn&                                          ! 38
        /)
  call IO_save_data(IO_path,"mdot_post.dat")
  call IO_deallocate_data
  return
  end subroutine dr_store_full_mdot_data





  subroutine dr_store_sweep_data
  use ph_vars, only: ph_msun,ph_year
  use cp_vars, only: cp_acc_mass,cp_don_mass,cp_bin_peri,&
                     cp_bin_sepa,cp_mdot_eq,cp_mdot_edd,&
                     cp_mdot_edd_infty,cp_don_mdot,&
                     cp_L_edd,cp_L_edd_infty,&
                     cp_acc_radius,cp_don_radius,cp_cir_radius,&
                     cp_min_radius,cp_mass_ratio,cp_q_stable,cp_q_a
  use IO_vars, only: IO_data,IO_path,IO_data,IO_save
  use IO_interface, only: IO_allocate_data,IO_deallocate_data,IO_save_data
  implicit none
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  call IO_allocate_data(17)
  IO_data(1:17) = (/ &
    cp_acc_mass/ph_msun,&               !1
    cp_don_mass/ph_msun,   &            !2
    cp_bin_peri,&                       !3
    cp_bin_sepa,&                       !4
    cp_mdot_eq*ph_year/ph_msun,&        !5
    cp_mdot_edd*ph_year/ph_msun,&       !6
    cp_mdot_edd_infty*ph_year/ph_msun,& !7
    cp_L_edd,                         & !8
    cp_L_edd_infty,                   & !9
    cp_don_mdot*ph_year/ph_msun,&       !10
    cp_acc_radius,&                     !11
    cp_don_radius,&                     !12
    cp_cir_radius,&                     !13
    cp_min_radius,&                     !14
    cp_mass_ratio,&                     !15
    cp_q_stable,&                       !16
    cp_q_a /)                           !17
  call IO_save_data(IO_path,"stability_sweep.dat")
  call IO_deallocate_data
  end subroutine dr_store_sweep_data





  subroutine dr_store_envelope
  use ph_vars, only: ph_year,ph_msun
  use dr_vars, only: dr_time,dr_time_step,dr_mdotdot,dr_time_tolerance,dr_mdotdot_env
  use cp_vars, only: cp_don_mdot,cp_env_mdot,cp_env_mass,cp_env_radius,cp_env_vesc,&
                     cp_ejection_eff,cp_env_mdot_in,cp_env_mdot_out,&
                     cp_env_toosmall,cp_acc_radius,cp_bin_sepa,&
                     cp_driver_drag,cp_driver_sepa,cp_zeta_sepa,cp_don_mass,&
                     cp_driver_reso_norm,cp_driver_reso,cp_driver_drag_norm,&
                     cp_mdotdot,cp_don_mass,cp_acc_mass
  use IO_vars, only: IO_data,IO_path,IO_data,IO_save
  use IO_interface, only: IO_allocate_data,IO_deallocate_data,IO_save_data
  implicit none
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  call IO_allocate_data(19)
  IO_data(1:19) = (/ &
        dr_time/ph_year,&                              ! 1
        dr_time_step/ph_year,&                         ! 2
        cp_don_mass/ph_msun,&                          ! 3
        cp_acc_mass/ph_msun,&                          ! 4
        cp_don_mdot    *ph_year/ph_msun,&              ! 5
        cp_env_mdot    *ph_year/ph_msun,&              ! 6
        cp_env_mdot_in *ph_year/ph_msun,&              ! 7
        cp_env_mdot_out*ph_year/ph_msun,&              ! 8
        cp_env_mass/ph_msun,&                          ! 9
        cp_env_radius,&                                ! 10
        cp_env_vesc,&                                  ! 11
        cp_ejection_eff,&                              ! 12
        cp_env_toosmall,&                              ! 13
        cp_acc_radius,&                                ! 14
        cp_bin_sepa,&                                  ! 15
        abs(cp_don_mdot/dr_mdotdot),&                  ! 16
        abs(cp_don_mass/cp_don_mdot),&                 ! 17
        abs(cp_env_mdot/dr_mdotdot_env),&              ! 18
        abs(cp_env_mass/cp_env_mdot)&                  ! 19
        /)
    call IO_save_data(IO_path,"env.dat")
    if (dr_time.ge.0.8*dr_time_tolerance) call IO_save_data(IO_path,"env_late.dat")
    call IO_deallocate_data
  end subroutine dr_store_envelope





  subroutine dr_store_pdots
  use dr_vars, only: dr_time, dr_time_step
  use ph_vars, only: ph_year, ph_msun
  use cp_vars
  use io_vars, only: io_data,io_save,io_path
  use io_interface, only: io_allocate_data,io_deallocate_data,io_save_data
  implicit none
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  call io_allocate_data(10)
  IO_data(1:10) = (/ &
        dr_time/ph_year,&                              ! 1
        dr_time_step/ph_year,&                         ! 2
        cp_don_mdot*ph_year/ph_msun,&                  ! 3
        cp_pdot_total,&                                ! 4
        cp_pdot_grw,&                                  ! 5
        cp_pdot_dontid,&                               ! 6
        cp_pdot_acctid,&                               ! 7 
        cp_pdot_mdot,&                                 ! 8
        cp_pdot_massfl,&                               ! 9
        cp_bin_peri &                                  ! 10 
        /)
  call IO_save_data(IO_path,"pdots.dat")
  call IO_deallocate_data
  end subroutine dr_store_pdots


  subroutine dr_store_drag
  use ph_vars, only: ph_year,ph_msun
  use dr_vars, only: dr_time,dr_time_step
  use cp_vars, only: cp_don_mdot,cp_env_mdot,cp_env_mass,cp_env_radius,cp_env_vesc,&
                     cp_ejection_eff,cp_env_mdot_in,cp_env_mdot_out,&
                     cp_env_toosmall,cp_acc_radius,cp_bin_sepa,&
                     cp_driver_drag,cp_driver_sepa,cp_zeta_sepa,cp_don_mass,&
                     cp_driver_reso_norm,cp_driver_reso,cp_driver_drag_norm,&
                     cp_drag_vel,cp_drag_area,cp_drag_dens,cp_driver_nodrag
  use IO_vars, only: IO_data,IO_path,IO_data,IO_save
  use IO_interface, only: IO_allocate_data,IO_deallocate_data,IO_save_data
  implicit none
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  call IO_allocate_data(14)
  IO_data(1:14) = (/ &
        dr_time/ph_year,&                              ! 1
        dr_time_step/ph_year,&                         ! 2
        cp_don_mdot    *ph_year/ph_msun,&              ! 3
        cp_env_mdot    *ph_year/ph_msun,&              ! 4
        cp_env_mass/ph_msun,&                          ! 5
        cp_drag_dens,&                                 ! 6
        cp_drag_area,&                                 ! 7
        cp_drag_vel,&                                  ! 8
        cp_driver_drag_norm,&                          ! 9
        cp_driver_drag,&                               ! 10
        cp_driver_reso_norm,&                          ! 11
        cp_driver_reso,&                               ! 12
        cp_driver_sepa + &
        cp_zeta_sepa*cp_don_mdot/cp_don_mass,&         ! 13 
        cp_driver_nodrag + &                          
        cp_zeta_sepa*cp_don_mdot/cp_don_mass &         ! 14 
        /)
    call IO_save_data(IO_path,"drag.dat")
    call IO_deallocate_data
  end subroutine dr_store_drag
