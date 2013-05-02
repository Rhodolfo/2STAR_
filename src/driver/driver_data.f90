  subroutine dr_store_mdot_data
  use   physics, only: year,solar_mass,solar_radius
  use    driver, only: dr_time,dr_time_step
  use component, only: cp_binary_separation,cp_binary_period,&
                       cp_mdot_donor,cp_donor_mass,cp_donor_radius,cp_roche_radius,&
                       cp_mdot_accretor,cp_accretor_mass,cp_accretor_radius,&
                       cp_circularization_radius,cp_min_radius,cp_accretion_radius,&
                       cp_mdot_eq,cp_mdot_eddington,cp_mdot_mir,cp_accretion_efficiency,&
                       cp_q_a,cp_q_stable,cp_gravitational_timescale,cp_total_timescale,&
                       cp_overflow_timescale,cp_mass_transfer_timescale,&
                       cp_mass_transfer_change_timescale,cp_tau_star,cp_v_escape,&
                       cp_rtau_wind,cp_teff_wind,cp_cons_wind,&
                       cp_rtau_rad,cp_teff_rad,cp_cons_rad,&
                       cp_rtau_bubble,cp_teff_bubble,cp_cons_bubble
  use        IO, only: IO_save,IO_data,IO_allocate_data,IO_deallocate_data,IO_save_data,IO_path,IO_file
  implicit none
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  call IO_allocate_data(36)
  IO_data(1:36) = (/ &
        dr_time/year,&                                 ! 1
        dr_time_step/year,&                            ! 2
        cp_binary_separation,&                         ! 3
        cp_binary_period,&                             ! 4
        cp_mdot_donor*year/solar_mass,&                ! 5
        cp_donor_mass/solar_mass,&                     ! 6
        cp_donor_radius/solar_radius,&                 ! 7
        cp_roche_radius/solar_radius,&                 ! 8
        cp_mdot_accretor*year/solar_mass,&             ! 9
        cp_accretor_mass/solar_mass,&                  ! 10
        cp_accretor_radius,&                           ! 11
        cp_circularization_radius,&                    ! 12
        cp_min_radius,&                                ! 13
        cp_accretion_radius,&                          ! 14
        cp_mdot_eq*year/solar_mass,&                   ! 15
        cp_mdot_eddington*year/solar_mass,&            ! 16
        cp_mdot_mir*year/solar_mass,&                  ! 17
        cp_accretion_efficiency, &                     ! 18
        cp_q_a, &                                      ! 19
        cp_q_stable,&                                  ! 20
        cp_gravitational_timescale/year,&              ! 21
        cp_total_timescale/year,&                      ! 22
        cp_overflow_timescale/year,&                   ! 23
        cp_mass_transfer_timescale/year,&              ! 24
        cp_mass_transfer_change_timescale/year,&       ! 25
        cp_tau_star/year,&                             ! 26
        cp_v_escape,&                                  ! 27
        cp_rtau_wind,&                                 ! 28
        cp_cons_wind,&                                 ! 29
        cp_teff_wind,&                                 ! 30
        cp_rtau_rad ,&                                 ! 31
        cp_cons_rad ,&                                 ! 32
        cp_teff_rad ,&                                 ! 33
        cp_rtau_bubble,&                               ! 34
        cp_cons_bubble,&                               ! 35
        cp_teff_bubble &                               ! 36
        /)
    call IO_save_data(IO_path,"mdot.dat")
    call IO_deallocate_data
  end subroutine dr_store_mdot_data





  subroutine dr_store_ballistic_data
  use driver, only: dr_time,dr_time_step,dr_phase_point_old
  use     IO, only: IO_save,IO_data,IO_allocate_data,IO_deallocate_data,IO_save_data,IO_path
  implicit none
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  call IO_allocate_data(6)
  IO_data(1:6) = (/ dr_time,dr_time_step,dr_phase_point_old /)
  call IO_save_data(IO_path,"ballistic_trayectory.dat")
  call IO_deallocate_data
  end subroutine dr_store_ballistic_data





  subroutine dr_store_full_mdot_data
  use   physics, only: G,year,solar_mass,solar_radius,ph_triple_alpha_timescale
  use    driver, only: dr_time,dr_time_step
  use component, only: cp_binary_separation,cp_binary_period,&
                       cp_mdot_donor,cp_donor_mass,cp_donor_radius,cp_roche_radius,&
                       cp_mdot_accretor,cp_accretor_mass,cp_accretor_radius,&
                       cp_circularization_radius,cp_min_radius,cp_accretion_radius,&
                       cp_mdot_eq,cp_mdot_eddington,cp_mdot_mir,cp_accretion_efficiency,&
                       cp_q_a,cp_q_stable,cp_gravitational_timescale,cp_total_timescale,&
                       cp_overflow_timescale,cp_mass_transfer_timescale,&
                       cp_mass_transfer_change_timescale,cp_tau_star,cp_v_escape,&
                       cp_rtau_wind,cp_teff_wind,cp_cons_wind,&
                       cp_rtau_rad,cp_teff_rad,cp_cons_rad,&
                       cp_rtau_bubble,cp_teff_bubble,cp_cons_bubble,&
                       cp_initial_density,cp_impact_density,cp_virial_temperature,cp_kinetic_energy
  use        IO, only: IO_save,IO_data,IO_allocate_data,IO_deallocate_data,IO_save_data,IO_path
  implicit none
  real :: tburn,tdyn
  if (.not.IO_save) return 
  if (allocated(IO_data)) deallocate(IO_data)
  tburn = ph_triple_alpha_timescale(cp_virial_temperature,cp_impact_density)
  tdyn  = sqrt(2.*(cp_accretor_radius**3.)/(G*cp_accretor_mass))
  call IO_allocate_data(42)
  IO_data(1:42) = (/ &
        dr_time/year,&                                 ! 1
        dr_time_step/year,&                            ! 2
        cp_binary_separation,&                         ! 3
        cp_binary_period,&                             ! 4
        cp_mdot_donor*year/solar_mass,&                ! 5
        cp_donor_mass/solar_mass,&                     ! 6
        cp_donor_radius/solar_radius,&                 ! 7
        cp_roche_radius/solar_radius,&                 ! 8
        cp_mdot_accretor*year/solar_mass,&             ! 9
        cp_accretor_mass/solar_mass,&                  ! 10
        cp_accretor_radius,&                           ! 11
        cp_circularization_radius,&                    ! 12
        cp_min_radius,&                                ! 13
        cp_accretion_radius,&                          ! 14
        cp_mdot_eq*year/solar_mass,&                   ! 15
        cp_mdot_eddington*year/solar_mass,&            ! 16
        cp_mdot_mir*year/solar_mass,&                  ! 17
        cp_accretion_efficiency, &                     ! 18
        cp_q_a, &                                      ! 19
        cp_q_stable,&                                  ! 20
        cp_gravitational_timescale/year,&              ! 21
        cp_total_timescale/year,&                      ! 22
        cp_overflow_timescale/year,&                   ! 23
        cp_mass_transfer_timescale/year,&              ! 24
        cp_mass_transfer_change_timescale/year,&       ! 25
        cp_tau_star/year,&                             ! 26
        cp_v_escape,&                                  ! 27
        cp_rtau_wind,&                                 ! 28
        cp_cons_wind,&                                 ! 29
        cp_teff_wind,&                                 ! 30
        cp_rtau_rad ,&                                 ! 31
        cp_cons_rad ,&                                 ! 32
        cp_teff_rad ,&                                 ! 33
        cp_rtau_bubble,&                               ! 34
        cp_cons_bubble,&                               ! 35
        cp_teff_bubble,&                               ! 36
        cp_initial_density,&                           ! 37
        cp_impact_density,&                            ! 38
        cp_virial_temperature,&                        ! 39
        cp_kinetic_energy,&                            ! 40
        tburn, &                                       ! 41
        tdyn   &                                       ! 42 
        /)
  call IO_save_data(IO_path,"mdot_post.dat")
  call IO_deallocate_data
  return
  end subroutine dr_store_full_mdot_data





  subroutine dr_store_sweep_data
  use   physics, only: solar_mass,year
  use component, only: cp_accretor_mass,cp_donor_mass,cp_binary_period,&
                       cp_binary_separation,cp_mdot_eq,cp_mdot_eddington,&
                       cp_mdot_eddington_infty,cp_mdot_donor,&
                       cp_L_eddington,cp_L_eddington_infty,&
                       cp_accretor_radius,cp_donor_radius,cp_circularization_radius,&
                       cp_min_radius,cp_mass_ratio,cp_q_stable,cp_q_a
  use        IO, only: IO_save,IO_data,IO_allocate_data,IO_deallocate_data,IO_save_data,IO_path
  implicit none
  if (.not.IO_save) return
  if (allocated(IO_data)) deallocate(IO_data)
  call IO_allocate_data(17)
  IO_data(1:17) = (/ &
    cp_accretor_mass/solar_mass,&             !1
    cp_donor_mass/solar_mass,   &             !2
    cp_binary_period,&                        !3
    cp_binary_separation,&                    !4
    cp_mdot_eq*year/solar_mass,&              !5
    cp_mdot_eddington*year/solar_mass,&       !6
    cp_mdot_eddington_infty*year/solar_mass,& !7
    cp_L_eddington,                         & !8
    cp_L_eddington_infty,                   & !9
    cp_mdot_donor*year/solar_mass,&           !10
    cp_accretor_radius,&                      !11
    cp_donor_radius,&                         !12
    cp_circularization_radius,&               !13
    cp_min_radius,&                           !14
    cp_mass_ratio,&                           !15
    cp_q_stable,&                             !16
    cp_q_a /)                                 !17
  call IO_save_data(IO_path,"stability_sweep.dat")
  call IO_deallocate_data
  end subroutine dr_store_sweep_data
