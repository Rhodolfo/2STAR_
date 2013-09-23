subroutine dr_update_ballistic
  use ph_vars, only: ph_amu,ph_k_B,ph_c
  use dr_vars, only: dr_time,dr_time_step,dr_phase_point_old,dr_phase_point_new,&
                     dr_hit_switch,dr_line_tolerance,dr_res_factor,&
                     dr_time_step_tolerance
  use cp_vars, only: cp_don_pos,cp_acc_pos,cp_particle_pos,&
                     cp_don_mass,cp_acc_mass,cp_bin_freq,&
                     cp_cir_radius,cp_impact_potential,cp_initial_potential,&
                     cp_particle_vel,cp_distance,cp_speed,cp_arc_distance,&
                     cp_hit_radius,cp_hit_time,cp_impact_pos,cp_impact_vel,&
                     cp_impact_potential,cp_impact_speed,cp_tangential_speed,cp_radial_speed,&
                     cp_stream_cs,cp_impact_dens,cp_virtemp,cp_ekin,&
                     cp_initial_dens,cp_initial_speed,cp_bin_peri,&
                     cp_stream_temp,cp_stream_mu
  use ph_interface, only: ph_roche_pot,ph_norm
  use cp_interface, only: cp_getvirtemp
  implicit none
  real               :: E,T,N
  real, dimension(2) :: normal
! Advancing time
  dr_time = dr_time + dr_time_step
! Passing new variables to old ones
  dr_phase_point_old        = dr_phase_point_new
  cp_particle_pos(1:2) = dr_phase_point_new(1:2)
  cp_particle_vel(1:2) = dr_phase_point_new(3:4)
! Doing some minor calculations
  normal          = cp_particle_pos-cp_acc_pos 
  cp_distance     = ph_norm(normal)
  normal          = normal / cp_distance 
  cp_speed        = ph_norm(cp_particle_vel)
  cp_arc_distance = cp_arc_distance + dr_time_step*cp_speed
! Have we crossed the circularization radius or the accretor surface?
  if ( (dr_hit_switch.eqv..false.).and.(cp_distance.le.cp_hit_radius) ) then
    cp_hit_time           = dr_time
    cp_impact_pos    = cp_particle_pos
    cp_impact_vel    = cp_particle_vel    
    cp_impact_potential   = ph_roche_pot(cp_impact_pos,&
                            cp_don_pos,cp_acc_pos,&
                            cp_don_mass,cp_acc_mass,&
                            cp_bin_freq)
    cp_impact_speed       = ph_norm(cp_impact_vel)
    cp_radial_speed       = abs(dot_product(cp_impact_vel,normal))
    cp_tangential_speed   = abs(cp_impact_speed**2-cp_radial_speed**2)
    cp_impact_dens     = cp_initial_dens*(cp_stream_cs/cp_radial_speed)
    E                     = 0.5*ph_amu*(cp_radial_speed**2.0)
    T                     = 0.
    N                     = cp_impact_dens/cp_stream_mu
    call cp_getvirtemp(E,T,N,50)
    cp_virtemp = T
    cp_ekin     = 0.5*ph_amu*(cp_impact_speed**2.0) / ph_k_B
    dr_hit_switch         = .true.
  else
  end if 
! Setting some resolution stuff
  dr_line_tolerance = dr_res_factor*cp_cir_radius
  dr_time_step      = min(dr_line_tolerance/cp_speed,dr_time_step_tolerance)
return
end subroutine dr_update_ballistic
