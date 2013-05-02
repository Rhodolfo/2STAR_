subroutine dr_update_ballistic
  use   physics, only: ph_norm,nucleon_mass,k_B,ph_roche_pot,c
  use    driver, only: dr_time,dr_time_step,dr_phase_point_old,dr_phase_point_new,&
                       dr_hit_switch,dr_line_tolerance,dr_res_factor,&
                       dr_time_step_tolerance
  use component, only: cp_donor_position,cp_accretor_position,cp_particle_position,&
                       cp_donor_mass,cp_accretor_mass,cp_binary_frequency,&
                       cp_circularization_radius,cp_impact_potential,cp_initial_potential,&
                       cp_particle_velocity,cp_distance,cp_speed,cp_arc_distance,&
                       cp_hit_radius,cp_hit_time,cp_impact_position,cp_impact_velocity,&
                       cp_impact_potential,cp_impact_speed,cp_tangential_speed,cp_radial_speed,&
                       cp_stream_sound_speed,cp_impact_density,cp_virial_temperature,cp_kinetic_energy,&
                       cp_initial_density,cp_initial_speed,cp_binary_period,&
                       cp_stream_temperature,cp_stream_molecular_mass
  implicit none
  real               :: E,T,N
  real, dimension(2) :: normal
! Advancing time
  dr_time = dr_time + dr_time_step
! Passing new variables to old ones
  dr_phase_point_old        = dr_phase_point_new
  cp_particle_position(1:2) = dr_phase_point_new(1:2)
  cp_particle_velocity(1:2) = dr_phase_point_new(3:4)
! Doing some minor calculations
  normal          = cp_particle_position-cp_accretor_position 
  cp_distance     = ph_norm(normal)
  normal          = normal / cp_distance 
  cp_speed        = ph_norm(cp_particle_velocity)
  cp_arc_distance = cp_arc_distance + dr_time_step*cp_speed
! Have we crossed the circularization radius or the accretor surface?
  if ( (dr_hit_switch.eqv..false.).and.(cp_distance.le.cp_hit_radius) ) then
    cp_hit_time           = dr_time
    cp_impact_position    = cp_particle_position
    cp_impact_velocity    = cp_particle_velocity    
    cp_impact_potential   = ph_roche_pot(cp_impact_position,&
                            cp_donor_position,cp_accretor_position,&
                            cp_donor_mass,cp_accretor_mass,&
                            cp_binary_frequency)
    cp_impact_speed       = ph_norm(cp_impact_velocity)
    cp_radial_speed       = abs(dot_product(cp_impact_velocity,normal))
    cp_tangential_speed   = abs(cp_impact_speed**2-cp_radial_speed**2)
    cp_impact_density     = cp_initial_density*(cp_stream_sound_speed/cp_radial_speed)
    E                     = 0.5*nucleon_mass*(cp_radial_speed**2.0)
    T                     = 0.
    N                     = cp_impact_density/cp_stream_molecular_mass
    call cp_getvirtemp(E,T,N,50)
    cp_virial_temperature = T
    cp_kinetic_energy     = 0.5*nucleon_mass*(cp_impact_speed**2.0) / k_B
    dr_hit_switch         = .true.
  else
  end if 
! Setting some resolution stuff
  dr_line_tolerance = dr_res_factor*cp_circularization_radius
  dr_time_step      = min(dr_line_tolerance/cp_speed,dr_time_step_tolerance)
return
end subroutine dr_update_ballistic
