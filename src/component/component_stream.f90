  subroutine cp_stream_parameters
  use   physics, only: pi,k_B,ph_norm,ph_roche_pot
  use component, only: cp_donor_mass,cp_donor_position,cp_accretor_mass,cp_accretor_position,&
                       cp_L1_position,cp_binary_separation,cp_binary_frequency,&
                       cp_initial_position,cp_initial_velocity,cp_initial_speed,&
                       cp_initial_density,cp_initial_potential,&
                       cp_stream_theta,cp_stream_sound_speed,cp_stream_temperature,&
                       cp_stream_molecular_mass,cp_mass_ratio,cp_mdot_donor
  implicit none
  real :: mu,x_L1,A_lubow,num,dum,q_dum,g_y,g_z,S

! Calculating stream angle
  mu      = cp_accretor_mass / (cp_donor_mass+cp_accretor_mass)
  x_L1    = cp_L1_position(1) / cp_binary_separation
  A_lubow = ( mu / (abs( x_L1 - 1.0 + mu )**3.0) ) + ( (1.0-mu) / (abs( x_L1 + mu )**3.0) ) 
  cp_stream_theta = - sqrt(8.0/(9.0*A_lubow)) * sqrt( 1.0 - (2.0/A_lubow) + 3.0*sqrt(1.0 -(8.0/(9.0*A_lubow))))
  cp_stream_theta = asin(cp_stream_theta) / 2.0

! Setting the sound speed
  cp_stream_sound_speed = sqrt(k_B*cp_stream_temperature/cp_stream_molecular_mass)

! Getting initial conditions for the stream
  cp_initial_position  = cp_L1_position
  cp_initial_velocity  = (/ cos(cp_stream_theta),sin(cp_stream_theta) /)
  cp_initial_velocity  = cp_stream_sound_speed*cp_initial_velocity
  cp_initial_speed     = ph_norm(cp_initial_velocity)
  cp_initial_potential = ph_roche_pot(cp_initial_position,&
                                      cp_donor_position,cp_accretor_position,&
                                      cp_donor_mass,cp_accretor_mass,&
                                      cp_binary_frequency)

! Getting the initial density Boyarchuv, Bisikalo, Kuznetsov, Chechetkin (2002)
  x_L1  = (cp_L1_position(1)-cp_donor_position(1))/cp_binary_separation
  q_dum = 1./cp_mass_ratio
  num   = 2.*sqrt(2.*(q_dum+1))
  dum   = q_dum / (x_L1**3.) + 1./((1.-x_L1)**3.)
  g_y   = num / sqrt(dum-q_dum-1)
  g_z   = num / sqrt(dum)
  S     = (pi/4.)*((cp_stream_sound_speed/cp_binary_frequency)**2.)*g_y*g_z
  cp_initial_density = abs(cp_mdot_donor)/(S*cp_initial_speed)

  end subroutine cp_stream_parameters
