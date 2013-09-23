  subroutine cp_stream_parameters
  use ph_vars, only: ph_pi,ph_k_B,ph_me,ph_amu
  use cp_vars, only: cp_don_mass,cp_don_pos,cp_acc_mass,cp_acc_pos,&
                     cp_L1_pos,cp_bin_sepa,cp_bin_freq,&
                     cp_initial_pos,cp_initial_vel,cp_initial_speed,&
                     cp_initial_dens,cp_initial_potential,&
                     cp_stream_theta,cp_stream_cs,cp_stream_temp,&
                     cp_stream_mu,cp_mass_ratio,cp_don_mdot
  use ph_interface, only: ph_norm,ph_roche_pot
  implicit none
  real :: mu,x_L1,A_lubow,num,dum,q_dum,g_y,g_z,S

! Calculating stream angle
  mu      = cp_acc_mass / (cp_don_mass+cp_acc_mass)
  x_L1    = cp_L1_pos(1) / cp_bin_sepa
  A_lubow = ( mu / (abs( x_L1 - 1.0 + mu )**3.0) ) + ( (1.0-mu) / (abs( x_L1 + mu )**3.0) ) 
  cp_stream_theta = - sqrt(8.0/(9.0*A_lubow)) * sqrt( 1.0 - (2.0/A_lubow) + 3.0*sqrt(1.0 -(8.0/(9.0*A_lubow))))
  cp_stream_theta = asin(cp_stream_theta) / 2.0

! Setting the sound speed
  cp_stream_cs = sqrt(ph_k_B*cp_stream_temp/(cp_stream_mu*ph_amu))

! Getting initial conditions for the stream
  cp_initial_pos       = cp_L1_pos
  cp_initial_vel       = (/ cos(cp_stream_theta),sin(cp_stream_theta) /)
  cp_initial_vel       = cp_stream_cs*cp_initial_vel
  cp_initial_speed     = ph_norm(cp_initial_vel)
  cp_initial_potential = ph_roche_pot(cp_initial_pos,&
                                      cp_don_pos,cp_acc_pos,&
                                      cp_don_mass,cp_acc_mass,&
                                      cp_bin_freq)

! Getting the initial dens Boyarchuv, Bisikalo, Kuznetsov, Chechetkin (2002)
  x_L1  = (cp_L1_pos(1)-cp_don_pos(1))/cp_bin_sepa
  q_dum = 1./cp_mass_ratio
  num   = 2.*sqrt(2.*(q_dum+1))
  dum   = q_dum / (x_L1**3.) + 1./((1.-x_L1)**3.)
  g_y   = num / sqrt(dum-q_dum-1)
  g_z   = num / sqrt(dum)
  S     = (ph_pi/4.)*((cp_stream_cs/cp_bin_freq)**2.)*g_y*g_z
  cp_initial_dens = abs(cp_don_mdot)/(S*cp_initial_speed)

  end subroutine cp_stream_parameters
