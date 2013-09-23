  subroutine cp_envelope
  use ph_vars, only: ph_G,ph_c,ph_msun
  use cp_vars, only: cp_env_mass,cp_env_radius,cp_env_vesc,&
                     cp_env_mdot_in,cp_env_mdot_out,cp_env_mdot,&
                     cp_don_radius,cp_tot_mass,cp_don_mdot,&
                     cp_L_edd,cp_ejection_eff,cp_tot_mass,cp_acc_radius
  implicit none
  cp_env_radius   = 1.7e15*sqrt(cp_env_mass/(0.5*ph_msun))
  cp_env_radius   = max(cp_env_radius,cp_acc_radius)
  cp_env_vesc     = sqrt(2.*ph_G*(cp_tot_mass+cp_env_mass)/cp_env_radius)
  cp_env_mdot_in  = abs(cp_ejection_eff*cp_don_mdot)
  cp_env_mdot_out = cp_L_edd / (ph_c*cp_env_vesc) 
  cp_env_mdot     = cp_env_mdot_in - cp_env_mdot_out
  end subroutine cp_envelope
