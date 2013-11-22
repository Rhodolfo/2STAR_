  subroutine dr_header_main(unit,path,file)
  use ph_vars, only: ph_msun,ph_Lsun,ph_pi,ph_year
  use io_vars, only: io_save,io_unit,io_path,io_file,io_verb
  use cp_vars, only: cp_mass_ratio,cp_q_stable,cp_bin_sepa,cp_bin_peri,cp_bin_freq,&
                     cp_don_mass,cp_don_radius,cp_don_dens,cp_don_pos,&
                     cp_acc_mass,cp_acc_radius,cp_acc_dens,cp_acc_pos,&
                     cp_roche_radius,cp_overflow_par,cp_don_mdot,cp_mdot_eq,cp_mdot_edd,&
                     cp_stream_theta,cp_stream_cs,cp_stream_temp,&
                     cp_mass_transfer_tscale,cp_gravitational_tscale,cp_total_tscale,&
                     cp_tau_star,cp_L_edd,cp_initial_pos,cp_initial_vel
  use dr_vars, only: dr_integration_mode,dr_mode_mdot,dr_mode_post,dr_mode_ballistic,&
                     dr_donor_mode,dr_accretor_mode,dr_mode_he_white_dwarf,dr_mode_co_white_dwarf,dr_mode_neutron_star,&
                     dr_advection_mode,dr_mode_direct_impact,dr_mode_disk_accretion,dr_mode_adaptative,&
                     dr_eddington,dr_res_factor,dr_time_step
  use dr_interface, only: dr_abort
  implicit none
  integer      , intent(in), optional :: unit
  character*(*), intent(in), optional :: path,file
  character(len=2)   :: comment
  character(len=100) :: p,f
  integer            :: un 
! Check wheher or not a unit is present
  un = io_unit
  p  = io_path
  f  = io_file
  if (present(unit)) un = unit
  if (present(path)) p  = path
  if (present(file)) f  = file
! If not printing to screen, one should exit if io_save is false
  select case (un) 
  case (6) 
    if (.not.io_verb) return
    comment = ""
  case default 
    if (.not.io_save) return
    open(unit=un,file=trim(adjustl(p))//"/"//trim(adjustl(f)),status="unknown")
    comment = "# "
  end select
! On tot he header
  write(un,*) comment//"INITIAL DATA AND DERIVED QUANTITITES"
  write(un,*) comment//""
  write(un,*) comment//"MODES AND FLAGS"
! Writting out the integration mode
  if      (dr_integration_mode.eq.dr_mode_mdot.or.dr_integration_mode.eq.dr_mode_post) then
    write(un,*) comment//"dr_integration_mode   = dr_mode_mdot"
  else if (dr_integration_mode.eq.dr_mode_ballistic) then
    write(un,*) comment//"dr_integration_mode   = dr_mode_ballistic"
  else
  end if
! Writting out the compos of the binary components
! Donor mode conditional
  if      (dr_donor_mode.eq.dr_mode_he_white_dwarf) then
    write(un,*) comment//"dr_donor_mode         = dr_mode_he_white_dwarf"
  else if (dr_donor_mode.eq.dr_mode_co_white_dwarf) then
    write(un,*) comment//"dr_donor_mode         = dr_mode_co_white_dwarf"
  else if (dr_donor_mode.eq.dr_mode_neutron_star) then
    write(un,*) comment//"dr_donor_mode         = dr_mode_neutron_star"
  end if
! End of donor mode conditional
! Accretor mode conditional
  if      (dr_accretor_mode.eq.dr_mode_he_white_dwarf) then
    write(un,*) comment//"dr_accretor_mode      = dr_mode_he_white_DWARF"
  else if (dr_accretor_mode.eq.dr_mode_co_white_dwarf) then
    write(un,*) comment//"dr_accretor_mode      = dr_mode_co_white_DWARF"
  else if (dr_accretor_mode.eq.dr_mode_neutron_star) then
    write(un,*) comment//"dr_accretor_mode      = dr_mode_neutron_star"
  end if
! End of accretor mode conditional
! Are the Eddington subroutines enabled?
! Eddington mode conditional
  if (dr_eddington) then
    write(un,*) comment//"dr_eddington          = .true."
  else
    write(un,*) comment//"dr_eddington          = .false."
  end if
! End of Eddington mode conditional
! Are we dealing with direct impact or disk accretion?
! Advection mode conditional
    if (dr_advection_mode.eq.dr_mode_direct_impact) then
    write(un,*) comment//"dr_advection_mode     = dr_mode_direct_impact"
    else if (dr_advection_mode.eq.dr_mode_disk_accretion) then
    write(un,*) comment//"dr_advection_mode     = dr_mode_disk_accretion"
    else if (dr_advection_mode.eq.dr_mode_adaptative) then
    write(un,*) comment//"dr_advection_mode     = dr_mode_adaptative"
    end if
! Resolution factor
    write(un,*) comment//"dr_resolution factor  =",dr_res_factor
! End of advection mode conditional
! Let's do the rest
    write(un,*) comment//""
    write(un,*) comment//"BINARY COMPONENT PARAMETERS"
    write(un,*) comment//"Mass ratio         =",cp_mass_ratio
    write(un,*) comment//"Critical ratio     =",cp_q_stable
    write(un,*) comment//"Donor mass         =",cp_don_mass/ph_msun,"msun"
    write(un,*) comment//"Donor radius       =",cp_don_radius    ,"cm"
    write(un,*) comment//"Donor dens         =",cp_don_dens   ,"g/cc" 
    write(un,*) comment//"Accretor mass      =",cp_acc_mass/ph_msun,"msun"
    write(un,*) comment//"Accretor radius    =",cp_acc_radius ,"cm"
    write(un,*) comment//"Accretor dens      =",cp_acc_dens,"g/cc"
    write(un,*) comment//" "
    write(un,*) comment//"BINARY ORBIT PARAMETERS"
    write(un,*) comment//"Binary separation  =",cp_bin_sepa,"cm"
    write(un,*) comment//"Binary periiod     =",cp_bin_peri,"s"
    write(un,*) comment//"Roche radius       =",cp_roche_radius,"cm"
    write(un,*) comment//"Overflow           =",1e2*cp_overflow_par/cp_don_radius,"percent"
    write(un,*) comment//"Mass transfer rate =",cp_don_mdot*ph_year/ph_msun,"msun/year"
    write(un,*) comment//"Binary frequency   =",cp_bin_freq,"Hz"
    write(un,*) comment//"Total binary mass  =",(cp_don_mass+cp_acc_mass)/ph_msun,"msun"
    write(un,*) comment//" "
! Data relevant to the path integration in the ballistic code
    write(un,*) comment//"PARAMETERS FOR THE INTEGRATION OF STREAM"
    write(un,*) comment//"Donor position     =",cp_don_pos(1)/cp_bin_sepa   ,"binary separation units"
    write(un,*) comment//"Accretor position  =",cp_acc_pos(1)/cp_bin_sepa,"binary separation units"
    write(un,*) comment//"Stream angle       =",cp_stream_theta*(360.0/(2.0*ph_pi)),"degrees"
    write(un,*) comment//"Stream sound speed =",cp_stream_cs,"cm/s"
    write(un,*) comment//"Initial position   =",cp_initial_pos/cp_bin_sepa,"binary separarion units"
    write(un,*) comment//"Initial speed      =",cp_initial_vel,"cm/s"
    write(un,*) comment//"Stream temperature =",cp_stream_temp,"K"
    write(un,*) comment//" "
! Data relevant for the integration of mdot
    write(un,*) comment//"PARAMETERS FOR THE INTEGRATION OF MDOT"
    write(un,*) comment//"Mdot equilibrium   =",cp_mdot_eq*ph_year/ph_msun,"msun/year"
    write(un,*) comment//"Mdot Eddington     =",-cp_mdot_edd*ph_year/ph_msun,"msun/year"
    write(un,*) comment//"Mass transfer time =",cp_mass_transfer_tscale,"s"
    write(un,*) comment//"Gravitational time =",cp_gravitational_tscale,"s"
    write(un,*) comment//"Total timescale    =",cp_total_tscale,"s"
    write(un,*) comment//"Relaxation time    =",cp_tau_star,"s"
    write(un,*) comment//"Time step          =",dr_time_step,"s"
    write(un,*) comment//""
    write(un,*) comment//"ENDINGTON PARAMETERS"
    write(un,*) comment//"Eddington limit    =",cp_L_edd/ph_Lsun,"Lsun"
    write(un,*) comment//"Eddington mdot     =",cp_mdot_edd*ph_year/ph_msun,"msun/year" 
    write(un,*) comment//" "
    if (un.eq.6) return
    if (dr_integration_mode.eq.dr_mode_ballistic) then 
      write(un,*) comment//"EVOLUTION DATA"
      write(un,*) comment//""
      write(un,*) comment//"Column  Variable"
      write(un,*) comment//"1       t(s)         "
      write(un,*) comment//"2       dt(s)        "
      write(un,*) comment//"3       x(cm)        "
      write(un,*) comment//"4       y(cm)        "
      write(un,*) comment//"5       v_x(cm/s)    "    
      write(un,*) comment//"6       v_y(cm/s)    "  
      write(un,*) " "
    else if (dr_integration_mode.eq.dr_mode_mdot.or.dr_integration_mode.eq.dr_mode_post) then
      write(un,*) comment//"EVOLUTION DATA"
      write(un,*) comment//""
      write(un,*) comment//"Column  Variable"
      write(un,*) comment//"1       t(yr)           "
      write(un,*) comment//"2       dt(yr)          "
      write(un,*) comment//"3       a(cm)           "
      write(un,*) comment//"4       Period(s)       "
      write(un,*) comment//"5       Mdot_2(msun/yr) "
      write(un,*) comment//"6       M_2(msun)       "
      write(un,*) comment//"7       R_2(cm)         "
      write(un,*) comment//"8       R_L(cm)         "
      write(un,*) comment//"9       Mdot_1(msun/yr) "
      write(un,*) comment//"10      M_1(msun/yr)    "
      write(un,*) comment//"11      R_1(cm)         "
      write(un,*) comment//"12      R_circ(cm)      "
      write(un,*) comment//"13      R_minim(cm)     "
      write(un,*) comment//"14      R_accret(cm)    "
      write(un,*) comment//"15      Mdot_eq(msun/yr)"
      write(un,*) comment//"16      Mdot_ed(msun/yr)"
      write(un,*) comment//"17      Mdotmir(msun/yr)"
      write(un,*) comment//"18      acc_eff         "
      write(un,*) comment//"19      q_a             "
      write(un,*) comment//"20      q_stable        "
      write(un,*) comment//"21      Gravitational Wave Loss Timescale (yr)"
      write(un,*) comment//"22      Separation Timescale (yr)"
      write(un,*) comment//"23      Overflow Timescale (yr)"
      write(un,*) comment//"24      Mass Transfer Timescale (yr)"
      write(un,*) comment//"25      Mass Transfer Variation Timescale (yr)"
      write(un,*) comment//"26      Solution Relaxation Timescale, &
                                   &tau_star as defined by Ghokale (yr)"
      if (dr_integration_mode.eq.dr_mode_post) then
        write(un,*) comment//"27      Density at L1 (g/cc)"
        write(un,*) comment//"28      Density at Impact Point (g/cc)"
        write(un,*) comment//"29      Virial Temperature at Impact (K)" 
        write(un,*) comment//"30      Burning Timescale (s)"
      else
      end if
    else
      call dr_abort("dr_header_main","Invalid integration mode for routine")
    end if
  return
  end subroutine dr_header_main
