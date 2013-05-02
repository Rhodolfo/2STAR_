  subroutine IO_write_header(unit,path,file)
  use physics
  use IO, only: IO_save,IO_unit,IO_path,IO_file,IO_verb
  use component
  use driver
  implicit none
  integer      , intent(in), optional :: unit
  character*(*), intent(in), optional :: path,file
  character(len=2)   :: comment
  character(len=100) :: p,f
  integer            :: un 
! Check wheher or not a unit is present
  un = IO_unit
  p  = IO_path
  f  = IO_file
  if (present(unit)) un = unit
  if (present(path)) p  = path
  if (present(file)) f  = file
! If not printing to screen, one should exit if IO_save is false
  select case (un) 
  case (6) 
    if (.not.IO_verb) return
    comment = ""
  case default 
    if (.not.IO_save) return
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
! Writting out the composition of the binary components
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
! Hybrid mode conditional
  if (dr_hybrid) then 
    write(un,*) comment//"dr_hybrid             = .true."
  else 
    write(un,*) comment//"dr_hybrid             = .false."
  end if
! End of Hyrbid mode conditional
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
    write(un,*) comment//"Donor mass         =",cp_donor_mass/solar_mass,"Solar masses"
    write(un,*) comment//"Donor radius       =",cp_donor_radius    ,"cm"
    write(un,*) comment//"Donor density      =",cp_donor_density   ,"g/cc" 
    write(un,*) comment//"Accretor mass      =",cp_accretor_mass/solar_mass,"Solar masses"
    write(un,*) comment//"Accretor radius    =",cp_accretor_radius ,"cm"
    write(un,*) comment//"Accretor density   =",cp_accretor_density,"g/cc"
    write(un,*) comment//" "
    write(un,*) comment//"BINARY ORBIT PARAMETERS"
    write(un,*) comment//"Binary separation  =",cp_binary_separation,"cm"
    write(un,*) comment//"Binary period      =",cp_binary_period,"seconds"
    write(un,*) comment//"Roche radius       =",cp_roche_radius,"cm"
    write(un,*) comment//"Overflow           =",1e2*cp_overflow_par/cp_donor_radius,"percent"
    write(un,*) comment//"Mass transfer rate =",cp_mdot_donor*year/solar_mass,"Solar masses per year"
    write(un,*) comment//"Binary frequency   =",cp_binary_frequency,"periods per second"
    write(un,*) comment//"Total binary mass  =",(cp_donor_mass+cp_accretor_mass)/solar_mass,"Solar masses"
    write(un,*) comment//""
! Data relevant to the path integration in the ballistic code
    write(un,*) comment//"PARAMETERS FOR THE INTEGRATION OF STREAM"
    write(un,*) comment//"Donor position     =",cp_donor_position(1)/cp_binary_separation   ,"binary separation units"
    write(un,*) comment//"Accretor position  =",cp_accretor_position(1)/cp_binary_separation,"binary separation units"
    write(un,*) comment//"Stream angle       =",cp_stream_theta*(360.0/(2.0*pi))         ,"degrees"
    write(un,*) comment//"Stream sound speed =",cp_stream_sound_speed,"cm per second"
    write(un,*) comment//"Initial position   =",cp_initial_position/cp_binary_separation,"binary separation units"
    write(un,*) comment//"Initial speed      =",cp_initial_velocity,"cm per second"
    write(un,*) comment//"Stream temperature =",cp_stream_temperature,"Kelvin"
    write(un,*) comment//" "
! Data relevant for the integration of mdot
    write(un,*) comment//"PARAMETERS FOR THE INTEGRATION OF MDOT"
    write(un,*) comment//"Mdot equilibrium   =",cp_mdot_eq*year/solar_mass,"solar masses per year"
    write(un,*) comment//"Mdot Eddington     =",-cp_mdot_eddington*year/solar_mass,"solar masses per year"
    write(un,*) comment//"Mass transfer time =",cp_mass_transfer_timescale,"seconds"
    write(un,*) comment//"Gravitational time =",cp_gravitational_timescale,"seconds"
    write(un,*) comment//"Total timescale    =",cp_total_timescale,"seconds"
    write(un,*) comment//"Relaxation time    =",cp_tau_star,"seconds"
    write(un,*) comment//"Time step          =",dr_time_step,"seconds"
    write(un,*) comment//""
    write(un,*) comment//"ENDINGTON PARAMETERS"
    write(un,*) comment//"Eddington limit    =",cp_L_eddington/solar_luminosity,"solar luminosity units"
    write(un,*) comment//"Eddington mdot     =",cp_mdot_eddington*year/solar_mass,"solar masses per year" 
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
      write(un,*) comment//"1       t(yr)        "
      write(un,*) comment//"2       dt(yr)       "
      write(un,*) comment//"3       a(cm)        "
      write(un,*) comment//"4       Period(s)    "
      write(un,*) comment//"5       M_dot_2(M/yr)"
      write(un,*) comment//"6       M_2(M)       "
      write(un,*) comment//"7       R_2(R)       "
      write(un,*) comment//"8       R_L(R)       "
      write(un,*) comment//"9       mdot_1(M/yr) "
      write(un,*) comment//"10      M_1(M/yr)    "
      write(un,*) comment//"11      R_1(cm)      "
      write(un,*) comment//"12      R_circ(cm)   "
      write(un,*) comment//"13      R_minim(cm)  "
      write(un,*) comment//"14      R_accret(cm) "
      write(un,*) comment//"15      mdot_eq(M/yr)"
      write(un,*) comment//"16      mdot_ed(M/yr)"
      write(un,*) comment//"17      mdotmir(M/yr)"
      write(un,*) comment//"18      acc_eff      "
      write(un,*) comment//"19      q_a          "
      write(un,*) comment//"20      q_stable     "
      write(un,*) comment//"21      Gravitational Wave Loss Timescale (yr)"
      write(un,*) comment//"22      Separation Timescale (yr)"
      write(un,*) comment//"23      Overflow Timescale (yr)"
      write(un,*) comment//"24      Mass Transfer Timescale (yr)"
      write(un,*) comment//"25      Mass Transfer Variation Timescale (yr)"
      write(un,*) comment//"26      Solution Relaxation Timescale, &
                                   &tau_star as defined by Ghokale (yr)"
      if (dr_hybrid.or.dr_integration_mode.eq.dr_mode_post) then
        write(un,*) comment//"27      Density at L1 (g/cc)"
        write(un,*) comment//"28      Density at Impact Point (g/cc)"
        write(un,*) comment//"29      Virial Temperature at Impact (K)" 
        write(un,*) comment//"30      Burning Timescale (s)"
      else
      end if
    else
      call dr_abort("IO_write_header","Invalid integration mode for routine")
    end if
  return
  end subroutine IO_write_header
