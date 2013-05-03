  subroutine IO_read_command_line_options
! This routine reads the command line options for the program 
! that uses this module.
! One can set component masses and such in the command line
! For example
! ./mdot_time -path data/test -accmass 1.2 -donmass 0.1
! will run the mdot evolution for a 0.1 Msun donor with a 1.2 Msun accretor
! an store the data in data/test
  use physics
  use driver
  use component
  use M_kracken
  use IO
  implicit none
  real              :: invar
  integer           :: iflen,ier
  character(len=40) :: accmode,donmode,flowmode,setmode,intmode
! Retrieving values from command line
  call kracken('cmd','-setup contact        &
                     &-mode mdot            &
                     &-path data/default    &
                     &-accmode wd           &
                     &-donmode wd           &
                     &-invar  0.0           &
                     &-accmass 1.0          &
                     &-donmass 0.5          &
                     &-res 0.001            &
                     &-hybrid .false.       &
                     &-eddington .true.     &
                     &-advection adapt      &                      
                     &-tidal     .false.    &
                     &-dontau    1.0        &
                     &-acctau    1.0        &
                     &-verbose .true.       &
                     &-interrupting .false. &
                     &-phase 0              &
                     &-dmdon 0.01           &
                     &-dmacc 0.01           &
                     &-minmass 0.1          &
                     &-maxmass 1.4        ')
  call retrev('cmd_setup',setmode,iflen,ier)  
  call retrev('cmd_mode',intmode,iflen,ier)   
  call retrev('cmd_path',IO_path,iflen,ier) 
  call retrev('cmd_accmode',accmode,iflen,ier) 
  call retrev('cmd_donmode',donmode,iflen,ier)     
  call retrev('cmd_advection',flowmode,iflen,ier)
  invar              = rget('cmd_invar')
  cp_donor_mass      = rget('cmd_donmass')
  cp_accretor_mass   = rget('cmd_accmass')
  cp_donor_tau       = rget('cmd_dontau')
  cp_accretor_mass   = rget('cmd_accmass')
  cp_accretor_tau    = rget('cmd_acctau')
  dr_res_factor      = rget('cmd_res')
  io_verb            = lget('cmd_verbose')
  dr_eddington       = lget('cmd_eddington')
  dr_hybrid          = lget('cmd_hybrid')
  dr_interrupting    = lget('cmd_interrupting')
  dr_include_tides   = lget('cmd_tidal')
  dr_phase           = iget('cmd_phase')
  dr_dif_mass_don    = rget('cmd_dmdon')
  dr_dif_mass_acc    = rget('cmd_dmacc')
  dr_low_mass        = rget('cmd_minmass')
  dr_hig_mass        = rget('cmd_maxmass')
  cp_donor_sync_0    = rget('cmd_dontau')
  cp_accretor_sync_0 = rget('cmd_acctau')
! Here we introduce ourselves
  call system("mkdir -p "//trim(adjustl(IO_path)))
! Calling some setup routines, these transform string values to integer values
  call IO_set_setup_mode(setmode)
  call IO_set_integration_mode(intmode)
  call IO_set_advection_mode(flowmode)
  call IO_set_star_mode(accmode,dr_accretor)
  call IO_set_star_mode(donmode,dr_donor)
! Transforming masses to CGS units
  cp_donor_mass        = cp_donor_mass*solar_mass
  cp_accretor_mass     = cp_accretor_mass*solar_mass
! Now acting accordingly to the setup mode
  if (dr_setup_mode.eq.dr_mode_period)     cp_binary_period     =  invar
  if (dr_setup_mode.eq.dr_mode_separation) cp_binary_separation =  invar
  if (dr_setup_mode.eq.dr_mode_mdot)       cp_mdot_donor        = -invar*year/solar_mass
  cp_setup_var  = invar
  if (dr_setup_mode.eq.dr_mode_mdot)       cp_setup_var         = -invar*year/solar_mass 
  IO_first_pass      = .false.
  cp_donor_sync_0    = cp_donor_sync_0*year
  cp_accretor_sync_0 = cp_accretor_sync_0*year
  return
  end subroutine IO_read_command_line_options




  subroutine IO_set_setup_mode(some_char)
  use      IO, only: IO_log
  use physics, only: year,solar_mass
  use  driver, only: dr_mode_contact,dr_mode_period,&
                     dr_mode_separation,dr_mode_mdot,&
                     dr_mode_overflow_eq,dr_mode_separation_eq,&
                     dr_mode_roche_limit,dr_integration_mode,&
                     dr_setup_mode
! Given a character some_mode which can take the values "contact", "period",
! "separation", "separation_eq", "overflow_eq", or "mdot"
! this routine assigns the right mode for setup of the program
! This routine eases is best used with command line arguments,
! it serves to ease parsing of program arguments in the command line.
  implicit none
  character(len=40),intent(in) :: some_char
  integer                      :: mode_dude
  if      (trim(adjustl(some_char)).eq."contact") then
    mode_dude = dr_mode_contact
  else if (trim(adjustl(some_char)).eq."period") then
    mode_dude = dr_mode_period
  else if (trim(adjustl(some_char)).eq."separation") then
    mode_dude = dr_mode_separation
  else if (trim(adjustl(some_char)).eq."mdot") then
    mode_dude = dr_mode_mdot
  else if (trim(adjustl(some_char)).eq."overflow_eq") then
    mode_dude = dr_mode_overflow_EQ
  else if (trim(adjustl(some_char)).eq."separation_eq") then
    mode_dude = dr_mode_separation_EQ
  else if (trim(adjustl(some_char)).eq."roche_limit") then
    mode_dude = dr_mode_roche_LIMIT
  else
    call dr_abort("IO_set_setup_mode","INVALID SETUP MODE")
  end if
  dr_setup_mode = mode_dude
  end subroutine IO_set_setup_mode





  subroutine IO_set_integration_mode(some_char)
  use driver, only: dr_mode_mdot,dr_mode_ballistic,&
                    dr_mode_stability,dr_mode_post,&
                    dr_integration_mode,dr_abort
  use     IO, only: IO_first_pass,IO_log,IO_file
! Given a character some_mode which can take the values "contact", "period",
! "separation", "separation_eq", "overflow_eq", or "mdot"
! this routine assigns the right mode for setup of the program
! This routine eases is best used with command line arguments,
! it serves to ease parsing of program arguments in the command line.
  implicit none
  character(len=40),intent(in) :: some_char
  integer                      :: mode_dude
  if      (trim(adjustl(some_char)).eq."mdot") then
    if (IO_first_pass) call IO_log("[io] Solving for mass transfer rate history")
    mode_dude = dr_mode_mdot
    IO_file   = "mdot.dat"
  else if (trim(adjustl(some_char)).eq."ballistic") then
    if (IO_first_pass) call IO_log("[io] Solving for ballistic trayectories")
    mode_dude = dr_mode_ballistic
    IO_file   = "ballistic_trayectory.dat"
  else if (trim(adjustl(some_char)).eq."stability") then
    if (IO_first_pass) call IO_log("[io] Solving for stability curves")
    mode_dude = dr_mode_stability
    IO_file   = "stability_general.dat"
  else if (trim(adjustl(some_char)).eq."postprocessing") then
    if (IO_first_pass) call IO_log("[io] Postprocessing mass transfer rate history")
    mode_dude = dr_mode_post 
    IO_file   = "mdot_post.dat"
  else
    call dr_abort("IO_set_integration_mode","INVALID PERFORMANCE MODE")
  end if
  dr_integration_mode = mode_dude
  end subroutine IO_set_integration_mode





  subroutine IO_set_star_mode(some_char,donor_accretor)
  use driver, only: dr_mode_he_white_dwarf,&
                    dr_mode_co_white_dwarf,&
                    dr_mode_neutron_star,&
                    dr_donor_mode,dr_accretor_mode,&
                    dr_donor,dr_accretor,dr_abort
  use     IO, only: IO_first_pass,IO_log
! Given a character some_mode which can take the values "he_wd", "co_wd", or "ns"
! this routine assigns the right mode for the relevant binary component
! donor_accretor. 
! For example, to set the donor to be a helium white dwarf
! -> call set_star_model("he_wd",dr_accretor)
! this is equivalent to writing
! -> accretor_mode = dr_mode_he_white_dwarf
! This routine eases is best used with command line arguments,
! it serves to ease parsing of program arguments in the command line.
  implicit none
  character(len=40),intent(in) :: some_char
  character(len=40)            :: piece1,piece2
  integer,intent(in)           :: donor_accretor
  integer                      :: mode_dude
  if      (trim(adjustl(some_char)).eq."he_wd") then
    piece1 = "Using HE WD as"
    mode_dude = dr_mode_he_white_dwarf
  else if (trim(adjustl(some_char)).eq."co_wd") then
    piece1 = "Using CO WD as"
    mode_dude = dr_mode_co_white_dwarf
  else if (trim(adjustl(some_char)).eq."ns"   ) then
    piece1 = "Using NS as"
    mode_dude = dr_mode_neutron_star
  else
    piece1 = "Using HE WD as"
    mode_dude = dr_mode_he_white_dwarf
  end if
  if (donor_accretor.eq.dr_donor) then 
    piece2 = "donor"
    dr_donor_mode = mode_dude
  else if (donor_accretor.eq.dr_accretor) then 
    piece2 = "accretor"
    dr_accretor_mode = mode_dude
  else 
    call dr_abort("IO_set_star_mode","Invalid identifier for star component, stopping")
  end if
  if (IO_first_pass) call IO_log("[io] "//trim(adjustl(piece1))//" "//trim(adjustl(piece2)))
  end subroutine IO_set_star_mode









  subroutine IO_set_advection_mode(some_char)
  use driver, only: dr_advection_mode,&
                    dr_mode_direct_impact,dr_mode_disk_accretion,&
                    dr_mode_adaptative
  use     IO, only: IO_first_pass,IO_log
  implicit none
  character(len=40) :: some_char
  character(len=40) :: piece1
  if      (trim(adjustl(some_char)).eq."direct") then
    piece1    = "Forcing direct impact"
    dr_advection_mode = dr_mode_direct_impact
  else if (trim(adjustl(some_char)).eq."disk") then
    piece1 = "Forcing disk impact"
    dr_advection_mode = dr_mode_disk_accretion
  else
    piece1 = "Adapting to size of accretor"
    dr_advection_mode = dr_mode_adaptative
  end if
  if (IO_first_pass) call IO_log("[io] "//trim(adjustl(piece1)))
  end subroutine IO_set_advection_mode 
