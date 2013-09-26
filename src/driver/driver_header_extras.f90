! *******************************
! *** Saves data for envelope ***
! *******************************

  subroutine dr_header_env(unit,path,file)
  use io_vars, only: io_unit,io_path,io_file
  implicit none
  integer      , intent(in), optional :: unit
  character*(*), intent(in), optional :: path,file
  character(len=2)   :: comment
  character(len=100) :: p,f
  integer            :: un 
  logical            :: is_stdout

! Check function args
  un = io_unit
  p  = io_path
  f  = io_file
  if (present(unit)) un = unit
  if (present(path)) p  = path
  if (present(file)) f  = file
  is_stdout = (un.eq.6)

! Opening and writing
  if (.not.is_stdout) &
  open(unit=un,file=trim(adjustl(path))//"/"//trim(adjustl(file)),status="unknown")
  write(un,*) "#  1 t(yr)"
  write(un,*) "#  2 dt(yr)"
  write(un,*) "#  3 mdot_don(msun/yr)"
  write(un,*) "#  4 mdot_ejected_by_binary(msun/yr)" 
  write(un,*) "#  5 mdot_ejected_by_binary(msun/yr)"
  write(un,*) "#  6 mdot_ejected_by_envelope(msun/yr)"
  write(un,*) "#  7 envelope_mass(msun)"
  write(un,*) "#  8 envelope_radius(cm)"
  write(un,*) "#  9 wind_speed(cm/s)"
  write(un,*) "# 10 ejection_efficiency(adminensional) == mdot_ejected / mdot_donor"
  write(un,*) "# 11 envelope_too_small_flag(adimensional) == 0 &
                &if envelope is over 1e-6 msun, 1 if not"
  write(un,*) "# 12 acc_radius(cm)"
  write(un,*) "# 13 bin_separation(cm)"
  if (.not.is_stdout) &
  close(un)

  end subroutine dr_header_env





! ****************************
! *** Saves data for pdots ***
! ****************************

  subroutine dr_header_pdots(unit,path,file)
  use io_vars, only: io_unit,io_path,io_file
  implicit none
  integer      , intent(in), optional :: unit
  character*(*), intent(in), optional :: path,file
  character(len=2)   :: comment
  character(len=100) :: p,f
  integer            :: un 
  logical            :: is_stdout

! Check function args
  un = io_unit
  p  = io_path
  f  = io_file
  if (present(unit)) un = unit
  if (present(path)) p  = path
  if (present(file)) f  = file
  is_stdout = (un.eq.6)

! Opening and writing
  if (un.gt.6.or.un.lt.6) then 
  open(unit=un,file=trim(adjustl(path))//"/"//trim(adjustl(file)),status="unknown")
  else
  end if
  write(un,*) "#  1 t(yr)"
  write(un,*) "#  2 dt(yr)"
  write(un,*) "#  3 mdot_don(msun/yr)"
  write(un,*) "#  4 pdot_total(adim)" 
  write(un,*) "#  5 pdot_grw(adim)"
  write(un,*) "#  6 pdot_tid_don(adim) caused by tidal torques on the donor, excluding disk"
  write(un,*) "#  7 pdot_tid_acc(adim) caused by tidal torques on the accretor, excluding disk"
  write(un,*) "#  8 pdot_mdot(adim) a term that comes from transforming adots into pdots"
  write(un,*) "#  9 pdot_mass_flows(adim) is more positive when there's a disk"
  write(un,*) "# 10 period(s) just a period"
  write(un,*) "# "
  write(un,*) "# Note on column 4 and 8: The code evolves a, so it knows  adots."
  write(un,*) "# Using kepler's 3rd law P^2 = (2*pi/G*Mtot) a^3, take the dot"
  write(un,*) "# Pdot   3 adot   1 Mdot "
  write(un,*) "# ---- = - ---- - - ---- "
  write(un,*) "#   P    2   a    2 Mtot "  
  write(un,*) "# Pdot in the above equation is column 4 of this data file"
  write(un,*) "# Define Pdot_mdot = - (1/2) P*Mdot / Mtot, that's column 8" 
  write(un,*) "# Mdot here is the total mass ejected by the system"  
  write(un,*) "# " 
  if (.not.is_stdout) &
  close(un)
  end subroutine dr_header_pdots

 
