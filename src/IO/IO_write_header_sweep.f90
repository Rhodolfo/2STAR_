  subroutine io_write_header_sweep(p,f)
  use io_vars, only: io_unit
  implicit none
  character*(*) :: p,f
! Writing header for sweep
  open(unit=io_unit,file=trim(adjustl(p))//"/"//trim(adjustl(f)),status="unknown")
  write(io_unit,*) "# 1  Accretor mass"
  write(io_unit,*) "# 2  Donor mass"
  write(io_unit,*) "# 3  Period"
  write(io_unit,*) "# 4  Separation"
  write(io_unit,*) "# 5  Equilibrium mass transfer rate"
  write(io_unit,*) "# 6  Eddington mass transfer rate, from L1"
  write(io_unit,*) "# 7  Eddington mass trassfer rate, from infinity"
  write(io_unit,*) "# 8  Eddington luminosity"
  write(io_unit,*) "# 9  Eddington luminosity, from infinity"
  write(io_unit,*) "# 10  Mass transfer rate (should be zero)" 
  write(io_unit,*) "# 11 Accretor radius"
  write(io_unit,*) "# 12 Donor radius"
  write(io_unit,*) "# 13 Circularization radius"
  write(io_unit,*) "# 14 Minimum approach radius"
  write(io_unit,*) "# 15 Mass Ratio"
  write(io_unit,*) "# 16 Critical Mass Ratio"
  write(io_unit,*) "# 17 q_a" 
  close(io_unit)
  end subroutine io_write_header_sweep
