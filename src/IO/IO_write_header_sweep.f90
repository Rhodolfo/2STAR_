  subroutine IO_write_header_sweep(p,f)
  use IO, only: IO_unit
  implicit none
  character*(*) :: p,f
! Writing header for sweep
  open(unit=IO_unit,file=trim(adjustl(p))//"/"//trim(adjustl(f)),status="unknown")
  write(IO_unit,*) "# 1  Accretor mass"
  write(IO_unit,*) "# 2  Donor mass"
  write(IO_unit,*) "# 3  Period"
  write(IO_unit,*) "# 4  Separation"
  write(IO_unit,*) "# 5  Equilibrium mass transfer rate"
  write(IO_unit,*) "# 6  Eddington mass transfer rate, from L1"
  write(IO_unit,*) "# 7  Eddington mass trassfer rate, from infinity"
  write(IO_unit,*) "# 8  Eddington luminosity"
  write(IO_unit,*) "# 9  Eddington luminosity, from infinity"
  write(IO_unit,*) "# 10  Mass transfer rate (should be zero)" 
  write(IO_unit,*) "# 11 Accretor radius"
  write(IO_unit,*) "# 12 Donor radius"
  write(IO_unit,*) "# 13 Circularization radius"
  write(IO_unit,*) "# 14 Minimum approach radius"
  write(IO_unit,*) "# 15 Mass Ratio"
  write(IO_unit,*) "# 16 Critical Mass Ratio"
  write(IO_unit,*) "# 17 q_a" 
  close(IO_unit)
  end subroutine IO_write_header_sweep
