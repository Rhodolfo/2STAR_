  subroutine io_log(mess)
  use io_vars, only: io_path,io_logf,io_unit,io_verb
  implicit none
  character*(*) :: mess
  open(unit=io_unit,file=trim(adjustl(io_path))//"/"//trim(adjustl(io_logf)),status="unknown",position="append")
  if (io_verb) write(6,*)  trim(adjustl(mess))
  write(io_unit,*) trim(adjustl(mess))
  close(io_unit)
  end subroutine io_log 
