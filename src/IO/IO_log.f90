  subroutine IO_log(mess)
  use IO, only: IO_path,IO_logf,IO_unit,IO_verb
  implicit none
  character*(*) :: mess
  open(unit=io_unit,file=trim(adjustl(IO_path))//"/"//trim(adjustl(IO_logf)),status="unknown",position="append")
  if (io_verb) write(6,*)  trim(adjustl(mess))
  write(io_unit,*) trim(adjustl(mess))
  close(io_unit)
  end subroutine IO_log 
