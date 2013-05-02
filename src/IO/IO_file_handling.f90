  subroutine IO_open(path,file)
  use IO, only: IO_unit,IO_save,IO_path,IO_file
  implicit none
  character*(*), optional, intent(in) :: path,file
  character(len=100)      :: p,f
  character(len=200)      :: full
  if (.not.IO_save) return
  p = IO_path
  f = IO_file
  if (present(path)) p = path
  if (present(file)) f = file
  call system("mkdir -p "//trim(adjustl(p))) 
  full = trim(adjustl(p))//"/"//trim(adjustl(f))
  call IO_log("Creating "//trim(adjustl(full)))
  call system("rm -f "//full)
  open(unit=IO_unit,file=trim(adjustl(full)),status="new")
  close(IO_unit)
  end subroutine IO_open





  subroutine IO_quick_write(p,f,m)
  use IO, only: IO_unit
  implicit none
  character*(*) :: p,f,m
  open(unit=IO_unit,file=trim(adjustl(p))//"/"//trim(adjustl(f)),status="unknown")
  write(IO_unit,*) trim(adjustl(m))
  close(IO_unit)
  end subroutine IO_quick_write





  subroutine IO_newline(path,file)
  use IO, only: IO_unit,IO_path,IO_file
  implicit none
  character*(*), intent(in), optional :: path,file
  character(len=100) :: p,f
  character(len=200) :: full
  p = IO_path
  f = IO_file
  if (present(path)) p = path
  if (present(file)) f = file
  full=trim(adjustl(p))//"/"//trim(adjustl(f)) 
  open(unit=IO_unit,file=trim(adjustl(full)),status="old",position="append")
  write(IO_unit,*) " "
  close(IO_unit) 
  end subroutine IO_newline
