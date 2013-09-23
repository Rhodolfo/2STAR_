  subroutine io_open(path,file)
  use io_vars, only: io_unit,io_save,io_path,io_file
  implicit none
  character*(*), optional, intent(in) :: path,file
  character(len=100)      :: p,f
  character(len=200)      :: full
  if (.not.io_save) return
  p = io_path
  f = io_file
  if (present(path)) p = path
  if (present(file)) f = file
  call system("mkdir -p "//trim(adjustl(p))) 
  full = trim(adjustl(p))//"/"//trim(adjustl(f))
  call io_log("Creating "//trim(adjustl(full)))
  call system("rm -f "//full)
  open(unit=io_unit,file=trim(adjustl(full)),status="new")
  close(io_unit)
  end subroutine io_open





  subroutine io_quick_write(p,f,m)
  use io_vars, only: io_unit
  implicit none
  character*(*) :: p,f,m
  open(unit=io_unit,file=trim(adjustl(p))//"/"//trim(adjustl(f)),status="unknown")
  write(io_unit,*) trim(adjustl(m))
  close(io_unit)
  end subroutine io_quick_write





  subroutine io_newline(path,file)
  use io_vars, only: io_unit,io_path,io_file
  implicit none
  character*(*), intent(in), optional :: path,file
  character(len=100) :: p,f
  character(len=200) :: full
  p = io_path
  f = io_file
  if (present(path)) p = path
  if (present(file)) f = file
  full=trim(adjustl(p))//"/"//trim(adjustl(f)) 
  open(unit=io_unit,file=trim(adjustl(full)),status="old",position="append")
  write(io_unit,*) " "
  close(io_unit) 
  end subroutine io_newline
