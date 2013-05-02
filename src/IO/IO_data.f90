  subroutine IO_allocate_data(ii)
  use IO, only: IO_data
  implicit none
  integer, intent(in) :: ii
  if (.not.allocated(io_data)) allocate(io_data(ii))
  end subroutine IO_allocate_data





  subroutine IO_deallocate_data
  use IO, only: IO_data
  implicit none
  if (allocated(io_data)) deallocate(io_data)
  end subroutine IO_deallocate_data





  subroutine IO_save_data(path,file)
  use driver, only: dr_abort
  use     IO, only: IO_unit,IO_data,IO_save,IO_path,IO_file
  implicit none
  character*(*), optional, intent(in) :: path,file
  character(len=100)      :: p,f
  character(len=201)      :: full
  if (.not.IO_save) return
  p = IO_path
  f = IO_file
  if (present(path)) p = path
  if (present(file)) f = file
  full = trim(adjustl(p))//"/"//trim(adjustl(f))
  if (.not.allocated(IO_data)) then
    call dr_abort("IO_save_data","Data array is unallocated")
  end if
  open(unit=IO_unit,file=trim(adjustl(full)),status="unknown",position="append") 
  write(IO_unit,*) IO_data
  close(IO_unit)
  return
  end subroutine IO_save_data
