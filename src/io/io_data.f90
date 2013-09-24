  subroutine io_allocate_data(ii)
  use io_vars, only: io_data
  implicit none
  integer, intent(in) :: ii
  if (.not.allocated(io_data)) allocate(io_data(ii))
  end subroutine io_allocate_data





  subroutine io_deallocate_data
  use io_vars, only: io_data
  implicit none
  if (allocated(io_data)) deallocate(io_data)
  end subroutine io_deallocate_data





  subroutine io_save_data(path,file)
  use io_vars, only: io_unit,io_data,io_save,io_path,io_file
  use dr_interface, only: dr_abort
  implicit none
  character*(*), optional, intent(in) :: path,file
  character(len=100)      :: p,f
  character(len=201)      :: full
  if (.not.io_save) return
  p = io_path
  f = io_file
  if (present(path)) p = path
  if (present(file)) f = file
  full = trim(adjustl(p))//"/"//trim(adjustl(f))
  if (.not.allocated(io_data)) then
    call dr_abort("io_save_data","Data array is unallocated")
  end if
  open(unit=io_unit,file=trim(adjustl(full)),status="unknown",position="append") 
  write(io_unit,*) io_data
  close(io_unit)
  return
  end subroutine io_save_data
