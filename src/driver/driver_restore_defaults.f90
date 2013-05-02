subroutine dr_restore_defaults
   use driver, only: dr_reset
use component, only: cp_reset
implicit none
call dr_reset
call cp_reset
end subroutine dr_restore_defaults
