subroutine dr_update
  use driver, only: dr_integration_mode,dr_mode_ballistic,dr_mode_mdot,&
                    dr_update_ballistic,dr_update_mdot,dr_abort
  implicit none
  select case (dr_integration_mode)
  case (dr_mode_ballistic) 
    call dr_update_ballistic
  case (dr_mode_mdot)
    call dr_update_mdot
  case default
    call dr_abort("dr_update","Invalid mode for dr_update routine")
  end select
return
end subroutine dr_update
