      subroutine eos_helmholtz
      use eos_interface, only: eos_read_helm_table,eos_helmeos
      use eos_vars, only: eos_pres,eos_dens,eos_temp,eos_entr
      use eos_vector
! tests the eos routine
! 
! ionmax  = number of isotopes in the network
! xmass   = mass fraction of isotope i
! aion    = number of nucleons in isotope i
! zion    = number of protons in isotope i

      integer          ionmax
      parameter        (ionmax=3)
      double precision xmass(ionmax),aion(ionmax),zion(ionmax),temp,den,abar,zbar


! set the mass fractions, z's and a's of the composition
! hydrogen, heliu, and carbon
      xmass(1) = 0.75d0 ; aion(1)  = 1.0d0  ; zion(1)  = 1.0d0
      xmass(2) = 0.23d0 ; aion(2)  = 4.0d0  ; zion(2)  = 2.0d0
      xmass(3) = 0.02d0 ; aion(3)  = 12.0d0 ; zion(3)  = 6.0d0

! average atomic weight and charge
      abar   = 1.0d0/sum(xmass(1:ionmax)/aion(1:ionmax))
      zbar   = abar * sum(xmass(1:ionmax) * zion(1:ionmax)/aion(1:ionmax))

! set the input vector. pipeline is only 1 element long
! eos_temp    = 1.0d8
! eos_dens    = 1.0d6
      temp_row(1) = eos_temp
      den_row(1)  = eos_dens
      abar_row(1) = abar 
      zbar_row(1) = zbar
      jlo_eos = 1 ; jhi_eos = 1

! read the data table and call the eos
      call eos_read_helm_table
      call eos_helmeos

! write out the results
!     call eos_pretty_eos_out('helm:  ')
      eos_pres = ptot_row(1)
      eos_entr = stot_row(1)

      end subroutine eos_helmholtz
