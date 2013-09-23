  module eos_vector
  implicit none
! declaration for pipelining the eos routines

! maximum length of the row vector
      integer   nrowmax
      parameter (nrowmax = 1)
!      parameter (nrowmax = 1000)


! maximum number of isotopes
      integer   irowmax
      parameter (irowmax = 1)
!      parameter (irowmax = 600)


! maximum number of ionization stages
      integer   jstagemax
      parameter (jstagemax = 1)
!      parameter (jstagemax = 30)


! failure of an eos
      logical          eosfail



! lower and upper limits of the loop over rows
      integer          jlo_eos,jhi_eos



! thermodynamic and composition inputs
      double precision &
                temp_row(nrowmax),den_row(nrowmax), &
                abar_row(nrowmax),zbar_row(nrowmax), &
                zeff_row(nrowmax),ye_row(nrowmax)

! composition input
      integer          niso
      double precision xmass_row(irowmax,nrowmax), &
                       aion_row(irowmax,nrowmax), &
                       zion_row(irowmax,nrowmax)



! composition output
      double precision frac_row(jstagemax,irowmax,nrowmax)


! composition output for sneos
      double precision xn_row(nrowmax),xp_row(nrowmax), &
                       xa_row(nrowmax),xhv_row(nrowmax), &
                       xmuhat_row(nrowmax)

! totals and their derivatives
      double precision &
                ptot_row(nrowmax), &
                dpt_row(nrowmax),dpd_row(nrowmax), &
                dpa_row(nrowmax),dpz_row(nrowmax), &
                dpdd_row(nrowmax),dpdt_row(nrowmax), &
                dpda_row(nrowmax),dpdz_row(nrowmax), &
                dptt_row(nrowmax),dpta_row(nrowmax), &
                dptz_row(nrowmax),dpaa_row(nrowmax), &
                dpaz_row(nrowmax),dpzz_row(nrowmax)

      double precision &
                etot_row(nrowmax), &
                det_row(nrowmax),ded_row(nrowmax), &
                dea_row(nrowmax),dez_row(nrowmax), &
                dedd_row(nrowmax),dedt_row(nrowmax), &
                deda_row(nrowmax),dedz_row(nrowmax), &
                dett_row(nrowmax),deta_row(nrowmax), &
                detz_row(nrowmax),deaa_row(nrowmax), &
                deaz_row(nrowmax),dezz_row(nrowmax)

      double precision &
                stot_row(nrowmax), &
                dst_row(nrowmax),dsd_row(nrowmax), &
                dsa_row(nrowmax),dsz_row(nrowmax), &
                dsdd_row(nrowmax),dsdt_row(nrowmax), &
                dsda_row(nrowmax),dsdz_row(nrowmax), &
                dstt_row(nrowmax),dsta_row(nrowmax), &
                dstz_row(nrowmax),dsaa_row(nrowmax), &
                dsaz_row(nrowmax),dszz_row(nrowmax)

! radiation contributions
      double precision &
                prad_row(nrowmax), &
                dpradt_row(nrowmax),dpradd_row(nrowmax), &
                dprada_row(nrowmax),dpradz_row(nrowmax), &
                dpraddd_row(nrowmax),dpraddt_row(nrowmax), &
                dpradda_row(nrowmax),dpraddz_row(nrowmax), &
                dpradtt_row(nrowmax),dpradta_row(nrowmax), &
                dpradtz_row(nrowmax),dpradaa_row(nrowmax), &
                dpradaz_row(nrowmax),dpradzz_row(nrowmax)

      double precision &
                erad_row(nrowmax), &
                deradt_row(nrowmax),deradd_row(nrowmax), &
                derada_row(nrowmax),deradz_row(nrowmax), &
                deraddd_row(nrowmax),deraddt_row(nrowmax), &
                deradda_row(nrowmax),deraddz_row(nrowmax), &
                deradtt_row(nrowmax),deradta_row(nrowmax), &
                deradtz_row(nrowmax),deradaa_row(nrowmax), &
                deradaz_row(nrowmax),deradzz_row(nrowmax)

      double precision &
                srad_row(nrowmax), &
                dsradt_row(nrowmax),dsradd_row(nrowmax), &
                dsrada_row(nrowmax),dsradz_row(nrowmax), &
                dsraddd_row(nrowmax),dsraddt_row(nrowmax), &
                dsradda_row(nrowmax),dsraddz_row(nrowmax), &
                dsradtt_row(nrowmax),dsradta_row(nrowmax), &
                dsradtz_row(nrowmax),dsradaa_row(nrowmax), &
                dsradaz_row(nrowmax),dsradzz_row(nrowmax)

! gas contributions
      double precision &
                pgas_row(nrowmax), &
                dpgast_row(nrowmax),dpgasd_row(nrowmax), &
                dpgasa_row(nrowmax),dpgasz_row(nrowmax), &
                dpgasdd_row(nrowmax),dpgasdt_row(nrowmax), &
                dpgasda_row(nrowmax),dpgasdz_row(nrowmax), &
                dpgastt_row(nrowmax),dpgasta_row(nrowmax), &
                dpgastz_row(nrowmax),dpgasaa_row(nrowmax), &
                dpgasaz_row(nrowmax),dpgaszz_row(nrowmax)

      double precision &
                egas_row(nrowmax), &
                degast_row(nrowmax),degasd_row(nrowmax), &
                degasa_row(nrowmax),degasz_row(nrowmax), &
                degasdd_row(nrowmax),degasdt_row(nrowmax), &
                degasda_row(nrowmax),degasdz_row(nrowmax), &
                degastt_row(nrowmax),degasta_row(nrowmax), &
                degastz_row(nrowmax),degasaa_row(nrowmax), &
                degasaz_row(nrowmax),degaszz_row(nrowmax)

      double precision &
                sgas_row(nrowmax), &
                dsgast_row(nrowmax),dsgasd_row(nrowmax), &
                dsgasa_row(nrowmax),dsgasz_row(nrowmax), &
                dsgasdd_row(nrowmax),dsgasdt_row(nrowmax), &
                dsgasda_row(nrowmax),dsgasdz_row(nrowmax), &
                dsgastt_row(nrowmax),dsgasta_row(nrowmax), &
                dsgastz_row(nrowmax),dsgasaa_row(nrowmax), &
                dsgasaz_row(nrowmax),dsgaszz_row(nrowmax)

! ion contributions
      double precision &
                pion_row(nrowmax), &
                dpiont_row(nrowmax),dpiond_row(nrowmax), &
                dpiona_row(nrowmax),dpionz_row(nrowmax), &
                dpiondd_row(nrowmax),dpiondt_row(nrowmax), &
                dpionda_row(nrowmax),dpiondz_row(nrowmax), &
                dpiontt_row(nrowmax),dpionta_row(nrowmax), &
                dpiontz_row(nrowmax),dpionaa_row(nrowmax), &
                dpionaz_row(nrowmax),dpionzz_row(nrowmax)

      double precision &
                eion_row(nrowmax), &
                deiont_row(nrowmax),deiond_row(nrowmax), &
                deiona_row(nrowmax),deionz_row(nrowmax), &
                deiondd_row(nrowmax),deiondt_row(nrowmax), &
                deionda_row(nrowmax),deiondz_row(nrowmax), &
                deiontt_row(nrowmax),deionta_row(nrowmax), &
                deiontz_row(nrowmax),deionaa_row(nrowmax), &
                deionaz_row(nrowmax),deionzz_row(nrowmax)

      double precision &
                sion_row(nrowmax), &
                dsiont_row(nrowmax),dsiond_row(nrowmax), &
                dsiona_row(nrowmax),dsionz_row(nrowmax), &
                dsiondd_row(nrowmax),dsiondt_row(nrowmax), &
                dsionda_row(nrowmax),dsiondz_row(nrowmax), &
                dsiontt_row(nrowmax),dsionta_row(nrowmax), &
                dsiontz_row(nrowmax),dsionaa_row(nrowmax), &
                dsionaz_row(nrowmax),dsionzz_row(nrowmax)

      double precision &
                etaion_row(nrowmax), &
                detait_row(nrowmax),detaid_row(nrowmax), &
                detaia_row(nrowmax),detaiz_row(nrowmax), &
                detaidd_row(nrowmax),detaidt_row(nrowmax), &
                detaida_row(nrowmax),detaidz_row(nrowmax), &
                detaitt_row(nrowmax),detaita_row(nrowmax), &
                detaitz_row(nrowmax),detaiaa_row(nrowmax), &
                detaiaz_row(nrowmax),detaizz_row(nrowmax)

      double precision &
                xni_row(nrowmax),xnim_row(nrowmax), &
                dxnit_row(nrowmax),dxnid_row(nrowmax), &
                dxnia_row(nrowmax),dxniz_row(nrowmax), &
                dxnidd_row(nrowmax),dxnidt_row(nrowmax), &
                dxnida_row(nrowmax),dxnidz_row(nrowmax), &
                dxnitt_row(nrowmax),dxnita_row(nrowmax), &
                dxnitz_row(nrowmax),dxniaa_row(nrowmax), &
                dxniaz_row(nrowmax),dxnizz_row(nrowmax)

! electron-positron contributions

      double precision &
               etaele_row(nrowmax),etapos_row(nrowmax), &
               detat_row(nrowmax),detad_row(nrowmax), &
               detaa_row(nrowmax),detaz_row(nrowmax), &
               detadd_row(nrowmax),detadt_row(nrowmax), &
               detada_row(nrowmax),detadz_row(nrowmax), &
               detatt_row(nrowmax),detata_row(nrowmax), &
               detatz_row(nrowmax),detaaa_row(nrowmax), &
               detaaz_row(nrowmax),detazz_row(nrowmax)

      double precision &
               pele_row(nrowmax),ppos_row(nrowmax), &
               dpept_row(nrowmax),dpepd_row(nrowmax), &
               dpepa_row(nrowmax),dpepz_row(nrowmax), &
               dpepdd_row(nrowmax),dpepdt_row(nrowmax), &
               dpepda_row(nrowmax),dpepdz_row(nrowmax), &
               dpeptt_row(nrowmax),dpepta_row(nrowmax), &
               dpeptz_row(nrowmax),dpepaa_row(nrowmax), &
               dpepaz_row(nrowmax),dpepzz_row(nrowmax)

      double precision &
               eele_row(nrowmax),epos_row(nrowmax), &
               deept_row(nrowmax),deepd_row(nrowmax), &
               deepa_row(nrowmax),deepz_row(nrowmax), &
               deepdd_row(nrowmax),deepdt_row(nrowmax), &
               deepda_row(nrowmax),deepdz_row(nrowmax), &
               deeptt_row(nrowmax),deepta_row(nrowmax), &
               deeptz_row(nrowmax),deepaa_row(nrowmax), &
               deepaz_row(nrowmax),deepzz_row(nrowmax)

      double precision &
               sele_row(nrowmax),spos_row(nrowmax), &
               dsept_row(nrowmax),dsepd_row(nrowmax), &
               dsepa_row(nrowmax),dsepz_row(nrowmax), &
               dsepdd_row(nrowmax),dsepdt_row(nrowmax), &
               dsepda_row(nrowmax),dsepdz_row(nrowmax), &
               dseptt_row(nrowmax),dsepta_row(nrowmax), &
               dseptz_row(nrowmax),dsepaa_row(nrowmax), &
               dsepaz_row(nrowmax),dsepzz_row(nrowmax)

      double precision &
               xne_row(nrowmax),xnp_row(nrowmax),xnem_row(nrowmax), &
               dxnet_row(nrowmax),dxned_row(nrowmax), &
               dxnea_row(nrowmax),dxnez_row(nrowmax), &
               dxnedd_row(nrowmax),dxnedt_row(nrowmax), &
               dxneda_row(nrowmax),dxnedz_row(nrowmax), &
               dxnett_row(nrowmax),dxneta_row(nrowmax), &
               dxnetz_row(nrowmax),dxneaa_row(nrowmax), &
               dxneaz_row(nrowmax),dxnezz_row(nrowmax)

! ionization potential contributions
      double precision pip_row(nrowmax), &
                       eip_row(nrowmax), &
                       sip_row(nrowmax)

! coulomb contributions
      double precision &
                pcou_row(nrowmax), &
                dpcout_row(nrowmax),dpcoud_row(nrowmax), &
                dpcoua_row(nrowmax),dpcouz_row(nrowmax), &
                ecou_row(nrowmax), &
                decout_row(nrowmax),decoud_row(nrowmax), &
                decoua_row(nrowmax),decouz_row(nrowmax), &
                scou_row(nrowmax), &
                dscout_row(nrowmax),dscoud_row(nrowmax), &
                dscoua_row(nrowmax),dscouz_row(nrowmax), &
                plasg_row(nrowmax)

! thermodynamic consistency checks; maxwell relations
      double precision &
                dse_row(nrowmax),dpe_row(nrowmax),dsp_row(nrowmax)

! derivative based quantities for the gas
      double precision &
                cp_gas_row(nrowmax), &
                dcp_gasdd_row(nrowmax),dcp_gasdt_row(nrowmax), &
                dcp_gasda_row(nrowmax),dcp_gasdz_row(nrowmax), &
                cv_gas_row(nrowmax), &
                dcv_gasdd_row(nrowmax),dcv_gasdt_row(nrowmax), &
                dcv_gasda_row(nrowmax),dcv_gasdz_row(nrowmax)

      double precision &
                gam1_gas_row(nrowmax), &
                dgam1_gasdd_row(nrowmax),dgam1_gasdt_row(nrowmax), &
                dgam1_gasda_row(nrowmax),dgam1_gasdz_row(nrowmax), &
                gam2_gas_row(nrowmax), &
                dgam2_gasdd_row(nrowmax),dgam2_gasdt_row(nrowmax), &
                dgam2_gasda_row(nrowmax),dgam2_gasdz_row(nrowmax), &
                gam3_gas_row(nrowmax), &
                dgam3_gasdd_row(nrowmax),dgam3_gasdt_row(nrowmax), &
                dgam3_gasda_row(nrowmax),dgam3_gasdz_row(nrowmax), &
                nabad_gas_row(nrowmax), &
                dnab_gasdd_row(nrowmax),dnab_gasdt_row(nrowmax), &
                dnab_gasda_row(nrowmax),dnab_gasdz_row(nrowmax), &
                cs_gas_row(nrowmax), &
                dcs_gasdd_row(nrowmax),dcs_gasdt_row(nrowmax), &
                dcs_gasda_row(nrowmax),dcs_gasdz_row(nrowmax)

! derivative based quantities for the totals
      double precision &
                cp_row(nrowmax), &
                dcpdd_row(nrowmax),dcpdt_row(nrowmax), &
                dcpda_row(nrowmax),dcpdz_row(nrowmax), &
                cv_row(nrowmax), &
                dcvdd_row(nrowmax),dcvdt_row(nrowmax), &
                dcvda_row(nrowmax),dcvdz_row(nrowmax)

      double precision &
                gam1_row(nrowmax), &
                dgam1dd_row(nrowmax),dgam1dt_row(nrowmax), &
                dgam1da_row(nrowmax),dgam1dz_row(nrowmax), &
                gam2_row(nrowmax), &
                dgam2dd_row(nrowmax),dgam2dt_row(nrowmax), &
                dgam2da_row(nrowmax),dgam2dz_row(nrowmax), &
                gam3_row(nrowmax), &
                dgam3dd_row(nrowmax),dgam3dt_row(nrowmax), &
                dgam3da_row(nrowmax),dgam3dz_row(nrowmax), &
                nabad_row(nrowmax), &
                dnabdd_row(nrowmax),dnabdt_row(nrowmax), &
                dnabda_row(nrowmax),dnabdz_row(nrowmax), &
                cs_row(nrowmax), &
                dcsdd_row(nrowmax),dcsdt_row(nrowmax), &
                dcsda_row(nrowmax),dcsdz_row(nrowmax)

! a few work arrays
      double precision eoswrk01(nrowmax),eoswrk02(nrowmax), &
                       eoswrk03(nrowmax),eoswrk04(nrowmax)

! for debugging
      double precision &
               crp_row(nrowmax), &
               dcrpt_row(nrowmax),dcrpd_row(nrowmax), &
               dcrpa_row(nrowmax),dcrpz_row(nrowmax), &
               dcrpdd_row(nrowmax),dcrpdt_row(nrowmax), &
               dcrpda_row(nrowmax),dcrpdz_row(nrowmax), &
               dcrptt_row(nrowmax),dcrpta_row(nrowmax), &
               dcrptz_row(nrowmax),dcrpaa_row(nrowmax), &
               dcrpaz_row(nrowmax),dcrpzz_row(nrowmax)
  end module eos_vector
