      subroutine eos_pretty_eos_out(whose)
      use eos_vector

! writes a pretty output for the eos tester


! declare the pass
      character*(*) whose


! local variables
      integer          i,j
      double precision ye,xcess,avo,kerg,xka
      parameter        (avo     = 6.0221417930d23, &
                        kerg    = 1.380650424d-16, &
                        xka = kerg*avo)


! popular formats
01    format(1x,t2,a,t11,a,t27,a,t43,a,t59,a,t75,a,t91,a,t107,a)
02    format(1x,t2,a,1p7e16.8)
03    format(1x,t2,a7,1pe12.4,t22,a7,1pe12.4, &
               t42,a7,1pe12.4,t62,a7,1pe12.4)
04    format(1x,t2,a,t11,'total',t24,'ion',t34,'e- + e+', &
             t58,'radiation',t70,'coulomb')
05    format(1x,t2,a,1p3e12.4,t56,1p2e12.4)
06    format(1x,t2,a,a,1pe12.4, &
                t30,a,a,1pe12.4, &
                t58,a,a,1pe12.4)



! loop over the pipeline
      do j=jlo_eos,jhi_eos


! the input
      write(6,03) 'temp  =',temp_row(j),'den   =',den_row(j), &
                  'abar  =',abar_row(j),'zbar  =',zbar_row(j)

      ye = zbar_row(1)/abar_row(1)
      xcess = 1.0d0 - 2.0d0*ye
      write(6,03) 'ye    =',ye,'xcess =',xcess
      write(6,*) ' '


! and the output

       write(6,01)  whose,'value','d/dd','d/dt','d/da','d/dz'

       write(6,02) 'p tot=',ptot_row(j), &
                    dpd_row(j),dpt_row(j),dpa_row(j),dpz_row(j)
       write(6,02) 'p gas=',pgas_row(j), &
                 dpgasd_row(j),dpgast_row(j),dpgasa_row(j),dpgasz_row(j)
       write(6,02) 'p rad=',prad_row(j), &
                 dpradd_row(j),dpradt_row(j),dprada_row(j),dpradz_row(j)
       write(6,02) 'p ion=',pion_row(j), &
                dpiond_row(j),dpiont_row(j),dpiona_row(j),dpionz_row(j)
       write(6,02) 'p  e-=',pele_row(j), &
                dpepd_row(j),dpept_row(j),dpepa_row(j),dpepz_row(j)
       write(6,02) 'p  e+=',ppos_row(j)
       write(6,02) 'p cou=',pcou_row(j), &
                dpcoud_row(j),dpcout_row(j),dpcoua_row(j),dpcouz_row(j)


       write(6,*)  ' '
       write(6,02) 'e tot=',etot_row(j), &
                    ded_row(j),det_row(j),dea_row(j),dez_row(j)
       write(6,02) 'e gas=',egas_row(j), &
                 degasd_row(j),degast_row(j),degasa_row(j),degasz_row(j)
       write(6,02) 'e rad=',erad_row(j), &
                deradd_row(j),deradt_row(j),derada_row(j),deradz_row(j)
       write(6,02) 'e ion=',eion_row(j), &
                deiond_row(j),deiont_row(j),deiona_row(j),deionz_row(j)
       write(6,02) 'e  e-=',eele_row(j), &
                deepd_row(j),deept_row(j),deepa_row(j),deepz_row(j)
       write(6,02) 'e  e+=',epos_row(j)
       write(6,02) 'e cou=',ecou_row(j), &
                decoud_row(j),decout_row(j),decoua_row(j),decouz_row(j)

       write(6,*)  ' '
       write(6,02) 's tot=',stot_row(j), &
                    dsd_row(j),dst_row(j),dsa_row(j),dsz_row(j)
       write(6,02) 's/xka=',stot_row(j)/xka, &
             dsd_row(j)/xka,dst_row(j)/xka,dsa_row(j)/xka,dsz_row(j)/xka
       write(6,02) 's gas=',sgas_row(j), &
                 dsgasd_row(j),dsgast_row(j),dsgasa_row(j),dsgasz_row(j)
       write(6,02) 's rad=',srad_row(j), &
                dsradd_row(j),dsradt_row(j),dsrada_row(j),dsradz_row(j)
       write(6,02) 's ion=',sion_row(j), &
                dsiond_row(j),dsiont_row(j),dsiona_row(j),dsionz_row(j)
       write(6,02) 's  e-=',sele_row(j), &
                dsepd_row(j),dsept_row(j),dsepa_row(j),dsepz_row(j)
       write(6,02) 's  e+=',spos_row(j)
       write(6,02) 's cou=',scou_row(j), &
                dscoud_row(j),dscout_row(j),dscoua_row(j),dscouz_row(j)


! specific heats, and ratio of electostatic to thermal energy
! the 3 gammas and the sound speed for both the gas and the total
       write(6,*)  ' '
       write(6,02) 'cv  =',cv_row(j)/(kerg*avo)*abar_row(1), &
                    dcvdd_row(j),dcvdt_row(j), &
                    dcvda_row(j),dcvdz_row(j)
       write(6,02) 'cp  =',cp_row(j), &
                    dcpdd_row(j),dcpdt_row(j), &
                    dcpda_row(j),dcpdz_row(j)
       write(6,02) 'gam1=',gam1_row(j), &
                    dgam1dd_row(j),dgam1dt_row(j), &
                    dgam1da_row(j),dgam1dz_row(j)
       write(6,02) 'gam2=',gam2_row(j), &
                    dgam2dd_row(j),dgam2dt_row(j), &
                    dgam2da_row(j),dgam2dz_row(j)
       write(6,02) 'gam3=',gam3_row(j), &
                    dgam3dd_row(j),dgam3dt_row(j), &
                    dgam3da_row(j),dgam3dz_row(j)
       write(6,02) 'cs  =',cs_row(j), &
                    dcsdd_row(j),dcsdt_row(j), &
                    dcsda_row(j),dcsdz_row(j)

       write(6,*)  ' '
       write(6,02) 'cvgas=',cv_gas_row(j)/(kerg*avo)*abar_row(1), &
                    dcv_gasdd_row(j),dcv_gasdt_row(j), &
                    dcv_gasda_row(j),dcv_gasdz_row(j)
       write(6,02) 'cpgas=',cp_gas_row(j), &
                    dcp_gasdd_row(j),dcp_gasdt_row(j), &
                    dcp_gasda_row(j),dcp_gasdz_row(j)
       write(6,02) 'g1gas=',gam1_gas_row(j), &
                    dgam1_gasdd_row(j),dgam1_gasdt_row(j), &
                    dgam1_gasda_row(j),dgam1_gasdz_row(j)
       write(6,02) 'g2gas=',gam2_gas_row(j), &
                    dgam2_gasdd_row(j),dgam2_gasdt_row(j), &
                    dgam2_gasda_row(j),dgam2_gasdz_row(j)
       write(6,02) 'g3gas=',gam3_gas_row(j), &
                    dgam3_gasdd_row(j),dgam3_gasdt_row(j), &
                    dgam3_gasda_row(j),dgam3_gasdz_row(j)
       write(6,02) 'csgas=',cs_gas_row(j), &
                    dcs_gasdd_row(j),dcs_gasdt_row(j), &
                    dcs_gasda_row(j),dcs_gasdz_row(j)


! the thermodynamic consistency relations, these should all be
! at the floating point limit of zero
       write(6,*) ' '
       write(6,03) 'maxw1 =',dse_row(j),'maxw2 =',dpe_row(j), &
                   'maxw3 =',dsp_row(j)

! number density of ions and its derivatives
       write(6,03) 'xni   =',xni_row(j),  'xnim  =',xnim_row(j)
       write(6,03) 'dxnidd=',dxned_row(j),'dxnidt=',dxnet_row(j), &
                   'dxnida=',dxnea_row(j),'dxnidz=',dxnez_row(j)

! ion chemical potential and its derivatives
       write(6,03) 'etaion=',etaion_row(j)
       write(6,03) 'detaid=',detaid_row(j),'detait=',detait_row(j), &
                   'detaia=',detaia_row(j),'detaiz=',detaiz_row(j)


! number density of electrons+positrons and its derivatives
       write(6,03) 'xnele =',xne_row(j),'xnpos =',xnp_row(j), &
                   'xnem  =',xnem_row(j)
       write(6,03) 'dxnedd=',dxned_row(j),'dxnedt=',dxnet_row(j), &
                   'dxneda=',dxnea_row(j),'dxnedz=',dxnez_row(j)


! electron chemical potential, positron chemical potential and its derivatives
       write(6,03) 'etaele=',etaele_row(j),'etapos=',etapos_row(j)
       write(6,03) 'detadd=',detad_row(j),'detadt=',detat_row(j), &
                   'detada=',detaa_row(j),'detadz=',detaz_row(j)

       write(6,03) 'zeff  =',zeff_row(j), &
                   'ionzd =',zeff_row(j)/zbar_row(j), &
                   'plasg =',plasg_row(j)

! end of pipeline loop
      enddo

      return
      end subroutine eos_pretty_eos_out
