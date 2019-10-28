module CallDll
    use FloodProjectInfo
    use ParseToDll
    use Fprj_Update_Mod
    
    
    
    
    contains
    subroutine Call_EasyDHM_Dll(fprj)
        implicit none
        
        type(FloodProjectInfoClass_OriginType) :: fprj
        integer :: test
        integer :: sid_int,npid,j,l
        integer :: obsorsim = 0,rainCount = 0
        integer :: len2_paramrangesIn
        
        integer,dimension(:),allocatable :: floodpidvar,upriver,upres
        real,dimension(:,:),allocatable ::ReachsVarsTran,UnitVarsTran,HydroInfosVarsTran
        real,dimension(:,:),allocatable ::upriversVars,upresVars
        character(30)::ReachsCharTrim
        character(30)::UpStreamParamRangeStrIn
        
        write(*,*) 'parsing parameters to easyDHM ...'
        npid = size(fprj%intendPids_KEYS)
        ! write(*,*) 'npid = ',npid
        call IniSolution_fun(1)
        
        call InitModelConfig(fprj%ModelconfigVars, npid)
        
        do j = 1,npid
            call InitModelConfigvars(j, fprj%IRunoffGenTypeStr(j), len(fprj%IRunoffGenTypeStr(j)), npid)
        end do
        
        ! write(*,*) 'fprj%uptype = ',fprj%uptype
        if (fprj%uptype .EQ. "预报")obsorsim = 0    !0是读取上游的pr表数据，1读取上游的实测            
        call GetSolution(fprj%SolutionVars, fprj%SolutionName, len(fprj%SolutionName), fprj%DateTemp1,&
        & fprj%DateTemp2, npid, obsorsim, fprj%steps, fprj%stepcounts)
        
        if (fprj%TimeStepOpt .EQ. 0)then
             call initDDSPara_Ex(fprj%Dmaxn,fprj%Dr,fprj%DinitP,fprj%DTmax,fprj%Dstd,fprj%Dmaxfun,fprj%DObjID)
        else if (fprj%TimeStepOpt .EQ. 1)then
             call initParasolin_Ex(fprj%maxn, fprj%kstop, fprj%pcento, fprj%ngs, fprj%iseed, fprj%nspl, fprj%istat,&
             & fprj%iprob, fprj%igoc, fprj%nintval)
        else if (fprj%TimeStepOpt .EQ. 2)then
             call initGAPara_Ex(fprj%Gmaxg, fprj%GP, fprj%GTmax, fprj%Gstd, fprj%GcrossoverId, fprj%GmutationId,&
             & fprj%Grp, fprj%Gblend_a, fprj%Gbinary_gama, fprj%Gbeta, fprj%GObjId)
        end if
        
        call initST_ObjRatioPara_Ex(fprj%OObjId, fprj%OHighNashR, fprj%OLowNashR, fprj%OFloodPeakR, fprj%OFloodVolR);
        call InitWaterShed(fprj%vars);
        
        ! write(*,*) 'fprj%NReachCKVar =',fprj%NReachCKVar
        allocate (ReachsVarsTran(20,fprj%NReachCKVar))
        ReachsVarsTran = transpose(fprj%ReachsVars)
        call InitReachs(fprj%NReachCKVar,ReachsVarsTran );
        do j = 1,fprj%NReachCKVar
             ReachsCharTrim = trim(fprj%ReachsChar(j))
             call InitReachersChars(j, 1, fprj%ReachsChar(j),len(ReachsCharTrim))
        end do
        
        allocate (UnitVarsTran(46,fprj%NUnitCKVar))
        UnitVarsTran = transpose(fprj%UnitVars)
        call InitUnits(fprj%NUnitCKVar, UnitVarsTran, fprj%Nlanduse);
        
        allocate (HydroInfosVarsTran(5,fprj%NHydroInfoCKVar))
        HydroInfosVarsTran = transpose(fprj%HydroInfosVars)
        call InitHydroInfos(fprj%NHydroInfoCKVar, HydroInfosVarsTran);
        
        do j = 1,fprj%NHydroInfoCKVar
            call InitHydroInfosChars(j, fprj%NHydroInfoCKVar, fprj%HydroInfosChars(j, 1)&
            &, len(trim(fprj%HydroInfosChars(j, 1))),&
            & fprj%HydroInfosChars(j, 2), len(trim(fprj%HydroInfosChars(j, 2))))   
        end do
        
        call InitResInfos(fprj%NResCKVar, transpose(fprj%ResInfosVars));
        
        do j = 1,fprj%NResCKVar
            call InitResInfosChars(j, fprj%NResCKVar, fprj%ResInfosChars(j, 1)&
            &, len(trim(fprj%ResInfosChars(j, 1))), fprj%ResInfosChars(j, 2),&
            & len(trim(fprj%ResInfosChars(j, 2))));
        end do
        
        call InitResRanges(fprj%NResRangeCKVar, transpose(fprj%ResRangesVars));
        
        if (fprj%NFLoodStores .GT. 0)then
            call InitFloodStoreInfos(fprj%NFLoodStores, transpose(fprj%StoreInfosVars))
        end if
        
        if (fprj%NDives .GT. 0)then
            call InitDiveInfos(fprj%NDives, transpose(fprj%DiveInfosVars));
        end if
         
        do j = 1,fprj%NResRangeCKVar
            call InitResRangesChars(j, fprj%NResRangeCKVar, fprj%ResRangesChars(j, 1)&
            &, len(trim(fprj%ResRangesChars(j, 1))), &
            &fprj%ResRangesChars(j, 2), len(trim(fprj%ResRangesChars(j, 2)))&
            &, fprj%ResRangesChars(j, 3), len(trim(fprj%ResRangesChars(j, 3))))
        end do
        
        call InitParamRanges(fprj%NParamRangeCKVar, transpose(fprj%ParamRangesVars));

        do j = 1,fprj%NParamRangeCKVar
            call  InitParamRangesChars(j, fprj%NParamRangeCKVar, fprj%ParamRangeName(j)&
            &, fprj%len1_paramranges(j),&
            & fprj%UpStreamParamRangeStr(j),fprj%len2_paramranges(j), fprj%PartSubbasinStr(j)&
            &, fprj%len3_paramranges(j))
        end do

        call InitSoilInfo(fprj%NSoilVar, transpose(fprj%SoilInfoVars))
        !call InitSoilInfo(fprj%NSoilVar, transpose(fprj%SoilInfoVars), numensemble) !Kalman
        
        do j = 1,fprj%floodtotalcount
            call initFloodPeakTime(j, fprj%TimeStart(j), fprj%TimeEnd(j), fprj%floodtotalcount&
            &, transpose(fprj%floodpidvars), npid, fprj%ranks(j))
        end do
        
        call InitWeatherInfos(npid, transpose(fprj%nWeatherCount0), transpose(fprj%nWeatherCount1)&
        &, transpose(fprj%weatherWeight0),&
        & transpose(fprj%weatherWeight1), fprj%sums_NSubbasin, fprj%nWeatherCount0max&
        &, fprj%nWeatherCount1max,fprj%yearcount);
        call iniYearTavg(fprj%iniYearTavgCount, fprj%m_y_WeatherVars);
        call ReadEasyDHMParam(fprj%NReadEasyDHMParamCKVar, transpose(fprj%ReadEasyDHMParamVars));
        
        do j = 1,fprj%NReadEasyDHMParamCKVar
            call ReadEasyDHMParamChars(j, fprj%NReadEasyDHMParamCKVar, fprj%ReadEasyDHMParamChars1(j, 1), &
            &len(trim(fprj%ReadEasyDHMParamChars1(j, 1))), fprj%ReadEasyDHMParamChars1(j, 2)&
            &, len(trim(fprj%ReadEasyDHMParamChars1(j, 2))));
        end do
        
         call ReadWetSpaParam(fprj%nWetSpaParamCount, transpose(fprj%ReadWetSpaParamVars))
         call ReadXAJParam(fprj%XAJParamCount, transpose(fprj%ReadXAJParamVars))
         call ReadHymodParam(fprj%NHymodRangeCheckVar, transpose(fprj%ReadHymodParamVars))
         call ReadReachParam(fprj%NParamRangeCheckVar, transpose(fprj%ReadReachParamVars))
         call ReadInitDHMStates(fprj%nInitDHMStatesCount, transpose(fprj%ReadInitDHMStatesVars))
         call ReadInitWSPStates(fprj%nInitWSPStatesCount, transpose(fprj%ReadInitWSPStatesVars))
         call ReadInitXAJStates(fprj%nXAJStatesCount, transpose(fprj%ReadInitXAJStatesVars))
         call ReadInitHymodStates(fprj%nHymodStatesCount, transpose(fprj%ReadInitHymodStatesVars))
         !call ReadInitHymodStates(fprj%nHymodStatesCount, transpose(fprj%ReadInitHymodStatesVars, numensemble)) !Kalman
         
         if (fprj%SolutionVars(5) .GT. 0)then
            rainCount = fprj%nCount;
         else
            rainCount = fprj%nweatherCount
         end if
         allocate (floodpidvar(size(fprj%intendPids_KEYS)))
         do j = 1,size(fprj%intendPids_KEYS)
            floodpidvar(j) = fprj%intendPids_KEYS(j)
         end do
         call GetHydroData(fprj%nCount, floodpidvar, npid, fprj%resnpid, fprj%respids, fprj%fQ, fprj%fq1,&
         & fprj%nweatherCount, rainCount, fprj%nWeatherCount1max, size(fprj%fPPtn,dim = 2)&
         &, transpose(fprj%fhmdt), transpose(fprj%fWsws), transpose(fprj%fIslr), transpose(fprj%fTavg)&
         &, transpose(fprj%fTmaxt), transpose(fprj%fTmin), transpose(fprj%fPPtn), fprj%yearcount, fprj%Updrp)
         !GetHydroData(fprj%nCount, floodpidvar, npid, fprj%resnpid, fprj%respids, transpose(fprj%fQ), transpose(prj%fq1), &
         !&fprj%nweatherCount, rainCount, fprj%nWeatherCount1max, fprj%fPPtn%GetLength(1), transpose(fprj%fhmdt), transpose(fprj%fWsws),&
         !& transpose(fprj%fIslr), transpose(fprj%fTavg), transpose(fprj%fTmaxt), transpose(fprj%fTmin), transpose(fprj%fPPtn), fprj%yearcount, transpose(fprj%Updrp), numensemble) !Kalman
         
         do j = 1,fprj%nCount
           call GetHydroDataDateTime(j, fprj%nCount, fprj%tm(j), floodpidvar, transpose(fprj%floodpidvars), npid)
         end do
         
         call initObjmet_Ex(fprj%i, fprj%j, fprj%k, fprj%l, fprj%m)
         

         call initSinChangePara_Ex(fprj%snCount, transpose(fprj%nSinP), fprj%Nparameter, npid)
         do j = 1,fprj%snCount
             call initSinChangeParaChars_Ex(j, fprj%nSinPChart(j), npid, fprj%snCount,len(trim(fprj%nSinPChart(j))))
         end do

         call initResponsmet_Ex(fprj%rm, fprj%ri, fprj%rj, fprj%rk, fprj%rl);
         call initSensin_Ex(fprj%nintvals, fprj%dt, fprj%iseeds);

         if (size(fPrj%uprivers) .GT. 0)then
             allocate(upriver(size(fPrj%uprivers)))
             do j = 1,size(fPrj%uprivers)
                 Read( fPrj%uprivers(j) , * ) upriver(j)
             end do
         elseif (size(fPrj%uprivers) .EQ. 0) then
            allocate(upriver(0))
         end if
         if (size(fPrj%uprsvs) .GT. 0)then
             allocate(upres(size(fPrj%uprsvs)) )
             do j = 1,size(fPrj%uprsvs)
                 Read( fPrj%uprsvs(j) , * ) upres(j)
             end do
         elseif (size(fPrj%uprivers) .EQ. 0) then
             allocate(upres(0))
         end if
        !  write(*,*) size(upriver)
         if (size(upriver) .GT. 0)then
             if(allocated(upriversVars)) deallocate(upriversVars)
             allocate(upriversVars(fprj%nCount,size(upriver)))
             if (size(fprj%upriverVars) .EQ. 0)then
                 upriversVars = 0
             else
                 do j = 1,fprj%nCount
                     do l = 1,size(upriver)
                         upriversVars(j,l) = fprj%upriverVars(j,l)
                     end do
                 end do
             end if
         end if
         
         if (size(fprj%uprsvVars) .GT. 0)then
             if(allocated(upresVars)) deallocate(upresVars)
             allocate(upresVars(fprj%nCount,size(upres))) 
             if (size(fprj%uprsvVars) .EQ. 0)then  
                 upresVars = 0
             else
                 do j = 1,fprj%nCount
                     do l = 1,size(upres)
                         upresVars(j,l) = fprj%uprsvVars(j,l)
                     end do
                 end do
             end if
         end if
         
         call GetUpStreamParamRangeData(transpose(upresVars), upres, transpose(upriversVars),&
         & upriver, fprj%nupriver, fprj%nuprsv, npid, fprj%nCount)
         
         if (fprj%Qcount .GT. 0)then
            call OptFloodInfo_Ex(fprj%optfloodid, fprj%Nlines, fprj%Npoints, fprj%IForcast&
            &, fprj%Qlimit, fprj%QLow, fprj%QHigh, fprj%Qcount)
            do j = 1,fprj%Qcount
                call OptFloodInfochar_Ex(fprj%QChar(j), j, len(fprj%QChar(j)))
            end do
         end if
         
         if (fprj%NResCK1 .GT. 0)then
             call ResOptSolution_Ex(transpose(fprj%resvars1),fprj%NResCK1)
         end if
         
         if (fprj%ntotalcount .GT. 0)then
             call OptFloodchar_Ex(transpose(fprj%floodparvars), fprj%nfloodpar&
             &, transpose(fprj%nline), transpose(fprj%npoint), fprj%ntotalcount)
         end if
        !  write(*,*) 'fprj%SID = ',fprj%SID
        !  read(*,*)
          Read( fprj%SID , * ) sid_int
          
          call EasyDHM_Dll(sid_int, npid)        
          !call EasyDHM_Dll(sid_int, npid, fprj%KalParInput, fprj%KalParInput.GetLength(0)); !Kalman
          return
          
    end subroutine
    
    subroutine Destroy()
        implicit none
        call Destroy_OutData()
    end subroutine

end module