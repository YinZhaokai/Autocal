module ParseToDll
    interface
        subroutine InitWaterShed(vars)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitWaterShed
            real, dimension(7) ::vars
        end subroutine
        
        subroutine IniSolution_fun(npid)
            !DEC$ ATTRIBUTES DLLIMPORT :: IniSolution_fun
            integer ::npid
        end subroutine
        
        subroutine InitModelConfig(ModelconfigVars,npid)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitModelConfig
            integer ::npid
            real,dimension(npid,12*npid) :: ModelconfigVars
        end subroutine
        
        subroutine InitModelConfigvars(nidx,IRunoffGenTypeStr,len_modelconfig,npid)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitModelConfigvars
            integer len_modelconfig,npid,nidx
            character(len_modelconfig)::IRunoffGenTypeStr
        end subroutine
        
        subroutine GetSolution(SolutionVars,SolutionName,len_solution,DateTemp1,DateTemp2,npid,obsorsim,DTALL,TIMESALL)
            !DEC$ ATTRIBUTES DLLIMPORT :: GetSolution
            integer len_solution,npid,obsorsim
            character(len_solution)::SolutionName
            real,dimension(6) ::SolutionVars
            real,dimension(6) ::DateTemp1
            real,dimension(6) ::DateTemp2
            integer,dimension(3) ::DTALl
            integer,dimension(3) ::TIMESALL
        end subroutine
        
        subroutine initParasolin_Ex(maxn,kstop,pcento,ngs,iseed,nspl,istat,iprob,igoc,nintval)   
            !DEC$ ATTRIBUTES DLLIMPORT :: initParasolin_Ex
            real    ::maxn,kstop,pcento,ngs,iseed,nspl,istat,iprob,igoc,nintval
        end subroutine
        
        subroutine initGAPara_Ex(maxg,P,Tmax,std,crossoverId,mutationId,rp,blend_a,binary_gama,beta,ObjId)
            !DEC$ ATTRIBUTES DLLIMPORT :: initGAPara_Ex
            integer        ::maxg,P,Tmax,crossoverId,mutationId,ObjId
            real std,rp,blend_a,binary_gama,beta
        end subroutine
        
        subroutine initDDSPara_Ex(Dmaxn,Dr, DinitP, DTmax,Dstd, Dmaxfun, DObjID)
            !DEC$ ATTRIBUTES DLLIMPORT :: initDDSPara_Ex
            integer ::Dmaxn, DinitP, DTmax, Dmaxfun, DObjID
            real :: Dr, Dstd
        end subroutine
        
        subroutine initST_ObjRatioPara_Ex(ObjId,HighNashR,LowNashR,FloodPeakR,FloodVolR)
            !DEC$ ATTRIBUTES DLLIMPORT :: initST_ObjRatioPara_Ex
            integer        ::ObjId
            real HighNashR,LowNashR,FloodPeakR,FloodVolR
        end subroutine

        subroutine InitReachs(NReachCKVar,ReachsVars) 
            !DEC$ ATTRIBUTES DLLIMPORT :: InitReachs
            integer                                ::NReachCKVar
            real,dimension(20*NReachCKVar)         ::ReachsVars
        endsubroutine
        
        subroutine InitReachersChars(Index,NReachCKVar,ReachsChar,len) 
            !DEC$ ATTRIBUTES DLLIMPORT :: InitReachersChars
            integer                                ::NReachCKVar,len,Index
            character(30)  ::ReachsChar
        endsubroutine
        
        subroutine InitUnits(NUnitCKVar,UnitVars,Nlanduse)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitUnits
            integer                        ::NUnitCKVar,Nlanduse
            real,dimension(46*NUnitCKVar) ::UnitVars
        endsubroutine
        
        subroutine InitHydroInfos(NHydroInfoCKVar,HydroInfosVars) 
            !DEC$ ATTRIBUTES DLLIMPORT :: InitHydroInfos
            integer ::NHydroInfoCKVar,len
            real,dimension(5*NHydroInfoCKVar) ::HydroInfosVars
        end subroutine
        
        subroutine InitHydroInfosChars(nIdx,NHydroInfoCKVar,HydroInfosVars1,len1,HydroInfosVars2,len2) 
            !DEC$ ATTRIBUTES DLLIMPORT :: InitHydroInfosChars
            integer ::nIdx,NHydroInfoCKVar,len1,len2
            character(20) ::HydroInfosVars1
            character(50) ::HydroInfosVars2
        end subroutine
        
        subroutine InitResInfos(NResCKVar,ResInfosVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitResInfos
            integer ::NResCKVar,len
            real,dimension(6*NResCKVar) ::ResInfosVars
        end subroutine
        
        subroutine InitResInfosChars(nidx,NResCKVar,stcd,lenstcd,resname,lenres)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitResInfosChars
            integer ::nidx,NResCKVar,lenstcd,lenres
            character(10)   ::stcd
            character(30)   ::resname
        end subroutine
        
        subroutine InitResRanges(NResRangeCKVar,ResRangesVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitResRanges
            integer ::NResRangeCKVar
            real,dimension(8*NResRangeCKVar) ::ResRangesVars
        end subroutine
        
        subroutine InitFloodStoreInfos(NFLoodStores,StoreInfosVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitFloodStoreInfos
            integer ::NFLoodStores
            real,dimension(7*NFLoodStores) ::StoreInfosVars
        end subroutine
        
        subroutine InitDiveInfos(NDives,DiveInfosVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitDiveInfos
            integer ::NDives
            real,dimension(5*NDives) ::DiveInfosVars
        end subroutine
        
        subroutine InitResRangesChars(nidx,NResRangeCKVar,stnm,len1,PartSubbasinString,len2,UpStreamResRangeString,len3)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitResRangesChars
            character(len1)  ::stnm
            character(len2)  ::PartSubbasinString
            character(len3)  ::UpStreamResRangeString
            integer          ::nidx,NResRangeCKVar,len1,len2,len3
        end subroutine
        
        subroutine InitParamRanges(NParamRangeCKVar,ParamRangesVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitParamRanges
            integer ::NParamRangeCKVar
            real,dimension(6*NParamRangeCKVar) ::ParamRangesVars
        end subroutine
        
        subroutine InitParamRangesChars(nidx,NParamRangeCKVar,ParamRangeName,len1_paramranges,UpStreamParamRangeStr&
        &,len2_paramranges,PartSubbasinStr,len3_paramranges)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitParamRangesChars
            integer ::nidx,NParamRangeCKVar
            integer len1_paramranges,len2_paramranges,len3_paramranges
            character(len1_paramranges) ::ParamRangeName
            character(len2_paramranges) ::UpStreamParamRangeStr
            character(len3_paramranges)::PartSubbasinStr
        end subroutine
        
        subroutine InitSoilInfo(NResCKVar,SoilInfoVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitSoilInfo
            integer ::NResCKVar
            real,dimension(5*NResCKVar) ::SoilInfoVars
        end subroutine
        
        subroutine initFloodPeakTime(nidx,TimeStart,TimeEnd,floodtotalcount,floodpidvar,npid,rank)
            !DEC$ ATTRIBUTES DLLIMPORT :: initFloodPeakTime
            integer npid,floodtotalcount,nidx,rank  
            character*30::timeStart,timeEnd
            integer floodpidvar(npid*2)
        end subroutine
        
        subroutine InitWeatherInfos(npid,nWeatherCount0,nWeatherCount1,weatherWeight0,weatherWeight1,sums_NSubbasin,&
        &nWeatherCount0max,nWeatherCount1max,nyear)
            !DEC$ ATTRIBUTES DLLIMPORT :: InitWeatherInfos
            integer::nWeatherCount0(npid*2),nWeatherCount1(npid*2),npid,sums_NSubbasin,nWeatherCount0max,nWeatherCount1max,nyear
            real,dimension((nWeatherCount0max+1)*sums_NSubbasin) ::weatherWeight0 !子流域数的和,nWeatherCount0第二列最大数
            real,dimension(nWeatherCount1max*sums_NSubbasin) ::weatherWeight1
        end subroutine
        
        subroutine iniYearTavg(nCount,m_y_WeatherVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: iniYearTavg
            real,dimension(nCount) ::m_y_WeatherVars
            integer ::nCount
        end subroutine
        
        subroutine ReadEasyDHMParam(NReadEasyDHMParamCKVar,ReadEasyDHMParamVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadEasyDHMParam
            integer::NReadEasyDHMParamCKVar
            real,dimension(35*NReadEasyDHMParamCKVar) ::ReadEasyDHMParamVars
        end subroutine
        
        subroutine ReadEasyDHMParamChars(nidx,ncount,strSolzcoeStr,len1,strConducMStr,len2)
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadEasyDHMParamChars
            character(len1)  ::strSolzcoeStr
            character(len2)  ::strConducMStr
            integer          ::nidx,ncount,len1,len2
        endsubroutine
        
        subroutine ReadWetSpaParam(nCount,ReadWetSpaParamVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadWetSpaParam
            integer::nCount
            real,dimension(30*nCount) ::ReadWetSpaParamVars
        end subroutine
        
        subroutine ReadXAJParam(nCount,ReadXAJParamVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadXAJParam
            integer::nCount
            real,dimension(14*nCount) ::ReadXAJParamVars
        end subroutine
        
        subroutine ReadHymodParam(NParamRangeCheckVar,ReadHymodParamVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadHymodParam
            integer ::NParamRangeCheckVar
            real,dimension(6*NParamRangeCheckVar) ::ReadHymodParamVars
        end subroutine
        
        subroutine ReadReachParam(NParamRangeCheckVar,ReadReachParamVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadReachParam
            integer ::NParamRangeCheckVar
            real,dimension(4*NParamRangeCheckVar) ::ReadReachParamVars 
            !write(5,*)"ReadReachParam successful"
        end subroutine
        
        subroutine ReadInitDHMStates(nCount,ReadInitDHMStatesVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadInitDHMStates
            integer::nCount  
            real,dimension(11*nCount) ::ReadInitDHMStatesVars
        end subroutine
        
        subroutine ReadInitWSPStates(nCount,ReadInitWSPStatesVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadInitWSPStates
            integer::nCount
            real,dimension(10*nCount) ::ReadInitWSPStatesVars
        end subroutine
        
        subroutine ReadInitXAJStates(nCount,ReadInitXAJStatesVars)
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadInitXAJStates
            integer::nCount3       
            real,dimension(10*nCount) ::ReadInitXAJStatesVars
        end subroutine
        
        subroutine ReadInitHymodStates(nCount,ReadInitHymodStatesVars) 
            !DEC$ ATTRIBUTES DLLIMPORT :: ReadInitHymodStates
            integer::nCount
            real,dimension(9*nCount) ::ReadInitHymodStatesVars  
        end subroutine
        
        subroutine GetHydroData(hydrocount,hydro,npid,resnpid,res,fQ,fq1,weatherdatacount,raincount,&
        &nWeatherCount1max,nWeatherCount0max,fhmdt,fWsws,fIslr,fTavg,fTmaxt,fTmin,fPPtn,nyear,updrp)
            !DEC$ ATTRIBUTES DLLIMPORT :: GetHydroData
            integer resnpid,npid,nWeatherCount1max,nWeatherCount0max,raincount,nyear
            integer hydro(npid),hydrocount,weatherdatacount,res(resnpid)
            real fq(npid*hydrocount),fq1(resnpid*hydrocount),updrp(npid*hydrocount)
            real fhmdt(nWeatherCount1max*npid*weatherdatacount),fWsws(nWeatherCount1max*npid*weatherdatacount),&
            &fIslr(nWeatherCount1max*npid*weatherdatacount),fTavg(nWeatherCount1max*npid*weatherdatacount),&
            &fTmaxt(nWeatherCount1max*npid*weatherdatacount),fTmin(nWeatherCount1max*npid*weatherdatacount)&
            &,fPPtn(nWeatherCount0max*npid*Raincount)
        end subroutine
        
        subroutine GetHydroDataDateTime(nidx,hydrocount,strDateTime,floodvar,floodpidvars,npid)
            !DEC$ ATTRIBUTES DLLIMPORT :: GetHydroDataDateTime
            integer         ::hydrocount,npid,nidx
            character*30  ::strDateTime

            integer floodpidvars(npid*2)
            integer floodvar(npid)
        end subroutine
        
        subroutine initObjmet_Ex(i,j,k,l,m)
            !DEC$ ATTRIBUTES DLLIMPORT :: initObjmet_Ex
            real ::i,j,k,l,m
        end subroutine
        
        subroutine initSinChangePara_Ex(nCount_SinChangePara,nSinP,Nparameter,npid)
            !DEC$ ATTRIBUTES DLLIMPORT :: initSinChangePara_Ex
            integer Nparameter,npid
            integer nCount_SinChangePara
            real,dimension(13*nCount_SinChangePara*npid)   ::nSinP
        end subroutine
        
        subroutine initSinChangeParaChars_Ex(idx,pname,npid,nCount_SinChangePara,lenX)
            !DEC$ ATTRIBUTES DLLIMPORT :: initSinChangeParaChars_Ex
            integer        ::npid,nCount_SinChangePara,lenX
!            character(255),dimension(npid*nCount_SinChangePara) ::pname
            character(lenX)::pname
        end subroutine 
        
        subroutine initResponsmet_Ex(m, i,j,k,l )
            !DEC$ ATTRIBUTES DLLIMPORT :: initResponsmet_Ex
            real     ::m, i,j,k,l 
        end subroutine
        
        subroutine initSensin_Ex(nintval,dt,iseed)
            !DEC$ ATTRIBUTES DLLIMPORT :: initSensin_Ex
            real ::nintval,dt,iseed
        end subroutine
        
        subroutine OptFloodInfo_Ex(optfloodid,Nlines,Npoints,IForcast,Qlimit,QLow,QHigh,count)
            !DEC$ ATTRIBUTES DLLIMPORT :: OptFloodInfo_Ex
            integer count
            integer ::optfloodid(count),Nlines(count),Npoints(count),IForcast(count)
            real Qlimit(count),QLow(count),QHigh(count)
        end subroutine
        
        subroutine OptFloodInfochar_Ex(Qchar,idx,len)
            !DEC$ ATTRIBUTES DLLIMPORT :: OptFloodInfochar_Ex
            integer idx,len
            character(len):: Qchar
        end subroutine
                
        subroutine ResOptSolution_Ex(resvars1,NResCK1)
            !DEC$ ATTRIBUTES DLLIMPORT :: ResOptSolution_Ex
            integer NResCK1
            real resvars1(NResCK1*14)
        end subroutine
        
        subroutine OptFloodchar_Ex(floodparvars,nfloodpar,nline,npoint,ntotalcount)
            !DEC$ ATTRIBUTES DLLIMPORT :: OptFloodchar_Ex
            integer ntotalcount,nfloodpar
            integer nline(nfloodpar*2),npoint(nfloodpar*2)
            real:: floodparvars(ntotalcount*6)
        end subroutine
        
        subroutine GetUpStreamParamRangeData(uprsvVars,rsv,upriverVars,river,uprivernpid,upresnpid,npid,hydrocount)
            !DEC$ ATTRIBUTES DLLIMPORT :: GetUpStreamParamRangeData
            integer ::hydrocount,uprivernpid,upresnpid
            integer ::rsv(upresnpid),river(uprivernpid)
            real,dimension(hydrocount*upresnpid)::uprsvVars
            real,dimension(hydrocount*uprivernpid)::upriverVars

        end subroutine
        
        subroutine EasyDHM_Dll(SolutionID,npid)
            !DEC$ ATTRIBUTES DLLIMPORT :: EasyDHM_Dll
            integer SolutionID,npid
        end subroutine EasyDHM_Dll
        
        Function GegPrDataPR_Ex(PR_TM,PR_sid,PR_IRunoffGenType,PR_pid,PR_Pmm,PR_Rsimms&
            &,PR_Rmeams,PR_Rsimmm,PR_Rmeamm,PR_PETmm,Qms,QFinal,Uprainmm )result(N)
            !DEC$ ATTRIBUTES DLLIMPORT :: EasyDHM_Dll
            character(19)   ::TM_Temp
            integer         ::PR_sid,PR_IRunoffGenType,PR_pid,N,PR_TM
            real            ::PR_Pmm,PR_Rsimms,PR_Rmeams,PR_Rsimmm,PR_Rmeamm,PR_PETmm,Qms,QFinal,Uprainmm
        end Function

        subroutine Destroy_OutData()
            !DEC$ ATTRIBUTES DLLIMPORT :: Destroy_OutData
            
        end subroutine
        
    end interface
    
    
end module