module Fprj_Update_Mod
    use FloodProjectInfo
    
    !由于jsonfortranlib所传出的变量有特别的类型
    !因此需要使用原生数据类型对fprj类型进行重构
    implicit none
    type :: FloodProjectInfoClass_OriginType
        
        real,dimension(:),allocatable :: SolutionVars                       !(无需改动)float[6]，分别对应ST_Solution表第0、4、2、3、5、13列(无需改动)
        character(len=:),allocatable :: SolutionName                   !(无需改动)对应ST_Solution表第1列(无需改动)
        real,dimension(:),allocatable :: DateTemp1                          !(须替换)float[6]，对应ST_Solution表InitTime列年、月、日、时、分、秒。为预热期开始时间(对应InitTime)，需要改动
        real,dimension(:),allocatable :: DateTemp2                          !(须替换)float[6]，对应ST_Solution表iyeare列年、月、日、时、分、秒。为预报结束时间(对应EndTime)，需要改动
        integer,dimension(:),allocatable :: steps                           !(无需改动？)int[3,]，对应ST_Solution表DT、YJLen、TSLen列(均为时间步长，不可单独改动)
        integer,dimension(:),allocatable :: stepcounts                      !(须替换)int[3]，分别为InitTime到ModeTime之间小时数除以steps[0]，无用，ModeTime到EndTime之间小时数除以steps[2]
        real,dimension(:),allocatable :: vars                               !(无需改动)float[7]，对应ST_WaterShed表(无需改动)
        
        integer :: NReachCKVar                                              !(无需改动)	以下三个为对应每个子流域的变量 与ST_ReachInfo表相关
        real,dimension(:,:),allocatable :: ReachsVars                       !
        character(len=255),dimension(:),allocatable :: ReachsChar        !
        
        integer :: NUnitCKVar                                               !(无需改动)  以下两个为对应每个土地利用的变量 与ST_UnitInfo表相关
        real,dimension(:,:),allocatable :: UnitVars                         !(无需改动)
        
        integer :: NHydroInfoCKVar                                          !(无需改动)  以下三个变量描述水文站站名、站码及位置
        real,dimension(:,:),allocatable :: HydroInfosVars                   !(无需改动)
        character(len=255),dimension(:,:),allocatable :: HydroInfosChars !(无需改动)
        
        integer :: NResCKVar                                                !(无需改动) 以下三个变量描述水库站名、站码及位置
        real,dimension(:,:),allocatable :: ResInfosVars                     !(无需改动)
        character(len=255),dimension(:,:),allocatable :: ResInfosChars   !(无需改动)
        
        integer :: NFLoodStores                                             !(无需改动) 以下四个变量用不到
        real,dimension(:,:),allocatable :: StoreInfosVars                   !(无需改动)
        integer :: NDives                                                   !(无需改动)
        real,dimension(:,:),allocatable :: DiveInfosVars                    !(无需改动)
        
        integer :: NResRangeCKVar                                           !(无需改动) 以下三个变量与ST_ResRange表关联
        real,dimension(:,:),allocatable :: ResRangesVars                    !(无需改动)
        character(len=255),dimension(:,:),allocatable :: ResRangesChars  !(无需改动)
        
        integer :: NParamRangeCKVar                                         !(无需改动)
        real,dimension(:,:),allocatable :: ParamRangesVars                  !(无需改动)
        
        integer :: NSoilVar                                                 !(无需改动)
        real,dimension(:,:),allocatable :: SoilInfoVars                     !(无需改动)
        
        integer :: iniYearTavgCount                                         !(无需改动)	下一个变量计数	 以下两个变量与年均气温、ST_SubWgn表关联
        real,dimension(:),allocatable :: m_y_WeatherVars                    !(无需改动)	每个子流域年均气温
        
        integer :: NReadEasyDHMParamCKVar                                   !(无需改动)  以下三个变量与每个参数分区的easyDHM模型参数及表ST_ParaDefEasyDHM有关
        real,dimension(:,:),allocatable :: ReadEasyDHMParamVars             !
        character(len=255),dimension(:,:),allocatable :: ReadEasyDHMParamChars1  !
        integer :: nWetSpaParamCount                                        !(无需改动) 以下两个变量与每个参数分区的WetSpa模型参数及表ST_ParaDefWetSpa有关
        real,dimension(:,:),allocatable :: ReadWetSpaParamVars              !(无需改动)
        integer :: XAJParamCount                                            !(无需改动) 以下两个变量与每个参数分区的新安江模型参数及表FROM ST_ParaDefXAJ有关
        real,dimension(:,:),allocatable :: ReadXAJParamVars                 !(无需改动)
        integer :: NHymodRangeCheckVar                                      !(无需改动) 以下两个变量与每个参数分区的Hymod模型参数及表ST_ParaDefHymod有关
        real,dimension(:,:),allocatable :: ReadHymodParamVars               !(无需改动)
        integer :: NParamRangeCheckVar                                      !(无需改动) 以下两个变量与产流参数及表ST_ReachParam有关
        real,dimension(:,:),allocatable :: ReadReachParamVars               !(无需改动)
        
        integer :: nInitDHMStatesCount                                      !当前参数分区所包含子流域个数
        real,dimension(:,:),allocatable :: ReadInitDHMStatesVars            !当前参数分区EasyDHM状态变量
        integer :: nInitWSPStatesCount                                      !当前参数分区所包含子流域个数
        real,dimension(:,:),allocatable :: ReadInitWSPStatesVars            !当前参数分区WSP状态变量
        integer :: nXAJStatesCount                                          !当前参数分区所包含子流域个数
        real,dimension(:,:),allocatable :: ReadInitXAJStatesVars            !当前参数分区XAJ状态变量
        integer :: nHymodStatesCount                                        !当前参数分区所包含子流域个数
        real,dimension(:,:),allocatable :: ReadInitHymodStatesVars          !当前参数分区Hymod状态变量
        
        real :: i                                                           !(无需改动) 以下五个变量无意义
        real :: j                                                           !
        real :: k                                                           !
        real :: l                                                           !
        real :: m                                                           !
        integer,dimension(:,:),allocatable :: Bcode                         !(无需改动) 无意义
        
        real :: maxn                                                        !(无需改动) 以下18个变量无意义
        real :: kstop
        real :: pcento
        real :: ngs
        real :: iseed
        real :: nspl
        real :: istat
        real :: iprob
        real :: igoc
        real :: nintval
        real :: rm
        real :: ri
        real :: rj
        real :: rk
        real :: rl
        real :: nintvals
        real :: dt
        real :: iseeds
        
        !modelconfig
        real,dimension(:,:),allocatable :: ModelconfigVars                  !当前参数分区  ST_ModelConfig表中的变量
        character(len=255),dimension(:),allocatable :: IRunoffGenTypeStr !当前参数分区  模型计算方法
        integer :: npid                                                     !参数分区列表数
        integer :: floodtotalcount                                          !ST_FloodPeakTime 中参与计算的总共洪水场数
        integer,dimension(:,:),allocatable :: floodpidvars                  !?
        
        !ST_SingleChangePara
        integer :: snCount                                                  !当前参数分区 下列3个变量与ST_SingleChangePara有关
        real,dimension(:,:),allocatable :: nSinP
        character(len=255),dimension(:),allocatable :: nSinPChart
        integer :: Nparameter                                               !当前参数分区用于模拟的参数个数(新安江 17  easyDHM 29)，可选29或49，其中，29个参数与土地利用分类1类相对应，49个参数与土地利用分6类相对应
        integer :: len_modelconfig
        integer :: len_solution
        integer :: OptYear                                                  !(无需改动)水库优化调度参数 与 ST_ResOptSolution表相关
        integer,dimension(:),allocatable :: len2_paramranges, len3_paramranges, len1_paramranges    !(无需改动)len1_paramranges是ParamRangeName 的长度 ,len2_paramranges是UpStreamParamRangeStr的长度,len3_paramranges::PartSubbasinStr,PartSubbasinStr1的长度(无需改动)
        character(len=255),dimension(:),allocatable :: ParamRangeName, UpStreamParamRangeStr, PartSubbasinStr    !(无需改动)与上一行共6个变量均与ParamRange 相关，
        
        character(len=255),dimension(:),allocatable :: TimeStart, TimeEnd    !见下
        integer,dimension(:),allocatable :: ranks                           !(无需改动) 与挑选洪水参数率定及表ST_FloodPeakTime有关。TimeStart, TimeEnd为floodpeaktime中该rank对应的每场洪水的起止时间。
        
        integer,dimension(:,:),allocatable :: floodpidvar                   !当前参数分区		不理解 与表ST_FloodPeakTime有关
        integer,dimension(:,:),allocatable :: nWeatherCount0                !(无需改动) 		最大雨量站点个数
        integer,dimension(:,:),allocatable :: nWeatherCount1                !(无需改动) 		最大气象站点个数
        real,dimension(:,:),allocatable :: weatherWeight0                   !(无需改动) 		雨量站权重表
        real,dimension(:,:),allocatable :: weatherWeight1                   !(无需改动)		气象站权重表
        integer :: sums_NSubbasin,nWeatherCount0max,nWeatherCount1max       !(无需改动)     	当前参数分区子流域，当前参数分区雨量站个数，当前参数分区气象站个数
        integer,dimension(:),allocatable :: hydropids, respids              !当前站点  参数分区列表、水库列表？？不理解含义
        integer :: nCount,resnpid,nweatherCount                             !(须替换)当前参数分区对应（出口）水文站水文数据总行数，?，当前参数分区对应气象站气象数据总行数
        
        real,dimension(:),allocatable :: fQ                                 !(须替换)当前参数分区出口对应水文站流量过程(时间间隔3小时，从InitTime到EndTime)
        real,dimension(:),allocatable :: fq1                                !(须替换)当前参数分区出口对应水库入库流量
        real,dimension(:,:),allocatable :: fhmdt                            ![行数，气象站个数]单个参数分区中每个气象站的逐日平均湿度  	如有新值可以替换(从InitTime到EndTime)
        real,dimension(:,:),allocatable :: fWsws                            !单个参数分区中每个气象站的逐日平均风速  	如有新值可以替换(从InitTime到EndTime)
        real,dimension(:,:),allocatable :: fIslr                            !单个参数分区中每个气象站的逐日日照时长	如有新值可以替换(从InitTime到EndTime)
        real,dimension(:,:),allocatable :: fTavg                            !单个参数分区中每个气象站的逐日平均气温
        real,dimension(:,:),allocatable :: fTmaxt                           !单个参数分区中每个气象站的逐日最高气温
        real,dimension(:,:),allocatable :: fTmin                            !单个参数分区中每个气象站的逐日最低气温
        real,dimension(:,:),allocatable :: fPPtn                            !(须替换)当前参数分区对应雨量站降雨过程
        character(len=255),dimension(:),allocatable :: tm                !(须替换)当前参数分区从InitTime到EndTime序列，格式yyyy-MM-dd HH:00
        
        integer :: upCount                                                  !(须替换)当前参数分区上游流量（即输入流量）数据总行数
        integer :: nuprsv, nupriver                                         !当前参数分区上游水库个数，水文站个数
        character(len=255),dimension(:),allocatable :: uprsvs, uprivers  !当前参数分区上游水库站码集合，水文站站码集合
        real,dimension(:,:),allocatable :: upriverVars, uprsvVars           !(须替换)当前参数分区上游水文站入流过程，水库入流过程(问题，这里的水库取的是入库流量[更新，是从ST_PR表里读的Qfinal字段][更新，可能跟变量 obsorsim 有关])
        
        real,dimension(:,:),allocatable :: resvars1                         !(无需改动)水库优化调度参数 与 ST_ResOptSolution表相关
        integer :: NResCK1                                                  !(无需改动)水库优化调度参数 与 ST_ResOptSolution表相关
        integer,dimension(:),allocatable :: optfloodid, Nlines, Npoints, IForcast   !(无需改动)  下列4行与表ST_FloodParInfo有关
        character(len=255),dimension(:),allocatable :: QChar
        real,dimension(:),allocatable :: Qlimit, QLow, QHigh
        integer :: Qcount
        
        integer :: Dmaxn, DinitP, DTmax, Dmaxfun, DObjID, Gmaxg, GP, GTmax, GcrossoverId, GmutationId, GObjId, OObjId   !(无需改动) 优化算法相关参数 与ST_DDSIN，ST_GAIN表有关
        real :: Dr, Dstd, Gstd, Grp, Gblend_a, Gbinary_gama, Gbeta, OHighNashR, OLowNashR, OFloodPeakR, OFloodVolR      !(无需改动) 优化算法相关参数 与ST_DDSIN，ST_GAIN表有关
        
        integer :: yearcount                                                !(应该无需改动)  在计算期跨年时为2，其他时候为1，用于跨年计算雨量站的权重(假定雨量站在不同年份权重不同)
        integer,dimension(:),allocatable :: Mrank
        real,dimension(:,:),allocatable :: floodparvars                     !(无需改动)   下列5行与表ST_FloodPar有关
        integer :: nfloodpar
        integer,dimension(:,:),allocatable :: nline
        integer,dimension(:,:),allocatable :: npoint
        integer :: ntotalcount
        
        integer :: Nlanduse                                                 !当前参数分区 	土地利用类型
        integer :: ylanduse
        integer :: TimeStepOpt                                              !(无需改动)		敏感性/参数优化/不确定性分析时段：1=日；2=月
        
        character(len=:),allocatable :: Iweather                                   !权重标识：1表示雨量站，0表示气象站
        character(len=:),allocatable :: SID                                        !方案ID
        
        real :: InitTime                                                    !初始时间
        real :: StartTime                                                   !开始时间
        real :: ModeTime                                                    !预报开始时间（预热期结束时间）
        real :: WBTime                                                      !无用，与ModeTime相同
        real :: EndTime                                                     !结束时间
        
        integer,dimension(:),allocatable :: CalculatedPids                  !已计算参数分区列表 (没有被调用)
        logical :: isConnutier                                              !没有被调用
        integer :: MainPid                                                  !主参数分区 (没有被调用)
        
        integer :: step                                                     !(无需改动)时间间隔
        integer :: ptype
        integer,dimension(:),allocatable :: pids                            !当前参数分区列表        
        character(len=:),allocatable :: uptype                                     !获取上游实测数据或计算数据
        real,dimension(:),allocatable :: Updrp                              !全为0，不明白
        integer :: Nin, Nup                                                 !没有被调用  下列5个变量与DWX模型有关
        real,dimension(:),allocatable :: NKin, NKout, inNK                  !没有被调用
        
        character(len=255),dimension(:),allocatable :: intendPids_KEYS_char
        integer,dimension(:),allocatable :: intendPids_KEYS                 !需要计算的参数分区列表(原本是Dictionary类型，传数据时只传keys因此读的时候只读key)
    
    end type
contains
    
    !
    subroutine fprj_update(fprj_old,fprj_new)
        implicit none
        
        type(FloodProjectInfoClass),intent(in) :: fprj_old
        type(FloodProjectInfoClass_OriginType),intent(out) :: fprj_new
        
        call update_1D_real(fprj_old%SolutionVars,fprj_new%SolutionVars)
        fprj_new%SolutionName = fprj_old%SolutionName
        call update_1D_real(fprj_old%DateTemp1,fprj_new%DateTemp1)
        call update_1D_real(fprj_old%DateTemp2,fprj_new%DateTemp2)        
        call update_1D_int(fprj_old%steps,fprj_new%steps)
        call update_1D_int(fprj_old%stepcounts,fprj_new%stepcounts)
        call update_1D_real(fprj_old%vars,fprj_new%vars)

        
        fprj_new%NReachCKVar = fprj_old%NReachCKVar        
        ! write(*,*) 'fprj_old%NReachCKVar=',fprj_old%NReachCKVar
        ! write(*,*) 'fprj_new%NReachCKVar=',fprj_new%NReachCKVar
        call update_2D_real(fprj_old%ReachsVars,fprj_new%ReachsVars)
        call update_1D_string(fprj_old%ReachsChar,fprj_new%ReachsChar)
        
        fprj_new%NUnitCKVar = fprj_old%NUnitCKVar        
        call update_2D_real(fprj_old%UnitVars,fprj_new%UnitVars)
        
        fprj_new%NHydroInfoCKVar = fprj_old%NHydroInfoCKVar
        call update_2D_real(fprj_old%HydroInfosVars,fprj_new%HydroInfosVars)
        call update_2D_string(fprj_old%HydroInfosChars,fprj_new%HydroInfosChars)
        
        fprj_new%NResCKVar = fprj_old%NResCKVar
        call update_2D_real(fprj_old%ResInfosVars,fprj_new%ResInfosVars)
        call update_2D_string(fprj_old%ResInfosChars,fprj_new%ResInfosChars)
        
        fprj_new%NFLoodStores = fprj_old%NFLoodStores
        call update_2D_real(fprj_old%StoreInfosVars,fprj_new%StoreInfosVars)
        fprj_new%NDives = fprj_old%NDives
        call update_2D_real(fprj_old%DiveInfosVars,fprj_new%DiveInfosVars)
        
        fprj_new%NResRangeCKVar = fprj_old%NResRangeCKVar
        call update_2D_real(fprj_old%ResRangesVars,fprj_new%ResRangesVars)
        call update_2D_string(fprj_old%ResRangesChars,fprj_new%ResRangesChars)
        
        fprj_new%NParamRangeCKVar = fprj_old%NParamRangeCKVar
        call update_2D_real(fprj_old%ParamRangesVars,fprj_new%ParamRangesVars)
        
        fprj_new%NSoilVar = fprj_old%NSoilVar
        call update_2D_real(fprj_old%SoilInfoVars,fprj_new%SoilInfoVars)
        
        fprj_new%iniYearTavgCount = fprj_old%iniYearTavgCount
        call update_1D_real(fprj_old%m_y_WeatherVars,fprj_new%m_y_WeatherVars)
        
        fprj_new%NReadEasyDHMParamCKVar = fprj_old%NReadEasyDHMParamCKVar
        call update_2D_real(fprj_old%ReadEasyDHMParamVars,fprj_new%ReadEasyDHMParamVars)
        call update_2D_string(fprj_old%ReadEasyDHMParamChars1,fprj_new%ReadEasyDHMParamChars1)
        fprj_new%nWetSpaParamCount = fprj_old%nWetSpaParamCount
        call update_2D_real(fprj_old%ReadWetSpaParamVars,fprj_new%ReadWetSpaParamVars)
        fprj_new%XAJParamCount = fprj_old%XAJParamCount
        call update_2D_real(fprj_old%ReadXAJParamVars,fprj_new%ReadXAJParamVars)
        fprj_new%NHymodRangeCheckVar = fprj_old%NHymodRangeCheckVar
        call update_2D_real(fprj_old%ReadHymodParamVars,fprj_new%ReadHymodParamVars)
        fprj_new%NParamRangeCheckVar = fprj_old%NParamRangeCheckVar
        call update_2D_real(fprj_old%ReadReachParamVars,fprj_new%ReadReachParamVars)
        
        fprj_new%nInitDHMStatesCount = fprj_old%nInitDHMStatesCount
        call update_2D_real(fprj_old%ReadInitDHMStatesVars,fprj_new%ReadInitDHMStatesVars)
        fprj_new%nInitWSPStatesCount = fprj_old%nInitWSPStatesCount
        call update_2D_real(fprj_old%ReadInitWSPStatesVars,fprj_new%ReadInitWSPStatesVars)
        fprj_new%nXAJStatesCount = fprj_old%nXAJStatesCount
        call update_2D_real(fprj_old%ReadInitXAJStatesVars,fprj_new%ReadInitXAJStatesVars)
        fprj_new%nHymodStatesCount = fprj_old%nHymodStatesCount
        call update_2D_real(fprj_old%ReadInitHymodStatesVars,fprj_new%ReadInitHymodStatesVars)
        
        fprj_new%i = fprj_old%i
        fprj_new%j = fprj_old%j
        fprj_new%k = fprj_old%k
        fprj_new%l = fprj_old%l
        fprj_new%m = fprj_old%m
        call update_2D_int(fprj_old%Bcode,fprj_new%Bcode)
        
        fprj_new%maxn = fprj_old%maxn
        fprj_new%kstop = fprj_old%kstop
        fprj_new%pcento = fprj_old%pcento
        fprj_new%ngs = fprj_old%ngs
        fprj_new%iseed = fprj_old%iseed
        fprj_new%nspl = fprj_old%nspl
        fprj_new%istat = fprj_old%istat
        fprj_new%iprob = fprj_old%iprob
        fprj_new%igoc = fprj_old%igoc
        fprj_new%nintval = fprj_old%nintval
        fprj_new%rm = fprj_old%rm
        fprj_new%ri = fprj_old%ri
        fprj_new%rj = fprj_old%rj
        fprj_new%rk = fprj_old%rk
        fprj_new%rl = fprj_old%rl
        fprj_new%nintvals = fprj_old%nintvals
        fprj_new%dt = fprj_old%dt
        fprj_new%iseeds = fprj_old%iseeds
        
        call update_2D_real(fprj_old%ModelconfigVars,fprj_new%ModelconfigVars)
        call update_1D_string(fprj_old%IRunoffGenTypeStr,fprj_new%IRunoffGenTypeStr)
        fprj_new%npid = fprj_old%npid
        fprj_new%floodtotalcount = fprj_old%floodtotalcount
        call update_2D_int(fprj_old%floodpidvars,fprj_new%floodpidvars)
        
        fprj_new%snCount = fprj_old%snCount
        call update_2D_real(fprj_old%nSinP,fprj_new%nSinP)
        call update_1D_string(fprj_old%nSinPChart,fprj_new%nSinPChart)
        fprj_new%Nparameter = fprj_old%Nparameter
        fprj_new%len_modelconfig = fprj_old%len_modelconfig
        fprj_new%len_solution = fprj_old%len_solution
        fprj_new%OptYear = fprj_old%OptYear
        call update_1D_int(fprj_old%len2_paramranges,fprj_new%len2_paramranges)
        call update_1D_int(fprj_old%len3_paramranges,fprj_new%len3_paramranges)
        call update_1D_int(fprj_old%len1_paramranges,fprj_new%len1_paramranges)
        call update_1D_string(fprj_old%ParamRangeName,fprj_new%ParamRangeName)
        call update_1D_string(fprj_old%UpStreamParamRangeStr,fprj_new%UpStreamParamRangeStr)
        call update_1D_string(fprj_old%PartSubbasinStr,fprj_new%PartSubbasinStr)
        
        call update_1D_string(fprj_old%TimeStart,fprj_new%TimeStart)
        call update_1D_string(fprj_old%TimeEnd,fprj_new%TimeEnd)
        call update_1D_int(fprj_old%ranks,fprj_new%ranks)
        
        call update_2D_int(fprj_old%floodpidvar,fprj_new%floodpidvar)
        call update_2D_int(fprj_old%nWeatherCount0,fprj_new%nWeatherCount0)
        call update_2D_int(fprj_old%nWeatherCount1,fprj_new%nWeatherCount1)
        call update_2D_real(fprj_old%weatherWeight0,fprj_new%weatherWeight0)
        call update_2D_real(fprj_old%weatherWeight1,fprj_new%weatherWeight1)
        fprj_new%sums_NSubbasin = fprj_old%sums_NSubbasin
        fprj_new%nWeatherCount0max = fprj_old%nWeatherCount0max
        fprj_new%nWeatherCount1max = fprj_old%nWeatherCount1max
        call update_1D_int(fprj_old%hydropids,fprj_new%hydropids)
        call update_1D_int(fprj_old%respids,fprj_new%respids)
        fprj_new%nCount = fprj_old%nCount
        fprj_new%resnpid = fprj_old%resnpid
        fprj_new%nweatherCount = fprj_old%nweatherCount
        
        call update_1D_real(fprj_old%fQ,fprj_new%fQ)
        call update_1D_real(fprj_old%fq1,fprj_new%fq1)
        call update_2D_real(fprj_old%fhmdt,fprj_new%fhmdt)
        call update_2D_real(fprj_old%fWsws,fprj_new%fWsws)
        call update_2D_real(fprj_old%fIslr,fprj_new%fIslr)
        call update_2D_real(fprj_old%fTavg,fprj_new%fTavg)
        call update_2D_real(fprj_old%fTmaxt,fprj_new%fTmaxt)
        call update_2D_real(fprj_old%fTmin,fprj_new%fTmin)
        call update_2D_real(fprj_old%fPPtn,fprj_new%fPPtn)
        call update_1D_string(fprj_old%tm,fprj_new%tm)
        
        fprj_new%upCount = fprj_old%upCount
        fprj_new%nuprsv = fprj_old%nuprsv
        fprj_new%nupriver = fprj_old%nupriver
        
        call update_1D_string(fprj_old%uprsvs,fprj_new%uprsvs)
        call update_1D_string(fprj_old%uprivers,fprj_new%uprivers)
        call update_2D_real(fprj_old%upriverVars,fprj_new%upriverVars)
        call update_2D_real(fprj_old%uprsvVars,fprj_new%uprsvVars)
        
        call update_2D_real(fprj_old%resvars1,fprj_new%resvars1)
        fprj_new%NResCK1 = fprj_old%NResCK1
        call update_1D_int(fprj_old%optfloodid,fprj_new%optfloodid)
        call update_1D_int(fprj_old%Nlines,fprj_new%Nlines)
        call update_1D_int(fprj_old%Npoints,fprj_new%Npoints)
        call update_1D_int(fprj_old%IForcast,fprj_new%IForcast)
        call update_1D_string(fprj_old%QChar,fprj_new%QChar)
        call update_1D_real(fprj_old%Qlimit,fprj_new%Qlimit)
        call update_1D_real(fprj_old%QLow,fprj_new%QLow)
        call update_1D_real(fprj_old%QHigh,fprj_new%QHigh)
        fprj_new%Qcount = fprj_old%Qcount
        
        fprj_new%Dmaxn = fprj_old%Dmaxn
        fprj_new%DinitP = fprj_old%DinitP
        fprj_new%DTmax = fprj_old%DTmax
        fprj_new%Dmaxfun = fprj_old%Dmaxfun
        fprj_new%DObjID = fprj_old%DObjID
        fprj_new%Gmaxg = fprj_old%Gmaxg
        fprj_new%GP = fprj_old%GP
        fprj_new%GTmax = fprj_old%GTmax
        fprj_new%GcrossoverId = fprj_old%GcrossoverId
        fprj_new%GmutationId = fprj_old%GmutationId
        fprj_new%GObjId = fprj_old%GObjId
        fprj_new%OObjId = fprj_old%OObjId
        fprj_new%Dr = fprj_old%Dr
        fprj_new%Dstd = fprj_old%Dstd
        fprj_new%Gstd = fprj_old%Gstd
        fprj_new%Grp = fprj_old%Grp
        fprj_new%Gblend_a = fprj_old%Gblend_a
        fprj_new%Gbinary_gama = fprj_old%Gbinary_gama
        fprj_new%Gbeta = fprj_old%Gbeta
        fprj_new%OHighNashR = fprj_old%OHighNashR
        fprj_new%OLowNashR = fprj_old%OLowNashR
        fprj_new%OFloodPeakR = fprj_old%OFloodPeakR
        fprj_new%OFloodVolR = fprj_old%OFloodVolR        
        
        fprj_new%yearcount = fprj_old%yearcount
        call update_1D_int(fprj_old%Mrank,fprj_new%Mrank)
        call update_2D_real(fprj_old%floodparvars,fprj_new%floodparvars)        
        fprj_new%nfloodpar = fprj_old%nfloodpar
        call update_2D_int(fprj_old%nline,fprj_new%nline)
        call update_2D_int(fprj_old%npoint,fprj_new%npoint)
        
        fprj_new%Nlanduse = fprj_old%Nlanduse
        fprj_new%ylanduse = fprj_old%ylanduse
        fprj_new%TimeStepOpt = fprj_old%TimeStepOpt
        
        fprj_new%Iweather = fprj_old%Iweather
        fprj_new%SID = fprj_old%SID
        
        fprj_new%InitTime = fprj_old%InitTime
        fprj_new%StartTime = fprj_old%StartTime
        fprj_new%ModeTime = fprj_old%ModeTime
        fprj_new%WBTime = fprj_old%WBTime
        fprj_new%EndTime = fprj_old%EndTime
        
        call update_1D_int(fprj_old%CalculatedPids,fprj_new%CalculatedPids)
        fprj_new%isConnutier = fprj_old%isConnutier
        fprj_new%MainPid = fprj_old%MainPid
        
        fprj_new%step = fprj_old%step
        fprj_new%ptype = fprj_old%ptype
        call update_1D_int(fprj_old%pids,fprj_new%pids)
        fprj_new%uptype = fprj_old%uptype
        call update_1D_real(fprj_old%Updrp,fprj_new%Updrp)
        fprj_new%Nin = fprj_old%Nin
        fprj_new%Nup = fprj_old%Nup
        call update_1D_real(fprj_old%NKin,fprj_new%NKin)
        call update_1D_real(fprj_old%NKout,fprj_new%NKout)
        call update_1D_real(fprj_old%inNK,fprj_new%inNK)
        
        call update_1D_int(fprj_old%intendPids_KEYS,fprj_new%intendPids_KEYS)
        
        !call update_2D_real(fprj_old%ReadWetSpaParamVars,fprj_new%ReadWetSpaParamVars)
        !
        !call update_2D_int(fprj_old%nWeatherCount0,fprj_new%nWeatherCount0)
        ! write(*,*) fprj_new%SolutionVars(1)
        
        
    
    end subroutine
    
    subroutine update_1D_real(array_old,array_new)
        implicit none
        !integer :: dim
        !integer :: x
        real(wp),dimension(:),allocatable,intent(in) :: array_old
        real,dimension(:),allocatable,intent(out) :: array_new
        !dim = size(shape(array_old))
        !x = size(array_old)
        allocate(array_new(size(array_old)))
        array_new = array_old
        return
    end subroutine
    
    subroutine update_2D_real(array_old,array_new)
        implicit none
        !integer :: dim
        !integer :: x
        real(wp),dimension(:,:),allocatable,intent(in) :: array_old
        real,dimension(:,:),allocatable,intent(out) :: array_new
        !dim = size(shape(array_old))
        !x = size(array_old)
        allocate(array_new(size(array_old,dim=1),size(array_old,dim=2)))
        array_new = array_old
        return
    end subroutine
    
    subroutine update_1D_int(array_old,array_new)
        implicit none
        !integer :: dim
        !integer :: x
        integer(IK),dimension(:),allocatable,intent(in) :: array_old
        integer,dimension(:),allocatable,intent(out) :: array_new
        !dim = size(shape(array_old))
        !x = size(array_old)
        allocate(array_new(size(array_old)))
        array_new = array_old
        return
    end subroutine
    
    subroutine update_2D_int(array_old,array_new)
        implicit none
        !integer :: dim
        !integer :: x
        integer(IK),dimension(:,:),allocatable,intent(in) :: array_old
        integer,dimension(:,:),allocatable,intent(out) :: array_new
        !dim = size(shape(array_old))
        !x = size(array_old)
        allocate(array_new(size(array_old,dim=1),size(array_old,dim=2)))
        array_new = array_old
        return
    end subroutine
    
    subroutine update_1D_string(array_old,array_new)
        implicit none
        !integer :: dim
        !integer :: x
        character(kind=CK,len=255),dimension(:),allocatable,intent(in) :: array_old
        character(len=255),dimension(:),allocatable,intent(out) :: array_new
        !dim = size(shape(array_old))
        !x = size(array_old)
        allocate(array_new(size(array_old)))
        array_new = array_old
        return
    end subroutine
    
    subroutine update_2D_string(array_old,array_new)
        implicit none
        !integer :: dim
        !integer :: x
        character(kind=CK,len=255),dimension(:,:),allocatable,intent(in) :: array_old
        character(len=255),dimension(:,:),allocatable,intent(out) :: array_new
        !dim = size(shape(array_old))
        !x = size(array_old)
        allocate(array_new(size(array_old,dim=1),size(array_old,dim=2)))
        array_new = array_old
        return
    end subroutine

end module 