module FloodProjectInfo
    
    use json_module, wp => json_RK, IK => json_IK, LK => json_LK, CK => json_CK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit
    
    implicit none
    !�������json���ݵ��࣬��ԭ�����е�FloodProjectInfo.cs�ļ��е�FloodProjectInfo����ͬ
    type :: FloodProjectInfoClass
    
        real(wp),dimension(:),allocatable :: SolutionVars                       !(����Ķ�)float[6]���ֱ��ӦST_Solution����0��4��2��3��5��13��(����Ķ�)
        character(kind=CK,len=:),allocatable :: SolutionName                   !(����Ķ�)��ӦST_Solution����1��(����Ķ�)
        real(wp),dimension(:),allocatable :: DateTemp1                          !(���滻)float[6]����ӦST_Solution��InitTime���ꡢ�¡��ա�ʱ���֡��롣ΪԤ���ڿ�ʼʱ��(��ӦInitTime)����Ҫ�Ķ�
        real(wp),dimension(:),allocatable :: DateTemp2                          !(���滻)float[6]����ӦST_Solution��iyeare���ꡢ�¡��ա�ʱ���֡��롣ΪԤ������ʱ��(��ӦEndTime)����Ҫ�Ķ�
        integer(IK),dimension(:),allocatable :: steps                           !(����Ķ���)int[3,]����ӦST_Solution��DT��YJLen��TSLen��(��Ϊʱ�䲽�������ɵ����Ķ�)
        integer(IK),dimension(:),allocatable :: stepcounts                      !(���滻)int[3]���ֱ�ΪInitTime��ModeTime֮��Сʱ������steps[0]�����ã�ModeTime��EndTime֮��Сʱ������steps[2]
        real(wp),dimension(:),allocatable :: vars                               !(����Ķ�)float[7]����ӦST_WaterShed��(����Ķ�)
        
        integer(IK) :: NReachCKVar                                              !(����Ķ�)	��������Ϊ��Ӧÿ��������ı��� ��ST_ReachInfo�����
        real(wp),dimension(:,:),allocatable :: ReachsVars                       !
        character(kind=CK,len=255),dimension(:),allocatable :: ReachsChar        !
        
        integer(IK) :: NUnitCKVar                                               !(����Ķ�)  ��������Ϊ��Ӧÿ���������õı��� ��ST_UnitInfo�����
        real(wp),dimension(:,:),allocatable :: UnitVars                         !(����Ķ�)
        
        integer(IK) :: NHydroInfoCKVar                                          !(����Ķ�)  ����������������ˮ��վվ����վ�뼰λ��
        real(wp),dimension(:,:),allocatable :: HydroInfosVars                   !(����Ķ�)
        character(kind=CK,len=255),dimension(:,:),allocatable :: HydroInfosChars !(����Ķ�)
        
        integer(IK) :: NResCKVar                                                !(����Ķ�) ����������������ˮ��վ����վ�뼰λ��
        real(wp),dimension(:,:),allocatable :: ResInfosVars                     !(����Ķ�)
        character(kind=CK,len=255),dimension(:,:),allocatable :: ResInfosChars   !(����Ķ�)
        
        integer(IK) :: NFLoodStores                                             !(����Ķ�) �����ĸ������ò���
        real(wp),dimension(:,:),allocatable :: StoreInfosVars                   !(����Ķ�)
        integer(IK) :: NDives                                                   !(����Ķ�)
        real(wp),dimension(:,:),allocatable :: DiveInfosVars                    !(����Ķ�)
        
        integer(IK) :: NResRangeCKVar                                           !(����Ķ�) ��������������ST_ResRange������
        real(wp),dimension(:,:),allocatable :: ResRangesVars                    !(����Ķ�)
        character(kind=CK,len=255),dimension(:,:),allocatable :: ResRangesChars  !(����Ķ�)
        
        integer(IK) :: NParamRangeCKVar                                         !(����Ķ�)
        real(wp),dimension(:,:),allocatable :: ParamRangesVars                  !(����Ķ�)
        
        integer(IK) :: NSoilVar                                                 !(����Ķ�)
        real(wp),dimension(:,:),allocatable :: SoilInfoVars                     !(����Ķ�)
        
        integer(IK) :: iniYearTavgCount                                         !(����Ķ�)	��һ����������	 ��������������������¡�ST_SubWgn������
        real(wp),dimension(:),allocatable :: m_y_WeatherVars                    !(����Ķ�)	ÿ���������������
        
        integer(IK) :: NReadEasyDHMParamCKVar                                   !(����Ķ�)  ��������������ÿ������������easyDHMģ�Ͳ�������ST_ParaDefEasyDHM�й�
        real(wp),dimension(:,:),allocatable :: ReadEasyDHMParamVars             !
        character(kind=CK,len=255),dimension(:,:),allocatable :: ReadEasyDHMParamChars1  !
        integer(IK) :: nWetSpaParamCount                                        !(����Ķ�) ��������������ÿ������������WetSpaģ�Ͳ�������ST_ParaDefWetSpa�й�
        real(wp),dimension(:,:),allocatable :: ReadWetSpaParamVars              !(����Ķ�)
        integer(IK) :: XAJParamCount                                            !(����Ķ�) ��������������ÿ�������������°���ģ�Ͳ�������FROM ST_ParaDefXAJ�й�
        real(wp),dimension(:,:),allocatable :: ReadXAJParamVars                 !(����Ķ�)
        integer(IK) :: NHymodRangeCheckVar                                      !(����Ķ�) ��������������ÿ������������Hymodģ�Ͳ�������ST_ParaDefHymod�й�
        real(wp),dimension(:,:),allocatable :: ReadHymodParamVars               !(����Ķ�)
        integer(IK) :: NParamRangeCheckVar                                      !(����Ķ�) �������������������������ST_ReachParam�й�
        real(wp),dimension(:,:),allocatable :: ReadReachParamVars               !(����Ķ�)
        
        integer(IK) :: nInitDHMStatesCount                                      !��ǰ�����������������������
        real(wp),dimension(:,:),allocatable :: ReadInitDHMStatesVars            !��ǰ��������EasyDHM״̬����
        integer(IK) :: nInitWSPStatesCount                                      !��ǰ�����������������������
        real(wp),dimension(:,:),allocatable :: ReadInitWSPStatesVars            !��ǰ��������WSP״̬����
        integer(IK) :: nXAJStatesCount                                          !��ǰ�����������������������
        real(wp),dimension(:,:),allocatable :: ReadInitXAJStatesVars            !��ǰ��������XAJ״̬����
        integer(IK) :: nHymodStatesCount                                        !��ǰ�����������������������
        real(wp),dimension(:,:),allocatable :: ReadInitHymodStatesVars          !��ǰ��������Hymod״̬����
        
        real(wp) :: i                                                           !(����Ķ�) �����������������
        real(wp) :: j                                                           !
        real(wp) :: k                                                           !
        real(wp) :: l                                                           !
        real(wp) :: m                                                           !
        integer(IK),dimension(:,:),allocatable :: Bcode                         !(����Ķ�) ������
        
        real(wp) :: maxn                                                        !(����Ķ�) ����18������������
        real(wp) :: kstop
        real(wp) :: pcento
        real(wp) :: ngs
        real(wp) :: iseed
        real(wp) :: nspl
        real(wp) :: istat
        real(wp) :: iprob
        real(wp) :: igoc
        real(wp) :: nintval
        real(wp) :: rm
        real(wp) :: ri
        real(wp) :: rj
        real(wp) :: rk
        real(wp) :: rl
        real(wp) :: nintvals
        real(wp) :: dt
        real(wp) :: iseeds
        
        !modelconfig
        real(wp),dimension(:,:),allocatable :: ModelconfigVars                  !��ǰ��������  ST_ModelConfig���еı���
        character(kind=CK,len=255),dimension(:),allocatable :: IRunoffGenTypeStr !��ǰ��������  ģ�ͼ��㷽��
        integer(IK) :: npid                                                     !���������б���
        integer(IK) :: floodtotalcount                                          !ST_FloodPeakTime �в��������ܹ���ˮ����
        integer(IK),dimension(:,:),allocatable :: floodpidvars                  !?
        
        !ST_SingleChangePara
        integer(IK) :: snCount                                                  !��ǰ�������� ����3��������ST_SingleChangePara�й�
        real(wp),dimension(:,:),allocatable :: nSinP
        character(kind=CK,len=255),dimension(:),allocatable :: nSinPChart
        integer(IK) :: Nparameter                                               !��ǰ������������ģ��Ĳ�������(�°��� 17  easyDHM 29)����ѡ29��49�����У�29���������������÷���1�����Ӧ��49���������������÷�6�����Ӧ
        integer(IK) :: len_modelconfig
        integer(IK) :: len_solution
        integer(IK) :: OptYear                                                  !(����Ķ�)ˮ���Ż����Ȳ��� �� ST_ResOptSolution�����
        integer(IK),dimension(:),allocatable :: len2_paramranges, len3_paramranges, len1_paramranges    !(����Ķ�)len1_paramranges��ParamRangeName �ĳ��� ,len2_paramranges��UpStreamParamRangeStr�ĳ���,len3_paramranges::PartSubbasinStr,PartSubbasinStr1�ĳ���(����Ķ�)
        character(kind=CK,len=255),dimension(:),allocatable :: ParamRangeName, UpStreamParamRangeStr, PartSubbasinStr    !(����Ķ�)����һ�й�6����������ParamRange ��أ�
        
        character(kind=CK,len=255),dimension(:),allocatable :: TimeStart, TimeEnd    !����
        integer(IK),dimension(:),allocatable :: ranks                           !(����Ķ�) ����ѡ��ˮ�����ʶ�����ST_FloodPeakTime�йء�TimeStart, TimeEndΪfloodpeaktime�и�rank��Ӧ��ÿ����ˮ����ֹʱ�䡣
        
        integer(IK),dimension(:,:),allocatable :: floodpidvar                   !��ǰ��������		������ ���ST_FloodPeakTime�й�
        integer(IK),dimension(:,:),allocatable :: nWeatherCount0                !(����Ķ�) 		�������վ�����
        integer(IK),dimension(:,:),allocatable :: nWeatherCount1                !(����Ķ�) 		�������վ�����
        real(wp),dimension(:,:),allocatable :: weatherWeight0                   !(����Ķ�) 		����վȨ�ر�
        real(wp),dimension(:,:),allocatable :: weatherWeight1                   !(����Ķ�)		����վȨ�ر�
        integer(IK) :: sums_NSubbasin,nWeatherCount0max,nWeatherCount1max       !(����Ķ�)     	��ǰ�������������򣬵�ǰ������������վ��������ǰ������������վ����
        integer(IK),dimension(:),allocatable :: hydropids, respids              !��ǰվ��  ���������б���ˮ���б����������⺬��
        integer(IK) :: nCount,resnpid,nweatherCount                             !(���滻)��ǰ����������Ӧ�����ڣ�ˮ��վˮ��������������?����ǰ����������Ӧ����վ��������������
        
        real(wp),dimension(:),allocatable :: fQ                                 !(���滻)��ǰ�����������ڶ�Ӧˮ��վ��������(ʱ����3Сʱ����InitTime��EndTime)
        real(wp),dimension(:),allocatable :: fq1                                !(���滻)��ǰ�����������ڶ�Ӧˮ���������
        real(wp),dimension(:,:),allocatable :: fhmdt                            ![����������վ����]��������������ÿ������վ������ƽ��ʪ��  	������ֵ�����滻(��InitTime��EndTime)
        real(wp),dimension(:,:),allocatable :: fWsws                            !��������������ÿ������վ������ƽ������  	������ֵ�����滻(��InitTime��EndTime)
        real(wp),dimension(:,:),allocatable :: fIslr                            !��������������ÿ������վ����������ʱ��	������ֵ�����滻(��InitTime��EndTime)
        real(wp),dimension(:,:),allocatable :: fTavg                            !��������������ÿ������վ������ƽ������
        real(wp),dimension(:,:),allocatable :: fTmaxt                           !��������������ÿ������վ�������������
        real(wp),dimension(:,:),allocatable :: fTmin                            !��������������ÿ������վ�������������
        real(wp),dimension(:,:),allocatable :: fPPtn                            !(���滻)��ǰ����������Ӧ����վ�������
        character(kind=CK,len=255),dimension(:),allocatable :: tm                !(���滻)��ǰ����������InitTime��EndTime���У���ʽyyyy-MM-dd HH:00
        
        integer(IK) :: upCount                                                  !(���滻)��ǰ����������������������������������������
        integer(IK) :: nuprsv, nupriver                                         !��ǰ������������ˮ�������ˮ��վ����
        character(kind=CK,len=255),dimension(:),allocatable :: uprsvs, uprivers  !��ǰ������������ˮ��վ�뼯�ϣ�ˮ��վվ�뼯��
        real(wp),dimension(:,:),allocatable :: upriverVars, uprsvVars           !(���滻)��ǰ������������ˮ��վ�������̣�ˮ����������(���⣬�����ˮ��ȡ�����������[���£��Ǵ�ST_PR�������Qfinal�ֶ�][���£����ܸ����� obsorsim �й�])
        
        real(wp),dimension(:,:),allocatable :: resvars1                         !(����Ķ�)ˮ���Ż����Ȳ��� �� ST_ResOptSolution�����
        integer(IK) :: NResCK1                                                  !(����Ķ�)ˮ���Ż����Ȳ��� �� ST_ResOptSolution�����
        integer(IK),dimension(:),allocatable :: optfloodid, Nlines, Npoints, IForcast   !(����Ķ�)  ����4�����ST_FloodParInfo�й�
        character(kind=CK,len=255),dimension(:),allocatable :: QChar
        real(wp),dimension(:),allocatable :: Qlimit, QLow, QHigh
        integer(IK) :: Qcount
        
        integer(IK) :: Dmaxn, DinitP, DTmax, Dmaxfun, DObjID, Gmaxg, GP, GTmax, GcrossoverId, GmutationId, GObjId, OObjId   !(����Ķ�) �Ż��㷨��ز��� ��ST_DDSIN��ST_GAIN���й�
        real(wp) :: Dr, Dstd, Gstd, Grp, Gblend_a, Gbinary_gama, Gbeta, OHighNashR, OLowNashR, OFloodPeakR, OFloodVolR      !(����Ķ�) �Ż��㷨��ز��� ��ST_DDSIN��ST_GAIN���й�
        
        integer(IK) :: yearcount                                                !(Ӧ������Ķ�)  �ڼ����ڿ���ʱΪ2������ʱ��Ϊ1�����ڿ����������վ��Ȩ��(�ٶ�����վ�ڲ�ͬ���Ȩ�ز�ͬ)
        integer(IK),dimension(:),allocatable :: Mrank
        real(wp),dimension(:,:),allocatable :: floodparvars                     !(����Ķ�)   ����5�����ST_FloodPar�й�
        integer(IK) :: nfloodpar
        integer(IK),dimension(:,:),allocatable :: nline
        integer(IK),dimension(:,:),allocatable :: npoint
        integer(IK) :: ntotalcount
        
        integer(IK) :: Nlanduse                                                 !��ǰ�������� 	������������
        integer(IK) :: ylanduse
        integer(IK) :: TimeStepOpt                                              !(����Ķ�)		������/�����Ż�/��ȷ���Է���ʱ�Σ�1=�գ�2=��
        
        character(kind=CK,len=:),allocatable :: Iweather                                   !Ȩ�ر�ʶ��1��ʾ����վ��0��ʾ����վ
        character(kind=CK,len=:),allocatable :: SID                                        !����ID
        
        real(wp) :: InitTime                                                    !��ʼʱ��
        real(wp) :: StartTime                                                   !��ʼʱ��
        real(wp) :: ModeTime                                                    !Ԥ����ʼʱ�䣨Ԥ���ڽ���ʱ�䣩
        real(wp) :: WBTime                                                      !���ã���ModeTime��ͬ
        real(wp) :: EndTime                                                     !����ʱ��
        
        integer(IK),dimension(:),allocatable :: CalculatedPids                  !�Ѽ�����������б� (û�б�����)
        logical(LK) :: isConnutier                                              !û�б�����
        integer(IK) :: MainPid                                                  !���������� (û�б�����)
        
        integer(IK) :: step                                                     !(����Ķ�)ʱ����
        integer(IK) :: ptype
        integer(IK),dimension(:),allocatable :: pids                            !��ǰ���������б�        
        character(kind=CK,len=:),allocatable :: uptype                                     !��ȡ����ʵ�����ݻ��������
        real(wp),dimension(:),allocatable :: Updrp                              !ȫΪ0��������
        integer(IK) :: Nin, Nup                                                 !û�б�����  ����5��������DWXģ���й�
        real(wp),dimension(:),allocatable :: NKin, NKout, inNK                  !û�б�����
        
        character(kind=CK,len=255),dimension(:),allocatable :: intendPids_KEYS_char
        integer(IK),dimension(:),allocatable :: intendPids_KEYS                 !��Ҫ����Ĳ��������б�(ԭ����Dictionary���ͣ�������ʱֻ��keys��˶���ʱ��ֻ��key)

        
        
    end type
    
contains
    
    !��json�ļ��ж�ȡ���ݲ�����fprj����
    !dir        �ַ����� Ϊjson�ļ�·����
    !filename   �ַ����� Ϊjson�ļ����ƣ�
    !fprj       EasyDHM�������ͣ��ں����д�����������
    !error_cnt  ���Σ�Ϊjsonfortranlib�������
    subroutine GetFprjFromJson(dir, filename0, fprj, error_cnt)
        implicit none
        
        
        
        character(len=*) :: dir     !! working directory
        character(len=*) :: filename0     !! file to read
        type(json_file) :: json     !! the JSON structure read from the file
        type(json_value),pointer :: p !! a pointer for low-level manipulations
        type(json_core) :: core       !! factory for manipulating `json_value` pointers
        integer,intent(out) :: error_cnt
        type(FloodProjectInfoClass),intent(out) :: fprj
        logical(LK) :: found
        
        character(len=30) :: tempstring

        error_cnt = 0
        call json%initialize()
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
                                                                                           
        ! parse the json file:
        write(error_unit,'(A)') ''
        write(error_unit,'(A)') 'parsing file '//dir//filename0

        call json%load_file(filename = dir//filename0)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        
        ! -------------------------
        ! print each variable:
        ! write(*,*) 'before initialize'
        call core%initialize()
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        ! write(*,*) 'before get root'
        call json%get(p) ! get root
        if (json%failed()) then
            call json%print_error_message(error_unit)
            error_cnt = error_cnt + 1
        end if
        ! write(*,*) 'before get SolutionVars'
        ! tempstring = 'SolutionVars'
        write(*,*) 'start reading...'
        ! call json%get('SolutionVars',fprj%SolutionVars)
        call core%get(p,'SolutionVars',fprj%SolutionVars)
        ! if(core%failed()) then
        !     call core%print_error_message(error_unit)
        ! end if
        ! write(*,*) 'before get SolutionName'
        call core%get(p,'SolutionName',fprj%SolutionName)
        ! if(core%failed()) then
        !     call core%print_error_message(error_unit)
        ! end if
        call core%get(p,'DateTemp1',fprj%DateTemp1)
        call core%get(p,'DateTemp2',fprj%DateTemp2)
        call core%get(p,'steps',fprj%steps)
        call core%get(p,'stepcounts',fprj%stepcounts)
        call core%get(p,'SolutionVars',fprj%SolutionVars)        
        call core%get(p,'vars',fprj%vars)
        call core%get(p,'NReachCKVar ',fprj%NReachCKVar )
        ! write(*,*) fprj%NReachCKVar        
        call Get2DimensionReal(core, p, 'ReachsVars', fprj%ReachsVars)
        call core%get(p,'ReachsChar',fprj%ReachsChar)
        
        call core%get(p,'NUnitCKVar',fprj%NUnitCKVar)
        call Get2DimensionReal(core, p, 'UnitVars', fprj%UnitVars)
        
        call core%get(p,'NHydroInfoCKVar',fprj%NHydroInfoCKVar)
        call Get2DimensionReal(core, p, 'HydroInfosVars', fprj%HydroInfosVars)
        call Get2DimensionString(core, p, 'HydroInfosChars', fprj%HydroInfosChars)
        
        call core%get(p,'NResCKVar',fprj%NResCKVar)
        call Get2DimensionReal(core, p, 'ResInfosVars', fprj%ResInfosVars)
        call Get2DimensionString(core, p, 'ResInfosChars', fprj%ResInfosChars)
        
        call core%get(p,'NFLoodStores',fprj%NFLoodStores)
        call Get2DimensionReal(core, p, 'StoreInfosVars', fprj%StoreInfosVars)
        call core%get(p,'NDives',fprj%NDives)
        call Get2DimensionReal(core, p, 'DiveInfosVars', fprj%DiveInfosVars)
        
        call core%get(p,'NResRangeCKVar',fprj%NResRangeCKVar)
        call Get2DimensionReal(core, p, 'ResRangesVars', fprj%ResRangesVars)
        call Get2DimensionString(core, p, 'ResRangesChars', fprj%ResRangesChars)
        
        call core%get(p,'NParamRangeCKVar',fprj%NParamRangeCKVar)
        call Get2DimensionReal(core, p, 'ParamRangesVars', fprj%ParamRangesVars)
        
        call core%get(p,'NSoilVar',fprj%NSoilVar)
        call Get2DimensionReal(core, p, 'SoilInfoVars', fprj%SoilInfoVars)
        
        call core%get(p,'iniYearTavgCount',fprj%iniYearTavgCount)
        call core%get(p, 'm_y_WeatherVars', fprj%m_y_WeatherVars)
        
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        call core%get(p,'NReadEasyDHMParamCKVar',fprj%NReadEasyDHMParamCKVar)
        call Get2DimensionReal(core, p, 'ReadEasyDHMParamVars', fprj%ReadEasyDHMParamVars)
        call Get2DimensionString(core, p, 'ReadEasyDHMParamChars1', fprj%ReadEasyDHMParamChars1)
        call core%get(p,'nWetSpaParamCount',fprj%nWetSpaParamCount)
        call Get2DimensionReal(core, p, 'ReadWetSpaParamVars', fprj%ReadWetSpaParamVars)
        call core%get(p,'XAJParamCount',fprj%XAJParamCount)
        call Get2DimensionReal(core, p, 'ReadXAJParamVars', fprj%ReadXAJParamVars)
        call core%get(p,'NHymodRangeCheckVar',fprj%NHymodRangeCheckVar)
        call Get2DimensionReal(core, p, 'ReadHymodParamVars', fprj%ReadHymodParamVars)
        call core%get(p,'NParamRangeCheckVar',fprj%NParamRangeCheckVar)
        call Get2DimensionReal(core, p, 'ReadReachParamVars', fprj%ReadReachParamVars)
        
        call core%get(p,'nInitDHMStatesCount',fprj%nInitDHMStatesCount)
        call Get2DimensionReal(core, p, 'ReadInitDHMStatesVars', fprj%ReadInitDHMStatesVars)
        call core%get(p,'nInitWSPStatesCount',fprj%nInitWSPStatesCount)
        call Get2DimensionReal(core, p, 'ReadInitWSPStatesVars', fprj%ReadInitWSPStatesVars)
        call core%get(p,'nXAJStatesCount',fprj%nXAJStatesCount)
        call Get2DimensionReal(core, p, 'ReadInitXAJStatesVars', fprj%ReadInitXAJStatesVars)
        call core%get(p,'nHymodStatesCount',fprj%nHymodStatesCount)
        call Get2DimensionReal(core, p, 'ReadInitHymodStatesVars', fprj%ReadInitHymodStatesVars)
        
        call core%get(p,'i',fprj%i)
        call core%get(p,'j',fprj%j)
        call core%get(p,'k',fprj%k)
        call core%get(p,'l',fprj%l)
        call core%get(p,'m',fprj%m)
        call Get2DimensionInt(core, p, 'Bcode', fprj%Bcode)
        
        call core%get(p,'maxn',fprj%maxn)
        call core%get(p,'kstop',fprj%kstop)
        call core%get(p,'pcento',fprj%pcento)
        call core%get(p,'ngs',fprj%ngs)
        call core%get(p,'iseed',fprj%iseed)
        call core%get(p,'nspl',fprj%nspl)
        call core%get(p,'istat',fprj%istat)
        call core%get(p,'iprob',fprj%iprob)
        call core%get(p,'igoc',fprj%igoc)
        call core%get(p,'nintval',fprj%nintval)
        call core%get(p,'rm',fprj%rm)
        call core%get(p,'ri',fprj%ri)
        call core%get(p,'rj',fprj%rj)
        call core%get(p,'rk',fprj%rk)
        call core%get(p,'rl',fprj%rl)
        call core%get(p,'nintvals',fprj%nintvals)
        call core%get(p,'dt',fprj%dt)
        call core%get(p,'iseeds',fprj%iseeds)
        
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        call Get2DimensionReal(core, p, 'ModelconfigVars', fprj%ModelconfigVars)
        call core%get(p,'IRunoffGenTypeStr',fprj%IRunoffGenTypeStr)
        call core%get(p,'npid',fprj%npid)
        call core%get(p,'floodtotalcount',fprj%floodtotalcount)
        call Get2DimensionInt(core, p, 'floodpidvars', fprj%floodpidvars)
        
        call core%get(p,'snCount',fprj%snCount)
        call Get2DimensionReal(core, p, 'nSinP', fprj%nSinP)
        call core%get(p,'nSinPChart',fprj%nSinPChart)
        !call Get2DimensionString(core, p, 'nSinPChart', fprj%nSinPChart)
        call core%get(p,'Nparameter',fprj%Nparameter)
        call core%get(p,'len_modelconfig',fprj%len_modelconfig)
        call core%get(p,'len_solution',fprj%len_solution)
        call core%get(p,'OptYear',fprj%OptYear)
        call core%get(p,'len2_paramranges',fprj%len2_paramranges)
        call core%get(p,'len3_paramranges',fprj%len3_paramranges)
        call core%get(p,'len1_paramranges',fprj%len1_paramranges)
        call core%get(p,'ParamRangeName',fprj%ParamRangeName)
        call core%get(p,'UpStreamParamRangeStr',fprj%UpStreamParamRangeStr)
        call core%get(p,'PartSubbasinStr',fprj%PartSubbasinStr)
        
        call core%get(p,'TimeStart',fprj%TimeStart)
        call core%get(p,'TimeEnd',fprj%TimeEnd)
        call core%get(p,'ranks',fprj%ranks)
        
        call Get2DimensionInt(core, p, 'floodpidvar', fprj%floodpidvar)
        call Get2DimensionInt(core, p, 'nWeatherCount0', fprj%nWeatherCount0)
        call Get2DimensionInt(core, p, 'nWeatherCount1', fprj%nWeatherCount1)
        call Get2DimensionReal(core, p, 'weatherWeight0', fprj%weatherWeight0)
        call Get2DimensionReal(core, p, 'weatherWeight1', fprj%weatherWeight1)
        call core%get(p,'sums_NSubbasin',fprj%sums_NSubbasin)
        call core%get(p,'nWeatherCount0max',fprj%nWeatherCount0max)
        call core%get(p,'nWeatherCount1max',fprj%nWeatherCount1max)
        call core%get(p,'hydropids',fprj%hydropids)
        call core%get(p,'respids',fprj%respids)
        call core%get(p,'nCount',fprj%nCount)
        call core%get(p,'resnpid',fprj%resnpid)
        call core%get(p,'nweatherCount',fprj%nweatherCount)
        
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        call core%get(p,'fQ',fprj%fQ)
        call core%get(p,'fq1',fprj%fq1)
        call Get2DimensionReal(core, p, 'fhmdt', fprj%fhmdt)
        call Get2DimensionReal(core, p, 'fWsws', fprj%fWsws)
        call Get2DimensionReal(core, p, 'fIslr', fprj%fIslr)
        call Get2DimensionReal(core, p, 'fTavg', fprj%fTavg)
        call Get2DimensionReal(core, p, 'fTmaxt', fprj%fTmaxt)
        call Get2DimensionReal(core, p, 'fTmin', fprj%fTmin)
        call Get2DimensionReal(core, p, 'fPPtn', fprj%fPPtn)
        call core%get(p,'tm',fprj%tm)
        
        call core%get(p,'upCount',fprj%upCount)
        call core%get(p,'nuprsv',fprj%nuprsv)
        call core%get(p,'nupriver',fprj%nupriver)
        call core%get(p,'uprsvs',fprj%uprsvs)
        call core%get(p,'uprivers',fprj%uprivers)
        call Get2DimensionReal(core, p, 'upriverVars', fprj%upriverVars)
        call Get2DimensionReal(core, p, 'uprsvVars', fprj%uprsvVars)
        
        call Get2DimensionReal(core, p, 'resvars1', fprj%resvars1)
        call core%get(p,'NResCK1',fprj%NResCK1)
        call core%get(p,'optfloodid',fprj%optfloodid)
        call core%get(p,'Nlines',fprj%Nlines)
        call core%get(p,'Npoints',fprj%Npoints)
        call core%get(p,'IForcast',fprj%IForcast)
        call core%get(p,'QChar',fprj%QChar)
        call core%get(p,'Qlimit',fprj%Qlimit)
        call core%get(p,'QLow',fprj%QLow)
        call core%get(p,'QHigh',fprj%QHigh)
        call core%get(p,'Qcount',fprj%Qcount)
        
        call core%get(p,'Dmaxn',fprj%Dmaxn)
        call core%get(p,'DinitP',fprj%DinitP)
        call core%get(p,'DTmax',fprj%DTmax)
        call core%get(p,'DObjID',fprj%DObjID)
        call core%get(p,'Gmaxg',fprj%Gmaxg)
        call core%get(p,'GP',fprj%GP)
        call core%get(p,'GTmax',fprj%GTmax)
        call core%get(p,'GcrossoverId',fprj%GcrossoverId)
        call core%get(p,'GmutationId',fprj%GmutationId)
        call core%get(p,'GObjId',fprj%GObjId)
        call core%get(p,'OObjId',fprj%OObjId)
        
        call core%get(p,'Dr',fprj%Dr)
        call core%get(p,'Dstd',fprj%Dstd)
        call core%get(p,'Gstd',fprj%Gstd)
        call core%get(p,'Grp',fprj%Grp)
        call core%get(p,'Gblend_a',fprj%Gblend_a)
        call core%get(p,'Gbinary_gama',fprj%Gbinary_gama)
        call core%get(p,'Gbeta',fprj%Gbeta)
        call core%get(p,'OHighNashR',fprj%OHighNashR)
        call core%get(p,'OLowNashR',fprj%OLowNashR)
        call core%get(p,'OFloodPeakR',fprj%OFloodPeakR)
        call core%get(p,'OFloodVolR',fprj%OFloodVolR)
        
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        call core%get(p,'yearcount',fprj%yearcount)
        call core%get(p,'Mrank',fprj%Mrank)
        call Get2DimensionReal(core, p, 'floodparvars', fprj%floodparvars)
        call core%get(p,'nfloodpar',fprj%nfloodpar)
        call Get2DimensionInt(core, p, 'nline', fprj%nline)
        call Get2DimensionInt(core, p, 'npoint', fprj%npoint)
        call core%get(p,'ntotalcount',fprj%ntotalcount)
        call core%get(p,'Nlanduse',fprj%Nlanduse)
        call core%get(p,'ylanduse',fprj%ylanduse)
        call core%get(p,'TimeStepOpt',fprj%TimeStepOpt)
        
        call core%get(p,'Iweather',fprj%Iweather)
        call core%get(p,'SID',fprj%SID)
        
        call core%get(p,'InitTime',fprj%InitTime)
        call core%get(p,'StartTime',fprj%StartTime)
        call core%get(p,'ModeTime',fprj%ModeTime)
        call core%get(p,'WBTime',fprj%WBTime)
        call core%get(p,'EndTime',fprj%EndTime)
        
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        call core%get(p,'CalculatedPids',fprj%CalculatedPids)
        call core%get(p,'isConnutier',fprj%isConnutier)
        call core%get(p,'MainPid',fprj%MainPid)
        
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        call core%get(p,'step',fprj%step)
        call core%get(p,'ptype',fprj%ptype)
        call core%get(p,'pids',fprj%pids)
        call core%get(p,'uptype',fprj%uptype)
        call core%get(p,'Updrp',fprj%Updrp)
        
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        call core%get(p,'Nin',fprj%Nin)
        call core%get(p,'Nup',fprj%Nup)
        call core%get(p,'NKin',fprj%NKin)
        if(core%failed()) then
            ! write(*,*) 'NKin is null'
            allocate(fprj%NKin(0))
            call core%clear_exceptions()
        end if
        call core%get(p,'NKout',fprj%NKout)
        if(core%failed()) then
            ! write(*,*) 'NKout is null'
            allocate(fprj%NKout(0))
            call core%clear_exceptions()
        end if
        call core%get(p,'inNK',fprj%inNK)
        if(core%failed()) then
            ! write(*,*) 'inNK is null'
            allocate(fprj%inNK(0))
            call core%clear_exceptions()
        end if
        
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        call GetKeysInt(core, p,'intendPids', fprj%intendPids_KEYS)

        !call Get2DimensionReal(core, p, 'ReadInitHymodStatesVars', fprj%ReadInitHymodStatesVars)
        !call Get2DimensionString(core, p, 'HydroInfosChars', fprj%HydroInfosChars)
        
        !call Get2DimensionInt(core, p, 'npoint', fprj%npoint)
        
        !write(*,*) size(fprj%npoint,dim=1), size(fprj%npoint,dim=2)
        

        
        write(error_unit,'(A)') ''
        
        return
    end subroutine
        
    !jsonfortranlib����ֱ�Ӷ�ȡjson�ļ��еĶ�ά����
    !�����Ҫ�Լ�д������ʵ��
    !ͨ���ֱ�ʶ���Ӧ��������������ά�ȵ�ֵ��json_core%info������n_children����ֵ���������������
    !��ѭ�����ά��������/�ж�ȡ����
    
    !core   json_core���ͣ�json������
    !p      json_value���ͣ�����json���ݵ�ָ��
    !name   �ַ����ͣ���Ҫ��ȡ������json�е�����
    !matrix ʵ�Ͷ�ά���飨real(wp),dimension(:,:)������ȡ��ά���鲢����
    subroutine Get2DimensionReal(core, p, name, matrix)
        implicit none
        
        type(json_value),pointer,intent(in) :: p !! a pointer for low-level manipulations
        type(json_core) :: core       !! factory for manipulating `json_value` pointers
        character(len=*),intent(in) :: name
        real(wp),dimension(:,:),allocatable,intent(out) :: matrix
        real(wp),dimension(:),allocatable :: temp_matrix
        logical(LK) :: found
        integer(IK) :: var_type
        integer(IK) :: n_children
        integer(IK) :: x,y,i
        character(5) :: chari
        !type(json_value),pointer :: p_child
        
        !���Ҹñ���������ά�������ά�ȵ�����������Ϊx
        call core%info(p,name,found,var_type,n_children)
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        x = n_children
        !������ά��Ϊ0����Ҫ���ص������趨Ϊ������
        if(x==0) then
            allocate(matrix(0,0))
            !matrix(1,1) = 0
            ! write(*,*) name//' is null'
            return
        !������ά�Ȳ�Ϊ0������������ڲ�ά��
        else
            call core%info(p,name//'[1]',found,var_type,n_children)
            if(core%failed()) then
                call core%print_error_message(error_unit)
            end if
            
            y = n_children
            
        end if
        !write(*,*) x,y
        allocate(matrix(x,y))
        allocate(temp_matrix(y))
        
        do i = 1,x
            write(chari,'(I5)') i
            !call core%get(p, name//'['//trim(chari)//']',matrix(i,:),found)
            call core%get(p, name//'['//adjustl(trim(chari))//']',temp_matrix,found)
            
            if(core%failed()) then
                call core%print_error_message(error_unit)
            end if
            !write(*,*) temp_matrix
            matrix(i,:) = temp_matrix
        end do
        
        return        
        
    end subroutine
    
    
     !core   json_core���ͣ�json������
    !p      json_value���ͣ�����json���ݵ�ָ��
    !name   �ַ����ͣ���Ҫ��ȡ������json�е�����
    !matrix ���Ͷ�ά���飨integer(IK),dimension(:,:)������ȡ��ά���鲢����
    subroutine Get2DimensionInt(core, p, name, matrix)
        implicit none
        
        type(json_value),pointer,intent(in) :: p !! a pointer for low-level manipulations
        type(json_core) :: core       !! factory for manipulating `json_value` pointers
        character(len=*),intent(in) :: name
        integer(IK),dimension(:,:),allocatable,intent(out) :: matrix
        integer(IK),dimension(:),allocatable :: temp_matrix
        logical(LK) :: found
        integer(IK) :: var_type
        integer(IK) :: n_children
        integer(IK) :: x,y,i
        character(5) :: chari
        !type(json_value),pointer :: p_child
        
        !���Ҹñ���������ά�������ά�ȵ�����������Ϊx
        call core%info(p,name,found,var_type,n_children)
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        x = n_children
        !������ά��Ϊ0����Ҫ���ص������趨Ϊ������
        if(x==0) then
            allocate(matrix(0,0))
            !matrix(1,1) = 0
            ! write(*,*) name//' is null'
            return
        !������ά�Ȳ�Ϊ0������������ڲ�ά��
        else
            call core%info(p,name//'[1]',found,var_type,n_children)
            if(core%failed()) then
                call core%print_error_message(error_unit)
            end if
            
            y = n_children
            
        end if
        !write(*,*) x,y
        allocate(matrix(x,y))
        allocate(temp_matrix(y))
        
        do i = 1,x
            write(chari,'(I5)') i
            !call core%get(p, name//'['//trim(chari)//']',matrix(i,:),found)
            call core%get(p, name//'['//adjustl(trim(chari))//']',temp_matrix,found)
            
            if(core%failed()) then
                call core%print_error_message(error_unit)
            end if
            !write(*,*) temp_matrix
            matrix(i,:) = temp_matrix
        end do
        
        return        
        
    end subroutine   
    
    
    
    
    
    
    !core   json_core���ͣ�json������
    !p      json_value���ͣ�����json���ݵ�ָ��
    !name   �ַ����ͣ���Ҫ��ȡ������json�е�����
    !matrix �ַ���ά���飨character(kind=CK,len=*),dimension(:,:)������ȡ��ά���鲢����
    subroutine Get2DimensionString(core, p, name, matrix)
        implicit none
        
        type(json_value),pointer,intent(in) :: p !! a pointer for low-level manipulations
        type(json_core) :: core       !! factory for manipulating `json_value` pointers
        character(len=*),intent(in) :: name
        character(kind=CK,len=255),dimension(:,:),allocatable,intent(out) :: matrix
        character(kind=CK,len=255),dimension(:),allocatable :: temp_matrix
        logical(LK) :: found
        integer(IK) :: var_type
        integer(IK) :: n_children
        integer(IK) :: x,y,i
        character(5) :: chari
        !type(json_value),pointer :: p_child
        
        !���Ҹñ���������ά�������ά�ȵ�����������Ϊx
        call core%info(p,name,found,var_type,n_children)
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        x = n_children
        !������ά��Ϊ0����Ҫ���ص������趨Ϊ������
        if(x==0) then
            allocate(matrix(0,0))
            !matrix(1,1) = '0'
            ! write(*,*) name//' is null'
            return
        !������ά�Ȳ�Ϊ0������������ڲ�ά��
        else
            call core%info(p,name//'[1]',found,var_type,n_children)
            if(core%failed()) then
                call core%print_error_message(error_unit)
            end if
            
            y = n_children
            
        end if
        !write(*,*) x,y
        allocate(matrix(x,y))
        allocate(temp_matrix(y))
        
        do i = 1,x
            write(chari,'(I5)') i
            !call core%get(p, name//'['//trim(chari)//']',matrix(i,:),found)
            call core%get(p, name//'['//trim(chari)//']',temp_matrix,found)
            
            if(core%failed()) then
                call core%print_error_message(error_unit)
            end if
            !write(*,*) temp_matrix
            matrix(i,:) = temp_matrix
        end do
        
        return        
        
    end subroutine
    
    !fortran��û��dictionary����
    !�����Ҫ�Լ�д��������ȡintendPids������keyֵ������������ʽ
    !ͨ��ʶ���Ӧ����������ά�ȵ�ֵ��json_core%info������n_children����ֵ���������������
    !��ѭ����ȡdictionary���͵�keyֵ
    
    !core   json_core���ͣ�json������
    !p      json_value���ͣ�����json���ݵ�ָ��
    !name   �ַ����ͣ���Ҫ��ȡ������json�е�����
    !array  ����һά���飨integer(IK),dimension(:,:)������ȡ��ά���鲢����
    subroutine GetKeysInt(core, p, name, array)
        implicit none
        
        type(json_value),pointer,intent(in) :: p !! a pointer for low-level manipulations
        type(json_core) :: core       !! factory for manipulating `json_value` pointers
        character(kind=CK, len=*),intent(in) :: name
        integer(IK),dimension(:),allocatable,intent(out) :: array
        
        !integer(IK),dimension(:),allocatable :: temp_array
        logical(LK) :: found
        integer(IK) :: var_type
        integer(IK) :: n_children
        character(kind=CK,len=:),allocatable :: key_char
        integer(IK) :: x,y,i
        character(5) :: chari
        
        !���Ҹñ���������ά�������ά�ȵ�����������Ϊx
        !call core%info(p,name,found,var_type,n_children)
        !if(core%failed()) then
        !    call core%print_error_message(error_unit)
        !end if
        call core%clear_exceptions()
        call core%info(p,name,found,var_type,n_children)
        if(core%failed()) then
            call core%print_error_message(error_unit)
        end if
        
        x = n_children
        allocate(array(x))
        
        do i = 1,x
            write(chari,'(I5)') i
            
            !call core%get(p, name//'['//trim(chari)//']',temp_matrix,found)
            call core%info(p,name//'['//trim(chari)//']',found,var_type,n_children,key_char)            
            if(core%failed()) then
                call core%print_error_message(error_unit)
            end if
            !write(*,*) temp_matrix
            read(key_char,*) array(i)
            
        end do
        return
    end subroutine

end module FloodProjectInfo