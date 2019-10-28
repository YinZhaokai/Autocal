!为参数率定/集合预报提供相应的配置信息，主要包括不同模型类型对应的参数上下限、预热期长度、实测流量过程、
module configmod
    use Fprj_Update_Mod
    implicit none
    !定义配置文件类型，包含参数率定/集合预报所需全部信息
    type configType
        integer :: spinUpTime       !预热期长度（时刻数）。预热期指的是模型开始计算时间与参数率定开始时间之间的差。
                                    !由于模型刚刚开始计算的时候内部变量尚处在初始值附近，因此模型结果与实测结果常常偏离很大。
                                    !因此通过设定预热期的方式来初始化模型内部变量，在率定时仅针对预热期之后到结束时刻的结果计算目标函数值。
                                    !在进行实际预报时也会先进行一段时间的预热再进行预报。
        integer :: len_para         !为模型参数数量（长度）
        real,dimension(:,:),allocatable :: boundarys !模型参数上下限（二维数组，维度分别为参数数量：2，分别代表某参数的上限/下限）。
                                                    !对于不同模型其模型参数种类、数量及上下限也不相同，因此不同的模型传出的数组是不同的。
                                                    !模型种类从fprj%IRunoffGenTypeStr变量中读取
        integer :: runtype          !模型运行方式。5为直接运行，8为进行参数率定。
        real,dimension(:),allocatable :: fq        !参数分区出口实测径流过程，从模型计算开始时刻到计算结束时刻。根据当前站点类型（ITYPE：0->水库；1->水文站）从fprj%fq(水文站)/fprj%fq1(水库)
                                                    !ITYPE类型从ParamRangesVars(6,i)中读取，i为当前参数分区代码，从fprj%intendPids_KEYS中读取
        real,dimension(:,:),allocatable :: param_u_d    !模型参数均值及方差（二维数组，维度分别为参数数量：2，分别代表某参数的均值/方差）
                                                        !对于不同模型其模型参数种类、数量及上下限也不相同，因此不同的模型传出的数组是不同的。
                                                        !初期时全部设为0.未来从配置文件中读取参数均值方差
    
    end type

contains
    subroutine Config_For_Calibration(fprj,config)
        implicit none
        type(FloodProjectInfoClass_OriginType),intent(in) :: fprj
        type(configType),intent(out) :: config
        
        integer :: i
        integer :: dim_para
        integer :: Itype
        
        
        config%runtype = 8          !未来从配置文件中读取模型运行方式
        config%spinUpTime = 365     !暂时使用固定长度的预热期长度。未来考虑从配置文件中读取或从fprj中识别日期计算时间
        
        !fprj%nSinP保存模型最优参数及相关信息，数据维度为(模型参数个数,13)，按顺序依次为：[ST_SingleChangePara]表的第0-2,5-6,8,3-4,10,9,11-13个字段（从0开始）
        !config%boundarys为(dim_para,2)维度的数组，其中(:,1)为全部参数的下限，(:,2)为全部参数的上限
        dim_para = size(fprj%nSinP,dim=1)
        config%len_para = dim_para
        ! write(*,*) dim_para
        allocate(config%boundarys(dim_para,2))
        allocate(config%param_u_d(dim_para,2))
        do i=1,dim_para
            config%boundarys(i,1) = fprj%nSinP(i,7)
            config%boundarys(i,2) = fprj%nSinP(i,8)
            config%param_u_d(i,1) = 0.5*(fprj%nSinP(i,7)+fprj%nSinP(i,8)) !初期时均值和方差全部设为0.未来从配置文件中读取参数均值方差
            config%param_u_d(i,2) = 0.25*(fprj%nSinP(i,8)-fprj%nSinP(i,7))
        end do        
        
        
        Itype = fprj%ParamRangesVars(fprj%intendPids_KEYS(1),6) !识别当前参数分区出口站点类型，0为水库，1为水文站
        
        allocate(config%fq(fprj%nCount))
        if(Itype == 0) then         !水库
            config%fq = fprj%fq1
        elseif(Itype == 1) then     !水文站
            config%fq = fprj%fQ
        end if
        
        return
    end subroutine
    
    subroutine update_param(fprj,new_para,len_para)
        implicit none
        integer,intent(in) :: len_para
        type(FloodProjectInfoClass_OriginType),intent(inout) :: fprj
        real,dimension(len_para),intent(in) :: new_para
        integer :: i
        
        do i=1,len_para
            fprj%nSinP(i,11) = new_para(i)
        end do
        
        return    
    end subroutine
        
    
    
end module