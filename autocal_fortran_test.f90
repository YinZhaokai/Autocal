
! #ifndef INTEGRATED_TESTS
! !*****************************************************************************************
program jf_test_1

    use FloodProjectInfo
    use ParseToDll
    use Fprj_Update_Mod
    use CallDll
    use configmod
    use Param_Sampling
    use SaveComputeData
    implicit none
    
    character(len=*),parameter :: dir = '../../files/inputs/'            !! working directory
    character(len=*),parameter :: filename = '86-1_test.json'       !! file to read

    type(FloodProjectInfoClass) fprj
    type(FloodProjectInfoClass_OriginType) :: fprj_new
    type(configType) :: config
    integer :: n_errors
    integer :: i,j
    real,dimension(:),allocatable :: param_vec
    real,dimension(:),allocatable :: result_vec
    real :: of  !模型评价指标的水平
    
    write(*,*) dir
    call GetFprjFromJson(dir, filename, fprj, n_errors)
    call fprj_update(fprj,fprj_new)
    
    call Config_For_Calibration(fprj_new,config)

    allocate(param_vec(config%len_para))
    allocate(result_vec(fprj_new%nCount))
    do i=1,10
        call testSampling(config%boundarys,config%len_para,param_vec)
        ! call update_param(fprj_new,param_vec,config%len_para)
        
        call Call_EasyDHM_Dll(fprj_new)

        
        call Result_out(fprj_new%nCount,result_vec)

        write(*,*) 'total len of result is ', size(result_vec)
        do j=100,110
            write(*,*) result_vec(j)
        end do
        of = objm_mpi(config%fq,result_vec,0)   !计算模型的评价指标。参数0指代NSE评价指标
        call save_result_proc(config%fq,result_vec,fprj_new%nCount,param_vec,i,200,of)
        call Destroy()
        
        write(*,*) 'the NSE of trai ',i,'  is ',of
    end do
    ! call SaveST_Pr()
    
    write(*,*) 'Program Finish!'
    
    ! read(*,*)
end program jf_test_1
! !*****************************************************************************************
! #endif
