
! #ifndef INTEGRATED_TESTS
! !*****************************************************************************************
program autocal_mpi_test_1

    use FloodProjectInfo
    use ParseToDll
    use Fprj_Update_Mod
    use CallDll
    use configmod
    use Param_Sampling
    use SaveComputeData
    use mpi
    implicit none

    !MPI相关变量：processor_name->某进程所在机器的名称
    character *(MPI_MAX_PROCESSOR_NAME) processor_name 
    !MPI相关变量：myid->当前进程号；numprocs->总进程数量；namelen->机器名称的长度；rc->MPI运行结束信息；ierr->MPI错误代码
    integer :: myid,numprocs,namelen,rc,ierr
    !MPI相关变量：status->返回状态，status(MPI_SOURCE)->发送数据的进程标识；status(MPI_TAT)->发送数据使用tag标识；status(MPI_ERROR)->该接收操作返回的错误代码
    integer :: status(MPI_STATUS_SIZE)
    
    character(len=*),parameter :: dir = '/home/liaowh2/Autocal_mpi/Autocal/files/inputs/'            !! working directory
    character(len=*),parameter :: filename = '86-1_test.json'       !! file to read
    type(FloodProjectInfoClass) fprj
    type(FloodProjectInfoClass_OriginType) :: fprj_new
    type(configType) :: config
    integer :: n_errors
    integer :: i,j
    integer :: total    !参数库总数
    integer :: local_num    !每个线程上的参数数量。由total/numprocs计算而来，因此要求total/numprocs可以整除
    integer :: buffer(2)    !用于消息传递的数组。其中buffer(1)为config%len_para，buffer(2)为local_num。用来初始化local_matrix
    real,dimension(:),allocatable :: param_vec
    real,dimension(:),allocatable :: result_vec

    real,dimension(:,:),allocatable :: param_matrix !总的参数抽样库
    real,dimension(:,:),allocatable :: local_matrix !每个线程的参数库

    real :: of  !模型评价指标的水平
    
    !开启MPI
    call MPI_INIT(ierr)
    !获取当前进程号(myid)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
    !获取总进程数量(numprocs)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,numprocs,ierr)
    !获取当前进程所在机器名称(processor_name)
    call MPI_GET_PROCESSOR_NAME(processor_name,namelen,ierr)

    if(myid .eq. 0) then    !主进程代码，首先读取json文件，并从中识别模型类别，设置相应配置参数
                            !并且生成参数库，并将参数库发布给其它线程（对等模式，MPI_SCATTER方法）
                            !在发布参数库后同样参与计算。每个线程计算后各自保存结果
        write(*,*) dir,' on the pid of ',myid,' of',numprocs,' on', processor_name
        ! write(*,*) dir
        call GetFprjFromJson(dir, filename, fprj, n_errors)
        call fprj_update(fprj,fprj_new)
        
        call Config_For_Calibration(fprj_new,config)


        total = 120000               !总集合数量
        local_num = total/numprocs  !每个线程集合数量
        buffer(1) = config%len_para
        buffer(2) = local_num
        call MPI_BCAST(buffer(1),2,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)   !将构建local_matrix的参数广播给其他进程
        allocate(param_matrix(config%len_para,total))
        allocate(local_matrix(config%len_para,local_num))
        
        call ensemble_gen(config%boundarys,config%len_para,param_matrix,&
        &config%param_u_d,total,1)
        write(*,*) 'buffer(1) = ', buffer(1), ' buffer(2) = ', buffer(2),' on ',myid
        call MPI_SCATTER(param_matrix(1,1),buffer(1)*buffer(2),MPI_REAL,&       !将参数库散发到不同的进程上
        &local_matrix(1,1),buffer(1)*buffer(2),MPI_REAL,0,MPI_COMM_WORLD,ierr)
        ! open(unit=916,file='param_sampling.txt')
        ! write(916,*) 'boundarays'
        ! write(916,*) config%boundarys(:,1)
        ! write(916,*) config%boundarys(:,2)
        ! write(916,*) 'samples'
        write(*,*) 'params on ',myid,' of ',numprocs,' on ',trim(processor_name), ' are ',local_matrix(6,:)       

        allocate(result_vec(fprj_new%nCount))
        do i=1,buffer(2)
            write(*,*) 'param of No.',i,' trial on ',myid,' is ',local_matrix(6,i)
            call update_param(fprj_new,local_matrix(:,i),buffer(1))
            call Call_EasyDHM_Dll(fprj_new)            
            call Result_out(fprj_new%nCount,result_vec)
            ! write(*,*) 'the first result of No.',i,' trial on ',myid,' is ',result_vec(1)

            of = objm_mpi(config%fq,result_vec,0)   !计算模型的评价指标。参数0指代NSE评价指标
            if (of .gt. 0.6) then                   !当评价指标超出某个水平后打印结果
                write(*,*) 'PASSED !! the NSE value of No.',i,' trial on ',myid,' is ',of
                !未来将在此加入保存结果为文件的函数
                call save_result_proc(config%fq,result_vec,fprj_new%nCount,local_matrix(:,i),i,myid,of)
            end if

            call Destroy()
        end do

    else    !从进程代码，首先读取json文件，然后接收构建local_matrix的参数
            !在为local_matrix分配内存后接收主进程分配过来的参数
        call GetFprjFromJson(dir, filename, fprj, n_errors)
        call fprj_update(fprj,fprj_new)
        call Config_For_Calibration(fprj_new,config)

        call MPI_BCAST(buffer(1),2,MPI_INTEGER,0,MPI_COMM_WORLD,ierr) !从主进程接收构建local_matrix的参数
        allocate(local_matrix(buffer(1),buffer(2)))
        write(*,*) 'buffer(1) = ', buffer(1), ' buffer(2) = ', buffer(2),' on ',myid
        call MPI_SCATTER(param_matrix(1,1),buffer(1)*buffer(2),MPI_REAL,&       !从主进程接收local_matrix中的数据
        &local_matrix(1,1),buffer(1)*buffer(2),MPI_REAL,0,MPI_COMM_WORLD,ierr)

        write(*,*) 'params on ',myid,' of ',numprocs,' on ',trim(processor_name), ' are ',local_matrix(6,:)

        allocate(result_vec(fprj_new%nCount))
        do i=1,buffer(2)
            write(*,*) 'param of No.',i,' trial on ',myid,' is ',local_matrix(6,i)
            call update_param(fprj_new,local_matrix(:,i),buffer(1))
            call Call_EasyDHM_Dll(fprj_new)            
            call Result_out(fprj_new%nCount,result_vec)
            ! write(*,*) 'the first result of No.',i,' trial on ',myid,' is ',result_vec(1)

            of = objm_mpi(config%fq,result_vec,0)   !计算模型的评价指标。参数0指代NSE评价指标

            if (of .gt. 0.6) then                   !当评价指标超出某个水平后打印结果
                write(*,*) 'PASSED !! the NSE value of No.',i,' trial on ',myid,' is ',of
                !未来将在此加入保存结果为文件的函数
                call save_result_proc(config%fq,result_vec,fprj_new%nCount,local_matrix(:,i),i,myid,of)
            end if
            call Destroy()
        end do


    end if
        ! close(916)

        ! do i=1,buffer(2)
        !     param_vec = local_matrix

    ! allocate(param_vec(config%len_para))
    ! call testSampling(config%boundarys,config%len_para,param_vec)
    ! call update_param(fprj_new,param_vec,config%len_para)
    
    ! call Call_EasyDHM_Dll(fprj_new)

    ! allocate(result_vec(fprj_new%nCount))
    ! call Result_out(fprj_new%nCount,result_vec)
    ! do i=1,fprj_new%nCount
    !     write(*,*) result_vec(i)
    ! end do
    ! call SaveST_Pr()
    
    ! write(*,*) 'Program Finish! on ',myid,' of',numprocs,' on', processor_name
    call MPI_FINALIZE(rc)
    ! read(*,*)
end program autocal_mpi_test_1
! !*****************************************************************************************
! #endif
