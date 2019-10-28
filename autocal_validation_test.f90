program autocal_validation
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
    !MPI相关变量：scounts->给不同的进程发送数据的个数；displs->发送数据偏移
    integer,dimension(:),allocatable::scounts,displs

    character(len=*),parameter :: dir = '../../files/inputs/'            !! working directory
    character(len=*),parameter :: filename = '86-1_validation.json'       !! file to read
    character(len=*),parameter :: list_dir = './result/'            !! working directory
    character(len=*),parameter :: list_file = 'param_files.txt'       !! file to read
    type(FloodProjectInfoClass) fprj
    type(FloodProjectInfoClass_OriginType) :: fprj_new
    type(configType) :: config
    integer :: n_errors,error
    integer :: i,j,k
    integer :: batch0,batch
    real,dimension(:),allocatable :: param_vec
    real,dimension(:),allocatable :: result_vec
    character(len=25),allocatable,dimension(:) :: param_files,local_param_files
    real :: of  !模型评价指标的水平


    !开启MPI
    call MPI_INIT(ierr)
    !获取当前进程号(myid)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
    !获取总进程数量(numprocs)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,numprocs,ierr)
    !获取当前进程所在机器名称(processor_name)
    call MPI_GET_PROCESSOR_NAME(processor_name,namelen,ierr)


    if(myid .eq. 0) then

        i=0 !i为参数的个数
        j=0
        !获取参数文件
        write(*,*) 'start reading list file..'
        open(108,file=list_dir//list_file)
        do while(.true.)
            read(108,'(A8)',iostat=error)
            if(error/=0) exit
            i = i+1
        end do
        rewind(108)
        allocate(param_files(i))
        do j=1,i
            read(108,*) param_files(j)
        end do
        close(108)

        batch0 = mod(i,numprocs)
        batch = i/numprocs
        ! write(*,*) 'i is',i,' blocks is ',blocks,' batch is ',batch
        call MPI_BCAST(batch,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)

        allocate(local_param_files(batch+batch0))

        allocate(scounts(numprocs))
        allocate(displs(numprocs))

        scounts(1) = (batch+batch0) * 25
        displs(1) = 0
        scounts(2) = batch * 25
        displs(2) = (batch+batch0) * 25

        do j=3,numprocs
            scounts(j) = batch * 25
            displs(j) = displs(j-1) + batch * 25
        end do

        ! scounts(numprocs) = (batch+batch0) * 25
        ! displs(numprocs) = (numprocs-1) * batch * 25

        call MPI_SCATTERV(param_files(1),scounts,displs,MPI_CHARACTER,&
        &local_param_files(1),((batch+batch0) * 25),MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)

        write(*,*) 'the first result on ',myid,' is ',local_param_files(1)
        write(*,*) 'the last result on ',myid,' is ',local_param_files(batch+batch0)


        ! write(*,*) dir
        call GetFprjFromJson(dir, filename, fprj, n_errors)
        call fprj_update(fprj,fprj_new)        
        call Config_For_Calibration(fprj_new,config)

        allocate(param_vec(config%len_para))
        allocate(result_vec(fprj_new%nCount))

    !要将循环读取参数的部分并行化。在主进程读取list_file，然后将param_files通过MPI_BCAST发送到其他进程，同步读取。
    !但有个问题，不知道param_files数组有多长，应该分配多少进程合适。这个可能需要问一下。
    !循环读取参数并计算
    do j=1,batch+batch0
        write(*,*) j
        open(109+myid,file=list_dir//trim(adjustl(local_param_files(j))))
        read(109+myid,*) !跳过参数文件第一行，这一行是率定期的NSE指标
        do k=1,config%len_para
            read(109+myid,*) param_vec(k)
        end do 
        call update_param(fprj_new,param_vec,config%len_para)
        call Call_EasyDHM_Dll(fprj_new)
        call Result_out(fprj_new%nCount,result_vec)
        write(*,*) 'total len of result is ', size(result_vec)
        of = objm_mpi(config%fq,result_vec,0)   !计算模型的评价指标。参数0指代NSE评价指标
        ! call save_result_proc(config%fq,result_vec,fprj_new%nCount,param_vec,j,888,of)
        !要在save_result_vali函数中增加文件名称参数，使得率定期计算结果能和验证期对得上
        call save_result_vali(config%fq,result_vec,fprj_new%nCount,j,of)
        call Destroy()

        write(*,*) 'the NSE of trai ',i,'  is ',of
    end do


        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        write(*,*) 'Program Finish!'
    else

        call MPI_BCAST(batch,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
        allocate(local_param_files(batch))
        call MPI_SCATTERV(param_files(1),scounts,displs,MPI_CHARACTER,&
        &local_param_files(1),((batch) * 25),MPI_CHARACTER,0,MPI_COMM_WORLD,ierr)
        write(*,*) 'the first result on ',myid,' is ',local_param_files(1)
        write(*,*) 'the first result on ',myid,' is ',local_param_files(batch)

        call GetFprjFromJson(dir, filename, fprj, n_errors)
        call fprj_update(fprj,fprj_new)        
        call Config_For_Calibration(fprj_new,config)

        allocate(param_vec(config%len_para))
        allocate(result_vec(fprj_new%nCount))

    !要将循环读取参数的部分并行化。在主进程读取list_file，然后将param_files通过MPI_BCAST发送到其他进程，同步读取。
    !但有个问题，不知道param_files数组有多长，应该分配多少进程合适。这个可能需要问一下。
    !循环读取参数并计算
        do j=1,batch
            write(*,*) j
            open(109+myid,file=list_dir//trim(adjustl(local_param_files(j))))
            read(109+myid,*) !跳过参数文件第一行，这一行是率定期的NSE指标
            do k=1,config%len_para
                read(109+myid,*) param_vec(k)
            end do 
            call update_param(fprj_new,param_vec,config%len_para)
            call Call_EasyDHM_Dll(fprj_new)
            call Result_out(fprj_new%nCount,result_vec)
            write(*,*) 'total len of result is ', size(result_vec)
            of = objm_mpi(config%fq,result_vec,0)   !计算模型的评价指标。参数0指代NSE评价指标
            ! call save_result_proc(config%fq,result_vec,fprj_new%nCount,param_vec,j,888,of)
            !要在save_result_vali函数中增加文件名称参数，使得率定期计算结果能和验证期对得上
            call save_result_vali(config%fq,result_vec,fprj_new%nCount,j,of)
            call Destroy()

            write(*,*) 'the NSE of trai ',i,'  is ',of
        end do
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
    end if
    call MPI_FINALIZE(rc)
    
end program autocal_validation