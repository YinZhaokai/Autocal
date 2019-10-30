module SaveComputeData
    use ParseToDll
    
    contains
    subroutine SaveST_Pr()
        implicit none
        integer :: PR_sid = 0, PR_IRunoffGenType = 0, PR_pid = 0, N = 0, a6 = 0
        real :: PR_Pmm = 0, PR_Rsimms = 0, PR_Rmeams = 0, PR_Rsimmm = 0, PR_Rmeamm = 0
        real :: PR_PETmm = 0, Qms = 0, QFinal = 0, Uprainmm=0
        character(10) :: a6_char
        character(23) :: TM

        open(125,file = "ST_PR.txt", status = 'replace')
        write(125,"(A10,A15,A17,A15,A13,A13,A10)")"SID","RunoffnType","TM","P","Rsim","Rmea","PET"
        do while (GegPrDataPR_Ex(a6, PR_sid, PR_IRunoffGenType, PR_pid, PR_Pmm&
            &, PR_Rsimms, PR_Rmeams, PR_Rsimmm, PR_Rmeamm, PR_PETmm, Qms, QFinal ,Uprainmm) .GE. 1)
            write(a6_char,"(i10)")a6
            TM = a6_char(1:4)//"-"//a6_char(5:6)//"-"//a6_char(7:8)//" "//a6_char(9:10)//":00:00.000"
            write(125,"(I10,I10,A30,F10.3,F10.3,F15.3,F10.3)") PR_sid,PR_IRunoffGenType,TM&
            &, PR_Pmm, PR_Rsimms, PR_Rmeams,PR_PETmm
        end do

    end subroutine

    subroutine Result_out(nCount,result_array)
        implicit none
        integer,intent(in) ::nCount
        real,dimension(nCount),intent(out) :: result_array
        integer :: PR_sid = 0, PR_IRunoffGenType = 0, PR_pid = 0, N = 0, a6 = 0
        real :: PR_Pmm = 0, PR_Rsimms = 0, PR_Rmeams = 0, PR_Rsimmm = 0, PR_Rmeamm = 0
        real :: PR_PETmm = 0, Qms = 0, QFinal = 0, Uprainmm=0
        character(10) :: a6_char
        character(23) :: TM
        integer :: i
        
        i = 1
        ! write(*,*) 'the i after Result_out is = ',i
        !if(allocated(result_array)) deallocate(result_array)
        !allocate(result_array(nCount))
        do while (GegPrDataPR_Ex(a6, PR_sid, PR_IRunoffGenType, PR_pid, PR_Pmm&
            &, PR_Rsimms, PR_Rmeams, PR_Rsimmm, PR_Rmeamm, PR_PETmm, Qms, QFinal ,Uprainmm) .GE. 1)
            !write(a6_char,"(i10)")a6
            !TM = a6_char(1:4)//"-"//a6_char(5:6)//"-"//a6_char(7:8)//" "//a6_char(9:10)//":00:00.000"
            !write(125,"(I10,I10,A30,F10.3,F10.3,F15.3,F10.3)") PR_sid,PR_IRunoffGenType,TM, PR_Pmm, PR_Rsimms, PR_Rmeams,PR_PETmm
            result_array(i) = PR_Rsimms
            i = i + 1
        end do

        ! write(*,*) 'the i after Result_out is = ',i
        
        return    
    end subroutine

    !每个线程上保存计算结果到文件中的函数
    !obs->实测流量过程
    !sim->模型计算结果
    !nCount->流量过程长度
    !param->参数数组
    !trial->当前循环次数-用于设置文件名称
    !process->当前进程号-用于设置文件名称
    !of->目标函数值
    subroutine save_result_proc(obs,sim,nCount,param,trial,process,of)
        implicit none
        integer,intent(in) :: nCount,trial,process
        real,intent(in) :: of
        real,dimension(nCount),intent(in) :: obs,sim
        real,dimension(:),intent(in) :: param
        character(len=255) :: name1,name2,trialchar,processchar
        integer :: i

        write(trialchar,*) trial
        write(processchar,*) process
        write(name1,*) './result/'&
        &//trim(adjustl(processchar))//'_'//trim(adjustl(trialchar))//'_param.txt'
        write(name2,*) './result/'&
        &//trim(adjustl(processchar))//'_'//trim(adjustl(trialchar))//'_result.txt'
        
        open(125+trial+process,file = trim(adjustl(name1)), status = 'replace')
        write(125+trial+process,*) of
        write(*,*) 'param size is ',size(param)
        do i=1,size(param)
            write(125+trial+process,*) param(i)
        !    write(*,*) 'param is ',param(i)
        end do
        close(125+trial+process)

        open(126+trial+process,file = trim(adjustl(name2)), status = 'replace')
        do i=1,nCount
            write(126+trial+process,*)obs(i),' ',sim(i)
        end do
        close(126+trial+process)
        return
    end subroutine



    !验证期保存计算结果到文件中的函数
    !obs->实测流量过程
    !sim->模型计算结果
    !nCount->流量过程长度    
    !trial->当前循环次数-用于设置文件名称    
    !member->当前参数组合对应的文件名称，用于生成新的文件名称
    !of->目标函数值
    subroutine save_result_vali(obs,sim,nCount,trial,member,of)
        implicit none
        integer,intent(in) :: nCount,trial
        real,intent(in) :: of
        real,dimension(nCount),intent(in) :: obs,sim
        character(len=:),allocatable,intent(in):: member
        ! real,dimension(:),intent(in) :: param
        character(len=255) :: name2
        integer :: i

        ! write(trialchar,*) trial
        ! write(processchar,*) process
        ! write(name1,*) './result/'&
        ! &//'vali_'//trim(adjustl(trialchar))//'_param.txt'
        ! write(name2,*) './result2/vali_'//trim(adjustl(trialchar))//'_result.txt'
        write(name2,*) './result2/vali_'//trim(adjustl(member))
        
        ! open(125+trial+process,file = trim(adjustl(name1)), status = 'replace')
        ! write(125+trial+process,*) of
        ! write(*,*) 'param size is ',size(param)
        ! do i=1,size(param)
        !     write(125+trial+process,*) param(i)
        ! !    write(*,*) 'param is ',param(i)
        ! end do
        ! close(125+trial+process)

        open(126+trial,file = trim(adjustl(name2)), status = 'replace')
        write(126+trial,*) of
        do i=1,nCount
            write(126+trial,*)obs(i),' ',sim(i)
        end do
        close(126+trial)
        return
    end subroutine

    ! !判断模型结果是否符合某种评价指标的水平
    ! !obs->实测流量过程
    ! !sim->模型计算结果
    ! !ncount->流量过程长度
    ! !obj ->评价指标代号：1->NSE
    ! !level -> 评价指标水平，指代评价指标达到这个值之上后认为该组参数效果满足精度要求
    ! !flag -> 返回是否满足精度要求的变量。true-> 满足精度要求；false -> 不满足精度要求
    ! subroutine check_perform(obs,sim,ncount,obj,level,flag)
    !     implicit none
    !     integer,intent(in) :: ncount,obj
    !     real,dimension(ncount),intent(in) :: obs,sim
    !     real,intent(in) :: level
    !     logical,intent(out) :: flag
    ! end subroutine


    !返回模型评价指标结果
    !obs->实测流量过程
    !sim->模型计算结果
    !objid ->评价指标代号：0->NSE
    !of -> 评价指标水平
    function objm_mpi(obs,sim,objid) result(of)
        implicit none
        real,dimension(:)::sim
        real,dimension(:)::obs
        integer :: objid
        real :: of
        real :: meanobs

        select case(objid)
            case (0) !计算NSE
                meanobs = sum(obs)/size(obs)
                of = 1 - sum((obs-sim)**2)/sum((obs-meanobs)**2)
            case default
                meanobs = sum(obs)/size(obs)
                of = 1 - sum((obs-sim)**2)/sum((obs-meanobs)**2)
        end select

        return
    end function



end module
