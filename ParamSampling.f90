module Param_Sampling
    implicit none
    
contains
    !随机抽样函数，根据提供的参数上下限进行参数抽样，并返回抽样后的参数结果。
    !用来模拟参数率定算法的运行状态
    !boundary表示包含该模型参数上下限的数组；
    !len_bound表示该模型参数数量，即boundary数组的1个维度；
    !param_vec表示传出的参数数组
    subroutine testSampling(boundary,len_bound,param_vec)
        implicit none
        integer,intent(in) :: len_bound
        real,dimension(len_bound,2),intent(in) :: boundary
        real,dimension(len_bound),intent(out) :: param_vec
        
        integer :: i
        real :: rand
        call RANDOM_SEED()
        do i=1,len_bound
            call RANDOM_NUMBER(rand)
            param_vec(i) = boundary(i,1) + rand * (boundary(i,2)-boundary(i,1))
        end do

        
        return
    end subroutine

    !生成抽样参数库（不包括全排列情形）
    !boundary表示包含该模型参数上下限的数组；
    !len_bound表示该模型参数数量，即boundary数组的1个维度；
    !param_matrix标识传出的参数库矩阵(参数个数*参数库规模)，1列为1组参数；
    !param_u_d表示某模型每个参数的均值、方差（为高斯抽样做准备）；
    !total表示参数库规模，即参数库中含有多少组参数；
    !sample_type表示抽样类型：0->完全随机生成；1->拉丁超立方抽样；2->高斯抽样1；3->高斯抽样2
    subroutine ensemble_gen(boundary,len_bound,param_matrix,param_u_d,total,sample_type)
        implicit none
        integer,intent(in) :: len_bound, total, sample_type
        real,dimension(len_bound,2),intent(in) :: boundary
        real,dimension(len_bound,2),intent(in) :: param_u_d
        real,dimension(len_bound,total),intent(out) :: param_matrix
        integer :: i,j  !循环计数器
        real :: rand_num    !随机数（0，1）
        real :: rand_num_2  !随机数（0，1）
        real :: ss
        real ::span         !拉丁超立方抽样时根据抽样总数不同而变化的参数取值段
        real :: u,d,z         !当前参数的均值/方差

        select case(sample_type)
            case (0)    !完全随机抽样
                call random_seed()
                do j=1,total
                    do i = 1,len_bound
                        call random_number(rand_num)
                        param_matrix(i,j) = boundary(i,1) + rand_num * (boundary(i,2)-boundary(i,1))
                    end do                    
                end do

            case (1)    !拉丁超立方抽样
                call random_seed()
                do i = 1,len_bound
                    do j=1,total
                        call random_number(rand_num)
                        span = (boundary(i,2)-boundary(i,1))/total
                        param_matrix(i,j) = boundary(i,1) + (j-1)*span + rand_num*span
                    end do 
                    call shuffle(param_matrix(i,:))
                end do

            case (2)    !高斯抽样-The Box-Muller transformation方法
                call random_seed()
                do i = 1,len_bound
                    u = param_u_d(i,1)      !从数组中找到当前参数对应的均值、方差
                    d = param_u_d(i,2)
                    do j=1,total
                        call random_number(rand_num)
                        call random_number(rand_num_2)
                        ! write(*,*) 'rand1 = ',rand_num,'rand2 = ',rand_num_2
                        z = sqrt(-2*log(rand_num))*sin(2*3.1415926536*rand_num_2)
                        ! write(*,*) 'z = ',z
                        param_matrix(i,j) = u + d*z
                    end do
                end do



            case (3)    !高斯抽样-Polar Method方法 
                call random_seed()
                do i = 1,len_bound
                    u = param_u_d(i,1)      !从数组中找到当前参数对应的均值、方差
                    d = param_u_d(i,2)
                    
                    do j=1,total
                        ss = 0
                        do while(ss .gt. 1 .or. ss .eq. 0)
                            call random_number(rand_num)
                            call random_number(rand_num_2)
                            ss = (rand_num*2-1)**2+(rand_num_2*2-1)**2
                        end do
                        ! write(*,*) 'ss = ',ss
                        z = sqrt(-2*log(ss)/ss) * (rand_num*2-1)
                        param_matrix(i,j) = u + d*z
                    end do
                end do


            case default
                call random_seed()
                do j=1,total
                    do i = 1,len_bound
                        call random_number(rand_num)
                        param_matrix(i,j) = boundary(i,1) + rand_num * (boundary(i,2)-boundary(i,1))
                    end do                    
                end do

        end select

        return
    end subroutine

    !一维real数组乱序排列
    subroutine shuffle(D)
        implicit none
        real,intent(INOUT):: D(:)
        integer::i,p
        real::r,t

        do i = size(D),2,-1
            call random_number(r)
            p = int(r * i) +1
            t = D(p)
            D(p) = D(i)
            D(i) = t
        end do
    end subroutine

    
end module