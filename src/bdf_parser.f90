!> author: 左志华
!> date: 2022-07-27
!>
!> 主要用来处理 MSC.Patran BDF 文件 <br>
!> - shell: 壳单元（点、三角面元、四边形面元）
module bdf_parser_m

    use, intrinsic :: iso_fortran_env, only: rk => real64, stdout => output_unit
    implicit none
    private

    public :: bdf_shell_t

    !> 节点结构体
    type grid_t
        integer :: id       !! Grid ID <br>
                            !! 节点编号
        real(rk) :: loc(3)  !! location of grid <br>
                            !! 节点坐标
    end type grid_t

    !> 三角网格结构体
    type card3_t
        integer :: id       !! Grid ID <br>
                            !! 节点编号
        integer :: grid(3)  !! Grid ID of three nodes <br>
                            !! 节点编号
        integer :: index(3) !! index of three nodes <br>
                            !! 节点编号
    end type card3_t

    !> 四边形网格结构体
    type card4_t
        integer :: id       !! Grid ID <br>
                            !! 节点编号
        integer :: grid(4)  !! Grid ID of four nodes <br>
                            !! 节点编号
        integer :: index(4) !! index of four nodes <br>
                            !! 节点编号
    end type card4_t

    !> BDF 文件结构体（2D）
    type bdf_shell_t
        integer :: card3_num, card4_num, grid_num
        type(card3_t), allocatable :: card3(:)  !! card3_t 类型的数组，用来存储三角面元的信息
        type(card4_t), allocatable :: card4(:)  !! card4_t 类型的数组，用来存储四边形面元的信息
        type(grid_t), allocatable :: grid(:)    !! grid_t 类型的数组，用来存储节点的信息
    contains
        procedure :: read => bdf_shell_t_read_file
        procedure :: link => bdf_shell_t_link_process
    end type bdf_shell_t

contains

    !> Load BDF file <br>
    !> 读取 BDF 文件（2D）
    subroutine bdf_shell_t_read_file(self, filename, info)
        class(bdf_shell_t), intent(inout) :: self
        character(len=*), intent(in) :: filename    !! BDF file name <br>
                                                    !! BDF 文件名
        logical, intent(in), optional :: info       !! print info <br>
                                                    !! 打印信息
        integer :: bdf_unit
        character(:), allocatable :: buffer_str
        integer :: io_stat
        integer :: count_card3, count_card4, count_grid

        count_card3 = 0
        count_card4 = 0
        count_grid = 0

        open (newunit=bdf_unit, file=filename, status='old', action='read')
        allocate (character(6) :: buffer_str)
        do  ! 索引数量
            read (bdf_unit, *, iostat=io_stat) buffer_str
            !> 读取到 EOF 时，退出循环
            if (is_iostat_end(io_stat)) exit

            select case (buffer_str(1:6))
            case ("GRID")
                count_grid = count_grid + 1
            case ("CQUAD4")
                count_card4 = count_card4 + 1
            case ("CTRIA3")
                count_card3 = count_card3 + 1
            end select
        end do

        allocate (self%card3(count_card3), self%card4(count_card4), self%grid(count_grid))
        self%card3_num = count_card3
        self%card4_num = count_card4
        self%grid_num = count_grid

        if (present(info)) then
            if (info) then
                write (stdout, '(a, i0)') "card3_num: ", count_card3
                write (stdout, '(a, i0)') "card4_num: ", count_card4
                write (stdout, '(a, i0)') "all cards: ", count_card3 + count_card4
                write (stdout, '(a, i0)') "grid_num : ", count_grid
            end if
        end if

        rewind (bdf_unit)
        deallocate (buffer_str)
        allocate (character(80) :: buffer_str)
        count_card3 = 0
        count_card4 = 0
        count_grid = 0

        do
            read (bdf_unit, "(a)", iostat=io_stat) buffer_str
            !> 读取到 EOF 时，退出循环
            if (is_iostat_end(io_stat)) exit

            select case (buffer_str(1:6))
            case ("GRID")
                count_grid = count_grid + 1
                read (buffer_str, "(t9,i8,t25,3g8.1)") &
                    self%grid(count_grid)%id, &
                    self%grid(count_grid)%loc(1:3)
            case ("CQUAD4")
                count_card4 = count_card4 + 1
                read (buffer_str, "(t9,i8,t25,4i8)") &
                    self%card4(count_card4)%id, &
                    self%card4(count_card4)%grid(1:4)
            case ("CTRIA3")
                count_card3 = count_card3 + 1
                read (buffer_str, "(t9,i8,t25,3i8)") &
                    self%card3(count_card3)%id, &
                    self%card3(count_card3)%grid(1:3)
            end select
        end do

        close (bdf_unit)
        deallocate (buffer_str)

    end subroutine bdf_shell_t_read_file

    !> Link process <br>
    !> 链接节点与壳单元
    pure subroutine bdf_shell_t_link_process(self)
        class(bdf_shell_t), intent(inout) :: self
        integer :: i, j

        do concurrent(i=1:self%grid_num)
            do concurrent(j=1:self%card3_num)
                if (self%grid(i)%id == self%card3(j)%grid(1)) then
                    self%card3(j)%index(1) = i
                elseif (self%grid(i)%id == self%card3(j)%grid(2)) then
                    self%card3(j)%index(2) = i
                elseif (self%grid(i)%id == self%card3(j)%grid(3)) then
                    self%card3(j)%index(3) = i
                end if
            end do

            do concurrent(j=1:self%card4_num)
                if (self%grid(i)%id == self%card4(j)%grid(1)) then
                    self%card4(j)%index(1) = i
                elseif (self%grid(i)%id == self%card4(j)%grid(2)) then
                    self%card4(j)%index(2) = i
                elseif (self%grid(i)%id == self%card4(j)%grid(3)) then
                    self%card4(j)%index(3) = i
                elseif (self%grid(i)%id == self%card4(j)%grid(4)) then
                    self%card4(j)%index(4) = i
                end if
            end do
        end do

    end subroutine bdf_shell_t_link_process

end module bdf_parser_m
