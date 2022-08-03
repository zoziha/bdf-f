!> author: 左志华
!> date: 2022-07-27
!>
!> 主要用来处理 MSC.Patran BDF 文件 <br>
!> - shell: 壳单元（点、三角面元、四边形面元）
!> @note 节点与网格分别存储，主要可以避免存储重复的数据，减少储存量。
!> 建议网格数量小于 20 万。
module bdf_parser_m

    use, intrinsic :: iso_fortran_env, only: rk => real64
    implicit none
    private

    public :: bdf_shell_t

    !> Grid type <br>
    !> 节点类型
    type grid_t
        integer :: id       !! Grid node ID <br>
                            !! 网格节点编号
        real(rk) :: loc(3)  !! Grid node location <br>
                            !! 网格节点坐标
    end type grid_t

    !> Triangle element type <br>
    !> 三角面元类型
    type card3_t
        integer :: id       !! Element ID <br>
                            !! 三角面元编号
        integer :: fx       !! Element property ID <br>
                            !! 三角面元属性编号
        integer :: grid(3)  !! Element grid nodes <br>
                            !! 三角面元节点
        integer :: index(3) !! index of three nodes <br>
                            !! 节点存储索引
    end type card3_t

    !> Quadrangle element type <br>
    !> 四边形面元类型
    type card4_t
        integer :: id       !! Element ID <br>
                            !! 四边形面元编号
        integer :: fx       !! Element property ID <br>
                            !! 四边形面元属性编号
        integer :: grid(4)  !! Grid ID of four nodes <br>
                            !! 网格节点编号
        integer :: index(4) !! index of four nodes <br>
                            !! 节点存储索引
    end type card4_t

    !> BDF type <br>
    !> BDF 文件结构体（2D）
    type bdf_shell_t
        integer :: card4_num                    !! Number of quadrangle elements <br>
                                                !! 四边形面元数量
        integer :: card3_num                    !! Number of triangle elements <br>
                                                !! 三角面元数量
        integer :: grid_num                     !! Number of nodes <br>
                                                !! 节点数量
        type(card3_t), allocatable :: card3(:)  !! Triangle element <br>
                                                !! card3_t 类型的数组，用来存储三角面元的信息
        type(card4_t), allocatable :: card4(:)  !! Quadrangle element <br>
                                                !! card4_t 类型的数组，用来存储四边形面元的信息
        type(grid_t), allocatable :: grid(:)    !! Grid node <br>
                                                !! grid_t 类型的数组，用来存储节点的信息
    contains
        procedure :: read => bdf_shell_t_read_file
        procedure :: link => bdf_shell_t_link_process
    end type bdf_shell_t

contains

    !> Load BDF file <br>
    !> 读取 BDF 文件（2D）
    impure subroutine bdf_shell_t_read_file(self, bdf_unit, info)
        class(bdf_shell_t), intent(inout) :: self   !! Patran BDF type <br>
                                                    !! Patran BDF 类型
        integer, intent(in) :: bdf_unit             !! BDF file unit <br>
                                                    !! BDF 文件单元
        logical, intent(in), optional :: info       !! print info <br>
                                                    !! 打印信息
        character(:), allocatable :: buffer_str
        integer :: istat, icard3, icard4, igrid

        ! 检索数据量
        icard3 = 0; icard4 = 0; igrid = 0
        allocate (character(6) :: buffer_str)
        do
            read (bdf_unit, '(6a)', iostat=istat) buffer_str
            if (is_iostat_end(istat)) exit    ! 读取到 EOF 时，退出循环

            select case (buffer_str(1:6))
            case ("GRID"); igrid = igrid + 1
            case ("CQUAD4"); icard4 = icard4 + 1
            case ("CTRIA3"); icard3 = icard3 + 1
            end select
        end do

        allocate (self%card3(icard3), self%card4(icard4), self%grid(igrid))
        self%card3_num = icard3
        self%card4_num = icard4
        self%grid_num = igrid

        if (present(info)) then
            if (info) then
                write (*, '(a)') "## BDF information:"
                write (*, '(a, i0)') "card3_num: ", icard3
                write (*, '(a, i0)') "card4_num: ", icard4
                write (*, '(a, i0)') "all cards: ", icard3 + icard4
                write (*, '(a, i0)') "grid_num : ", igrid
            end if
        end if

        rewind (bdf_unit)
        deallocate (buffer_str); allocate (character(80) :: buffer_str)
        icard3 = 0; icard4 = 0; igrid = 0

        do
            read (bdf_unit, "(a)", iostat=istat) buffer_str
            if (is_iostat_end(istat)) exit    ! 读取到 EOF 时，退出循环

            select case (buffer_str(1:6))
            case ("GRID")
                igrid = igrid + 1
                read (buffer_str, "(t9,i8,t25,3g8.1)") self%grid(igrid)
            case ("CQUAD4")
                icard4 = icard4 + 1
                read (buffer_str, "(t9,6i8)") &
                    self%card4(icard4)%id, &
                    self%card4(icard4)%fx, &
                    self%card4(icard4)%grid(1:4)
            case ("CTRIA3")
                icard3 = icard3 + 1
                read (buffer_str, "(t9,5i8)") &
                    self%card3(icard3)%id, &
                    self%card3(icard3)%fx, &
                    self%card3(icard3)%grid(1:3)
            end select
        end do

        close (bdf_unit); deallocate (buffer_str)

    end subroutine bdf_shell_t_read_file

    !> Link process <br>
    !> 链接节点与壳单元
    !> @note 超 20 万网格，可设置 link 并行
    pure subroutine bdf_shell_t_link_process(self)
        class(bdf_shell_t), intent(inout) :: self   !! Patran BDF type <br>
                                                    !! Patran BDF 类型
        integer :: i, j

        do concurrent(i=1:self%grid_num)
            do concurrent(j=1:self%card3_num)
                if (self%grid(i)%id == self%card3(j)%grid(1)) then
                    self%card3(j)%index(1) = i
                else if (self%grid(i)%id == self%card3(j)%grid(2)) then
                    self%card3(j)%index(2) = i
                else if (self%grid(i)%id == self%card3(j)%grid(3)) then
                    self%card3(j)%index(3) = i
                end if
            end do

            do concurrent(j=1:self%card4_num)
                if (self%grid(i)%id == self%card4(j)%grid(1)) then
                    self%card4(j)%index(1) = i
                else if (self%grid(i)%id == self%card4(j)%grid(2)) then
                    self%card4(j)%index(2) = i
                else if (self%grid(i)%id == self%card4(j)%grid(3)) then
                    self%card4(j)%index(3) = i
                else if (self%grid(i)%id == self%card4(j)%grid(4)) then
                    self%card4(j)%index(4) = i
                end if
            end do
        end do

    end subroutine bdf_shell_t_link_process

end module bdf_parser_m
