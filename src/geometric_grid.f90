!> author: 左志华
!> date: 2022-08-03
!>
!> 主要用来从 BDF 数据中抽离纯几何网格信息：网格编号、属性编号、对应节点坐标
!> @note 节点的编号将被忽略
module geometric_grid_m

    use, intrinsic :: iso_fortran_env, only: rk => real64
    use bdf_parser_m, only: bdf_shell_t
    implicit none
    private

    public :: geometric_grid_t

    !> Grid type <br>
    !> 节点类型
    type grid_t
        real(rk) :: loc(3)  !! Grid node location <br>
                            !! 网格节点坐标
    end type grid_t

    !> Triangle element type <br>
    !> 三角面元类型
    type card3_t
        integer :: ID           !! Element ID <br>
                                !! 三角面元编号
        integer :: FX           !! Element property ID <br>
                                !! 三角面元属性编号
        type(grid_t) :: grid(3) !! Element grid nodes <br>
                                !! 三角面元节点
    end type card3_t

    !> Quadrangle element type <br>
    !> 四边形面元类型
    type card4_t
        integer :: ID           !! Element ID <br>
                                !! 四边形面元编号
        integer :: FX           !! Element property ID <br>
                                !! 四边形面元属性编号
        type(grid_t) :: grid(4) !! Element grid nodes <br>
                                !! 四边形面元节点
    end type card4_t

    !> Patran BDF geometric grid type <br>
    !> Patran BDF 几何网格类型
    type geometric_grid_t
        integer :: card4_num                    !! Number of quadrangle elements <br>
                                                !! 四边形面元数量
        integer :: card3_num                    !! Number of triangle elements <br>
                                                !! 三角面元数量
        type(card4_t), allocatable :: card4(:)  !! Quadrangle elements <br>
                                                !! 四边形面元
        type(card3_t), allocatable :: card3(:)  !! Triangle elements <br>
                                                !! 三角面元
    contains
        procedure :: read => geometric_grid_t_read
    end type geometric_grid_t

contains

    !> Load mesh data from a BDF file <br>
    !> 读取 BDF 文件中几何网格
    impure subroutine geometric_grid_t_read(self, bdf_unit, info)
        class(geometric_grid_t), intent(out) :: self!! Patran BDF geometric grid type <br>
                                                    !! Patran BDF 几何网格类型
        integer, intent(in) :: bdf_unit             !! BDF file unit <br>
                                                    !! BDF 文件单元
        logical, intent(in), optional :: info       !! print info <br>
                                                    !! 打印信息
        type(bdf_shell_t) :: bdf_shell
        integer :: i, j

        call bdf_shell%read(bdf_unit, info)
        call bdf_shell%link()
        self%card3_num = bdf_shell%card3_num; self%card4_num = bdf_shell%card4_num
        allocate (self%card4(bdf_shell%card4_num), self%card3(bdf_shell%card3_num))

        do concurrent(i=1:self%card3_num)
            self%card3(i)%ID = bdf_shell%card3(i)%ID
            self%card3(i)%FX = bdf_shell%card3(i)%FX
            do concurrent(j=1:3)
                self%card3(i)%grid(j)%loc(:) = bdf_shell%grid(bdf_shell%card3(i)%index(j))%loc(:)
            end do
        end do

        do concurrent(i=1:self%card4_num)
            self%card4(i)%ID = bdf_shell%card4(i)%ID
            self%card4(i)%FX = bdf_shell%card4(i)%FX
            do concurrent(j=1:4)
                self%card4(i)%grid(j)%loc(:) = bdf_shell%grid(bdf_shell%card4(i)%index(j))%loc(:)
            end do
        end do

    end subroutine geometric_grid_t_read

end module geometric_grid_m
