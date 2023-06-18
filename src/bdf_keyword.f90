!> Keyword
module bdf_keyword

    use bdf_kinds, only: rk
    implicit none

    private
    public :: grid, card3, card4, pload2

    !> 节点类型
    type grid
        integer :: id  !! 节点号
        real(rk) :: loc(3)  !! 坐标
        logical :: is_active = .false.  !! 是否激活
    end type grid

    !> 三角形单元类型
    type card3
        integer :: id  !! 单元号
        integer :: pid  !! 材料号
        integer :: grids(3)  !! 节点号
    end type card3

    !> 四边形单元类型
    type card4
        integer :: id  !! 单元号
        integer :: pid  !! 材料号
        integer :: grids(4)  !! 节点号
    end type card4

    !> PLOAD2 压强类型
    type pload2
        integer :: pid  !! 材料号
        real(rk) :: pressure  !! 压强
        integer :: eid  !! 单元号
    end type pload2

end module bdf_keyword
