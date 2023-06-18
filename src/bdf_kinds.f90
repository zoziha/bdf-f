!> 精度
module bdf_kinds

    implicit none

    private
    public :: rk

    integer, parameter :: rk = kind(0.0)  !! real kind

end module bdf_kinds
