!> 工具集测试
module test_bdf_module

    use testdrive, only: new_unittest, unittest_type, error_type, check
    use bdf_module, only: bdf_file
    use bdf_kinds, only: rk
    implicit none

    private
    public :: collect_module

contains

    subroutine collect_module(testsuite)
        type(unittest_type), intent(out), allocatable :: testsuite(:)

        allocate (testsuite, source=[ &
                  new_unittest('bdf_file', test_bdf_file) &
                  ])

    end subroutine collect_module

    subroutine test_bdf_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(bdf_file) :: bdf
        integer :: istat

#ifdef FPM
        call bdf%read('data/demo.bdf', istat)
#else
        call bdf%read('../data/demo.bdf', istat)
#endif
        call check(error, istat, 0)
        if (allocated(error)) return

        call bdf%init_pressure()
        call bdf%add_pressure(bdf%card3s(1)%id, bdf%card3s(1)%pid, 1.0_rk)
        call check(error, bdf%pload2%len, 1)

    end subroutine test_bdf_file

end module test_bdf_module
