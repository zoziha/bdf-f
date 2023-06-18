!> 工具集测试
module test_bdf_utils

    use testdrive, only: new_unittest, unittest_type, error_type, check
    use bdf_utils, only: to_string
    use bdf_kinds, only: rk
    implicit none

    private
    public :: collect_utils

contains

    subroutine collect_utils(testsuite)
        type(unittest_type), intent(out), allocatable :: testsuite(:)

        allocate (testsuite, source=[ &
                  new_unittest('to_string', test_to_string) &
                  ])

    end subroutine collect_utils

    subroutine test_to_string(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_string(-123456789123.456_rk), '-1.23+11')

    end subroutine test_to_string

end module test_bdf_utils
