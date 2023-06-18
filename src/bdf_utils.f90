!> Utils
module bdf_utils

    use bdf_kinds, only: rk
    implicit none

    private
    public :: to_string

contains

    !> Convert a real number to a string
    pure function to_string(re) result(str)
        real(rk), intent(in) :: re
        character(len=8) :: str
        character(10) :: tem_char

        write (tem_char, '(es10.3)') re

        if (tem_char(9:9) == '0') then
            str(1:8) = tem_char(1:6)//tem_char(8:8)//tem_char(10:10)
        else
            str(1:8) = tem_char(1:5)//tem_char(8:10)
        end if

    end function to_string

end module bdf_utils

