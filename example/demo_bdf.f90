!> author: 左志华
!> date: 2022-07-27
!>
!> Load the data from the bdf file <br>
!> 读取 bdf 文件中的数据
program main

    use bdf_module, only: bdf_file
    implicit none
    type(bdf_file) :: bdf
#ifdef FPM
    character(*), parameter :: file = "data/demo.bdf"
#else
    character(*), parameter :: file = "../../data/demo.bdf"
#endif
    logical :: is_exist
    real :: t1, t2
    integer :: istat

    inquire (file=file, exist=is_exist)

    if (is_exist) then
        call cpu_time(t1)
        call bdf%read(file, istat)
        call cpu_time(t2)
        if (istat /= 0) then
            print *, istat
            write (*, '(a)') 'read error'
            stop
        end if
        print '(a, f6.2)', 'read time /sec: ', t2 - t1
        call bdf%info()

    else
        write (*, '(a)') file//' does not exist'
        stop

    end if

end program main

! Benchmarking:
! card3_num: 80
! card4_num: 174058
! all cards: 174138
! grid_num : 175305
! read time:   2.05
! link time:  87.77
