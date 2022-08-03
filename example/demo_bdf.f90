!> author: 左志华
!> date: 2022-07-27
!>
!> Load the data from the bdf file <br>
!> 读取 bdf 文件中的数据
program main

    use bdf_parser_m, only: bdf_shell_t
    implicit none
    type(bdf_shell_t) :: bdf
    character(*), parameter :: bdf_file = "data/demo.bdf"
    logical :: is_exist
    real :: t1, t2

    inquire (file=bdf_file, exist=is_exist)
    if (is_exist) then
        open (1, file=bdf_file, status='old')
        call cpu_time(t1)
        call bdf%read(1, .true.)
        call cpu_time(t2)
        close (1)
        print '(a, f6.2)', '**read time /s**: ', t2 - t1
        call bdf%link()
        call cpu_time(t1)
        print '(a, f6.2)', '**link time /s**: ', t1 - t2
    else
        write (*, '(a)') bdf_file//' does not exist'
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
