!> author: 左志华
!> date: 2022-07-27
program main

    use bdf_parser_m, only: bdf_shell_t
    implicit none
    type(bdf_shell_t) :: bdf_2d
    logical :: is_exist
    real :: t1, t2
    inquire (file='data/demo.bdf', exist=is_exist)

    if (is_exist) then
        call cpu_time(t1)
        call bdf_2d%read('data/demo.bdf', .true.)
        call cpu_time(t2)
        print '(a, f6.2)', 'read time: ', t2 - t1
        call bdf_2d%link()
        call cpu_time(t1)
        print '(a, f6.2)', 'link time: ', t1 - t2
    else
        write (*, '(a)') 'data/demo.bdf does not exist'
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
