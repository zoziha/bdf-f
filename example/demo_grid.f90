!> author: 左志华
!> date: 2022-08-03
!>
!> Load the mesh data from a bdf file <br>
!> 读取 bdf 文件中的网格数据
program main

    use geometric_grid_m, only: geometric_grid_t
    implicit none
    type(geometric_grid_t) :: grid
    character(*), parameter :: bdf_file = "data/demo.bdf"
    logical :: is_exist
    real :: t1, t2

    inquire (file=bdf_file, exist=is_exist)
    if (is_exist) then
        open (1, file=bdf_file, status='old')
        call cpu_time(t1)
        call grid%read(1, .true.)
        call cpu_time(t2)
        close (1)
        print '(a,f6.2)', '**read time /s**: ', t2 - t1
    else
        write (*, '(a)') bdf_file//' does not exist'
        stop
    end if

end program main

! Result:
! ## BDF information:
! card3_num: 0
! card4_num: 5393
! all cards: 5393
! grid_num : 5457
! **read time**:   1.08