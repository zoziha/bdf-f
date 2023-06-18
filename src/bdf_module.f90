!> MSC.Nastran BDF file IO
module bdf_module

    use bdf_kinds, only: rk
    use bdf_keyword, only: grid, card3, card4, pload2
    use bdf_pload2_vector, only: pload2_vector
    implicit none

    private
    public :: rk, bdf_file

    !> bdf文件类型
    type bdf_file
        integer :: ngrid  !! 节点数
        integer :: ncard3  !! 三角形单元数
        integer :: ncard4  !! 四边形单元数
        type(grid), allocatable :: grids(:)  !! 节点数组
        type(card3), allocatable :: card3s(:)  !! 三角形单元数组
        type(card4), allocatable :: card4s(:)  !! 四边形单元数组
        type(pload2_vector) :: pload2 !! PLOAD2 压强数组
    contains
        procedure :: read => read_bdf_file
        procedure :: write => write_bdf_file
        procedure :: init_pressure, add_pressure
        procedure :: info
        procedure :: clear
    end type bdf_file

contains

    !> 析构
    subroutine clear(self)
        class(bdf_file), intent(inout) :: self

        if (allocated(self%grids)) deallocate (self%grids)
        if (allocated(self%card3s)) deallocate (self%card3s)
        if (allocated(self%card4s)) deallocate (self%card4s)
        call self%pload2%clear()

        self%ngrid = 0
        self%ncard3 = 0
        self%ncard4 = 0

    end subroutine clear

    !> 读取bdf文件
    subroutine read_bdf_file(self, filename, istat)
        class(bdf_file), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(out) :: istat  !! 0: 成功, -2: 文件格式错误, 其它: 其它错误
        integer :: unit, igrid, icard3, icard4, gridid
        character(:), allocatable :: line

        call self%clear()

        open (newunit=unit, file=filename, status='old', action='read', iostat=istat)
        if (istat /= 0) return

        igrid = 0; icard3 = 0; icard4 = 0
        allocate (character(16) :: line)
        do
            read (unit, '(16a)', iostat=istat) line
            if (is_iostat_end(istat)) exit

            select case (line(1:6))
            case ('GRID*')
                istat = -2
                return
            case ('GRID')  ! 获得最大节点号
                read (line, "(t9,i8)") gridid
                igrid = max(igrid, gridid)
            case ('CTRIA3')
                icard3 = icard3 + 1
            case ('CQUAD4')
                icard4 = icard4 + 1
            end select

        end do

        self%ngrid = igrid
        self%ncard3 = icard3
        self%ncard4 = icard4
        allocate (self%grids(igrid), self%card3s(icard3), self%card4s(icard4), stat=istat)
        if (istat /= 0) return

        rewind (unit)
        deallocate (line)
        allocate (character(80) :: line)
        igrid = 0; icard3 = 0; icard4 = 0

        do
            read (unit, '(80a)', iostat=istat) line
            if (is_iostat_end(istat)) exit

            select case (line(1:6))
            case ('GRID')  ! 读取节点, 并激活
                read (line, "(t9,i8)") gridid
                self%grids(gridid)%id = gridid
                self%grids(gridid)%is_active = .true.
                read (line, "(t25,3g8.1)") self%grids(gridid)%loc
            case ('CTRIA3')
                icard3 = icard3 + 1
                read (line, "(t9,5i8)") self%card3s(icard3)
            case ('CQUAD4')
                icard4 = icard4 + 1
                read (line, "(t9,6i8)") self%card4s(icard4)
            end select

        end do

        close (unit)
        istat = 0

    end subroutine read_bdf_file

    !> 初始化压强
    subroutine init_pressure(self)
        class(bdf_file), intent(inout) :: self

        call self%pload2%init()

    end subroutine init_pressure

    !> 写入压强
    subroutine add_pressure(self, eid, pid, pressure)
        class(bdf_file), intent(inout) :: self
        integer, intent(in) :: eid, pid
        real(rk), intent(in) :: pressure

        call self%pload2%push(pload2(pid, pressure, eid))

    end subroutine add_pressure

    !> 写入bdf文件
    subroutine write_bdf_file(self, filename, header, istat)
        class(bdf_file), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: header
        integer, intent(out) :: istat  !! 0: 成功, 其它: 其它错误
        integer :: unit

        open (newunit=unit, file=filename, status='replace', action='write', iostat=istat)
        if (istat /= 0) return

        if (present(header)) then
            write (unit, "(a)") header
        end if

        call self%pload2%write(unit)

        write (unit, "(a)") "ENDDATA"
        close (unit)

    end subroutine write_bdf_file

    !> 输出信息
    subroutine info(self)
        class(bdf_file), intent(in) :: self

100     format(a, t16, ': ', g0.4)
        print "(a)", "# BDF file info #"
        print 100, "ngrid", self%ngrid
        print 100, "ncard3", self%ncard3
        print 100, "ncard4", self%ncard4
        print 100, "npload2", self%pload2%len

    end subroutine info

end module bdf_module
