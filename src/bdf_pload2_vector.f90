!> PLOAD2 Vector
module bdf_pload2_vector

    use bdf_kinds, only: rk
    use bdf_keyword, only: pload2
    use bdf_utils, only: to_string
    implicit none

    private
    public :: pload2_vector

    !> PLOAD2 Vector
    type pload2_vector
        private
        integer, public :: len = 0  !! Length of the vector
        type(pload2), allocatable :: items(:)  !! Vector of PLOAD2 items
    contains
        procedure :: init
        procedure :: push
        procedure :: write
        procedure :: clear
        procedure, private :: extend
    end type pload2_vector

contains

    !> Initialize PLOAD2 vector
    pure subroutine init(self, n)
        class(pload2_vector), intent(inout) :: self
        integer, intent(in), optional :: n

        self%len = 0
        if (.not. allocated(self%items)) then
            if (present(n)) then
                allocate (self%items(n))
            else
                allocate (self%items(1000))
            end if
        end if

    end subroutine init

    !> Push PLOAD2 item to the vector
    pure subroutine push(self, item)
        class(pload2_vector), intent(inout) :: self
        type(pload2), intent(in) :: item

        if (self%len == size(self%items)) call self%extend()
        self%len = self%len + 1
        self%items(self%len) = item

    end subroutine push

    !> Write PLOAD2 vector to the BDF file
    subroutine write (self, unit)
        class(pload2_vector), intent(in) :: self
        integer, intent(in) :: unit
        integer :: i

100     format(a6, 2x, i8, a8, i8)

        do i = 1, self%len
            write (unit, 100) 'PLOAD2', self%items(i)%pid, to_string(self%items(i)%pressure), &
                self%items(i)%eid
        end do

    end subroutine write

    !> Clear PLOAD2 vector
    pure subroutine clear(self)
        class(pload2_vector), intent(inout) :: self

        self%len = 0
        if (allocated(self%items)) deallocate (self%items)

    end subroutine clear

    !> Extend PLOAD2 vector
    pure subroutine extend(self)
        class(pload2_vector), intent(inout) :: self
        type(pload2), allocatable :: tmp(:)

        allocate (tmp(size(self%items)))
        self%items = [self%items, tmp]

    end subroutine extend

end module bdf_pload2_vector
