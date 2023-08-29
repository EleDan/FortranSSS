module types
    use arithmetics

    implicit none

    type :: polynomial
        integer, allocatable :: degree, modulus
        integer, dimension(:), allocatable :: coefficients
        contains
            procedure :: init, value
    end type

    type :: point
        integer :: x, y
        contains
            procedure :: print, write
    end type
                
    contains

    subroutine init(this, degree, modulus, coefficients)
        class(polynomial), intent(inout) :: this
        integer, intent(in) :: degree, modulus
        integer, dimension(:), intent(in) :: coefficients

        if (size(coefficients) /= degree+1) then
            error stop 'Not enough/too many coefficients.'
        end if

        this%degree = degree
        this%modulus = modulus
        allocate(this%coefficients(size(coefficients)))
        this%coefficients = coefficients

    end subroutine init

    function value(this, x) result(y)
        class(polynomial), intent(in) :: this
        integer, intent(in) :: x
        integer :: i, y

        y = 0
        do i = 0, this%degree
            y = modularAddition(y, modularMultiplication(this%coefficients(i+1), &
                modularPower(x, i, this%modulus), this%modulus), this%modulus)
        end do

    end function value

    subroutine print(this)
        class(point), intent(in) :: this
        
        print*, this%x, ',', this%y

    end subroutine print

    subroutine write(this, file)
        class(point), intent(in) :: this
        integer, intent(in) :: file

        write(file,*) this%x, ',', this%y

    end subroutine write

end module types