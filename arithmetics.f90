module arithmetics
    implicit none

    contains

    ! In Fortran il modulo ha lo stesso segno del dividendo
    ! perciò bisogna usare le proprietà della somma e della
    ! moltiplicazione modulari per ovviare a questo problema
    ! e rendere tutti il risultato positivo.

    function modularMultiplication(a, b, modulo) result(c)
        integer, intent(in) :: a, b, modulo
        integer :: c

        ! c = mod(a*b+modulo, modulo)
        c = mod(mod(a+modulo, modulo) * mod(b+modulo, modulo) + modulo, modulo)

    end function modularMultiplication


    function modularAddition(a, b, modulo) result(c)
        integer, intent(in) :: a, b, modulo
        integer :: c

        ! c = mod(a+b+modulo, modulo)
        c = mod(mod(a+modulo, modulo) + mod(b+modulo, modulo) + modulo, modulo)

    end function modularAddition


    function modularPower(a, e, modulo) result(c)
        integer, intent(in) :: a, e, modulo
        integer :: c, i

        c = 1
        do i = 1, e
            c = modularMultiplication(c, a, modulo)
        end do

    end function modularPower


    function modularDivision(num, den, p) result(q)
        integer, intent(in) :: num, den, p
        integer :: q, inv
        integer, dimension(2) :: bezout

        bezout = extendedGCD(den, p)
        inv = bezout(1)
        q = modularMultiplication(num, inv, p)
    end function modularDivision


    function numsProduct(nums, except) result(tot)
        integer, intent(in) :: except
        integer, dimension(:) :: nums
        integer :: tot, i

        tot = 1
        do i = 1, size(nums)
            if (i /= except) then
                tot = tot * nums(i)
            end if
        end do
    end function numsProduct


    function log2(x) result(y)
        real, intent(in) :: x
        real :: y

        y = log(x)/log(2.0)

    end function log2


    ! https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
    ! https://en.wikipedia.org/wiki/Shamir%27s_secret_sharing#Python_code
    !
    ! può essere usato per calcolare l'inverso modulare di un intero, ma in questo caso
    ! p, cioè il modulo, deve essere primo
    function extendedGCD(n, p) result(bezout)
        integer, intent(in) :: n, p
        integer :: q, x, y, a, b, tmp, last_x, last_y
        integer, dimension(2) :: bezout

        a = n
        b = p
        x = 0
        last_x = 1
        y = 1
        last_y = 0
        do while (b /= 0)
            q = a / b
            tmp = a
            a = b
            b = mod(tmp,b)
            tmp = x
            x = last_x-q*x
            last_x = tmp
            tmp = y
            y = last_y-q*y
            last_y = tmp
        end do

        bezout = [last_x, last_y]

    end function extendedGCD

end module arithmetics