program shamir
    use functions
    use types
    
    implicit none

    integer, dimension(5), parameter :: FERMAT_PRIMES = [3, 5, 17, 257, 65537]
    character(12), parameter :: EXECUTABLE_FILENAME = 'shamir.exe'
    integer :: N, K, modulus, secretMaxLength, integerBits, i
    character(:), allocatable :: sharesFilename
    integer :: commandLength
    integer, dimension(1) :: checkModulus
    ! character :: c
    character(:), allocatable :: command, secret
    real :: r

    ! Default values to be modified
    K = 3
    N = 8
    modulus = 227
    secretMaxLength = 30
    integerBits = 8 ! Inutile cambiarlo finché non implemento il cambio di kind
    sharesFilename = 'shares.txt'
    secret = ''

    ! Get the command
    call get_command(length=commandLength)
    allocate(character(commandLength) :: command)
    call get_command(command)
    command = command(len(EXECUTABLE_FILENAME)+2:)

    call parseCommand(command, K, N, modulus, secretMaxLength, &
        integerBits, sharesFilename, secret)

    ! DEBUGGGGGG
    ! do i = 1,1000
    !     call random_number(r)
    !     if (int(r*257) <= 0 .or. int(r*257) >= 257) then
    !         print*, int(r*257)
    !     end if
    ! end do

    ! Checks
    if (modulus > 2**integerBits) then
        print*, 'WARNING: modulus too large, cannot assure correct reconstruction.'
    end if

    if (secret == '') then
        print*, 'Loading shares from file ', sharesFilename, '.'
        call recomposition(k, n, modulus, secretMaxLength, integerBits, sharesFilename)
        stop
        ! read(*,*) c
        ! if (c == 'y' .or. c == 'Y') then ! Do the recomposition
        !     call recomposition(k, n, modulus, secretMaxLength, integerBits, sharesFilename)
        !     stop
        ! else
        !     error stop 'Please input a secret to be encrypted.'
        ! end if
    end if

    if (len(secret) > secretMaxLength) then
        error stop 'Secret too long.'
    end if

    checkModulus = findloc(FERMAT_PRIMES, modulus)
    if (checkModulus(1) == 0) then
        print*, 'WARNING: chosen modulus is not a Fermat prime.'
    end if

    ! Decomposition
    call decomposition(K, N, modulus, secretMaxLength, &
        integerBits, sharesFilename, secret)
    

    ! DEBUG

    ! print*, ''
    ! print*, 'Via al DEBUG'
    ! print*, ''

    

end program shamir