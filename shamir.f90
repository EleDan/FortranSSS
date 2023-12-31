program shamir
    use functions
    use types
    
    implicit none

    integer, dimension(5), parameter :: FERMAT_PRIMES = [3, 5, 17, 257, 65537]
    character(12), parameter :: EXECUTABLE_FILENAME = 'shamir.exe'
    integer :: N, K, modulus, secretMaxLength, numBits
    character(:), allocatable :: sharesFilename
    integer :: commandLength
    integer, dimension(1) :: checkModulus
    character(:), allocatable :: command, secret

    ! DEBUGGGG

    ! stop
    ! END DEBUGGGG



    ! Default values to be modified
    K = 5
    N = 8
    modulus = 257
    secretMaxLength = 200
    sharesFilename = 'shares.txt'
    secret = ''

    ! Get the command
    call get_command(length=commandLength)
    allocate(character(commandLength) :: command)
    call get_command(command)
    command = command(len(EXECUTABLE_FILENAME)+2:)

    call parseCommand(command, K, N, modulus, secretMaxLength, &
        sharesFilename, secret)

    numBits = pow2by4(modulus)

    ! Checks on numBits
    if (numBits >= 32) then
        print*, 'WARNING: integers biggers than 32 bits are not yet implemented.'
    end if

    ! Checks on modulus
    checkModulus = findloc(FERMAT_PRIMES, modulus)
    if (checkModulus(1) == 0) then
        print*, 'WARNING: chosen modulus is not a Fermat prime.'
    end if
    if (modulus > 2**numBits) then
        print*, 'WARNING: modulus too large, cannot assure correct reconstruction.'
    end if

    ! Recomposition
    if (secret == '') then
        print*, 'Loading shares from file ', sharesFilename, '.'
        call recomposition(k, n, modulus, secretMaxLength, numBits/4, sharesFilename)
        stop
    end if

    ! Check on secret
    if (len(secret) > secretMaxLength) then
        error stop 'Secret too long.'
    end if

    ! Decomposition
    call decomposition(K, N, modulus, secretMaxLength, &
        numBits/4, sharesFilename, secret)
    

end program shamir