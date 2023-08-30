module functions
    use types
    use arithmetics
    use iso_fortran_env, only: iostat_eor, iostat_end
    implicit none
        
    contains

    subroutine parseCommand(command, K, N, modulus, secretMaxLength, &
        numBits, sharesFilename, secret)
        character(*) :: command
        integer :: K, N, i, iostat, index, argStart, argEnd
        integer, dimension(:), allocatable :: optionsIndexes
        integer:: modulus, secretMaxLength, numBits
        character(:), allocatable, optional :: sharesFilename
        character(:), allocatable :: secret

        call findSubstring(command, '--', optionsIndexes)
        do i = 1, size(optionsIndexes)
            index = optionsIndexes(i)+2
            argStart = nextWhitespace(command, index)+1
            argEnd = nextWhitespace(command, argStart)-1
            if (argStart == 0) then
                error stop 'Incomplete option.'
            end if
            if (argEnd == -2) then
                argEnd = len(command)
            end if

            if (command(index:index) == 'k') then
                read(command(argStart:argEnd), '(I8.1)', iostat=iostat) K
                if (iostat /= 0) then
                    error stop 'Failed reading K.'
                end if
            else if (command(index:index) == 'n') then
                read(command(argStart:argEnd), '(I8.1)', iostat=iostat) N
                if (iostat /= 0) then
                    error stop 'Failed reading N.'
                end if
            else if (command(index:index+6) == 'modulus') then
                read(command(argStart:argEnd), '(I8.1)', iostat=iostat) modulus
                if (iostat /= 0) then
                    error stop 'Failed reading secret.'
                end if
            else if (command(index:index+17) == 'secret-max-length') then
                read(command(argStart:argEnd), '(I8.1)', iostat=iostat) secretMaxLength
                if (iostat /= 0) then
                    error stop 'Failed reading secretMaxLength.'
                end if
            else if (command(index:index+12) == 'integer-bits') then
                read(command(argStart:argEnd), '(I8.1)', iostat=iostat) numBits
                if (iostat /= 0) then
                    error stop 'Failed reading numBits.'
                end if
            else if (command(index:index+15) == 'shares-filename') then
                deallocate(sharesFilename)
                allocate(character(argEnd-argStart+1) :: sharesFilename)
                read(command(argStart:argEnd), '(A)', iostat=iostat) sharesFilename
                if (iostat /= 0) then
                    error stop 'Failed reading sharesFilename.'
                end if
            else if (command(index:index+6) == 'secret') then
                deallocate(secret)
                allocate(character(len(command)-argStart+1) :: secret)
                read(command(argStart:), '(A)', iostat=iostat) secret
                if (iostat /= 0) then
                    error stop 'Failed reading secret.'
                end if
                exit
            else
                error stop 'Option not valid.'
            end if
            if (argEnd == len(command)) then
                exit
            end if
        end do

    end subroutine parseCommand


    ! Return the index of the next whitespace starting from index
    ! and -1 if there isn't any
    function nextWhitespace(command, index) result(whitespaceIndex)
        character(*), intent(in) :: command
        integer, intent(in) :: index
        integer :: whitespaceIndex, i
        
        whitespaceIndex = -1
        do i = index, len(command)
            if (command(i:i) == ' ') then
                whitespaceIndex = i
                exit
            end if
        end do

    end function nextWhitespace

    ! Va aggiunto il *0.25 quando regolo bene la storia degli numBits
    function dec2hex(dec, numBits) result(hex)
        integer, intent(in) :: dec, numBits
        character(numBits) :: hex
        character(8) :: c

        write(c,'(I0)') numBits
        write(hex,'(Z' // c // '.' // c // ')') dec

    end function dec2hex

    ! Anche qui
    function hex2dec(hex, numBits) result(dec)
        integer, intent(in) :: numBits
        character(*), intent(in) :: hex
        integer :: dec
        character(8) :: c

        write(c,'(I0)') numBits
        read(hex,'(Z' // c // '.' // c // ')') dec

    end function hex2dec

    ! Adds ' ' to both left and right of the secret, in order to hide its length when crypted
    function spacesPad(str, maxLength) result(newStr)
        character(*), intent(in) :: str
        integer, intent(in) :: maxLength
        integer :: strLength, startIndex
        character(maxLength) :: newStr

        strLength = len(str)
        if (strLength >= maxLength) then
            newStr = str
            return
        end if

        startIndex = int((maxLength-strLength)*0.5)
        
        newStr(1:startIndex-1) = repeat(' ', startIndex-1)
        newStr(startIndex:startIndex+strLength-1) = str
        newStr(startIndex+strLength:maxLength) = repeat(' ', maxLength-strLength-startIndex+1)

    end function spacesPad


    function singleCrypt(dec, N, K, mod) result(crypted)
        integer, intent(in) :: dec, N, K, mod
        type(point), dimension(N) :: crypted
        integer, dimension(K) :: coeffs
        type(polynomial) :: poly
        real :: r
        integer :: i, y, file

        coeffs(1) = dec
        do i = 2, size(coeffs)
            call random_number(r)
            coeffs(i) = int(r*(mod-1)+1)
        end do
        
        poly = polynomial()
        call poly%init(K-1, mod, coeffs)

        do i = 1, N
            y = poly%value(i)
            crypted(i) = point(i,y)
        end do

    end function singleCrypt

    ! Operates the decomposition in N shares
    function shamirDecomposition(K, N, modulus, secretMaxLength, numBits, secret) result(shares)
        character(*), intent(in) :: secret
        integer, intent(in) :: N, K, secretMaxLength, numBits, modulus
        character(secretMaxLength) :: paddedSecret
        character(2*numBits*secretMaxLength), dimension(N) :: shares
        type(point), dimension(N) :: cryptedChar
        integer :: i, j, decChar

        if (N < 3) then
            error stop 'N must be 3 or greater.'
        else if (K < 2) then
            error stop 'K must be 2 or greater.'
        end if

        paddedSecret = spacesPad(secret, secretMaxLength)

        do i = 1, secretMaxLength
            decChar = iachar(paddedSecret(i:i))
            cryptedChar = singleCrypt(decChar, N, K, modulus)
            do j = 1, N
                shares(j)(1+(i-1)*2*numBits:numBits+(i-1)*2*numBits) = &
                    dec2hex(cryptedChar(j)%x, numBits)
                ! print*, dec2hex(cryptedChar(j)%x, numBits) ! DEBUGGGG
                shares(j)(1+numBits+(i-1)*2*numBits:2*numBits+(i-1)*2*numBits) = &
                    dec2hex(cryptedChar(j)%y, numBits)
                ! print*, dec2hex(cryptedChar(j)%y, numBits) ! DEBUGGGG
            end do
        end do
        
    end function shamirDecomposition


    function shamirRecomposition(shares, K, numBits, modulus) result(secret)
        character(*), dimension(:), intent(in) :: shares
        integer, intent(in) :: K, numBits, modulus
        character(:), allocatable :: secret
        integer :: i, shareLength, nPoints, dec
        type(point), dimension(:,:), allocatable :: pointShares

        shareLength = len(shares(1))
        if (mod(shareLength,numBits) /= 0 .or. mod(shareLength/numBits, 2) /= 0) then
            error stop 'Invalid shares.'
        end if
        nPoints = int(shareLength/numBits/2)

        if (size(shares) > K) then
            print*, 'MESSAGE: More shares than necessary. Using only first K ones.'
        else if (size(shares) < K) then
            print*, 'WARNING: Less shares than necessary.'
        end if

        allocate(pointShares(K,nPoints))
        allocate(character(nPoints) :: secret)

        do i = 1, K
            if (len(shares(i)) /= shareLength) then
                error stop 'Shares not equal in length.'
            end if
            ! print*, 'DEBUGGG pre parseShare'
            pointShares(i,:) = parseShare(shares(i), numBits)
            ! print*, 'DEBUGGG post parseShare'
        end do

        do i = 1, nPoints
            dec = lagrangePolynomial(pointShares(:,i), modulus)
            secret(i:i) = achar(dec)
        end do

    end function shamirRecomposition


    function parseShare(share, numBits) result(pointShare)
        character(*), intent(in) :: share
        integer, intent(in) :: numBits
        type(point), dimension(:), allocatable :: pointShare
        integer :: i, shareLength, x, y

        shareLength = len(share)
        allocate(pointShare(int(shareLength/numBits/2)))

        do i = 1, int(shareLength/numBits/2)
            ! print*, 'DEBUGGG parseShare do ', i, ' pre hex2dec 1'
            x = hex2dec(share(1+(i-1)*2*numBits:numBits+(i-1)*2*numBits), numBits)
            ! print*, 'DEBUGGG parseShare do ', i, ' pre hex2dec 2'
            y = hex2dec(share(1+numBits+(i-1)*2*numBits:2*numBits+(i-1)*2*numBits), numBits)
            ! print*, 'DEBUGGG parseShare do ', i, ' post hex2dec'
            pointShare(i) = point(x,y)
        end do

    end function parseShare


    function lagrangePolynomial(points, modulus) result(constant)
        type(point), dimension(:), intent(in) :: points
        integer, intent(in) :: modulus
        integer :: n, i, j, constant, num, den

        n = size(points)
        constant = 0
        do i = 1, n
            num = 1
            den = 1
            do j = 1, n
                if (j /= i) then
                    num = modularMultiplication(num, points(j)%x, modulus)
                    den = modularMultiplication(den, points(j)%x-points(i)%x, modulus)
                end if
            end do
            constant = modularAddition(constant, modularMultiplication(points(i)%y, &
                modularDivision(num, den, modulus), modulus), modulus)
        end do

    end function lagrangePolynomial


    subroutine readWholeLine(file, string, iostat, iomsg, bufferLen)
        integer, intent(in) :: file, bufferLen
        character(*) :: string
        character(bufferLen):: buffer
        character(*) :: iomsg
        integer :: iostat, sizeRead, nextPosition
 
        string = ''
        nextPosition = 1
        iostat = 0
        do while (iostat == 0)
            read (file, '(A)', iostat=iostat, iomsg=iomsg, advance='no', size=sizeRead) buffer
            if (is_iostat_eor(iostat) .or. is_iostat_end(iostat)) then
                string(nextPosition:nextPosition+sizeRead) = buffer(1:sizeRead)
                iostat = 0
                exit
            else if (iostat == 0) then
                string(nextPosition:nextPosition+sizeRead) = buffer(1:sizeRead)
            else
                error stop 'File read error.'
            end if
            nextPosition = nextPosition+sizeRead
        end do

    end subroutine readWholeLine


    function randomChoice(array, n) result(choices)
        integer, intent(in) :: n
        integer, dimension(:), intent(in) :: array
        integer, dimension(n) :: choices
        integer :: i, arraySize
        real :: r

        arraySize = size(array)
        do i = 1, n
            call random_number(r)
            choices(i) = array(1+int(r*arraySize))
        end do

    end function randomChoice


    subroutine findSubstring(string, substring, finalIndexes)
        character(*), intent(in) :: string, substring
        integer :: i, counter, cursor, substringLen
        integer, dimension(len(string)) :: indexes
        integer, dimension(:), allocatable :: finalIndexes

        substringLen = len(substring)
        i = 1
        cursor = 1
        counter = 0
        do while (cursor < len(string))
            i = index(string(cursor:), substring)
            if (i == 0) then
                exit
            end if
            cursor = cursor + i + substringLen - 1
            counter = counter+1
            indexes(counter) = cursor-substringLen
        end do

        allocate(finalIndexes(counter))
        finalIndexes = indexes(1:counter)

    end subroutine findSubstring

    
    subroutine decomposition(k, n, modulus, secretMaxLength, numBits, sharesFilename, secret)
        integer, intent(in) :: k, n, modulus, secretMaxLength, numBits
        character(*), intent(in) :: secret, sharesFilename
        character(2*secretMaxLength*numBits), dimension(n) :: shares
        integer :: fileID, i

        shares = shamirDecomposition(k, n, modulus, secretMaxLength, numBits, secret)

        open(newunit=fileID, status='replace', file=sharesFilename, action='write')
        do i = 1, N
            write(fileID, '(A)') shares(i)
        end do
        close(fileID)

    end subroutine decomposition


    subroutine recomposition(k, n, modulus, secretMaxLength, numBits, sharesFilename)
        integer, intent(in) :: k, n, modulus, secretMaxLength, numBits
        character(*), intent(in) :: sharesFilename
        character(2*secretMaxLength*numBits), dimension(n) :: recoveredShares
        character(secretMaxLength) :: recoveredSecret
        character(256) :: iomsg
        integer :: fileID, iostat, i

        open(newunit=fileID, status='old', file=sharesFilename, action='read')
        i = 1
        iostat = 0
        do while (iostat == 0 .and. i <= n)
            call readWholeLine(fileID, recoveredShares(i), iostat, iomsg, bufferLen=71)
            i = i+1
        end do
        close(fileID)
        if (iostat /= 0) then
            print*, 'ERROR READING SHARES: ', iomsg
            error stop
        end if

        if (k > size(recoveredShares)) then
            error stop 'Too few shares: unrecoverable secret.'
        end if
        recoveredSecret = trim(adjustl(shamirRecomposition(recoveredShares, k, numBits, modulus)))

        print*, 'RECOVERED SECRET:'
        print*, ''
        print*, recoveredSecret ! Add end character to avoid whitespaces

    end subroutine recomposition

end module functions