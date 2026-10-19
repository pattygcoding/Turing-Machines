program TuringMachine
    implicit none

    character(len=512) :: line
    character(len=512) :: tape
    integer :: length
    integer :: index
    integer :: head
    integer :: status
    logical :: carry

    line = ' '
    read (*, '(A)', iostat=status) line
    if (status /= 0) line = ' '
    length = len_trim(line)
    if (length > 0) then
        if (line(length:length) == char(13)) length = length - 1
    end if

    do index = 1, length
        if (line(index:index) /= '0' .and. line(index:index) /= '1') then
            print '(A)', 'Invalid input: expected a binary string.'
            stop
        end if
    end do

    ! The canonical machine: a binary incrementer.  Cell 1 is the spare blank
    ! used when the carry runs off the left end of the tape.
    tape = repeat(' ', 512)
    if (length > 0) tape(2:length + 1) = line(1:length)

    carry = .true.
    head = length + 1
    do while (carry .and. head >= 2)
        if (tape(head:head) == '1') then
            tape(head:head) = '0'
            head = head - 1
        else
            tape(head:head) = '1'
            carry = .false.
        end if
    end do

    print '(A,A)', 'Initial tape: ', line(1:length)
    if (carry) then
        tape(1:1) = '1'
        print '(A,A)', 'Final tape: ', tape(1:length + 1)
    else
        print '(A,A)', 'Final tape: ', tape(2:length + 1)
    end if
end program TuringMachine
