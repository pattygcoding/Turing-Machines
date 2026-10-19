PROGRAM TuringMachine
  IMPLICIT NONE

  CHARACTER(LEN=10) :: tape = "1101      "  ! Initial tape
  INTEGER :: head = 1                       ! Initial head position
  CHARACTER(LEN=2) :: state = "q0"          ! Initial state
  CHARACTER(LEN=1) :: symbol                ! Current symbol
  CHARACTER(LEN=4) :: transitionKey         ! Transition key
  CHARACTER(LEN=2) :: nextState             ! Next state
  CHARACTER(LEN=1) :: writeSymbol           ! Symbol to write
  INTEGER :: direction                      ! Direction to move (-1 for left, +1 for right)

  ! Main loop to run the Turing machine
  DO
    symbol = tape(head:head)

    ! Construct the transition key
    transitionKey = TRIM(ADJUSTL(state)) // TRIM(ADJUSTL(symbol))

    ! Determine the action based on the current state and symbol
    CALL DetermineAction(transitionKey, nextState, writeSymbol, direction)

    ! Check if the action is valid
    IF (TRIM(ADJUSTL(nextState)) == "HALT" .OR. TRIM(ADJUSTL(nextState)) == "") THEN
      PRINT *, "No valid transition found or HALT state reached. Halting."
      EXIT
    END IF

    ! Apply the action
    state = nextState
    tape(head:head) = writeSymbol
    head = head + direction

    ! Print the current state of the tape and head position
    PRINT *, "State: ", TRIM(ADJUSTL(state)), ", Tape: ", TRIM(ADJUSTL(tape)), ", Head: ", head

    ! Halt if the state is HALT
    IF (TRIM(ADJUSTL(state)) == "HALT") EXIT
  END DO

  ! Print the final tape
  PRINT *, "Final Tape: ", TRIM(ADJUSTL(tape))

CONTAINS

  ! Subroutine to determine the action based on the transition key
  SUBROUTINE DetermineAction(transitionKey, nextState, writeSymbol, direction)
    CHARACTER(LEN=4), INTENT(IN) :: transitionKey
    CHARACTER(LEN=2), INTENT(OUT) :: nextState
    CHARACTER(LEN=1), INTENT(OUT) :: writeSymbol
    INTEGER, INTENT(OUT) :: direction

    ! Initialize the output variables
    nextState = "    "
    writeSymbol = " "
    direction = 0

    SELECT CASE (TRIM(ADJUSTL(transitionKey)))
    CASE ("q01")
      nextState = "q1"
      writeSymbol = "1"
      direction = 1
    CASE ("q00")
      nextState = "q2"
      writeSymbol = "1"
      direction = 1
    CASE ("q11")
      nextState = "q0"
      writeSymbol = "1"
      direction = 1
    CASE ("q10")
      nextState = "q1"
      writeSymbol = "1"
      direction = 1
    CASE ("q21")
      nextState = "q2"
      writeSymbol = "1"
      direction = 1
    CASE ("q20")
      nextState = "HALT"
      writeSymbol = "0"
      direction = 0
    END SELECT
  END SUBROUTINE DetermineAction

END PROGRAM TuringMachine
