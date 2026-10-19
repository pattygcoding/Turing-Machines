       IDENTIFICATION DIVISION.
       PROGRAM-ID. TuringMachine.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TAPE.
           05 TAPE-ENTRY PIC X(10) VALUE '1101      '.
       01 HEAD            PIC 9(2) VALUE 1.
       01 STATE           PIC X(2) VALUE 'q0'.
       01 SYMBOL          PIC X(1).
       01 TRANSITION-KEY  PIC X(4).
       01 NEXT-STATE      PIC X(2).
       01 WRITE-SYMBOL    PIC X(1).
       01 DIRECTION       PIC S9(2).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM UNTIL STATE = 'HALT'
               MOVE TAPE-ENTRY(HEAD:1) TO SYMBOL
               MOVE STATE TO TRANSITION-KEY(1:2)
               MOVE SYMBOL TO TRANSITION-KEY(3:1)
               PERFORM DETERMINE-ACTION
               IF NEXT-STATE = '    '
                   DISPLAY 'No valid transition found. Halting.'
                   MOVE 'HALT' TO STATE
               ELSE
                   MOVE NEXT-STATE TO STATE
                   MOVE WRITE-SYMBOL TO TAPE-ENTRY(HEAD:1)
                   ADD DIRECTION TO HEAD
                   DISPLAY 'State: ' STATE ', Tape: ' TAPE-ENTRY ', Head: ' HEAD
               END-IF
           END-PERFORM
           DISPLAY 'Final Tape: ' TAPE-ENTRY
           STOP RUN.

       DETERMINE-ACTION.
           EVALUATE TRANSITION-KEY
               WHEN 'q0' '1'
                   MOVE 'q1' TO NEXT-STATE
                   MOVE '1' TO WRITE-SYMBOL
                   MOVE 1 TO DIRECTION
               WHEN 'q0' '0'
                   MOVE 'q2' TO NEXT-STATE
                   MOVE '1' TO WRITE-SYMBOL
                   MOVE 1 TO DIRECTION
               WHEN 'q1' '1'
                   MOVE 'q0' TO NEXT-STATE
                   MOVE '1' TO WRITE-SYMBOL
                   MOVE 1 TO DIRECTION
               WHEN 'q1' '0'
                   MOVE 'q1' TO NEXT-STATE
                   MOVE '1' TO WRITE-SYMBOL
                   MOVE 1 TO DIRECTION
               WHEN 'q2' '1'
                   MOVE 'q2' TO NEXT-STATE
                   MOVE '1' TO WRITE-SYMBOL
                   MOVE 1 TO DIRECTION
               WHEN 'q2' '0'
                   MOVE 'HALT' TO NEXT-STATE
                   MOVE '0' TO WRITE-SYMBOL
                   MOVE 0 TO DIRECTION
               WHEN OTHER
                   MOVE '    ' TO NEXT-STATE
                   MOVE ' ' TO WRITE-SYMBOL
                   MOVE 0 TO DIRECTION
           END-EVALUATE.
       END PROGRAM TuringMachine.
