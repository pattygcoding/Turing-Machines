Module TuringMachine

    Sub Main()
        Dim tape As Char() = {"1"c, "1"c, "0"c, "1"c}  ' Initial input on the tape
        Dim head As Integer = 0                        ' Initial head position
        Dim state As String = "q0"                     ' Initial state

        ' Function to determine the action based on the current state and symbol
        While state <> "HALT"
            Dim symbol As Char = If(head < tape.Length, tape(head), "0"c)

            Dim newState As String = ""
            Dim writeSymbol As Char = ""
            Dim direction As Integer = 0

            Select Case state & "_" & symbol
                Case "q0_1"
                    newState = "q1"
                    writeSymbol = "1"c
                    direction = 1
                Case "q0_0"
                    newState = "q2"
                    writeSymbol = "1"c
                    direction = 1
                Case "q1_1"
                    newState = "q0"
                    writeSymbol = "1"c
                    direction = 1
                Case "q1_0"
                    newState = "q1"
                    writeSymbol = "1"c
                    direction = 1
                Case "q2_1"
                    newState = "q2"
                    writeSymbol = "1"c
                    direction = 1
                Case "q2_0"
                    newState = "HALT"
                    writeSymbol = "0"c
                    direction = 0
                Case Else
                    newState = "HALT"
                    writeSymbol = "0"c
                    direction = 0
            End Select

            state = newState
            If head < tape.Length Then
                tape(head) = writeSymbol
            Else
                Array.Resize(tape, tape.Length + 1)
                tape(tape.Length - 1) = writeSymbol
            End If
            head += direction

            ' Output the current state of the tape and head position
            Console.WriteLine($"State: {state}, Tape: {New String(tape)}, Head: {head}")
        End While

        ' Output the final tape
        Console.WriteLine($"Final Tape: {New String(tape)}")
    End Sub

End Module
