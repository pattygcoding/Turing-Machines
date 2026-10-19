Sub TuringMachine()
    Dim tape() As String
    tape = Split("1 1 0 1", " ") ' Initial input on the tape
    Dim head As Integer
    head = 1                    ' Initial head position
    Dim state As String
    state = "q0"                ' Initial state

    ' Define the transition table as a dictionary
    Dim transitions As Object
    Set transitions = CreateObject("Scripting.Dictionary")

    ' Adding transitions to the dictionary
    transitions.Add "q0_1", Array("q1", "1", 1)
    transitions.Add "q0_0", Array("q2", "1", 1)
    transitions.Add "q1_1", Array("q0", "1", 1)
    transitions.Add "q1_0", Array("q1", "1", 1)
    transitions.Add "q2_1", Array("q2", "1", 1)
    transitions.Add "q2_0", Array("HALT", "0", 0)

    Do While True
        ' Get the current symbol on the tape
        Dim symbol As String
        If head <= UBound(tape) + 1 Then
            symbol = tape(head - 1)
        Else
            symbol = "0"
        End If

        ' Create the key for the transition lookup
        Dim key As String
        key = state & "_" & symbol

        ' Determine the action based on the current state and symbol
        If Not transitions.Exists(key) Then
            Debug.Print "No valid transition found. Halting."
            Exit Do
        End If

        Dim action As Variant
        action = transitions(key)

        ' Apply the action
        state = action(0)
        If head <= UBound(tape) + 1 Then
            tape(head - 1) = action(1)
        Else
            ReDim Preserve tape(head - 1)
            tape(head - 1) = action(1)
        End If
        head = head + action(2)

        ' Output the current state of the tape and head position
        Debug.Print "State: " & state & ", Tape: " & Join(tape, "") & ", Head: " & head

        ' Halt if we reach the HALT state
        If state = "HALT" Then Exit Do
    Loop

    ' Output the final tape
    Debug.Print "Final Tape: " & Join(tape, "")
End Sub
