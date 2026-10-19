' VBA has no console stdin, so the tape is read from "input.txt" beside the
' workbook/module and the two output lines go to the Immediate window
' (Debug.Print), which is the closest thing VBA has to stdout.
Sub TuringMachine()
    Dim line As String
    Dim handle As Integer
    handle = FreeFile
    Open "input.txt" For Input As #handle
    Line Input #handle, line
    Close #handle

    If Not IsBinary(line) Then
        Debug.Print "Invalid input: expected a binary string."
        Exit Sub
    End If

    ' The canonical machine: a binary incrementer.  Reading the tape from the
    ' right, every 1 becomes a 0 (carry); the first 0 becomes a 1 and the carry
    ' stops; if the carry runs off the tape a new leading 1 is added.
    Dim tape As String
    tape = line
    Dim index As Long
    index = Len(tape)
    Dim carry As Boolean
    carry = True

    Do While carry And index >= 1
        If Mid(tape, index, 1) = "1" Then
            tape = Left(tape, index - 1) & "0" & Mid(tape, index + 1)
            index = index - 1
        Else
            tape = Left(tape, index - 1) & "1" & Mid(tape, index + 1)
            carry = False
        End If
    Loop

    If carry Then
        tape = "1" & tape
    End If

    Debug.Print "Initial tape: " & line
    Debug.Print "Final tape: " & tape
End Sub

Private Function IsBinary(ByVal value As String) As Boolean
    Dim position As Long
    For position = 1 To Len(value)
        Dim character As String
        character = Mid(value, position, 1)
        If character <> "0" And character <> "1" Then
            IsBinary = False
            Exit Function
        End If
    Next position
    IsBinary = True
End Function
