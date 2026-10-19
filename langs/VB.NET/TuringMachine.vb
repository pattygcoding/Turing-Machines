Imports System
Imports System.Text

Module TuringMachine
    Sub Main()
        Dim line As String = Console.ReadLine()
        If line Is Nothing Then
            line = ""
        End If

        For Each character As Char In line
            If character <> "0"c AndAlso character <> "1"c Then
                Console.WriteLine("Invalid input: expected a binary string.")
                Return
            End If
        Next

        ' The canonical machine: a binary incrementer.  In q0 a 1 is rewritten
        ' to 0 and the head moves left (carry); a 0 becomes 1 and the machine
        ' halts; a blank becomes 1 (a new leading digit) and the machine halts.
        Dim tape As New StringBuilder(line)
        Dim head As Integer = tape.Length - 1
        Do
            Dim symbol As Char = If(head >= 0, tape(head), "_"c)
            If symbol = "1"c Then
                tape(head) = "0"c
                head -= 1
            ElseIf symbol = "0"c Then
                tape(head) = "1"c
                Exit Do
            Else
                tape.Insert(0, "1"c)
                Exit Do
            End If
        Loop

        Console.WriteLine("Initial tape: " & line)
        Console.WriteLine("Final tape: " & tape.ToString())
    End Sub
End Module
