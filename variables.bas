Attribute VB_Name = "variables"
Public board(720) As Integer
Public numMines As Integer
Public rows As Integer
Public columns As Integer
Public over As Boolean
Public newGame As Boolean
Sub makeBoard()
    
    Dim count%
    count = 0
    
    Do
        For X = 0 To (rows * columns) - 1
            If (Int(Rnd(Second(Time)) * 10) Mod 16 = 0) Then
                If board(X) <> -1 Then
                    board(X) = -1
                    count = count + 1
                End If
                If count = numMines Then
                    Exit Sub
                End If
            End If
        Next X
                
    Loop While count <= numMines
        
End Sub
