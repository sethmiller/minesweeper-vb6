VERSION 5.00
Begin VB.Form Dialog 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Custom Field"
   ClientHeight    =   2070
   ClientLeft      =   2760
   ClientTop       =   3750
   ClientWidth     =   2925
   BeginProperty Font 
      Name            =   "Palatino Linotype"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2070
   ScaleWidth      =   2925
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin VB.TextBox txtMines 
      Height          =   285
      Left            =   960
      TabIndex        =   4
      Top             =   1200
      Width           =   615
   End
   Begin VB.TextBox txtWidth 
      Height          =   285
      Left            =   960
      TabIndex        =   3
      Top             =   840
      Width           =   615
   End
   Begin VB.TextBox txtHeight 
      Height          =   285
      Left            =   960
      TabIndex        =   2
      Top             =   480
      Width           =   615
   End
   Begin VB.CommandButton CancelButton 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   1800
      TabIndex        =   1
      Top             =   1080
      Width           =   855
   End
   Begin VB.CommandButton OKButton 
      Caption         =   "OK"
      Height          =   375
      Left            =   1800
      TabIndex        =   0
      Top             =   480
      Width           =   855
   End
   Begin VB.Label lblMines 
      Caption         =   "Mines:"
      Height          =   255
      Left            =   240
      TabIndex        =   7
      Top             =   1200
      Width           =   615
   End
   Begin VB.Label lblWidth 
      Caption         =   "Width:"
      Height          =   255
      Left            =   240
      TabIndex        =   6
      Top             =   840
      Width           =   615
   End
   Begin VB.Label lblHeight 
      Caption         =   "Height:"
      Height          =   255
      Left            =   240
      TabIndex        =   5
      Top             =   480
      Width           =   615
   End
End
Attribute VB_Name = "Dialog"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
Dim minWidth As Integer
Dim maxWidth As Integer
Dim minHeight As Integer
Dim maxHeight As Integer

Private Sub CancelButton_Click()
            
    Dialog.Visible = False
    Unload Dialog
    Game.Enabled = True
    Game.SetFocus
    
End Sub
Private Sub Form_Load()

    txtHeight.Text = rows
    txtWidth.Text = columns
    txtMines.Text = numMines
    
    minWidth = 9
    maxWidth = 24
    minHeight = 9
    maxHeight = 30

End Sub

Private Sub Form_Unload(Cancel As Integer)
                
    Dialog.Visible = False
    Unload Dialog
    Game.Enabled = True
    Game.SetFocus
        
End Sub

Private Sub OKButton_Click()
    
    Dim minMines%
    Dim maxMines%

    If txtHeight.Text >= minHeight And txtHeight.Text <= maxWidth Then
        rows = txtHeight.Text
    ElseIf txtHeight.Text < minHeight Then
        rows = minHeight
    ElseIf txtHeight.Text > maxWidth Then
        rows = maxWidth
    End If
     
    If txtWidth.Text >= minWidth And txtWidth.Text <= 30 Then
        columns = txtWidth.Text
    ElseIf txtWidth.Text < minWidth Then
        columns = minWidth
    ElseIf txtWidth.Text > maxHeight Then
        columns = maxHeight
    End If
    
    maxMines = Int(0.5 * rows * columns)
    
    If Int(0.15 * rows * columns) < 10 Then
        minMines = 10
    ElseIf Int(0.15 * rows * columns) > 10 Then
        minMines = Int(0.15 * rows * columns)
    End If
        
    If txtMines.Text < maxMines And txtMines.Text > minMines Then
        numMines = txtMines.Text
    ElseIf txtMines.Text >= maxMines Then
        numMines = maxMines
    ElseIf txtMines.Text <= minMines Then
        numMines = minMines
    End If
    
    Game.start
    Dialog.Visible = False
    Unload Dialog
    Game.Enabled = True
    Game.SetFocus
    
End Sub
