VERSION 5.00
Begin VB.Form Game 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Minesweeper"
   ClientHeight    =   7635
   ClientLeft      =   45
   ClientTop       =   615
   ClientWidth     =   8565
   BeginProperty Font 
      Name            =   "Palatino Linotype"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "Game.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   7635
   ScaleWidth      =   8565
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton show 
      Height          =   255
      Left            =   0
      TabIndex        =   2
      Top             =   120
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Timer Timer1 
      Interval        =   1000
      Left            =   0
      Top             =   4320
   End
   Begin VB.Image topright 
      Height          =   195
      Left            =   8040
      Picture         =   "Game.frx":030A
      Top             =   840
      Width           =   135
   End
   Begin VB.Image rightPiece 
      Height          =   6080
      Left            =   8040
      Picture         =   "Game.frx":03F4
      Stretch         =   -1  'True
      Top             =   1050
      Width           =   135
   End
   Begin VB.Image bottomright 
      Height          =   135
      Left            =   8040
      Picture         =   "Game.frx":0576
      Top             =   7080
      Width           =   135
   End
   Begin VB.Image bottomPiece 
      Height          =   135
      Left            =   1080
      Picture         =   "Game.frx":0640
      Stretch         =   -1  'True
      Top             =   7080
      Width           =   6980
   End
   Begin VB.Image bottomleft 
      Height          =   135
      Left            =   855
      Picture         =   "Game.frx":0752
      Top             =   7080
      Width           =   195
   End
   Begin VB.Image leftPiece 
      Height          =   480
      Left            =   855
      Picture         =   "Game.frx":081C
      Stretch         =   -1  'True
      Top             =   1200
      Width           =   195
   End
   Begin VB.Image topleft 
      Height          =   195
      Left            =   855
      Picture         =   "Game.frx":099E
      Top             =   840
      Width           =   195
   End
   Begin VB.Image topPiece 
      Height          =   195
      Left            =   1065
      Picture         =   "Game.frx":0A88
      Stretch         =   -1  'True
      Top             =   840
      Width           =   480
   End
   Begin VB.Image Flag 
      DragIcon        =   "Game.frx":0BDA
      DragMode        =   1  'Automatic
      Height          =   225
      Index           =   1
      Left            =   0
      Picture         =   "Game.frx":0EE4
      Tag             =   "flag"
      Top             =   840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   719
      Left            =   7320
      Picture         =   "Game.frx":0FDE
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   718
      Left            =   6600
      Picture         =   "Game.frx":10D8
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   717
      Left            =   7080
      Picture         =   "Game.frx":11D2
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   716
      Left            =   6600
      Picture         =   "Game.frx":12CC
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   715
      Left            =   6840
      Picture         =   "Game.frx":13C6
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   714
      Left            =   6600
      Picture         =   "Game.frx":14C0
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   713
      Left            =   6840
      Picture         =   "Game.frx":15BA
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   712
      Left            =   6600
      Picture         =   "Game.frx":16B4
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   711
      Left            =   2520
      Picture         =   "Game.frx":17AE
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   710
      Left            =   3000
      Picture         =   "Game.frx":18A8
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   709
      Left            =   2280
      Picture         =   "Game.frx":19A2
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   708
      Left            =   6600
      Picture         =   "Game.frx":1A9C
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   707
      Left            =   7800
      Picture         =   "Game.frx":1B96
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   706
      Left            =   6600
      Picture         =   "Game.frx":1C90
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   705
      Left            =   7560
      Picture         =   "Game.frx":1D8A
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   704
      Left            =   6600
      Picture         =   "Game.frx":1E84
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   703
      Left            =   7560
      Picture         =   "Game.frx":1F7E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   702
      Left            =   7320
      Picture         =   "Game.frx":2078
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   701
      Left            =   7560
      Picture         =   "Game.frx":2172
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   700
      Left            =   7320
      Picture         =   "Game.frx":226C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   699
      Left            =   6840
      Picture         =   "Game.frx":2366
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   698
      Left            =   7560
      Picture         =   "Game.frx":2460
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   697
      Left            =   7800
      Picture         =   "Game.frx":255A
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   696
      Left            =   7560
      Picture         =   "Game.frx":2654
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   695
      Left            =   7080
      Picture         =   "Game.frx":274E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   694
      Left            =   6840
      Picture         =   "Game.frx":2848
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   693
      Left            =   7080
      Picture         =   "Game.frx":2942
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   692
      Left            =   6840
      Picture         =   "Game.frx":2A3C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   691
      Left            =   7320
      Picture         =   "Game.frx":2B36
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   690
      Left            =   7800
      Picture         =   "Game.frx":2C30
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   689
      Left            =   7320
      Picture         =   "Game.frx":2D2A
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   688
      Left            =   7080
      Picture         =   "Game.frx":2E24
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   687
      Left            =   4200
      Picture         =   "Game.frx":2F1E
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   686
      Left            =   3960
      Picture         =   "Game.frx":3018
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   685
      Left            =   6360
      Picture         =   "Game.frx":3112
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   684
      Left            =   6120
      Picture         =   "Game.frx":320C
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   683
      Left            =   7560
      Picture         =   "Game.frx":3306
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   682
      Left            =   7320
      Picture         =   "Game.frx":3400
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   681
      Left            =   6840
      Picture         =   "Game.frx":34FA
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   680
      Left            =   3720
      Picture         =   "Game.frx":35F4
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   679
      Left            =   7320
      Picture         =   "Game.frx":36EE
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   678
      Left            =   7080
      Picture         =   "Game.frx":37E8
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   677
      Left            =   4200
      Picture         =   "Game.frx":38E2
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   676
      Left            =   3960
      Picture         =   "Game.frx":39DC
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   675
      Left            =   7080
      Picture         =   "Game.frx":3AD6
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   674
      Left            =   6840
      Picture         =   "Game.frx":3BD0
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   673
      Left            =   3480
      Picture         =   "Game.frx":3CCA
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   672
      Left            =   3240
      Picture         =   "Game.frx":3DC4
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   671
      Left            =   7800
      Picture         =   "Game.frx":3EBE
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   670
      Left            =   7560
      Picture         =   "Game.frx":3FB8
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   669
      Left            =   7800
      Picture         =   "Game.frx":40B2
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   668
      Left            =   7560
      Picture         =   "Game.frx":41AC
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   667
      Left            =   7800
      Picture         =   "Game.frx":42A6
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   666
      Left            =   7560
      Picture         =   "Game.frx":43A0
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   665
      Left            =   7800
      Picture         =   "Game.frx":449A
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   664
      Left            =   7560
      Picture         =   "Game.frx":4594
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   663
      Left            =   7320
      Picture         =   "Game.frx":468E
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   662
      Left            =   7080
      Picture         =   "Game.frx":4788
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   661
      Left            =   7320
      Picture         =   "Game.frx":4882
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   660
      Left            =   7080
      Picture         =   "Game.frx":497C
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   659
      Left            =   7320
      Picture         =   "Game.frx":4A76
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   658
      Left            =   7080
      Picture         =   "Game.frx":4B70
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   657
      Left            =   7320
      Picture         =   "Game.frx":4C6A
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   656
      Left            =   7080
      Picture         =   "Game.frx":4D64
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   655
      Left            =   6840
      Picture         =   "Game.frx":4E5E
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   654
      Left            =   3720
      Picture         =   "Game.frx":4F58
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   653
      Left            =   6840
      Picture         =   "Game.frx":5052
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   652
      Left            =   5880
      Picture         =   "Game.frx":514C
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   651
      Left            =   6840
      Picture         =   "Game.frx":5246
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   650
      Left            =   5880
      Picture         =   "Game.frx":5340
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   649
      Left            =   6840
      Picture         =   "Game.frx":543A
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   648
      Left            =   1560
      Picture         =   "Game.frx":5534
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   647
      Left            =   3480
      Picture         =   "Game.frx":562E
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   646
      Left            =   3240
      Picture         =   "Game.frx":5728
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   645
      Left            =   5640
      Picture         =   "Game.frx":5822
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   644
      Left            =   5400
      Picture         =   "Game.frx":591C
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   643
      Left            =   5640
      Picture         =   "Game.frx":5A16
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   642
      Left            =   5400
      Picture         =   "Game.frx":5B10
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   641
      Left            =   1320
      Picture         =   "Game.frx":5C0A
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   640
      Left            =   1080
      Picture         =   "Game.frx":5D04
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   639
      Left            =   2280
      Picture         =   "Game.frx":5DFE
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   638
      Left            =   7800
      Picture         =   "Game.frx":5EF8
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   637
      Left            =   4680
      Picture         =   "Game.frx":5FF2
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   636
      Left            =   4440
      Picture         =   "Game.frx":60EC
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   635
      Left            =   4680
      Picture         =   "Game.frx":61E6
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   634
      Left            =   4440
      Picture         =   "Game.frx":62E0
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   633
      Left            =   4680
      Picture         =   "Game.frx":63DA
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   632
      Left            =   4440
      Picture         =   "Game.frx":64D4
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   631
      Left            =   7560
      Picture         =   "Game.frx":65CE
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   630
      Left            =   7320
      Picture         =   "Game.frx":66C8
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   629
      Left            =   4200
      Picture         =   "Game.frx":67C2
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   628
      Left            =   3960
      Picture         =   "Game.frx":68BC
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   627
      Left            =   4200
      Picture         =   "Game.frx":69B6
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   626
      Left            =   3960
      Picture         =   "Game.frx":6AB0
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   625
      Left            =   4200
      Picture         =   "Game.frx":6BAA
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   624
      Left            =   3960
      Picture         =   "Game.frx":6CA4
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   623
      Left            =   7080
      Picture         =   "Game.frx":6D9E
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   622
      Left            =   6840
      Picture         =   "Game.frx":6E98
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   621
      Left            =   3720
      Picture         =   "Game.frx":6F92
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   620
      Left            =   3480
      Picture         =   "Game.frx":708C
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   619
      Left            =   3720
      Picture         =   "Game.frx":7186
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   618
      Left            =   3480
      Picture         =   "Game.frx":7280
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   617
      Left            =   3720
      Picture         =   "Game.frx":737A
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   616
      Left            =   3480
      Picture         =   "Game.frx":7474
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   615
      Left            =   2040
      Picture         =   "Game.frx":756E
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   614
      Left            =   1800
      Picture         =   "Game.frx":7668
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   613
      Left            =   3240
      Picture         =   "Game.frx":7762
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   612
      Left            =   3000
      Picture         =   "Game.frx":785C
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   611
      Left            =   3240
      Picture         =   "Game.frx":7956
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   610
      Left            =   3000
      Picture         =   "Game.frx":7A50
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   609
      Left            =   3240
      Picture         =   "Game.frx":7B4A
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   608
      Left            =   3000
      Picture         =   "Game.frx":7C44
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   607
      Left            =   4680
      Picture         =   "Game.frx":7D3E
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   606
      Left            =   4440
      Picture         =   "Game.frx":7E38
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   605
      Left            =   4680
      Picture         =   "Game.frx":7F32
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   604
      Left            =   4440
      Picture         =   "Game.frx":802C
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   603
      Left            =   4680
      Picture         =   "Game.frx":8126
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   602
      Left            =   4440
      Picture         =   "Game.frx":8220
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   601
      Left            =   4680
      Picture         =   "Game.frx":831A
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   600
      Left            =   4440
      Picture         =   "Game.frx":8414
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   599
      Left            =   4200
      Picture         =   "Game.frx":850E
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   598
      Left            =   3960
      Picture         =   "Game.frx":8608
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   597
      Left            =   4200
      Picture         =   "Game.frx":8702
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   596
      Left            =   3960
      Picture         =   "Game.frx":87FC
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   595
      Left            =   4200
      Picture         =   "Game.frx":88F6
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   594
      Left            =   3960
      Picture         =   "Game.frx":89F0
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   593
      Left            =   4200
      Picture         =   "Game.frx":8AEA
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   592
      Left            =   3960
      Picture         =   "Game.frx":8BE4
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   591
      Left            =   3720
      Picture         =   "Game.frx":8CDE
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   590
      Left            =   3480
      Picture         =   "Game.frx":8DD8
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   589
      Left            =   3720
      Picture         =   "Game.frx":8ED2
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   588
      Left            =   3480
      Picture         =   "Game.frx":8FCC
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   587
      Left            =   3720
      Picture         =   "Game.frx":90C6
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   586
      Left            =   3480
      Picture         =   "Game.frx":91C0
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   585
      Left            =   3720
      Picture         =   "Game.frx":92BA
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   584
      Left            =   3480
      Picture         =   "Game.frx":93B4
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   583
      Left            =   3240
      Picture         =   "Game.frx":94AE
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   582
      Left            =   3000
      Picture         =   "Game.frx":95A8
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   581
      Left            =   3240
      Picture         =   "Game.frx":96A2
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   580
      Left            =   3000
      Picture         =   "Game.frx":979C
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   579
      Left            =   3240
      Picture         =   "Game.frx":9896
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   578
      Left            =   3000
      Picture         =   "Game.frx":9990
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   577
      Left            =   3240
      Picture         =   "Game.frx":9A8A
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   576
      Left            =   3000
      Picture         =   "Game.frx":9B84
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   575
      Left            =   7800
      Picture         =   "Game.frx":9C7E
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   574
      Left            =   7560
      Picture         =   "Game.frx":9D78
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   573
      Left            =   2760
      Picture         =   "Game.frx":9E72
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   572
      Left            =   2520
      Picture         =   "Game.frx":9F6C
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   571
      Left            =   2760
      Picture         =   "Game.frx":A066
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   570
      Left            =   2520
      Picture         =   "Game.frx":A160
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   569
      Left            =   2760
      Picture         =   "Game.frx":A25A
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   568
      Left            =   2520
      Picture         =   "Game.frx":A354
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   567
      Left            =   7320
      Picture         =   "Game.frx":A44E
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   566
      Left            =   7080
      Picture         =   "Game.frx":A548
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   565
      Left            =   2280
      Picture         =   "Game.frx":A642
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   564
      Left            =   2040
      Picture         =   "Game.frx":A73C
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   563
      Left            =   2280
      Picture         =   "Game.frx":A836
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   562
      Left            =   2040
      Picture         =   "Game.frx":A930
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   561
      Left            =   2280
      Picture         =   "Game.frx":AA2A
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   560
      Left            =   2040
      Picture         =   "Game.frx":AB24
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   559
      Left            =   6840
      Picture         =   "Game.frx":AC1E
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   558
      Left            =   1560
      Picture         =   "Game.frx":AD18
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   557
      Left            =   1800
      Picture         =   "Game.frx":AE12
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   556
      Left            =   1560
      Picture         =   "Game.frx":AF0C
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   555
      Left            =   1800
      Picture         =   "Game.frx":B006
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   554
      Left            =   1560
      Picture         =   "Game.frx":B100
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   553
      Left            =   1800
      Picture         =   "Game.frx":B1FA
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   552
      Left            =   1560
      Picture         =   "Game.frx":B2F4
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   551
      Left            =   1320
      Picture         =   "Game.frx":B3EE
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   550
      Left            =   1080
      Picture         =   "Game.frx":B4E8
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   549
      Left            =   1320
      Picture         =   "Game.frx":B5E2
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   548
      Left            =   1080
      Picture         =   "Game.frx":B6DC
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   547
      Left            =   1320
      Picture         =   "Game.frx":B7D6
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   546
      Left            =   1080
      Picture         =   "Game.frx":B8D0
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   545
      Left            =   1320
      Picture         =   "Game.frx":B9CA
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   544
      Left            =   1080
      Picture         =   "Game.frx":BAC4
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   543
      Left            =   2760
      Picture         =   "Game.frx":BBBE
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   542
      Left            =   2520
      Picture         =   "Game.frx":BCB8
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   541
      Left            =   2760
      Picture         =   "Game.frx":BDB2
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   540
      Left            =   2520
      Picture         =   "Game.frx":BEAC
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   539
      Left            =   2760
      Picture         =   "Game.frx":BFA6
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   538
      Left            =   2520
      Picture         =   "Game.frx":C0A0
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   537
      Left            =   2760
      Picture         =   "Game.frx":C19A
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   536
      Left            =   2520
      Picture         =   "Game.frx":C294
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   535
      Left            =   2280
      Picture         =   "Game.frx":C38E
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   534
      Left            =   2040
      Picture         =   "Game.frx":C488
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   533
      Left            =   2280
      Picture         =   "Game.frx":C582
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   532
      Left            =   2040
      Picture         =   "Game.frx":C67C
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   531
      Left            =   2280
      Picture         =   "Game.frx":C776
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   530
      Left            =   2040
      Picture         =   "Game.frx":C870
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   529
      Left            =   2280
      Picture         =   "Game.frx":C96A
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   528
      Left            =   2040
      Picture         =   "Game.frx":CA64
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   527
      Left            =   1800
      Picture         =   "Game.frx":CB5E
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   526
      Left            =   1560
      Picture         =   "Game.frx":CC58
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   525
      Left            =   1800
      Picture         =   "Game.frx":CD52
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   524
      Left            =   1560
      Picture         =   "Game.frx":CE4C
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   523
      Left            =   1800
      Picture         =   "Game.frx":CF46
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   522
      Left            =   1560
      Picture         =   "Game.frx":D040
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   521
      Left            =   1800
      Picture         =   "Game.frx":D13A
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   520
      Left            =   1560
      Picture         =   "Game.frx":D234
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   519
      Left            =   1320
      Picture         =   "Game.frx":D32E
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   518
      Left            =   1080
      Picture         =   "Game.frx":D428
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   517
      Left            =   1320
      Picture         =   "Game.frx":D522
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   516
      Left            =   1080
      Picture         =   "Game.frx":D61C
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   515
      Left            =   1320
      Picture         =   "Game.frx":D716
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   514
      Left            =   1080
      Picture         =   "Game.frx":D810
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   513
      Left            =   1320
      Picture         =   "Game.frx":D90A
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   512
      Left            =   1080
      Picture         =   "Game.frx":DA04
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   511
      Left            =   7800
      Picture         =   "Game.frx":DAFE
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   510
      Left            =   7560
      Picture         =   "Game.frx":DBF8
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   509
      Left            =   7800
      Picture         =   "Game.frx":DCF2
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   508
      Left            =   7560
      Picture         =   "Game.frx":DDEC
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   507
      Left            =   6840
      Picture         =   "Game.frx":DEE6
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   506
      Left            =   4680
      Picture         =   "Game.frx":DFE0
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   505
      Left            =   6840
      Picture         =   "Game.frx":E0DA
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   504
      Left            =   4680
      Picture         =   "Game.frx":E1D4
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   503
      Left            =   7320
      Picture         =   "Game.frx":E2CE
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   502
      Left            =   7080
      Picture         =   "Game.frx":E3C8
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   501
      Left            =   7320
      Picture         =   "Game.frx":E4C2
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   500
      Left            =   7080
      Picture         =   "Game.frx":E5BC
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   499
      Left            =   4440
      Picture         =   "Game.frx":E6B6
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   498
      Left            =   7800
      Picture         =   "Game.frx":E7B0
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   497
      Left            =   4440
      Picture         =   "Game.frx":E8AA
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   496
      Left            =   7800
      Picture         =   "Game.frx":E9A4
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   495
      Left            =   6840
      Picture         =   "Game.frx":EA9E
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   494
      Left            =   6360
      Picture         =   "Game.frx":EB98
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   493
      Left            =   6840
      Picture         =   "Game.frx":EC92
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   492
      Left            =   2040
      Picture         =   "Game.frx":ED8C
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   491
      Left            =   6840
      Picture         =   "Game.frx":EE86
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   490
      Left            =   7320
      Picture         =   "Game.frx":EF80
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   489
      Left            =   6840
      Picture         =   "Game.frx":F07A
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   488
      Left            =   7560
      Picture         =   "Game.frx":F174
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   487
      Left            =   6120
      Picture         =   "Game.frx":F26E
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   486
      Left            =   7800
      Picture         =   "Game.frx":F368
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   485
      Left            =   1800
      Picture         =   "Game.frx":F462
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   484
      Left            =   7560
      Picture         =   "Game.frx":F55C
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   483
      Left            =   7080
      Picture         =   "Game.frx":F656
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   482
      Left            =   7800
      Picture         =   "Game.frx":F750
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   481
      Left            =   6600
      Picture         =   "Game.frx":F84A
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   480
      Left            =   7560
      Picture         =   "Game.frx":F944
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   479
      Left            =   7800
      Picture         =   "Game.frx":FA3E
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   478
      Left            =   5160
      Picture         =   "Game.frx":FB38
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   477
      Left            =   4920
      Picture         =   "Game.frx":FC32
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   476
      Left            =   7080
      Picture         =   "Game.frx":FD2C
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   475
      Left            =   5160
      Picture         =   "Game.frx":FE26
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   474
      Left            =   6840
      Picture         =   "Game.frx":FF20
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   473
      Left            =   7560
      Picture         =   "Game.frx":1001A
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   472
      Left            =   3000
      Picture         =   "Game.frx":10114
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   471
      Left            =   4920
      Picture         =   "Game.frx":1020E
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   470
      Left            =   7800
      Picture         =   "Game.frx":10308
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   469
      Left            =   7560
      Picture         =   "Game.frx":10402
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   468
      Left            =   7800
      Picture         =   "Game.frx":104FC
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   467
      Left            =   6600
      Picture         =   "Game.frx":105F6
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   466
      Left            =   7800
      Picture         =   "Game.frx":106F0
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   465
      Left            =   2760
      Picture         =   "Game.frx":107EA
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   464
      Left            =   7800
      Picture         =   "Game.frx":108E4
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   463
      Left            =   7560
      Picture         =   "Game.frx":109DE
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   462
      Left            =   7320
      Picture         =   "Game.frx":10AD8
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   461
      Left            =   7560
      Picture         =   "Game.frx":10BD2
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   460
      Left            =   7320
      Picture         =   "Game.frx":10CCC
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   459
      Left            =   7560
      Picture         =   "Game.frx":10DC6
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   458
      Left            =   7320
      Picture         =   "Game.frx":10EC0
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   457
      Left            =   7560
      Picture         =   "Game.frx":10FBA
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   456
      Left            =   7320
      Picture         =   "Game.frx":110B4
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   455
      Left            =   7080
      Picture         =   "Game.frx":111AE
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   454
      Left            =   7320
      Picture         =   "Game.frx":112A8
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   453
      Left            =   7080
      Picture         =   "Game.frx":113A2
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   452
      Left            =   7080
      Picture         =   "Game.frx":1149C
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   451
      Left            =   7080
      Picture         =   "Game.frx":11596
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   450
      Left            =   6840
      Picture         =   "Game.frx":11690
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   449
      Left            =   7080
      Picture         =   "Game.frx":1178A
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   448
      Left            =   7320
      Picture         =   "Game.frx":11884
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   447
      Left            =   6600
      Picture         =   "Game.frx":1197E
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   446
      Left            =   6360
      Picture         =   "Game.frx":11A78
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   445
      Left            =   6600
      Picture         =   "Game.frx":11B72
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   444
      Left            =   6360
      Picture         =   "Game.frx":11C6C
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   443
      Left            =   6600
      Picture         =   "Game.frx":11D66
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   442
      Left            =   6360
      Picture         =   "Game.frx":11E60
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   441
      Left            =   6600
      Picture         =   "Game.frx":11F5A
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   440
      Left            =   6360
      Picture         =   "Game.frx":12054
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   439
      Left            =   6120
      Picture         =   "Game.frx":1214E
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   438
      Left            =   5880
      Picture         =   "Game.frx":12248
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   437
      Left            =   6120
      Picture         =   "Game.frx":12342
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   436
      Left            =   5880
      Picture         =   "Game.frx":1243C
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   435
      Left            =   6120
      Picture         =   "Game.frx":12536
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   434
      Left            =   5880
      Picture         =   "Game.frx":12630
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   433
      Left            =   6120
      Picture         =   "Game.frx":1272A
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   432
      Left            =   5880
      Picture         =   "Game.frx":12824
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   431
      Left            =   5640
      Picture         =   "Game.frx":1291E
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   430
      Left            =   5400
      Picture         =   "Game.frx":12A18
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   429
      Left            =   5640
      Picture         =   "Game.frx":12B12
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   428
      Left            =   5400
      Picture         =   "Game.frx":12C0C
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   427
      Left            =   5640
      Picture         =   "Game.frx":12D06
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   426
      Left            =   5400
      Picture         =   "Game.frx":12E00
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   425
      Left            =   5640
      Picture         =   "Game.frx":12EFA
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   424
      Left            =   5400
      Picture         =   "Game.frx":12FF4
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   423
      Left            =   5160
      Picture         =   "Game.frx":130EE
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   422
      Left            =   4920
      Picture         =   "Game.frx":131E8
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   421
      Left            =   5160
      Picture         =   "Game.frx":132E2
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   420
      Left            =   4920
      Picture         =   "Game.frx":133DC
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   419
      Left            =   5160
      Picture         =   "Game.frx":134D6
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   418
      Left            =   4920
      Picture         =   "Game.frx":135D0
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   417
      Left            =   5160
      Picture         =   "Game.frx":136CA
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   416
      Left            =   4920
      Picture         =   "Game.frx":137C4
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   415
      Left            =   6600
      Picture         =   "Game.frx":138BE
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   414
      Left            =   6360
      Picture         =   "Game.frx":139B8
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   413
      Left            =   6600
      Picture         =   "Game.frx":13AB2
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   412
      Left            =   6360
      Picture         =   "Game.frx":13BAC
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   411
      Left            =   6600
      Picture         =   "Game.frx":13CA6
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   410
      Left            =   6360
      Picture         =   "Game.frx":13DA0
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   409
      Left            =   6600
      Picture         =   "Game.frx":13E9A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   408
      Left            =   6360
      Picture         =   "Game.frx":13F94
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   407
      Left            =   6120
      Picture         =   "Game.frx":1408E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   406
      Left            =   5880
      Picture         =   "Game.frx":14188
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   405
      Left            =   6120
      Picture         =   "Game.frx":14282
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   404
      Left            =   5880
      Picture         =   "Game.frx":1437C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   403
      Left            =   6120
      Picture         =   "Game.frx":14476
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   402
      Left            =   5880
      Picture         =   "Game.frx":14570
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   401
      Left            =   6120
      Picture         =   "Game.frx":1466A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   400
      Left            =   5880
      Picture         =   "Game.frx":14764
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   399
      Left            =   5640
      Picture         =   "Game.frx":1485E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   398
      Left            =   5400
      Picture         =   "Game.frx":14958
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   397
      Left            =   5640
      Picture         =   "Game.frx":14A52
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   396
      Left            =   5400
      Picture         =   "Game.frx":14B4C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   395
      Left            =   5640
      Picture         =   "Game.frx":14C46
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   394
      Left            =   5400
      Picture         =   "Game.frx":14D40
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   393
      Left            =   5640
      Picture         =   "Game.frx":14E3A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   392
      Left            =   5400
      Picture         =   "Game.frx":14F34
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   391
      Left            =   5160
      Picture         =   "Game.frx":1502E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   390
      Left            =   4920
      Picture         =   "Game.frx":15128
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   389
      Left            =   5160
      Picture         =   "Game.frx":15222
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   388
      Left            =   4920
      Picture         =   "Game.frx":1531C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   387
      Left            =   5160
      Picture         =   "Game.frx":15416
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   386
      Left            =   4920
      Picture         =   "Game.frx":15510
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   385
      Left            =   5160
      Picture         =   "Game.frx":1560A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   384
      Left            =   4920
      Picture         =   "Game.frx":15704
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   383
      Left            =   2760
      Picture         =   "Game.frx":157FE
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   382
      Left            =   7800
      Picture         =   "Game.frx":158F8
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   381
      Left            =   6360
      Picture         =   "Game.frx":159F2
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   380
      Left            =   6120
      Picture         =   "Game.frx":15AEC
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   379
      Left            =   6360
      Picture         =   "Game.frx":15BE6
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   378
      Left            =   6120
      Picture         =   "Game.frx":15CE0
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   377
      Left            =   6360
      Picture         =   "Game.frx":15DDA
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   376
      Left            =   6120
      Picture         =   "Game.frx":15ED4
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   375
      Left            =   7560
      Picture         =   "Game.frx":15FCE
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   374
      Left            =   7320
      Picture         =   "Game.frx":160C8
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   373
      Left            =   5880
      Picture         =   "Game.frx":161C2
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   372
      Left            =   5640
      Picture         =   "Game.frx":162BC
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   371
      Left            =   5880
      Picture         =   "Game.frx":163B6
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   370
      Left            =   5640
      Picture         =   "Game.frx":164B0
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   369
      Left            =   5880
      Picture         =   "Game.frx":165AA
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   368
      Left            =   5640
      Picture         =   "Game.frx":166A4
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   367
      Left            =   7080
      Picture         =   "Game.frx":1679E
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   366
      Left            =   6840
      Picture         =   "Game.frx":16898
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   365
      Left            =   5400
      Picture         =   "Game.frx":16992
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   364
      Left            =   5160
      Picture         =   "Game.frx":16A8C
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   363
      Left            =   5400
      Picture         =   "Game.frx":16B86
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   362
      Left            =   5160
      Picture         =   "Game.frx":16C80
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   361
      Left            =   5400
      Picture         =   "Game.frx":16D7A
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   360
      Left            =   5160
      Picture         =   "Game.frx":16E74
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   359
      Left            =   2520
      Picture         =   "Game.frx":16F6E
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   358
      Left            =   7800
      Picture         =   "Game.frx":17068
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   357
      Left            =   4920
      Picture         =   "Game.frx":17162
      Top             =   6360
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   356
      Left            =   7080
      Picture         =   "Game.frx":1725C
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   355
      Left            =   4920
      Picture         =   "Game.frx":17356
      Top             =   6120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   354
      Left            =   6840
      Picture         =   "Game.frx":17450
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   353
      Left            =   4920
      Picture         =   "Game.frx":1754A
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   352
      Left            =   7080
      Picture         =   "Game.frx":17644
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   351
      Left            =   6360
      Picture         =   "Game.frx":1773E
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   350
      Left            =   6120
      Picture         =   "Game.frx":17838
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   349
      Left            =   6360
      Picture         =   "Game.frx":17932
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   348
      Left            =   6120
      Picture         =   "Game.frx":17A2C
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   347
      Left            =   6360
      Picture         =   "Game.frx":17B26
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   346
      Left            =   6120
      Picture         =   "Game.frx":17C20
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   345
      Left            =   6360
      Picture         =   "Game.frx":17D1A
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   344
      Left            =   6120
      Picture         =   "Game.frx":17E14
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   343
      Left            =   5880
      Picture         =   "Game.frx":17F0E
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   342
      Left            =   5640
      Picture         =   "Game.frx":18008
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   341
      Left            =   5880
      Picture         =   "Game.frx":18102
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   340
      Left            =   5640
      Picture         =   "Game.frx":181FC
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   339
      Left            =   5880
      Picture         =   "Game.frx":182F6
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   338
      Left            =   5640
      Picture         =   "Game.frx":183F0
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   337
      Left            =   5880
      Picture         =   "Game.frx":184EA
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   336
      Left            =   5640
      Picture         =   "Game.frx":185E4
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   335
      Left            =   5400
      Picture         =   "Game.frx":186DE
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   334
      Left            =   5160
      Picture         =   "Game.frx":187D8
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   333
      Left            =   5400
      Picture         =   "Game.frx":188D2
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   332
      Left            =   5160
      Picture         =   "Game.frx":189CC
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   331
      Left            =   5400
      Picture         =   "Game.frx":18AC6
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   330
      Left            =   5160
      Picture         =   "Game.frx":18BC0
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   329
      Left            =   5400
      Picture         =   "Game.frx":18CBA
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   328
      Left            =   5160
      Picture         =   "Game.frx":18DB4
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   327
      Left            =   4920
      Picture         =   "Game.frx":18EAE
      Top             =   5640
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   326
      Left            =   6840
      Picture         =   "Game.frx":18FA8
      Top             =   5880
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   325
      Left            =   4920
      Picture         =   "Game.frx":190A2
      Top             =   5400
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   324
      Left            =   6840
      Picture         =   "Game.frx":1919C
      Top             =   6840
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   323
      Left            =   4920
      Picture         =   "Game.frx":19296
      Top             =   5160
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   322
      Left            =   7320
      Picture         =   "Game.frx":19390
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   321
      Left            =   4920
      Picture         =   "Game.frx":1948A
      Top             =   4920
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   320
      Left            =   7080
      Picture         =   "Game.frx":19584
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   319
      Left            =   6600
      Picture         =   "Game.frx":1967E
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   318
      Left            =   6360
      Picture         =   "Game.frx":19778
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   317
      Left            =   6600
      Picture         =   "Game.frx":19872
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   316
      Left            =   6360
      Picture         =   "Game.frx":1996C
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   315
      Left            =   6600
      Picture         =   "Game.frx":19A66
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   314
      Left            =   6360
      Picture         =   "Game.frx":19B60
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   313
      Left            =   6600
      Picture         =   "Game.frx":19C5A
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   312
      Left            =   6360
      Picture         =   "Game.frx":19D54
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   311
      Left            =   6120
      Picture         =   "Game.frx":19E4E
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   310
      Left            =   5880
      Picture         =   "Game.frx":19F48
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   309
      Left            =   6120
      Picture         =   "Game.frx":1A042
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   308
      Left            =   5880
      Picture         =   "Game.frx":1A13C
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   307
      Left            =   6120
      Picture         =   "Game.frx":1A236
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   306
      Left            =   5880
      Picture         =   "Game.frx":1A330
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   305
      Left            =   6120
      Picture         =   "Game.frx":1A42A
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   304
      Left            =   5880
      Picture         =   "Game.frx":1A524
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   303
      Left            =   5640
      Picture         =   "Game.frx":1A61E
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   302
      Left            =   5400
      Picture         =   "Game.frx":1A718
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   301
      Left            =   5640
      Picture         =   "Game.frx":1A812
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   300
      Left            =   5400
      Picture         =   "Game.frx":1A90C
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   299
      Left            =   5640
      Picture         =   "Game.frx":1AA06
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   298
      Left            =   5400
      Picture         =   "Game.frx":1AB00
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   297
      Left            =   5640
      Picture         =   "Game.frx":1ABFA
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   296
      Left            =   5400
      Picture         =   "Game.frx":1ACF4
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   295
      Left            =   5160
      Picture         =   "Game.frx":1ADEE
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   294
      Left            =   4920
      Picture         =   "Game.frx":1AEE8
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   293
      Left            =   5160
      Picture         =   "Game.frx":1AFE2
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   292
      Left            =   4920
      Picture         =   "Game.frx":1B0DC
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   291
      Left            =   5160
      Picture         =   "Game.frx":1B1D6
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   290
      Left            =   4920
      Picture         =   "Game.frx":1B2D0
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   289
      Left            =   5160
      Picture         =   "Game.frx":1B3CA
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   288
      Left            =   4920
      Picture         =   "Game.frx":1B4C4
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   287
      Left            =   6600
      Picture         =   "Game.frx":1B5BE
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   286
      Left            =   6360
      Picture         =   "Game.frx":1B6B8
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   285
      Left            =   6600
      Picture         =   "Game.frx":1B7B2
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   284
      Left            =   6360
      Picture         =   "Game.frx":1B8AC
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   283
      Left            =   6600
      Picture         =   "Game.frx":1B9A6
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   282
      Left            =   6360
      Picture         =   "Game.frx":1BAA0
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   281
      Left            =   6600
      Picture         =   "Game.frx":1BB9A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   280
      Left            =   6360
      Picture         =   "Game.frx":1BC94
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   279
      Left            =   6120
      Picture         =   "Game.frx":1BD8E
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   278
      Left            =   5880
      Picture         =   "Game.frx":1BE88
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   277
      Left            =   6120
      Picture         =   "Game.frx":1BF82
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   276
      Left            =   5880
      Picture         =   "Game.frx":1C07C
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   275
      Left            =   6120
      Picture         =   "Game.frx":1C176
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   274
      Left            =   5880
      Picture         =   "Game.frx":1C270
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   273
      Left            =   6120
      Picture         =   "Game.frx":1C36A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   272
      Left            =   5880
      Picture         =   "Game.frx":1C464
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   271
      Left            =   5640
      Picture         =   "Game.frx":1C55E
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   270
      Left            =   5400
      Picture         =   "Game.frx":1C658
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   269
      Left            =   5640
      Picture         =   "Game.frx":1C752
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   268
      Left            =   5400
      Picture         =   "Game.frx":1C84C
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   267
      Left            =   5640
      Picture         =   "Game.frx":1C946
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   266
      Left            =   5400
      Picture         =   "Game.frx":1CA40
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   265
      Left            =   5640
      Picture         =   "Game.frx":1CB3A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   264
      Left            =   5400
      Picture         =   "Game.frx":1CC34
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   263
      Left            =   5160
      Picture         =   "Game.frx":1CD2E
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   262
      Left            =   4920
      Picture         =   "Game.frx":1CE28
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   261
      Left            =   5160
      Picture         =   "Game.frx":1CF22
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   260
      Left            =   4920
      Picture         =   "Game.frx":1D01C
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   259
      Left            =   5160
      Picture         =   "Game.frx":1D116
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   258
      Left            =   4920
      Picture         =   "Game.frx":1D210
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   257
      Left            =   5160
      Picture         =   "Game.frx":1D30A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   256
      Left            =   4920
      Picture         =   "Game.frx":1D404
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   255
      Left            =   4680
      Picture         =   "Game.frx":1D4FE
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   254
      Left            =   4440
      Picture         =   "Game.frx":1D5F8
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   253
      Left            =   4680
      Picture         =   "Game.frx":1D6F2
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   252
      Left            =   4440
      Picture         =   "Game.frx":1D7EC
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   251
      Left            =   4680
      Picture         =   "Game.frx":1D8E6
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   250
      Left            =   4440
      Picture         =   "Game.frx":1D9E0
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   249
      Left            =   4680
      Picture         =   "Game.frx":1DADA
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   248
      Left            =   4440
      Picture         =   "Game.frx":1DBD4
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   247
      Left            =   4200
      Picture         =   "Game.frx":1DCCE
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   246
      Left            =   3960
      Picture         =   "Game.frx":1DDC8
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   245
      Left            =   4200
      Picture         =   "Game.frx":1DEC2
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   244
      Left            =   3960
      Picture         =   "Game.frx":1DFBC
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   243
      Left            =   4200
      Picture         =   "Game.frx":1E0B6
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   242
      Left            =   3960
      Picture         =   "Game.frx":1E1B0
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   241
      Left            =   4200
      Picture         =   "Game.frx":1E2AA
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   240
      Left            =   3960
      Picture         =   "Game.frx":1E3A4
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   239
      Left            =   3720
      Picture         =   "Game.frx":1E49E
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   238
      Left            =   3480
      Picture         =   "Game.frx":1E598
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   237
      Left            =   3720
      Picture         =   "Game.frx":1E692
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   236
      Left            =   3480
      Picture         =   "Game.frx":1E78C
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   235
      Left            =   3720
      Picture         =   "Game.frx":1E886
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   234
      Left            =   3480
      Picture         =   "Game.frx":1E980
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   233
      Left            =   3720
      Picture         =   "Game.frx":1EA7A
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   232
      Left            =   3480
      Picture         =   "Game.frx":1EB74
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   231
      Left            =   3240
      Picture         =   "Game.frx":1EC6E
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   230
      Left            =   3000
      Picture         =   "Game.frx":1ED68
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   229
      Left            =   3240
      Picture         =   "Game.frx":1EE62
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   228
      Left            =   3000
      Picture         =   "Game.frx":1EF5C
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   227
      Left            =   3240
      Picture         =   "Game.frx":1F056
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   226
      Left            =   3000
      Picture         =   "Game.frx":1F150
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   225
      Left            =   3240
      Picture         =   "Game.frx":1F24A
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   224
      Left            =   3000
      Picture         =   "Game.frx":1F344
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   223
      Left            =   4680
      Picture         =   "Game.frx":1F43E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   222
      Left            =   4440
      Picture         =   "Game.frx":1F538
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   221
      Left            =   4680
      Picture         =   "Game.frx":1F632
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   220
      Left            =   4440
      Picture         =   "Game.frx":1F72C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   219
      Left            =   4680
      Picture         =   "Game.frx":1F826
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   218
      Left            =   4440
      Picture         =   "Game.frx":1F920
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   217
      Left            =   4680
      Picture         =   "Game.frx":1FA1A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   216
      Left            =   4440
      Picture         =   "Game.frx":1FB14
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   215
      Left            =   4200
      Picture         =   "Game.frx":1FC0E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   214
      Left            =   3960
      Picture         =   "Game.frx":1FD08
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   213
      Left            =   4200
      Picture         =   "Game.frx":1FE02
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   212
      Left            =   3960
      Picture         =   "Game.frx":1FEFC
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   211
      Left            =   4200
      Picture         =   "Game.frx":1FFF6
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   210
      Left            =   3960
      Picture         =   "Game.frx":200F0
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   209
      Left            =   4200
      Picture         =   "Game.frx":201EA
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   208
      Left            =   3960
      Picture         =   "Game.frx":202E4
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   207
      Left            =   3720
      Picture         =   "Game.frx":203DE
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   206
      Left            =   3480
      Picture         =   "Game.frx":204D8
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   205
      Left            =   3720
      Picture         =   "Game.frx":205D2
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   204
      Left            =   3480
      Picture         =   "Game.frx":206CC
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   203
      Left            =   3720
      Picture         =   "Game.frx":207C6
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   202
      Left            =   3480
      Picture         =   "Game.frx":208C0
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   201
      Left            =   3720
      Picture         =   "Game.frx":209BA
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   200
      Left            =   3480
      Picture         =   "Game.frx":20AB4
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   199
      Left            =   3240
      Picture         =   "Game.frx":20BAE
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   198
      Left            =   3000
      Picture         =   "Game.frx":20CA8
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   197
      Left            =   3240
      Picture         =   "Game.frx":20DA2
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   196
      Left            =   3000
      Picture         =   "Game.frx":20E9C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   195
      Left            =   3240
      Picture         =   "Game.frx":20F96
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   194
      Left            =   3000
      Picture         =   "Game.frx":21090
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   193
      Left            =   3240
      Picture         =   "Game.frx":2118A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   192
      Left            =   3000
      Picture         =   "Game.frx":21284
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   191
      Left            =   2760
      Picture         =   "Game.frx":2137E
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   190
      Left            =   2520
      Picture         =   "Game.frx":21478
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   189
      Left            =   2760
      Picture         =   "Game.frx":21572
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   188
      Left            =   2520
      Picture         =   "Game.frx":2166C
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   187
      Left            =   2760
      Picture         =   "Game.frx":21766
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   186
      Left            =   2520
      Picture         =   "Game.frx":21860
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   185
      Left            =   2760
      Picture         =   "Game.frx":2195A
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   184
      Left            =   2520
      Picture         =   "Game.frx":21A54
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   183
      Left            =   2280
      Picture         =   "Game.frx":21B4E
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   182
      Left            =   2040
      Picture         =   "Game.frx":21C48
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   181
      Left            =   2280
      Picture         =   "Game.frx":21D42
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   180
      Left            =   2040
      Picture         =   "Game.frx":21E3C
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   179
      Left            =   2280
      Picture         =   "Game.frx":21F36
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   178
      Left            =   2040
      Picture         =   "Game.frx":22030
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   177
      Left            =   2280
      Picture         =   "Game.frx":2212A
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   176
      Left            =   2040
      Picture         =   "Game.frx":22224
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   175
      Left            =   1800
      Picture         =   "Game.frx":2231E
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   174
      Left            =   1560
      Picture         =   "Game.frx":22418
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   173
      Left            =   1800
      Picture         =   "Game.frx":22512
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   172
      Left            =   1560
      Picture         =   "Game.frx":2260C
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   171
      Left            =   1800
      Picture         =   "Game.frx":22706
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   170
      Left            =   1560
      Picture         =   "Game.frx":22800
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   169
      Left            =   1800
      Picture         =   "Game.frx":228FA
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   168
      Left            =   1560
      Picture         =   "Game.frx":229F4
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   167
      Left            =   1320
      Picture         =   "Game.frx":22AEE
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   166
      Left            =   1080
      Picture         =   "Game.frx":22BE8
      Top             =   4680
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   165
      Left            =   1320
      Picture         =   "Game.frx":22CE2
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   164
      Left            =   1080
      Picture         =   "Game.frx":22DDC
      Top             =   4440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   163
      Left            =   1320
      Picture         =   "Game.frx":22ED6
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   162
      Left            =   1080
      Picture         =   "Game.frx":22FD0
      Top             =   4200
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   161
      Left            =   1320
      Picture         =   "Game.frx":230CA
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   160
      Left            =   1080
      Picture         =   "Game.frx":231C4
      Top             =   3960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   159
      Left            =   2760
      Picture         =   "Game.frx":232BE
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   158
      Left            =   2520
      Picture         =   "Game.frx":233B8
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   157
      Left            =   2760
      Picture         =   "Game.frx":234B2
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   156
      Left            =   2520
      Picture         =   "Game.frx":235AC
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   155
      Left            =   2760
      Picture         =   "Game.frx":236A6
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   154
      Left            =   2520
      Picture         =   "Game.frx":237A0
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   153
      Left            =   2760
      Picture         =   "Game.frx":2389A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   152
      Left            =   2520
      Picture         =   "Game.frx":23994
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   151
      Left            =   2280
      Picture         =   "Game.frx":23A8E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   150
      Left            =   2040
      Picture         =   "Game.frx":23B88
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   149
      Left            =   2280
      Picture         =   "Game.frx":23C82
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   148
      Left            =   2040
      Picture         =   "Game.frx":23D7C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   147
      Left            =   2280
      Picture         =   "Game.frx":23E76
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   146
      Left            =   2040
      Picture         =   "Game.frx":23F70
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   145
      Left            =   2280
      Picture         =   "Game.frx":2406A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   144
      Left            =   2040
      Picture         =   "Game.frx":24164
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   143
      Left            =   1800
      Picture         =   "Game.frx":2425E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   142
      Left            =   1560
      Picture         =   "Game.frx":24358
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   141
      Left            =   1800
      Picture         =   "Game.frx":24452
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   140
      Left            =   1560
      Picture         =   "Game.frx":2454C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   139
      Left            =   1800
      Picture         =   "Game.frx":24646
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   138
      Left            =   1560
      Picture         =   "Game.frx":24740
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   137
      Left            =   1800
      Picture         =   "Game.frx":2483A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   136
      Left            =   1560
      Picture         =   "Game.frx":24934
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   135
      Left            =   1320
      Picture         =   "Game.frx":24A2E
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   134
      Left            =   1080
      Picture         =   "Game.frx":24B28
      Top             =   3720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   133
      Left            =   1320
      Picture         =   "Game.frx":24C22
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   132
      Left            =   1080
      Picture         =   "Game.frx":24D1C
      Top             =   3480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   131
      Left            =   1320
      Picture         =   "Game.frx":24E16
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   130
      Left            =   1080
      Picture         =   "Game.frx":24F10
      Top             =   3240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   129
      Left            =   1320
      Picture         =   "Game.frx":2500A
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   128
      Left            =   1080
      Picture         =   "Game.frx":25104
      Top             =   3000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   127
      Left            =   4680
      Picture         =   "Game.frx":251FE
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   126
      Left            =   4440
      Picture         =   "Game.frx":252F8
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   125
      Left            =   4680
      Picture         =   "Game.frx":253F2
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   124
      Left            =   4440
      Picture         =   "Game.frx":254EC
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   123
      Left            =   4680
      Picture         =   "Game.frx":255E6
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   122
      Left            =   4440
      Picture         =   "Game.frx":256E0
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   121
      Left            =   4680
      Picture         =   "Game.frx":257DA
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   120
      Left            =   4440
      Picture         =   "Game.frx":258D4
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   119
      Left            =   4200
      Picture         =   "Game.frx":259CE
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   118
      Left            =   3960
      Picture         =   "Game.frx":25AC8
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   117
      Left            =   4200
      Picture         =   "Game.frx":25BC2
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   116
      Left            =   3960
      Picture         =   "Game.frx":25CBC
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   115
      Left            =   4200
      Picture         =   "Game.frx":25DB6
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   114
      Left            =   3960
      Picture         =   "Game.frx":25EB0
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   113
      Left            =   4200
      Picture         =   "Game.frx":25FAA
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   112
      Left            =   3960
      Picture         =   "Game.frx":260A4
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   111
      Left            =   3720
      Picture         =   "Game.frx":2619E
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   110
      Left            =   3480
      Picture         =   "Game.frx":26298
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   109
      Left            =   3720
      Picture         =   "Game.frx":26392
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   108
      Left            =   3480
      Picture         =   "Game.frx":2648C
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   107
      Left            =   3720
      Picture         =   "Game.frx":26586
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   106
      Left            =   3480
      Picture         =   "Game.frx":26680
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   105
      Left            =   3720
      Picture         =   "Game.frx":2677A
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   104
      Left            =   3480
      Picture         =   "Game.frx":26874
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   103
      Left            =   3240
      Picture         =   "Game.frx":2696E
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   102
      Left            =   3000
      Picture         =   "Game.frx":26A68
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   101
      Left            =   3240
      Picture         =   "Game.frx":26B62
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   100
      Left            =   3000
      Picture         =   "Game.frx":26C5C
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   99
      Left            =   3240
      Picture         =   "Game.frx":26D56
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   98
      Left            =   3000
      Picture         =   "Game.frx":26E50
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   97
      Left            =   3240
      Picture         =   "Game.frx":26F4A
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   96
      Left            =   3000
      Picture         =   "Game.frx":27044
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   95
      Left            =   4680
      Picture         =   "Game.frx":2713E
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   94
      Left            =   4440
      Picture         =   "Game.frx":27238
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   93
      Left            =   4680
      Picture         =   "Game.frx":27332
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   92
      Left            =   4440
      Picture         =   "Game.frx":2742C
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   91
      Left            =   4680
      Picture         =   "Game.frx":27526
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   90
      Left            =   4440
      Picture         =   "Game.frx":27620
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   89
      Left            =   4680
      Picture         =   "Game.frx":2771A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   88
      Left            =   4440
      Picture         =   "Game.frx":27814
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   87
      Left            =   4200
      Picture         =   "Game.frx":2790E
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   86
      Left            =   3960
      Picture         =   "Game.frx":27A08
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   85
      Left            =   4200
      Picture         =   "Game.frx":27B02
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   84
      Left            =   3960
      Picture         =   "Game.frx":27BFC
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   83
      Left            =   4200
      Picture         =   "Game.frx":27CF6
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   82
      Left            =   3960
      Picture         =   "Game.frx":27DF0
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   81
      Left            =   4200
      Picture         =   "Game.frx":27EEA
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   80
      Left            =   3960
      Picture         =   "Game.frx":27FE4
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   79
      Left            =   3720
      Picture         =   "Game.frx":280DE
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   78
      Left            =   3480
      Picture         =   "Game.frx":281D8
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   77
      Left            =   3720
      Picture         =   "Game.frx":282D2
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   76
      Left            =   3480
      Picture         =   "Game.frx":283CC
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   75
      Left            =   3720
      Picture         =   "Game.frx":284C6
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   74
      Left            =   3480
      Picture         =   "Game.frx":285C0
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   73
      Left            =   3720
      Picture         =   "Game.frx":286BA
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   72
      Left            =   3480
      Picture         =   "Game.frx":287B4
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   71
      Left            =   3240
      Picture         =   "Game.frx":288AE
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   70
      Left            =   3000
      Picture         =   "Game.frx":289A8
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   69
      Left            =   3240
      Picture         =   "Game.frx":28AA2
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   68
      Left            =   3000
      Picture         =   "Game.frx":28B9C
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   67
      Left            =   3240
      Picture         =   "Game.frx":28C96
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   66
      Left            =   3000
      Picture         =   "Game.frx":28D90
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   65
      Left            =   3240
      Picture         =   "Game.frx":28E8A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   64
      Left            =   3000
      Picture         =   "Game.frx":28F84
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   63
      Left            =   2760
      Picture         =   "Game.frx":2907E
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   62
      Left            =   2520
      Picture         =   "Game.frx":29178
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   61
      Left            =   2760
      Picture         =   "Game.frx":29272
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   60
      Left            =   2520
      Picture         =   "Game.frx":2936C
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   59
      Left            =   2760
      Picture         =   "Game.frx":29466
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   58
      Left            =   2520
      Picture         =   "Game.frx":29560
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   57
      Left            =   2760
      Picture         =   "Game.frx":2965A
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   56
      Left            =   2520
      Picture         =   "Game.frx":29754
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   55
      Left            =   2280
      Picture         =   "Game.frx":2984E
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   54
      Left            =   2040
      Picture         =   "Game.frx":29948
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   53
      Left            =   2280
      Picture         =   "Game.frx":29A42
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   52
      Left            =   2040
      Picture         =   "Game.frx":29B3C
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   51
      Left            =   2280
      Picture         =   "Game.frx":29C36
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   50
      Left            =   2040
      Picture         =   "Game.frx":29D30
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   49
      Left            =   2280
      Picture         =   "Game.frx":29E2A
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   48
      Left            =   2040
      Picture         =   "Game.frx":29F24
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   47
      Left            =   1800
      Picture         =   "Game.frx":2A01E
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   46
      Left            =   1560
      Picture         =   "Game.frx":2A118
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   45
      Left            =   1800
      Picture         =   "Game.frx":2A212
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   44
      Left            =   1560
      Picture         =   "Game.frx":2A30C
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   43
      Left            =   1800
      Picture         =   "Game.frx":2A406
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   42
      Left            =   1560
      Picture         =   "Game.frx":2A500
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   41
      Left            =   1800
      Picture         =   "Game.frx":2A5FA
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   40
      Left            =   1560
      Picture         =   "Game.frx":2A6F4
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   39
      Left            =   1320
      Picture         =   "Game.frx":2A7EE
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   38
      Left            =   1080
      Picture         =   "Game.frx":2A8E8
      Top             =   2760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   37
      Left            =   1320
      Picture         =   "Game.frx":2A9E2
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   36
      Left            =   1080
      Picture         =   "Game.frx":2AADC
      Top             =   2520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   35
      Left            =   1320
      Picture         =   "Game.frx":2ABD6
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   34
      Left            =   1080
      Picture         =   "Game.frx":2ACD0
      Top             =   2280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   33
      Left            =   1320
      Picture         =   "Game.frx":2ADCA
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   32
      Left            =   1080
      Picture         =   "Game.frx":2AEC4
      Top             =   2040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   31
      Left            =   2760
      Picture         =   "Game.frx":2AFBE
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   30
      Left            =   2520
      Picture         =   "Game.frx":2B0B8
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   29
      Left            =   2760
      Picture         =   "Game.frx":2B1B2
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   28
      Left            =   2520
      Picture         =   "Game.frx":2B2AC
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   27
      Left            =   2760
      Picture         =   "Game.frx":2B3A6
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   26
      Left            =   2520
      Picture         =   "Game.frx":2B4A0
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   25
      Left            =   2760
      Picture         =   "Game.frx":2B59A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   24
      Left            =   2520
      Picture         =   "Game.frx":2B694
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   23
      Left            =   2280
      Picture         =   "Game.frx":2B78E
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   22
      Left            =   2040
      Picture         =   "Game.frx":2B888
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   21
      Left            =   2280
      Picture         =   "Game.frx":2B982
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   20
      Left            =   2040
      Picture         =   "Game.frx":2BA7C
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   19
      Left            =   2280
      Picture         =   "Game.frx":2BB76
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   18
      Left            =   2040
      Picture         =   "Game.frx":2BC70
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   17
      Left            =   2280
      Picture         =   "Game.frx":2BD6A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   16
      Left            =   2040
      Picture         =   "Game.frx":2BE64
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   15
      Left            =   1800
      Picture         =   "Game.frx":2BF5E
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   14
      Left            =   1560
      Picture         =   "Game.frx":2C058
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   13
      Left            =   1800
      Picture         =   "Game.frx":2C152
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   12
      Left            =   1560
      Picture         =   "Game.frx":2C24C
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   11
      Left            =   1800
      Picture         =   "Game.frx":2C346
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   10
      Left            =   1560
      Picture         =   "Game.frx":2C440
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   9
      Left            =   1800
      Picture         =   "Game.frx":2C53A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   8
      Left            =   1560
      Picture         =   "Game.frx":2C634
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   7
      Left            =   1320
      Picture         =   "Game.frx":2C72E
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   6
      Left            =   1080
      Picture         =   "Game.frx":2C828
      Top             =   1800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   5
      Left            =   1320
      Picture         =   "Game.frx":2C922
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   4
      Left            =   1080
      Picture         =   "Game.frx":2CA1C
      Top             =   1560
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   3
      Left            =   1320
      Picture         =   "Game.frx":2CB16
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   2
      Left            =   1080
      Picture         =   "Game.frx":2CC10
      Top             =   1320
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   1
      Left            =   1320
      Picture         =   "Game.frx":2CD0A
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image imgButton 
      Height          =   225
      Index           =   0
      Left            =   1080
      Picture         =   "Game.frx":2CE04
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Label counter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00000000&
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   13.5
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   375
      Left            =   2640
      TabIndex        =   1
      Top             =   360
      Width           =   735
   End
   Begin VB.Image smilewin 
      Height          =   495
      Left            =   0
      Picture         =   "Game.frx":2CEFE
      Top             =   2880
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label display 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00000000&
      BorderStyle     =   1  'Fixed Single
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   13.5
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   375
      Left            =   4080
      TabIndex        =   0
      Top             =   360
      Width           =   735
   End
   Begin VB.Image nobomb 
      Height          =   225
      Left            =   480
      Picture         =   "Game.frx":2D214
      Top             =   6960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image smile 
      Height          =   495
      Left            =   0
      Picture         =   "Game.frx":2D30E
      Top             =   2880
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Image smilepush 
      Height          =   495
      Left            =   0
      Picture         =   "Game.frx":2D624
      Top             =   2880
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Image uhoh 
      Height          =   495
      Left            =   0
      Picture         =   "Game.frx":2D93A
      Top             =   2880
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Image dead 
      Height          =   495
      Left            =   0
      Picture         =   "Game.frx":2DC50
      Top             =   2880
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Image go 
      Height          =   495
      Left            =   3480
      Picture         =   "Game.frx":2DF66
      Stretch         =   -1  'True
      Top             =   240
      Width           =   495
   End
   Begin VB.Image Flag 
      DragIcon        =   "Game.frx":2E27C
      DragMode        =   1  'Automatic
      Height          =   225
      Index           =   0
      Left            =   0
      Picture         =   "Game.frx":2E586
      Tag             =   "flag"
      Top             =   1080
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image squarenum 
      Height          =   225
      Index           =   8
      Left            =   480
      Picture         =   "Game.frx":2E680
      Top             =   6720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image explode 
      Height          =   225
      Left            =   240
      Picture         =   "Game.frx":2E77A
      Top             =   6960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image bomb 
      Height          =   225
      Left            =   0
      Picture         =   "Game.frx":2E874
      Top             =   6960
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image squarenum 
      Height          =   225
      Index           =   7
      Left            =   480
      Picture         =   "Game.frx":2E96E
      Top             =   6480
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image squarenum 
      Height          =   225
      Index           =   6
      Left            =   480
      Picture         =   "Game.frx":2EA68
      Top             =   6240
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image squarenum 
      Height          =   225
      Index           =   5
      Left            =   480
      Picture         =   "Game.frx":2EB62
      Top             =   6000
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image squarenum 
      Height          =   225
      Index           =   4
      Left            =   480
      Picture         =   "Game.frx":2EC5C
      Top             =   5760
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image squarenum 
      Height          =   225
      Index           =   3
      Left            =   480
      Picture         =   "Game.frx":2ED56
      Top             =   5520
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image squarenum 
      Height          =   225
      Index           =   2
      Left            =   480
      Picture         =   "Game.frx":2EE50
      Top             =   5280
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image squarenum 
      Height          =   225
      Index           =   1
      Left            =   480
      Picture         =   "Game.frx":2EF4A
      Top             =   5040
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image squarenum 
      Height          =   225
      Index           =   0
      Left            =   480
      Picture         =   "Game.frx":2F044
      Top             =   4800
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image square 
      Height          =   225
      Left            =   0
      Picture         =   "Game.frx":2F13E
      Top             =   6720
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Image blank 
      Height          =   225
      Left            =   0
      Picture         =   "Game.frx":2F238
      Top             =   6600
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Menu mnuGame 
      Caption         =   "&Game"
      WindowList      =   -1  'True
      Begin VB.Menu mnuNew 
         Caption         =   "New"
         Shortcut        =   {F2}
      End
      Begin VB.Menu mnuBeginner 
         Caption         =   "&Beginner"
         Checked         =   -1  'True
      End
      Begin VB.Menu mnuInter 
         Caption         =   "&Intermediate"
      End
      Begin VB.Menu mnuExpert 
         Caption         =   "&Expert"
      End
      Begin VB.Menu mnuCustom 
         Caption         =   "&Custom..."
      End
      Begin VB.Menu mnuExit 
         Caption         =   "Exit"
         Shortcut        =   ^{F4}
      End
   End
End
Attribute VB_Name = "Game"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim numflags As Integer
Dim over As Boolean
Dim first As Boolean
Dim timeRunning As Double
Dim realNumMines As Integer

Private Sub minecounter()
    
    Dim tempcount%
    
    tempcount = 0
    
    For X = 0 To (rows * columns)
        If board(X) = -1 Then
            tempcount = tempcount + 1
        End If
    Next X
    
    realNumMines = tempcount
    numflags = tempcount
    display.Caption = numflags
    
End Sub

Private Sub Form_Load()
        
    Randomize
    
    rows = 9
    columns = 9
    
    numflags = 0
    numMines = 10
    over = True
    
    For X = 0 To imgButton.count - 1
        imgButton(X).Picture = square.Picture
        display.Caption = numflags
    Next X
    
    start
    
End Sub

Sub start()
    
    setpieces
    Cls
    first = True
       
    go.Picture = smile.Picture
       
    For X = 0 To imgButton.count - 1
        board(X) = 0
        imgButton(X).Picture = square.Picture
    Next X
    
    numflags = 0
    timeRunning = 0
    
    counter.Caption = timeRunning
    display.Caption = numflags
    makeBoard
    numbersquares
    minecounter
    'showall
    

End Sub
Sub setpieces()

    For X = 0 To imgButton.count - 1
        imgButton(X).Visible = False
    Next X
    
    Game.Width = 460 + (240 * columns)
    Game.Height = 1700 + (240 * rows)
    go.Top = 625 - (go.Height)
    go.Left = (210 + (120 * columns) - (go.Width / 2))
    display.Top = 625 - (display.Height)
    display.Left = go.Left + go.Width + 45
    counter.Top = 625 - (counter.Height)
    counter.Left = go.Left - 50 - counter.Width
    topleft.Top = 900 - (topleft.Height + 30)
    topleft.Left = 130 - topright.Width
    topPiece.Width = (240 * columns) + 45
    topPiece.Top = 900 - (topPiece.Height + 30)
    topPiece.Left = topleft.Left + topleft.Width
    leftPiece.Top = topleft.Top + topleft.Height
    leftPiece.Left = topleft.Left
    leftPiece.Height = (240 * rows) + 45
    bottomleft.Top = leftPiece.Top + leftPiece.Height
    bottomleft.Left = leftPiece.Left
    bottomPiece.Left = bottomleft.Left + bottomleft.Width
    bottomPiece.Top = bottomleft.Top
    bottomPiece.Width = topPiece.Width
    topright.Top = 900 - (topright.Height + 30)
    topright.Left = (topPiece.Left + topPiece.Width)
    rightPiece.Left = topright.Left
    rightPiece.Top = leftPiece.Top
    rightPiece.Height = (240 * rows) + 60
    bottomright.Top = bottomPiece.Top
    bottomright.Left = bottomPiece.Left + bottomPiece.Width
    'Flag(0).Top = (go.Top + go.Height) - Flag(0).Height
    'Flag(0).Left = topleft.Left
    'Flag(1).Top = (go.Top + go.Height) - Flag(1).Height
    'Flag(1).Left = topright.Left + topright.Width - Flag(1).Width
    
    
    For X = 0 To (rows * columns) - 1
        imgButton(X).Left = ((X Mod columns) * 240) + 220
        imgButton(X).Top = ((X \ columns) * 240) + 900
    Next X
    
    For X = 0 To (rows * columns) - 1
        imgButton(X).Visible = True
    Next X
    
End Sub
Private Sub go_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    
    go.Picture = smilepush.Picture
    
End Sub

Private Sub go_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    
    go.Picture = smile.Picture
    over = True
    start
    
End Sub

Private Sub imgButton_DragDrop(Index As Integer, Source As Control, X As Single, Y As Single)
    
    'If Source.Tag = "flag" Then
        'If imgButton(Index).Picture = square.Picture Then
            'imgButton(Index).Picture = Flag(0).Picture
            'numflags = numflags - 1
            'display.Caption = numflags
        'ElseIf imgButton(Index).Picture = Flag(0).Picture Then
            'imgButton(Index).Picture = square.Picture
            'numflags = numflags + 1
            'display.Caption = numflags
        'End If
    'End If
    
    'win
    
End Sub

Private Sub imgButton_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)

    If imgButton(Index).Picture <> Flag(0).Picture And over = False And Button <> 2 Then
        go.Picture = uhoh.Picture
        imgButton(Index).Picture = blank.Picture
    End If
    
End Sub
Private Sub imgButton_MouseUp(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
        
    If Button = 2 Then
        If imgButton(Index).Picture = square.Picture Then
            imgButton(Index).Picture = Flag(0).Picture
            numflags = numflags - 1
            display.Caption = numflags
        ElseIf imgButton(Index).Picture = Flag(0).Picture Then
            imgButton(Index).Picture = square.Picture
            numflags = numflags + 1
            display.Caption = numflags
        End If
    ElseIf Button = 1 Then
        If first = True Then
            over = False
            first = False
        End If
    
        If imgButton(Index).Picture <> Flag(0).Picture And over = False Then
        
            imgButton(Index).Picture = square.Picture
        End If
        
        If over = False Then
            go.Picture = smile.Picture
            If imgButton(Index).Picture <> Flag(0).Picture Then
                If board(Index) = -1 Then
                    imgButton(Index).Picture = explode.Picture
                    go.Picture = dead.Picture
                    over = True
                    showboard (Index)
                ElseIf board(Index) = 0 Then
                    imgButton(Index).Picture = blank.Picture
                    showblank (Index)
                ElseIf board(Index) > 0 Then
                    'For X = 0 To 8
                        'If board(Index) = X Then
                     imgButton(Index).Picture = squarenum(board(Index)).Picture
                        'End If
                    'Next X
                End If
            ElseIf imgButton(Index).Picture = Flag(0).Picture Then
                imgButton(Index).Picture = square.Picture
                numflags = numflags + 1
                display.Caption = numflags
            End If
        End If
    End If
    
    win
    
End Sub

Sub numbersquares()
    'puts the numbers on the squares
    Dim numBombs%
    numBombs = 0
    
    For X = 0 To (rows * columns) - 1
        If board(X) <> -1 Then
            If X Mod columns <> 0 Then
                If board(X - 1) = -1 Then
                    numBombs = numBombs + 1
                End If
            End If
            If X Mod columns <> columns - 1 Then
                If board(X + 1) = -1 Then
                    numBombs = numBombs + 1
                End If
            End If
            If X > columns - 1 Then
                If board(X - columns) = -1 Then
                    numBombs = numBombs + 1
                End If
            End If
            If X < (columns * (rows - 1)) Then
                If board(X + columns) = -1 Then
                    numBombs = numBombs + 1
                End If
            End If
            If X Mod columns <> 0 And X > columns - 1 Then
                If board(X - (columns + 1)) = -1 Then
                    numBombs = numBombs + 1
                End If
            End If
            If X Mod columns <> columns - 1 And X < (columns * (rows - 1)) Then
                If board(X + (columns + 1)) = -1 Then
                    numBombs = numBombs + 1
                End If
            End If
            If X Mod columns <> columns - 1 And X > columns - 1 Then
                If board(X - (columns - 1)) = -1 Then
                    numBombs = numBombs + 1
                End If
            End If
            If X Mod columns <> 0 And X < (columns * (rows - 1)) Then
                If board(X + (columns - 1)) = -1 Then
                    numBombs = numBombs + 1
                End If
            End If
            board(X) = numBombs
            numBombs = 0
        End If
    Next X
    
End Sub
Sub win()
    
    Dim count%
    
    For X = 0 To (rows * columns) - 1
        If board(X) = -1 And imgButton(X).Picture = Flag(0).Picture Then
            count = count + 1
        End If
    Next X
    
    If count = realNumMines And numflags = 0 Then
        go.Picture = smilewin.Picture
        over = True
    End If
    
    count = 0
    
End Sub
Sub showboard(exclude As Integer)

    For X = 0 To (rows * columns) - 1
        If board(X) = -1 And X <> exclude And imgButton(X).Picture <> Flag(0).Picture Then
            imgButton(X).Picture = bomb.Picture
        ElseIf board(X) <> -1 And X <> exclude And imgButton(X).Picture = Flag(0).Picture Then
            imgButton(X).Picture = nobomb.Picture
        End If
    Next X
    
End Sub
Sub showblank(center As Integer)

    If center Mod columns <> 0 Then
        If board(center - 1) <> -1 And imgButton(center - 1).Picture <> Flag(0).Picture And imgButton(center - 1).Picture <> squarenum(0).Picture Then
             imgButton(center - 1).Picture = squarenum(board(center - 1)).Picture
             If board(center - 1) = 0 Then
                showblank (center - 1)
             End If
        End If
     End If
     If center Mod columns <> (columns - 1) Then
         If board(center + 1) <> -1 And imgButton(center + 1).Picture <> Flag(0).Picture And imgButton(center + 1).Picture <> squarenum(0).Picture Then
             imgButton(center + 1).Picture = squarenum(board(center + 1)).Picture
             If board(center + 1) = 0 Then
                showblank (center + 1)
             End If
         End If
     End If
     If center > (columns - 1) Then
         If board(center - columns) <> -1 And imgButton(center - columns).Picture <> Flag(0).Picture And imgButton(center - columns).Picture <> squarenum(0).Picture Then
             imgButton(center - columns).Picture = squarenum(board(center - columns)).Picture
             If board(center - columns) = 0 Then
                showblank (center - columns)
             End If
         End If
     End If
     If center < (columns * (rows - 1)) Then
         If board(center + columns) <> -1 And imgButton(center + columns).Picture <> Flag(0).Picture And imgButton(center + columns).Picture <> squarenum(0).Picture Then
             imgButton(center + columns).Picture = squarenum(board(center + columns)).Picture
             If board(center + columns) = 0 Then
                showblank (center + columns)
             End If
        End If
     End If
     If center Mod columns <> 0 And center > (columns - 1) Then
         If board(center - (columns + 1)) <> -1 And imgButton(center - (columns + 1)).Picture <> Flag(0).Picture And imgButton(center - (columns + 1)).Picture <> squarenum(0).Picture Then
             imgButton(center - (columns + 1)).Picture = squarenum(board(center - (columns + 1))).Picture
             If board(center - (columns + 1)) = 0 Then
                showblank (center - (columns + 1))
             End If
         End If
     End If
     If center Mod columns <> (columns - 1) And center < (columns * (rows - 1)) Then
         If board(center + (columns + 1)) <> -1 And imgButton(center + (columns + 1)).Picture <> Flag(0).Picture And imgButton(center + (columns + 1)).Picture <> squarenum(0).Picture Then
             imgButton(center + (columns + 1)).Picture = squarenum(board(center + (columns + 1))).Picture
             If board(center + (columns + 1)) = 0 Then
                showblank (center + (columns + 1))
             End If
         End If
     End If
     If center Mod columns <> (columns - 1) And center > (columns - 1) Then
         If board(center - (columns - 1)) <> -1 And imgButton(center - (columns - 1)).Picture <> Flag(0).Picture And imgButton(center - (columns - 1)).Picture <> squarenum(0).Picture Then
             imgButton(center - (columns - 1)).Picture = squarenum(board(center - (columns - 1))).Picture
             If board(center - (columns - 1)) = 0 Then
                showblank (center - (columns - 1))
             End If
         End If
     End If
     If center Mod columns <> 0 And center < (columns * (rows - 1)) Then
         If board(center + (columns - 1)) <> -1 And imgButton(center + (columns - 1)).Picture <> Flag(0).Picture And imgButton(center + (columns - 1)).Picture <> squarenum(0).Picture Then
             imgButton(center + (columns - 1)).Picture = squarenum(board(center + (columns - 1))).Picture
             If board(center + (columns - 1)) = 0 Then
                showblank (center + (columns - 1))
             End If
         End If
     End If

End Sub
Sub showall()

    over = True
    For X = 0 To (rows * columns) - 1
        If board(X) = -1 Then
            imgButton(X).Picture = bomb.Picture
        ElseIf board(X) <> -1 Then
            For Y = 0 To 8
                If board(X) = Y Then
                    imgButton(X).Picture = squarenum(Y).Picture
                End If
            Next Y
        End If
    Next X

End Sub

Private Sub mnuBeginner_Click()
    
    mnuCustom.Checked = False
    mnuExpert.Checked = False
    mnuInter.Checked = False
    mnuBeginner.Checked = True
    rows = 9
    columns = 9
    numMines = 10
    start
    
End Sub

Private Sub mnuCustom_Click()

    mnuCustom.Checked = True
    mnuExpert.Checked = False
    mnuInter.Checked = False
    mnuBeginner.Checked = False
    
    Load Dialog
    Dialog.Visible = True
    Game.Enabled = False
        
End Sub

Private Sub mnuExit_Click()
    
    End
    
End Sub

Private Sub mnuExpert_Click()
    
    mnuCustom.Checked = False
    mnuExpert.Checked = True
    mnuInter.Checked = False
    mnuBeginner.Checked = False
    rows = 16
    columns = 30
    numMines = 99
    start
 
End Sub

Private Sub mnuInter_Click()
    
    mnuCustom.Checked = False
    mnuExpert.Checked = False
    mnuInter.Checked = True
    mnuBeginner.Checked = False
    rows = 16
    columns = 16
    numMines = 40
    start
 
End Sub

Private Sub mnuNew_Click()
    
    over = True
    start
    
End Sub

Private Sub show_Click()

    showall
        
End Sub
Private Sub Timer1_Timer()

    If over = False Then
        timeRunning = timeRunning + 1
        counter.Caption = timeRunning
    End If
    
End Sub
