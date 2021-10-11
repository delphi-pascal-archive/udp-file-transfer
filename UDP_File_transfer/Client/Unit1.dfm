object Form1: TForm1
  Left = 239
  Top = 132
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'File Transfer Client'
  ClientHeight = 210
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object PanelCommande: TPanel
    Left = 0
    Top = 0
    Width = 406
    Height = 110
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LabelPortTCP: TLabel
      Left = 8
      Top = 48
      Width = 57
      Height = 16
      Caption = 'Port TCP:'
    end
    object LabelPortUdp: TLabel
      Left = 8
      Top = 80
      Width = 59
      Height = 16
      Caption = 'Port UDP:'
    end
    object LabelHost: TLabel
      Left = 8
      Top = 16
      Width = 31
      Height = 16
      Caption = 'Host:'
    end
    object Label1: TLabel
      Left = 216
      Top = 64
      Width = 105
      Height = 16
      Caption = 'Transfer speed: 4'
    end
    object EditHost: TEdit
      Left = 80
      Top = 8
      Width = 121
      Height = 24
      TabOrder = 0
      Text = 'localhost'
    end
    object EditPortTCP: TEdit
      Left = 79
      Top = 40
      Width = 122
      Height = 24
      TabOrder = 1
      Text = '45666'
    end
    object EditPortUDP: TEdit
      Left = 79
      Top = 72
      Width = 122
      Height = 24
      TabOrder = 2
      Text = '45666'
    end
    object ButtonEnvoyer: TButton
      Left = 216
      Top = 16
      Width = 185
      Height = 25
      Caption = 'Open file ...'
      TabOrder = 3
      OnClick = ButtonEnvoyerClick
    end
    object TrackBar1: TTrackBar
      Left = 328
      Top = 64
      Width = 73
      Height = 21
      Max = 5
      Position = 4
      TabOrder = 4
      ThumbLength = 14
      OnChange = TrackBar1Change
    end
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 110
    Width = 406
    Height = 20
    Align = alTop
    Smooth = True
    TabOrder = 1
  end
  object ListBox1: TListBox
    Left = 0
    Top = 130
    Width = 406
    Height = 80
    Align = alClient
    ItemHeight = 16
    TabOrder = 2
  end
  object OpenDialog1: TOpenDialog
    Left = 224
    Top = 16
  end
end
