object Form1: TForm1
  Left = 224
  Top = 132
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'File Transfer Server'
  ClientHeight = 602
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCanResize = FormCanResize
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object PanelConnect: TPanel
    Left = 0
    Top = 0
    Width = 369
    Height = 105
    Align = alTop
    TabOrder = 0
    object LabelPortTCP: TLabel
      Left = 8
      Top = 16
      Width = 60
      Height = 16
      Caption = 'Port TCP: '
    end
    object LabelPortUdp: TLabel
      Left = 8
      Top = 47
      Width = 59
      Height = 16
      Caption = 'Port UDP:'
    end
    object LabelDossier: TLabel
      Left = 8
      Top = 79
      Width = 101
      Height = 16
      Caption = 'Download folder:'
    end
    object ButtonOpen: TButton
      Left = 208
      Top = 24
      Width = 153
      Height = 25
      Caption = 'Start Server'
      TabOrder = 0
      OnClick = ButtonOpenClick
    end
    object EditPortTCP: TEdit
      Left = 80
      Top = 8
      Width = 121
      Height = 24
      Hint = 'Port sur lequel on recevra les requetes TCP'
      TabOrder = 1
      Text = '45666'
    end
    object EditPortUDP: TEdit
      Left = 80
      Top = 39
      Width = 121
      Height = 24
      Hint = 'Port sur lequel on recoit les morceaux de fichier en UDP'
      TabOrder = 2
      Text = '45666'
    end
    object EditDossier: TEdit
      Left = 120
      Top = 72
      Width = 145
      Height = 24
      TabOrder = 3
    end
    object ButtonChgDs: TButton
      Left = 272
      Top = 72
      Width = 89
      Height = 25
      Hint = 'Changer le dossier'
      Caption = 'Change ...'
      TabOrder = 4
      OnClick = ButtonChgDsClick
    end
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      'Dossier|ici.sjlkbfupiqrbukLHGVFQSUHFNGRBGS21332bjfdkgndglkdgergh' +
      'bn.l'
    Left = 216
    Top = 16
  end
end
