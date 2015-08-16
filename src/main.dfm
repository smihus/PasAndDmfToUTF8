object fmPasAndDfmtoUTF8: TfmPasAndDfmtoUTF8
  Left = 0
  Top = 0
  ActiveControl = bnSelectDir
  BorderStyle = bsDialog
  Caption = 'Files to UTF-8'
  ClientHeight = 184
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lDirPath: TLabel
    Left = 8
    Top = 8
    Width = 71
    Height = 13
    Caption = #1055#1091#1090#1100' '#1082' '#1087#1072#1087#1082#1077':'
  end
  object lFilePattern: TLabel
    Left = 8
    Top = 80
    Width = 145
    Height = 13
    Caption = #1064#1072#1073#1083#1086#1085' '#1076#1083#1103' '#1087#1086#1080#1089#1082#1072' '#1092#1072#1081#1083#1086#1074':'
  end
  object lEncoding: TLabel
    Left = 8
    Top = 115
    Width = 75
    Height = 13
    Caption = #1050#1086#1076#1080#1088#1086#1074#1072#1090#1100' '#1074':'
  end
  object eDir: TEdit
    Left = 8
    Top = 24
    Width = 392
    Height = 21
    TabOrder = 0
  end
  object bnSelectDir: TButton
    Left = 406
    Top = 24
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = bnSelectDirClick
  end
  object bnConvert: TButton
    Left = 8
    Top = 139
    Width = 423
    Height = 38
    Caption = #1050#1086#1085#1074#1077#1088#1090#1080#1088#1086#1074#1072#1090#1100
    TabOrder = 2
    OnClick = bnConvertClick
  end
  object cbRecursive: TCheckBox
    Left = 8
    Top = 51
    Width = 423
    Height = 17
    Caption = #1074#1082#1083#1102#1095#1072#1103' '#1087#1086#1076#1087#1072#1087#1082#1080
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object ePattern: TEdit
    Left = 159
    Top = 77
    Width = 272
    Height = 21
    TabOrder = 4
    Text = '*.pas, *.dfm'
  end
  object cbEncoding: TComboBox
    Left = 159
    Top = 112
    Width = 272
    Height = 21
    TabOrder = 5
    Text = 'cbEncoding'
  end
end
