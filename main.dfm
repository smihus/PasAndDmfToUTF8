object fmPasAndDfmtoUTF8: TfmPasAndDfmtoUTF8
  Left = 0
  Top = 0
  ActiveControl = bnSelectDir
  BorderStyle = bsDialog
  Caption = 'Pas and Dfm files to UTF-8'
  ClientHeight = 80
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
  object eDir: TEdit
    Left = 4
    Top = 8
    Width = 401
    Height = 21
    TabOrder = 0
    Text = 'eDir'
  end
  object bnSelectDir: TButton
    Left = 408
    Top = 8
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = bnSelectDirClick
  end
  object bnConvert: TButton
    Left = 4
    Top = 35
    Width = 429
    Height = 38
    Caption = #1050#1086#1085#1074#1077#1088#1090#1080#1088#1086#1074#1072#1090#1100
    TabOrder = 2
    OnClick = bnConvertClick
  end
end
