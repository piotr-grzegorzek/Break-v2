object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Panel1: TPanel
    Left = 224
    Top = 192
    Width = 190
    Height = 57
    BevelOuter = bvNone
    TabOrder = 0
    object lblBreakTime: TLabel
      Left = -1
      Top = 31
      Width = 56
      Height = 15
      Caption = 'Break time'
    end
    object lblWorkTime: TLabel
      Left = 0
      Top = 5
      Width = 55
      Height = 15
      Caption = 'Work time'
    end
    object edtWorkTime: TRxCalcEdit
      Left = 61
      Top = 0
      Width = 121
      Height = 23
      DecimalPlaces = 0
      NumGlyphs = 2
      TabOrder = 0
    end
    object edtBreakTime: TRxCalcEdit
      Left = 61
      Top = 26
      Width = 121
      Height = 23
      DecimalPlaces = 0
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object btnCancel: TButton
    Left = 545
    Top = 409
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Button1: TButton
    Left = 457
    Top = 409
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
end
