object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 15
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 640
    Height = 480
    Align = alClient
    Alignment = taCenter
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = 72
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitWidth = 29
    ExplicitHeight = 72
  end
  object btnOK: TButton
    Left = 557
    Top = 8
    Width = 75
    Height = 25
    Align = alCustom
    Caption = 'OK'
    DisabledImageName = 'btnOK'
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnBreak: TButton
    Left = 557
    Top = 447
    Width = 75
    Height = 25
    Align = alCustom
    Caption = 'Break'
    DisabledImageName = 'btnBreak'
    TabOrder = 1
  end
  object btnSettings: TButton
    Left = 8
    Top = 447
    Width = 75
    Height = 25
    Align = alCustom
    Caption = 'Settings'
    DisabledImageName = 'btnOK'
    TabOrder = 2
    OnClick = btnSettingsClick
  end
  object btnMute: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Align = alCustom
    Caption = 'Mute'
    DisabledImageName = 'btnOK'
    TabOrder = 3
  end
  object TrayIcon1: TTrayIcon
    OnClick = TrayIcon1Click
    Left = 96
    Top = 112
  end
end
