unit cci_king_console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, cci_about, rdkafka, cci_dev_info;

type

  { Tfrm_cci_mini }

  Tfrm_cci_mini = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frm_cci_mini: Tfrm_cci_mini;

implementation

{$R *.lfm}

{ Tfrm_cci_mini }

procedure Tfrm_cci_mini.Button1Click(Sender: TObject);
begin
   frm_dev_info.ShowModal;
end;

procedure Tfrm_cci_mini.Button2Click(Sender: TObject);
begin
   frm_about.ShowModal;
end;

procedure Tfrm_cci_mini.SpeedButton2Click(Sender: TObject);
begin

end;

end.

