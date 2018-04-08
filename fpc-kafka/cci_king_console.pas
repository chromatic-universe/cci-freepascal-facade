unit cci_king_console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, cci_about, rdkafka, cci_dev_info;

type

  { Tfrm_cci_mini }

  Tfrm_cci_mini = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

end.

