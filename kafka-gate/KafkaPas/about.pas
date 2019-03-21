unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { Tfrm_about }

  Tfrm_about = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    procedure Image1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frm_about: Tfrm_about;

implementation

{$R *.lfm}

{ Tfrm_about }

procedure Tfrm_about.Image1Click(Sender: TObject);
begin

end;

end.

