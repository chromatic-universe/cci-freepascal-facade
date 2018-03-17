unit cci_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { Tfrm_about }

  Tfrm_about = class(TForm)
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private

  public

  end;

var
  frm_about: Tfrm_about;

implementation

{$R *.lfm}

end.

