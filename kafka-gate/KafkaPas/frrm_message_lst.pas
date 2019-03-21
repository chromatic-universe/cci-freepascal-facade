unit frrm_message_lst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus , fpjson, jsonparser;

type

  { Tfrm_message_lst }

  Tfrm_message_lst = class(TForm)
    stream_messages: TListBox;
    PopupMenu1: TPopupMenu;
    procedure stream_messagesClick(Sender: TObject);
    procedure stream_messagesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure stream_messagesShowHint(Sender: TObject; HintInfo: PHintInfo);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frm_message_lst: Tfrm_message_lst;

implementation

{$R *.lfm}

{ Tfrm_message_lst }

procedure Tfrm_message_lst.stream_messagesClick(Sender: TObject);
var
  out : string;
  jdata : TJSONData;
begin
     //if stream_messages.ItemIndex > -1 then
       //ShowMessage( stream_messages.Items[stream_messages.ItemIndex] );
    //   out := stream_messages.Items[stream_messages.ItemIndex];
    // ShowMessage( out );
     //jdata :=  GetJson( out);
     //dw :=
end;

procedure Tfrm_message_lst.stream_messagesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  k : integer;
begin
    k := stream_messages.ItemAtPos( Point( X , Y ) , true );
    if k = -1 then begin
      stream_messages.ShowHint := false;
    end else begin
      stream_messages.ShowHint := true;
      stream_messages.hint := stream_messages.Items[k];
    end;
end;

procedure Tfrm_message_lst.stream_messagesShowHint(Sender: TObject;
  HintInfo: PHintInfo);

begin

end;

end.

