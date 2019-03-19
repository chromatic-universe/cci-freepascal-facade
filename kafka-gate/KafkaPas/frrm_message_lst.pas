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
begin
     if stream_messages.ItemIndex > -1 then
        ShowMessage( GetJson( stream_messages.Items[stream_messages.ItemIndex] ).FormatJSON() );
end;

end.

