unit frrm_message_lst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tfrm_message_lst }

  Tfrm_message_lst = class(TForm)
    stream_messages: TListBox;
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

end;

end.

