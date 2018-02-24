unit cci_king_console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ExtCtrls;

type

  { Tfrm_mini_kafka_main }

  Tfrm_mini_kafka_main = class(TForm)
    Image1: TImage;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    mi_utils: TMenuItem;
    mi_dev_info: TMenuItem;
    sb_kafka: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure btn_kafkaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mi_dev_infoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;



var
  frm_mini_kafka_main: Tfrm_mini_kafka_main;

implementation

 uses
  cci_mini_kafka , cci_dev_info;

{$R *.lfm}

{ Tfrm_mini_kafka_main }

procedure Tfrm_mini_kafka_main.btn_kafkaClick(Sender: TObject);
begin

end;

procedure Tfrm_mini_kafka_main.FormCreate(Sender: TObject);
begin
      sb_kafka.Panels[0].Text :=  concat( 'kafka version->' , cci_mini_kafka.rd_kafka_version_str );

end;

procedure Tfrm_mini_kafka_main.mi_dev_infoClick(Sender: TObject);
begin
      cci_dev_info.frm_dev_info.ShowMOdal;
end;

end.

