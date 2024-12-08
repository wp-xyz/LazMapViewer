unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvPluginCore, mvPlugins;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    lblUserdefinedPluginMessage: TLabel;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    procedure UserdefinedPluginMouseUp(Sender: TObject; AMapView: TMapView; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
    procedure UserdefinedPluginMouseDown(Sender: TObject; AMapView: TMapView; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  udp : TUserDefinedPlugin;
begin
  udp := TUserdefinedPlugin.Create(MvPluginManager1);
  udp.OnMouseUp := @UserdefinedPluginMouseUp;
  udp.OnMouseDown := @UserdefinedPluginMouseDown;
end;

procedure TForm1.UserdefinedPluginMouseUp(Sender: TObject; AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
begin
  lblUserdefinedPluginMessage.Caption := Format('MouseUp X:%d Y:%d',[X,Y]);
end;

procedure TForm1.UserdefinedPluginMouseDown(Sender: TObject; AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
begin
  lblUserdefinedPluginMessage.Caption := Format('MouseDown X:%d Y:%d',[X,Y]);
end;

end.

