unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvPluginCore, mvNullPlugin;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    lblNullPluginMessage: TLabel;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    procedure NullPluginMouseUp(Sender : TObject; AMapView: TMapView; Button: TMouseButton;
                                Shift: TShiftState;
                                X, Y: Integer; var Handled: Boolean);
    procedure NullPluginMouseDown(Sender : TObject; AMapView: TMapView; Button: TMouseButton;
                                Shift: TShiftState;
                                X, Y: Integer; var Handled: Boolean);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  np : TMvNullPlugin;
begin
  np := TMvNullPlugin.Create(Self);
  np.OnMouseUp:=@NullPluginMouseUp;
  np.OnMouseDown:=@NullPluginMouseDown;
  MvPluginManager1.PluginList.Add(np);
end;

procedure TForm1.NullPluginMouseUp(Sender: TObject; AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
begin
  lblNullPluginMessage.Caption := Format('MouseUp X:%d Y:%d',[X,Y]);
end;

procedure TForm1.NullPluginMouseDown(Sender: TObject; AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
begin
  lblNullPluginMessage.Caption := Format('MouseDown X:%d Y:%d',[X,Y]);
end;

end.

