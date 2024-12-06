unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  LCLIntf, Forms, Controls, Graphics, ExtCtrls, StdCtrls, Dialogs,
  TAGraph, TATools,
  mvMapViewer, mvPluginCore, mvPlugins;

type
  TForm1 = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMapView1: TMapView;
    FMapView2: TMapView;
    FPluginManager: TMvPluginManager;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPluginManager := TMvPluginManager.Create(Self);

  FMapView1 := TMapView.Create(self);
  FMapView1.Align := alClient;
  FMapView1.Parent := Panel2;
  FMapView1.MapProvider := 'OpenStreetMap Mapnik';
  FMapView1.UseThreads := true;
  FMapView1.Zoom := 9;
  FMapView1.MapCenter.Longitude := 11;
  FMapView1.MapCenter.Latitude := 46.3;
  FMapView1.Active := true;
  FMapView1.PluginManager := FPluginManager;

  FMapView2 := TMapView.Create(self);
  FMapView2.Align := alClient;
  FMapView2.Parent := Panel3;
  FMapView2.MapProvider := 'Maps for free';
  FMapView2.UseThreads := true;
  FMapView2.Zoom := 9;
  FMapView2.MapCenter.Longitude := 11;
  FMapView2.MapCenter.Latitude := 46.3;
  FMapView2.Active := true;
  FMapView2.PluginManager := FPluginManager;

  with TLegalNoticePlugin.Create(FPluginManager) do
  begin
    LegalNotice := '(c) OpenStreetMap and contributors';
    LegalNoticeURL := 'https://www.openstreetmap.org/copyright';
    Spacing := 5;
    Font.Size := 8;
    Font.Color := clBlue;
    BackgroundColor := clWhite;
    MapView := FMapView1;

    Edit1.Text := LegalNotice;
  end;

  with TLegalNoticePlugin.Create(FPluginManager) do
  begin
    LegalNotice := 'maps-for-free';
    LegalNoticeURL := 'https://maps-for-free.com/html/about.html';
    Spacing := 5;
    Font.Size := 8;
    Font.Color := clBlue;
    BackgroundColor := clWhite;
    MapView := FMapView2;
  end;

  with TCenterMarkerPlugin.Create(FPluginManager) do
  begin
    Size := 15;
    Pen.Width := 3;
    Pen.Color := clRed;
    //PluginManager := FPluginManager;
  end;

  with TLinkedMapsPlugin.Create(FPluginManager) do ;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  (FPluginManager.Item[0] as TLegalNoticePlugin).LegalNotice := Edit1.Text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FMapView1.SaveToFile(TPortableNetworkGraphic, 'map1.png');
  FMapView2.SaveToFile(TPortableNetworkGraphic, 'map2.png');
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  (FPluginManager.Item[2] as TCenterMarkerPlugin).Enabled := Checkbox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  (FPluginManager.Item[0] as TLegalNoticePlugin).Enabled := Checkbox2.Checked;
  (FPluginManager.Item[1] as TLegalNoticePlugin).Enabled := Checkbox2.Checked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  (FPluginManager.Item[0] as TLegalNoticePlugin).Position := TLegalNoticePosition(Combobox1.ItemIndex);
  (FPluginManager.Item[1] as TLegalNoticePlugin).Position := TLegalNoticePosition(Combobox1.ItemIndex);
end;

end.

