unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLIntf, Forms, Controls, Graphics, ExtCtrls, StdCtrls, Dialogs, Spin,
  mvMapViewer, mvPluginCore, mvPlugins;

type
  TMainForm = class(TForm)
    FormCenterBevel: TBevel;
    btnSaveToImage: TButton;
    cbShowMapCenter: TCheckBox;
    cbShowLegalNotice: TCheckBox;
    cmbPosition: TComboBox;
    edLegalNotice: TEdit;
    seOpacity: TSpinEdit;
    lblLegalNotice: TLabel;
    lblOpacity: TLabel;
    ParamsPanel: TPanel;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    procedure btnSaveToImageClick(Sender: TObject);
    procedure cbShowMapCenterChange(Sender: TObject);
    procedure cbShowLegalNoticeChange(Sender: TObject);
    procedure cmbPositionChange(Sender: TObject);
    procedure edLegalNoticeChange(Sender: TObject);
    procedure FloatSpinEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMapView1: TMapView;
    FMapView2: TMapView;
    FPluginManager: TMvPluginManager;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FPluginManager := TMvPluginManager.Create(Self);

  FMapView1 := TMapView.Create(self);
  FMapView1.Align := alClient;
  FMapView1.Parent := LeftPanel;
  FMapView1.MapProvider := 'OpenStreetMap Mapnik';
  FMapView1.UseThreads := true;
  FMapView1.Zoom := 9;
  FMapView1.MapCenter.Longitude := 11;
  FMapView1.MapCenter.Latitude := 46.3;
  FMapView1.Active := true;
  FMapView1.PluginManager := FPluginManager;

  FMapView2 := TMapView.Create(self);
  FMapView2.Align := alClient;
  FMapView2.Parent := RightPanel;
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

    edLegalNotice.Text := LegalNotice;
    seOpacity.Value := round(BackgroundOpacity * 100);
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
  end;

  with TLinkedMapsPlugin.Create(FPluginManager) do ;
end;

procedure TMainForm.edLegalNoticeChange(Sender: TObject);
begin
  (FPluginManager.Item[0] as TLegalNoticePlugin).LegalNotice := edLegalNotice.Text;
end;

procedure TMainForm.FloatSpinEdit1Change(Sender: TObject);
begin
  (FPluginManager.Item[0] as TLegalNoticePlugin).BackgroundOpacity := seOpacity.Value / 100;
  if FPluginManager.PluginList.Count > 1 then
    (FPluginManager.Item[1] as TLegalNoticePlugin).BackgroundOpacity := seOpacity.Value / 100;
end;

procedure TMainForm.btnSaveToImageClick(Sender: TObject);
begin
  FMapView1.SaveToFile(TPortableNetworkGraphic, 'map1.png');
  FMapView2.SaveToFile(TPortableNetworkGraphic, 'map2.png');
end;

procedure TMainForm.cbShowMapCenterChange(Sender: TObject);
begin
  (FPluginManager.Item[2] as TCenterMarkerPlugin).Enabled := cbShowMapCenter.Checked;
end;

procedure TMainForm.cbShowLegalNoticeChange(Sender: TObject);
begin
  (FPluginManager.Item[0] as TLegalNoticePlugin).Enabled := cbShowLegalNotice.Checked;
  (FPluginManager.Item[1] as TLegalNoticePlugin).Enabled := cbShowLegalNotice.Checked;
end;

procedure TMainForm.cmbPositionChange(Sender: TObject);
begin
  (FPluginManager.Item[0] as TLegalNoticePlugin).Position := TLegalNoticePosition(cmbPosition.ItemIndex);
  (FPluginManager.Item[1] as TLegalNoticePlugin).Position := TLegalNoticePosition(cmbPosition.ItemIndex);
end;

end.

