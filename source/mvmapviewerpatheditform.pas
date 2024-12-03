unit mvMapViewerPathEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ActnList, mvMapViewer;

type

  { TMapViewerPathEditForm }

  TMapViewerPathEditForm = class(TForm, IFPObserver)
    actDelTP: TAction;
    actNewArea: TAction;
    actSelect: TAction;
    actNewTP: TAction;
    actNewTrack: TAction;
    actNewPOI: TAction;
    actZoomOut: TAction;
    actZoomIn: TAction;
    alEditActions: TActionList;
    cbSelectedLayer: TEdit;
    cbSelectedPt: TEdit;
    ilImages: TImageList;
    lblSelectedPt: TLabel;
    lblSelectedLayer: TLabel;
    pnlFrame: TPanel;
    pnlTools: TPanel;
    btnSelect: TSpeedButton;
    btnNewTP: TSpeedButton;
    btnDelTP: TSpeedButton;
    btnNewPOI: TSpeedButton;
    btnNewArea: TSpeedButton;
    btnNewTrack: TSpeedButton;
    btnZoomIn: TSpeedButton;
    btnZoomOut: TSpeedButton;
    procedure actDelTPExecute(Sender: TObject);
    procedure actNewAreaExecute(Sender: TObject);
    procedure actNewPOIExecute(Sender: TObject);
    procedure actNewTPExecute(Sender: TObject);
    procedure actNewTrackExecute(Sender: TObject);
    procedure actSelectExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
  private
    FMapLayer: TMapLayer;
    FMapView: TMapView;
    FInternalSelect: Boolean;
    procedure SetMapLayer(AValue: TMapLayer);
    procedure SetMapView(AValue: TMapView);
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  protected
    procedure UpdateControls;
    procedure PersistentAdded({%H-}APersistent: TPersistent; {%H-}Select: Boolean); virtual;
    procedure DeletePersistent({%H-}APersistent: TPersistent); virtual;
    procedure UnselectPersistent({%H-}APersistent: TPersistent); virtual;
    procedure ObjectModified({%H-}AObject: TObject; {%H-}PropName: ShortString = ''); virtual;
    procedure SelectInOI(AView: TMapView; {%H-}ForceUpdate: Boolean); virtual;
    property InternalSelect: Boolean read FInternalSelect write FInternalSelect;
  public
    destructor Destroy; override;
    property MapView: TMapView read FMapView write SetMapView;
    property MapLayer: TMapLayer read FMapLayer write SetMapLayer;
  end;

var
  MapViewerPathEditForm: TMapViewerPathEditForm;

implementation

uses mvTypes, Math;

{$R *.lfm}

{ TMapViewerPathEditForm }

procedure TMapViewerPathEditForm.actSelectExecute(Sender: TObject);
begin
  ; //
end;

procedure TMapViewerPathEditForm.actNewPOIExecute(Sender: TObject);
var
  CP: TMapPoint;
  NP: TPointOfInterest;
  R: TRealPoint;
  V: TMapView;
begin
  V := MapView;
  if not Assigned(MapLayer) or not V.EditMark.HasSelection then
    Exit;

  CP := MapView.EditMark.CurrentPoint;
  NP := MapLayer.PointsOfInterest.Add as TPointOfInterest;
  NP.Assign(CP);
  if CP is TPointOfInterest then
  begin
    NP.Caption := TPointOfInterest(CP).Caption;
    NP.ImageIndex := TPointOfInterest(CP).ImageIndex;
  end;
  R := MapView.ScreenToLatLon(CP.ToScreen + Point(10, 10));
  NP.Latitude := R.Lat;
  NP.Longitude := R.Lon;

  MapView.EditMark.ClearSelection;
  MapView.EditMark.Selection.Insert(0, NP);

  try
    PersistentAdded(NP, True);
  finally
    MapView := V;
    SelectInOI(V, False);
  end;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.actNewTrackExecute(Sender: TObject);
var
  P, NP: TMapPoint;
  T, NT: TMapTrack;
  R: TRealPoint;
  I: Integer;
  V: TMapView;
begin
  V := MapView;
  if not Assigned(MapLayer) or not V.EditMark.HasSelection or
    (V.EditMark.Selection.Count < 2) then
    Exit;

  T := MapView.EditMark.CurrentTrack;
  NT := MapLayer.Tracks.Add as TMapTrack;

  for P in V.EditMark.Selection.Points do
  begin
    NP := NT.Points.Add as TMapPoint;
    NP.Assign(P);
    R := MapView.ScreenToLatLon(P.ToScreen + Point(10, 10));
    NP.Latitude := R.Lat;
    NP.Longitude := R.Lon;
  end;

  MapView.EditMark.ClearSelection;
  for I := 0 to Pred(NT.Points.Count) do
  begin
    MapView.EditMark.Selection.Add(NT.Points);
    MapView.EditMark.Selection.Insert(0, NT.Points[I]);
  end;

  if Assigned(T) then
  begin // NT.Assign(T)?
    NT.LineColor := T.LineColor;
    NT.LineWidth := T.LineWidth;
    NT.ConnectColor := T.ConnectColor;
    NT.ConnectWidth := T.ConnectWidth;
    NT.Opacity := T.Opacity;
  end;

  try
    PersistentAdded(NT, True);
  finally
    MapView := V;
    SelectInOI(V, False);
  end;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.actNewAreaExecute(Sender: TObject);
var
  P, NP: TMapPoint;
  A, NA: TMapArea;
  R: TRealPoint;
  I: Integer;
  V: TMapView;
begin
  V := MapView;
  if not Assigned(MapLayer) or not V.EditMark.HasSelection or
    (V.EditMark.Selection.Count < 3) then
    Exit;

  A := MapView.EditMark.CurrentArea;
  NA := MapLayer.Areas.Add as TMapArea;

  for P in V.EditMark.Selection.Points do
  begin
    NP := NA.Points.Add as TMapPoint;
    NP.Assign(P);
    R := MapView.ScreenToLatLon(P.ToScreen + Point(10, 10));
    NP.Latitude := R.Lat;
    NP.Longitude := R.Lon;
  end;

  MapView.EditMark.ClearSelection;
  for I := 0 to Pred(NA.Points.Count) do
  begin
    MapView.EditMark.Selection.Add(NA.Points);
    MapView.EditMark.Selection.Insert(0, NA.Points[I]);
  end;

  if Assigned(A) then
  begin // NA.Assign(A)?
    NA.LineColor := A.LineColor;
    NA.LineWidth := A.LineWidth;
    NA.FillColor := A.FillColor;
    NA.Opacity := A.Opacity;
  end;

  try
    PersistentAdded(NA, True);
  finally
    MapView := V;
    SelectInOI(V, False);
  end;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.actNewTPExecute(Sender: TObject);
var
  P: TMapPoint;
  L: TMapTrack;
  A: TMapArea;
  C: TCollection;
  I, I1, I2: Integer;
  V: TMapView;

  procedure InsertPt(J: Integer; ANext: TMapPoint);
  var
    PN: TMapPoint;
  begin
    PN := C.Insert(J) as TMapPoint;
    PN.Latitude := (P.Latitude + ANext.Latitude) / 2;
    PN.Longitude := (P.Longitude + ANext.Longitude) / 2;
    MapView.EditMark.Selection.Add(C);
    MapView.EditMark.Selection.Insert(0, PN);
    PersistentAdded(PN, True);
  end;

begin
  P := MapView.EditMark.CurrentPoint;
  L := MapView.EditMark.CurrentTrack;
  if Assigned(L) then
    C := L.Points
  else
  begin
    A := MapView.EditMark.CurrentArea;
    if not Assigned(A) then
      Exit;
    C := A.Points;
  end;
  I := P.Index;
  if I < 0 then
    Exit;
  I1 := (I + 1) mod C.Count; // Next point index
  if I > 0
    then I2 := Pred(I)  // Prev point index
    else I2 := Pred(C.Count);
  V := MapView;
  try
    // If the next point on track/area is selected
    // then insert before next
    if MapView.EditMark.IsSelected(C.Items[I1]) then
      InsertPt(I1, C.Items[I1] as TMapPoint)
    // If the prev point on track/area is selected
    // then insert before current
    else if MapView.EditMark.IsSelected(C.Items[I2]) then
      InsertPt(I, C.Items[I2] as TMapPoint)
    // If the current point is not the last
    // then insert before next(last)
    else if I < Pred(C.Count) then
      InsertPt(I1, C.Items[I1] as TMapPoint)
    // else insert before current
    else
      InsertPt(I, C.Items[I2] as TMapPoint);
  finally
    MapView := V;
    SelectInOI(V, False);
  end;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.actDelTPExecute(Sender: TObject);
var
  Pt: TMapPoint;
  PtCol: TCollection;
  P: TPersistent;
begin
  Pt := MapView.EditMark.CurrentPoint;
  PtCol := Pt.Collection;

  if not (Pt is TPointOfInterest) then
    if PtCol.Count < 3 then
      ; // TODO Just 3 points left?

  if MessageDlg('Confirm deletion', 'Are you sure you want to delete ''' +
    Pt.DisplayName + '''?', mtConfirmation, mbYesNo, 0 ) <> mrYes then
    Exit;

  // Exclude from the selection
  MapView.EditMark.Selection.DelIfPresent(PtCol);
  MapView.EditMark.Selection.DelIfPresent(Pt);

  // No points left?
  if MapView.EditMark.HasSelection and
    not (MapView.EditMark.Selection[0] is TMapPoint) then
    MapView.EditMark.ClearSelection; // Clear the whole selection

  // Delete from the object inspector
  UnselectPersistent(Pt);
  P := Pt; DeletePersistent(P);

  UpdateControls;
end;

procedure TMapViewerPathEditForm.actZoomInExecute(Sender: TObject);
begin
  MapView.Zoom := MapView.Zoom + 1;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.actZoomOutExecute(Sender: TObject);
begin
  MapView.Zoom := MapView.Zoom - 1;
  UpdateControls;
end;


procedure TMapViewerPathEditForm.SetMapLayer(AValue: TMapLayer);
begin
  if FMapLayer = AValue then Exit;
  FMapLayer := AValue;
end;

procedure TMapViewerPathEditForm.SetMapView(AValue: TMapView);
begin
  if FMapView = AValue then
    Exit;
  // Detach the old FMapView
  if Assigned(FMapView) then
  begin
    FMapView.FPODetachObserver(Self);
    FMapView.ControlStyle := FMapView.ControlStyle - [csDesignInteractive];
  end;
  // Attach the new FMapView
  if Assigned(AValue) then
  begin
    AValue.FPOAttachObserver(Self);
    if Visible then
      AValue.ControlStyle := AValue.ControlStyle + [csDesignInteractive];
  end;
  FMapView := AValue;
  UpdateControls;
end;

procedure TMapViewerPathEditForm.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
var
  What: TMapObserverCustomOperation;
  V: TMapView;

  procedure Clicked;
  begin
    if actSelect.Checked then
      Exit;
    //...
  end;

begin
  if Operation <> ooCustom then
    Exit;
  V := ASender as TMapView;
  What := PMapObserverCustomOperation(Data)^;
  case What of
    mooSelectionCompleted:
      SelectInOI(V, False);
    mooStartDrag:
      Clicked;
    mooIsDirty: ;
    mooDrag: ;
    mooEndDrag:
      if V.EditMark.Dirty then
      begin
        ObjectModified(Self, 'Layers');
        V.EditMark.Dirty := False;
      end;
  end;
end;

procedure TMapViewerPathEditForm.SelectInOI(AView: TMapView; ForceUpdate: Boolean);
begin
  if not Assigned(AView) then
    Exit;
  UpdateControls;
end;

destructor TMapViewerPathEditForm.Destroy;
begin
  MapView := Nil;
  inherited Destroy;
end;

procedure TMapViewerPathEditForm.UpdateControls;
var
  P: TMapPoint;
  PtTxt: String;
  PtCnt: Integer = 0;
  HaveView, HaveLayer, HaveSel, HavePt: Boolean;
begin
  HaveView := Assigned(MapView);
  HaveSel := HaveView and MapView.EditMark.HasSelection;
  HavePt := HaveSel and (MapView.EditMark.Selection[0] is TMapPoint);
  HaveLayer := Assigned(MapLayer) and HaveSel;

  if HavePt and not HaveLayer then
  begin
    MapLayer := (MapView.EditMark.Selection[0] as TMapPoint).Layer;
    HaveLayer := True;
  end;

  if HaveView
    then Caption := MapView.Name + ': ' + TMapView.ClassName
    else Caption := TMapView.ClassName + ' (Not selected)';

  // Update layer name
  if HaveLayer
    then cbSelectedLayer.Text := MapLayer.DisplayName
    else cbSelectedLayer.Text := '(none)';
  cbSelectedLayer.Hint := cbSelectedLayer.Text;

  // Update currently selected point
  PtTxt := '(none)';
  if HavePt then
  begin
    for P in MapView.EditMark.Selection.Points do
      Inc(PtCnt);
    P := TMapPoint(MapView.EditMark.Selection[0]);
    if PtCnt > 0 then
    begin
      PtTxt := P.DisplayName;
      if PtTxt <> P.ClassName then
        PtTxt := PtTxt + ': ' + P.ClassName;
      if PtCnt > 1 then
        PtTxt := PtTxt + Format(' +%d more', [PtCnt - 1]);
    end;
  end;
  cbSelectedPt.Text := PtTxt;
  cbSelectedPt.Hint := PtTxt;

  // Update actions
  actZoomIn.Enabled := HaveView and (MapView.Zoom < MapView.ZoomMax);
  actZoomOut.Enabled := HaveView and (MapView.Zoom > MapView.ZoomMin);
  actNewPOI.Enabled := HaveView and HaveLayer;
  actNewTrack.Enabled := HaveView and HaveLayer and InRange(PtCnt, 2, 10);
  actNewArea.Enabled := HaveView and HaveLayer and InRange(PtCnt, 3, 10);
  actNewTP.Enabled := HaveView and HavePt;
  actDelTP.Enabled := HaveView and HavePt;
  actSelect.Enabled := HaveView;
end;

procedure TMapViewerPathEditForm.PersistentAdded(APersistent: TPersistent;
  Select: Boolean);
begin
  ;
end;

procedure TMapViewerPathEditForm.DeletePersistent(APersistent: TPersistent);
begin
  ;
end;

procedure TMapViewerPathEditForm.UnselectPersistent(APersistent: TPersistent);
begin
  ;
end;

procedure TMapViewerPathEditForm.ObjectModified(AObject: TObject;
  PropName: ShortString);
begin
  ;
end;

end.

