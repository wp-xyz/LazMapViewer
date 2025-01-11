{
  Property editors for LazMapViewer
  Copyright (C) 2019 user alpine at Lazarus forum https://forum.lazarus.freepascal.org

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}
unit
  mvMapViewerPropEdits;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, SysUtils, LCLIntf, LCLType, ImgList, LazLoggerBase,
  PropEdits, GraphPropEdits, ComponentEditors,
  mvMapProvider;

type

  { TMapViewComponentEditor }

  TMapViewComponentEditor = class(TComponentEditor)
  private
    FOldDirtyHandler: TNotifyEvent;
  protected
    procedure DoShowPointsEditor;
    procedure DoShowLayerEditor;
  public
    constructor Create(AComponent: TComponent;
      ADesigner: TComponentEditorDesigner); override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

  { TMapLayersPropertyEditor }

  TMapLayersPropertyEditor = class(TCollectionPropertyEditor)
  public
    class function ShowCollectionEditor(ACollection: TCollection;
      OwnerPersistent: TPersistent; const PropName: String): TCustomForm;
      override;
  end;

  { TMapTrackPointsPropertyEditor }

  TMapTrackPointsPropertyEditor = class(TCollectionPropertyEditor)
  public
    class function ShowCollectionEditor(ACollection: TCollection;
      OwnerPersistent: TPersistent; const PropName: String): TCustomForm;
      override;
  end;

  { TMapProviderPropertyEditor }

  TMapProviderPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: AnsiString; override;
    procedure SetValue(const NewValue: AnsiString); override;
  end;

  { TPointOfInterestImageIndexPropertyEditor }

  TPointOfInterestImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

  { TPointDateTimePropertyEditor }

  TPointDateTimePropertyEditor = class(TDateTimePropertyEditor)
  public
    function GetValue: string; override;
    procedure SetValue(const NewValue: AnsiString); override;
  end;

  { TPointElevationPropertyEditor }

  TPointElevationPropertyEditor = class(TFloatPropertyEditor)
  public
    function GetValue: string; override;
    procedure SetValue(const NewValue: AnsiString); override;
  end;

  { TLatLonDMSPropertyEditor }

  TLatLonDMSPropertyEditor = class(TFloatPropertyEditor)
  private
    function UseDMS: Boolean;
  public
    function GetValue: string; override;
    procedure SetValue(const NewValue: AnsiString); override;
  end;

implementation

uses
  System.UITypes, Dialogs, IDEWindowIntf, LazIDEIntf, StrUtils, mvMapViewer,
  mvGpsObj, mvLayersPropEditForm, mvEngine, mvGeoMath,
  mvMapViewerPathEditDsgForm, mvPluginCommon;

const
  sNONE = '(none)';
  sEDITMapView = 'Edit MapView Points';
  sLayerEditor = 'Layer Editor...';

{ TMapTrackPointsPropertyEditor }

class function TMapTrackPointsPropertyEditor.ShowCollectionEditor(
  ACollection: TCollection; OwnerPersistent: TPersistent; const PropName: String
  ): TCustomForm;
begin
  Result := inherited ShowCollectionEditor(ACollection, OwnerPersistent, PropName
    );
end;

{ TMapViewComponentEditor }

constructor TMapViewComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
end;

procedure TMapViewComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: DoShowLayerEditor;
    1: DoShowPointsEditor;
  end;
end;

function TMapViewComponentEditor.GetVerb(Index: Integer): String;
begin
  Result := '';
  case Index of
    0: Result := sLayerEditor;
    1: Result := sEDITMapView;
  end;
end;

function TMapViewComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TMapViewComponentEditor.DoShowPointsEditor;

  function FindPopupPoint(AMapView: TMapView): TPoint;
  var
    R: TRect;
    Orig: TPoint;  // Top/left corner of map in screen coordinates
    P: TPoint;     // EditMark position in screen coordinates
    W, H: Integer; // Width and heigh tof editor form
  begin
    R := Screen.WorkAreaRect;
    W := MapViewerPathEditDsgForm.Width;
    H := MapViewerPathEditDsgForm.Height;
    Orig := AMapView.ClientToScreen(Point(0, 0));
    Result := Orig;

    // Make sure that the editor form is fully visible vertically
    if Result.Y < R.Top then
      Result.Y := R.Top
    else
    if Result.Y + H > R.Bottom then
      Result.Y := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);

    // Check position at the right of the map
    Result.X := Orig.X + AMapView.Width;
    if Result.X + W <= R.Right then
      exit;

    // Check position at the left of the map
    Result.X := Orig.X - W;
    if Result.X >= R.Left then
      exit;

    if AMapView.EditMark.Visible then
    begin
      // Check position inside. Make sure that the EditMark is not covered by
      // the editor form.
      P := AMapView.LatLonToScreen(AMapView.EditMark.RealPt);
      // Check form position at the right of the EditMark
      Result.X := Orig.X + P.X + 10;
      if Result.X <= R.Right then
        exit;
      // Check form position at the left of the EditMark
      Result.X := Orig.X + P.X - W - 10;
      if Result.X >= R.Left then
        exit;
    end;

    // Fallback: center
    Result.X := Orig.X + (AMapView.Width - W) div 2;
  end;

var
  V: TMapView;
  P: TPoint;
begin
  if not Assigned(MapViewerPathEditDsgForm) then
     MapViewerPathEditDsgForm := TMapViewerPathEditDsgForm.Create(LazarusIDE.OwningComponent);
  V := GetComponent as TMapView;
  if not (mvoEditorEnabled in V.Options) then
     case QuestionDlg(V.ClassName,
       V.Name + ' doesn''t have mvoEditorEnabled set in Options.' + LineEnding +
       LineEnding +
       'Do you want to enable the editor?',
       mtWarning,
       [mrYes, '&Enable and continue', mrCancel, '&Cancel', 'IsDefault', 'IsCancel'], 0 )
     of
       mrYes: V.Options := V.Options + [mvoEditorEnabled];
     otherwise
       Exit;
     end;
  P := FindPopupPoint(V);
  MapViewerPathEditDsgForm.MapView := V;
  MapViewerPathEditDsgForm.Left := P.X;
  MapViewerPathEditDsgForm.Top := P.Y;
  MapViewerPathEditDsgForm.Show;
end;

procedure TMapViewComponentEditor.DoShowLayerEditor;
var
  V: TMapView;
begin
  V := GetComponent as TMapView;
  if LayersPropertyEditForm = nil then
    LayersPropertyEditForm := TLayersPropertyEditForm.Create(Application);
  LayersPropertyEditForm.SetCollection(V.Layers, V, 'Layers');
  LayersPropertyEditForm.actAdd.Visible := true;
  LayersPropertyEditForm.actDel.Visible := true;
  LayersPropertyEditForm.AddButton.Left := 0;
  LayersPropertyEditForm.DeleteButton.Left := 1;
  LayersPropertyEditForm.DividerToolButton.Show;
  LayersPropertyEditForm.DividerToolButton.Left := LayersPropertyEditForm.DeleteButton.Left + 1;
  SetPopupModeParentForPropertyEditor(LayersPropertyEditForm);
  LayersPropertyEditForm.EnsureVisible;
  LayersPropertyEditForm.UpdateButtons;
  //Result := LayersPropertyEditForm;
end;


{ TMapLayersPropertyEditor }

class function TMapLayersPropertyEditor.ShowCollectionEditor(
  ACollection: TCollection; OwnerPersistent: TPersistent; const PropName: String
  ): TCustomForm;
begin
  if LayersPropertyEditForm = Nil then
    LayersPropertyEditForm := TLayersPropertyEditForm.Create(Application);
  LayersPropertyEditForm.SetCollection(ACollection, OwnerPersistent, PropName);
  LayersPropertyEditForm.actAdd.Visible := true;
  LayersPropertyEditForm.actDel.Visible := true;
  LayersPropertyEditForm.AddButton.Left := 0;
  LayersPropertyEditForm.DeleteButton.Left := 1;
  LayersPropertyEditForm.DividerToolButton.Show;
  LayersPropertyEditForm.DividerToolButton.Left := LayersPropertyEditForm.DeleteButton.Left + 1;
  SetPopupModeParentForPropertyEditor(LayersPropertyEditForm);
  LayersPropertyEditForm.EnsureVisible;
  LayersPropertyEditForm.UpdateButtons;
  Result := LayersPropertyEditForm;
end;

{ TPointElevationPropertyEditor }

function TPointElevationPropertyEditor.GetValue: string;
begin
  Result := inherited GetValue;
  if GetFloatValue = NO_ELE then
    Result := sNONE;
end;

procedure TPointElevationPropertyEditor.SetValue(const NewValue: AnsiString);
begin
  if (NewValue = sNONE) or (NewValue = '')
    then inherited SetFloatValue(NO_ELE)
    else inherited SetValue(NewValue);
end;

{ TLatLonDMSPropertyEditor }

function TLatLonDMSPropertyEditor.UseDMS: Boolean;
var
  Inst: TPersistent;
begin
  Inst := GetComponent(0);
  Result := (Inst is TMapPoint) and TMapPoint(Inst).LatLonInDMS
    or (Inst is TMapCenter) and TMapCenter(Inst).LatLonInDMS;
end;

function TLatLonDMSPropertyEditor.GetValue: string;
begin
  if not UseDMS
    then Result := inherited GetValue
    else if StartsText('Lat', GetName)
      then Result := LatToStr(GetFloatValue, True)
      else Result := LonToStr(GetFloatValue, True);
end;

procedure TLatLonDMSPropertyEditor.SetValue(const NewValue: AnsiString);
var
  Deg: Double;
begin
  if UseDMS and TryStrDMSToDeg(NewValue, Deg)
    then SetFloatValue(Deg)
    else inherited SetValue(NewValue);
end;

{ TPointDateTimePropertyEditor }

function TPointDateTimePropertyEditor.GetValue: string;
begin
  Result := inherited GetValue;
  if GetFloatValue = NO_DATE then
    Result := sNONE;
end;

procedure TPointDateTimePropertyEditor.SetValue(const NewValue: AnsiString);
begin
  if (NewValue = sNONE) or (NewValue = '')
    then inherited SetFloatValue(NO_DATE)
    else inherited SetValue(NewValue);
end;

{ TPointOfInterestImageIndexPropertyEditor }

function TPointOfInterestImageIndexPropertyEditor.GetImageList: TCustomImageList;
var
  P: TPersistent;
begin
  Result := Nil;
  P := GetComponent(0);
  if (P is TMapPointOfInterest) then
  begin
    Result := TMapPointOfInterest(P).View.POIImages;
  end;
end;

{ TMapProviderPropertyEditor }

function TMapProviderPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paPickList, paRevertable];
end;

procedure TMapProviderPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Providers: TStringList;
  S: String;
  Inst: TPersistent;
  MV: TMapView;
  {%H-}Filter: Boolean = False;
  {%H-}PT: TProjectionType;
begin
  Inst := GetComponent(0);
  Providers := TStringList.Create;
  try
    if Inst is TMapView then
      MV := TMapView(Inst)
    else if Inst is TMapLayer then
    begin
      MV := TMapLayer(Inst).View;
      Filter := True;
      PT := MV.Engine.MapProjectionType;
    end
    else
      Exit;
    MV.Engine.GetMapProviders(Providers);
    //if not (Inst is TMapView) then
      Proc(sNONE);
    Providers.Sort;
    for S in Providers do
      // TODO: When filtered it is not clear what is the full list.
      //if not Filter or (PT = MV.Engine.MapProviderByName(S).ProjectionType) then
        Proc(S);
  finally
    Providers.Free;
  end;
end;

function TMapProviderPropertyEditor.GetValue: AnsiString;
begin
  Result := inherited GetValue;
  if Result = '' then
    Result := sNONE;
end;

procedure TMapProviderPropertyEditor.SetValue(const NewValue: AnsiString);
begin
  if NewValue = sNONE
    then inherited SetValue('')
    else inherited SetValue(NewValue);
end;

end.

