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
  Classes, Forms, SysUtils, PropEdits, GraphPropEdits, ComponentEditors,
  ImgList, mvMapProvider;

type

  { TMapViewComponentEditor }

  TMapViewComponentEditor = class(TComponentEditor)
    FOldDirtyHandler: TNotifyEvent;
  public
    constructor Create(AComponent: TComponent;
      ADesigner: TComponentEditorDesigner); override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
    procedure DoEdit;
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
  mvMapViewerPathEditDsgForm, mvPluginCore;

const
  sNONE = '(none)';
  sEDITMapView = 'Edit MapView Points';

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
    0: DoEdit;
  end;
end;

function TMapViewComponentEditor.GetVerb(Index: Integer): String;
begin
  Result := '';
  case Index of
    0: Result := sEDITMapView;
  end;
end;

function TMapViewComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TMapViewComponentEditor.DoEdit;
var
  V: TMapView;
  P: TPoint;
begin
  if not Assigned(MapViewerPathEditDsgForm) then
     MapViewerPathEditDsgForm := TMapViewerPathEditDsgForm.Create(
       LazarusIDE.OwningComponent);
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
  MapViewerPathEditDsgForm.MapView := V;
  P := V.ControlToScreen(Point(V.Width, 0));
  MapViewerPathEditDsgForm.Left := P.X;
  MapViewerPathEditDsgForm.Top := P.Y;
  MapViewerPathEditDsgForm.Show;
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
  if (P is TPointOfInterest) then
  begin
    Result := TPointOfInterest(P).View.POIImages;
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

