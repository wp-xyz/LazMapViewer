{
  Plugin component and property editor. Adapted from TAChart.
}
unit mvPluginEditors;

{$MODE ObjFPC}{$H+}

interface

uses
  SysUtils, Math, Classes, LazLoggerBase,
  PropEdits, ComponentEditors, IDEImagesIntf,
  Forms, Menus, StdCtrls, ComCtrls;

type

  { TMvSubComponentListEditor }

  TMvSubComponentListEditor = class(TComponentEditor)
  protected
    function MakeEditorForm: TForm; virtual; abstract;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerbCount: Integer; override;
  end;

  { TMvComponentListPropertyEditor }

  TMvComponentListPropertyEditor = class(TPropertyEditor)
  protected
    function GetChildrenCount: Integer; virtual; abstract;
    function MakeEditorForm: TForm; virtual; abstract;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
  end;

  { TMvComponentListEditorForm }

  TMvComponentListEditorForm = class(TForm)
    ChildrenListBox: TListBox;
    menuAddItem: TPopupMenu;
    tbAdd: TToolButton;
    tbCommands: TToolBar;
    tbDelete: TToolButton;
    tbMoveDown: TToolButton;
    tbMoveUp: TToolButton;
    procedure ChildrenListBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAddClick(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
    procedure tbMoveDownClick(Sender: TObject);
    procedure tbMoveUpClick(Sender: TObject);
  private
    FComponentEditor: TMvSubComponentListEditor;
    FDesigner: TComponentEditorDesigner;
    FParent: TComponent;
    FPropertyEditor: TMvComponentListPropertyEditor;
    function FindChild(ACandidate: TPersistent; out AIndex: Integer): Boolean;
    procedure MoveSelection(AStart, ADir: Integer);
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; ASelect: Boolean);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure RefreshList;
    procedure SelectionChanged(AOrderChanged: Boolean = false);
  protected
    procedure AddSubcomponent(AParent, AChild: TComponent); virtual; abstract;
    procedure AddSubcomponentClass(const ACaption: String; ATag: Integer);
    procedure BuildCaption; virtual; abstract;
    function ChildClass: TComponentClass; virtual; abstract;
    procedure EnumerateSubcomponentClasses; virtual; abstract;
    function GetChildrenList: TFPList; virtual; abstract;
    function MakeSubcomponent(
      AOwner: TComponent; ATag: Integer): TComponent; virtual; abstract;
    property Parent: TComponent read FParent;
  public
    constructor Create(
      AOwner, AParent: TComponent; AComponentEditor: TMvSubComponentListEditor;
      APropertyEditor: TMvComponentListPropertyEditor); reintroduce;
    destructor Destroy; override;
  end;

type
  { TMvPluginManagerComponentEditor }

  TMvPluginManagerComponentEditor = class(TMvSubComponentListEditor)
  protected
    function MakeEditorForm: TForm; override;
  public
    function GetVerb(Index: Integer): string; override;
  end;

  { TMvPluginListPropertyEditor }

  TMvPluginListPropertyEditor = class(TMvComponentListPropertyEditor)
  protected
    function GetChildrenCount: Integer; override;
    function MakeEditorForm: TForm; override;
  end;

  { TMvPluginManagerEditorForm }

  TMvPluginManagerEditorForm = class(TMvComponentListEditorForm)
  protected
    procedure AddSubcomponent(AParent, AChild: TComponent); override;
    procedure BuildCaption; override;
    function ChildClass: TComponentClass; override;
    procedure EnumerateSubcomponentClasses; override;
    function GetChildrenList: TFPList; override;
    function MakeSubcomponent(
      AOwner: TComponent; ATag: Integer): TComponent; override;
  end;

implementation

{$R *.lfm}

uses
  mvPluginCommon;

{ TMvComponentListPropertyEditor }

procedure TMvComponentListPropertyEditor.Edit;
var
  propValue: TPersistent;
  editorForm: TForm;
begin
  propValue := GetComponent(0);
  if propValue = nil then
    raise Exception.Create('TComponentListPropertyEditor.Component=nil');
  editorForm := FindEditorForm(propValue) as TForm;
  if editorForm = nil then begin
    editorForm := MakeEditorForm;
    RegisterEditorForm(editorForm, propValue);
  end;
  editorForm.EnsureVisible;
end;

function TMvComponentListPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TMvComponentListPropertyEditor.GetValue: AnsiString;
var
  c: Integer;
begin
  c := GetChildrenCount;
  if c = 1 then
    Result := '1 item'
  else
    Result := IntToStr(c) + ' items';
end;


{ TMvSubComponentListEditor }

procedure TMvSubComponentListEditor.ExecuteVerb(Index: Integer);
var
  propValue: TPersistent;
  editorForm: TForm;
begin
  if Index <> 0 then exit;
  propValue := GetComponent;
  if propValue = nil then
    raise Exception.Create('TSubComponentListEditor.Component=nil');
  editorForm := FindEditorForm(propValue) as TForm;
  if editorForm = nil then begin
    editorForm := MakeEditorForm;
    RegisterEditorForm(editorForm, propValue);
  end;
  editorForm.ShowOnTop;
end;

function TMvSubComponentListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


{ TMvComponentListEditorForm }

constructor TMvComponentListEditorForm.Create(
  AOwner, AParent: TComponent; AComponentEditor: TMvSubComponentListEditor;
  APropertyEditor: TMvComponentListPropertyEditor);
begin
  inherited Create(AOwner);
  FParent := AParent;
  FComponentEditor := AComponentEditor;
  FPropertyEditor := APropertyEditor;
  if FComponentEditor <> nil then
    FDesigner := FComponentEditor.Designer
  else
    FDesigner := FindRootDesigner(FParent) as TComponentEditorDesigner;
  BuildCaption;
  EnumerateSubcomponentClasses;
  RefreshList;
  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
    GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
    GlobalDesignHook.AddHandlerGetSelection(@OnGetSelection);
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
    GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);
  end;
  SelectionChanged;
end;

destructor TMvComponentListEditorForm.Destroy;
begin
  UnregisterEditorForm(Self);
  inherited Destroy;
end;

procedure TMvComponentListEditorForm.AddSubcomponentClass(
  const ACaption: String; ATag: Integer);
var
  mi: TMenuItem;
begin
  if ACaption = '' then exit; // Empty names denote deprecated components.
  mi := TMenuItem.Create(Self);
  mi.OnClick := @miAddClick;
  mi.Caption := ACaption;
  mi.Tag := ATag;
  menuAddItem.Items.Add(mi);
end;

procedure TMvComponentListEditorForm.ChildrenListBoxClick(Sender: TObject);
begin
  SelectionChanged;
end;

function TMvComponentListEditorForm.FindChild(
  ACandidate: TPersistent; out AIndex: Integer): Boolean;
begin
  if ACandidate is ChildClass then
    AIndex := ChildrenListBox.Items.IndexOfObject(ACandidate)
  else
    AIndex := -1;
  Result := AIndex >= 0;
end;

procedure TMvComponentListEditorForm.FormClose(
  Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TMvComponentListEditorForm.FormCreate(Sender: TObject);
begin
  tbCommands.Images := IDEImages.Images_16;
  tbAdd.ImageIndex := IDEImages.LoadImage('laz_add');
  tbDelete.ImageIndex := IDEImages.LoadImage('laz_delete');
  tbMoveDown.ImageIndex := IDEImages.LoadImage('arrow_down');
  tbMoveUp.ImageIndex := IDEImages.LoadImage('arrow_up');
  tbAdd.Caption := 'Add';
  tbDelete.Caption := 'Delete';
  tbMoveUp.Caption := 'Up';
  tbMoveDown.Caption := 'Down';
end;

procedure TMvComponentListEditorForm.FormDestroy(Sender: TObject);
begin
  if GlobalDesignHook = Nil then
    Exit;
  if Assigned(FComponentEditor) and Assigned(FParent) and
     not (csDestroying in FParent.ComponentState) and
     (ChildrenListBox.SelCount > 0)
  then
    GlobalDesignHook.SelectOnlyThis(FParent);
  GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TMvComponentListEditorForm.miAddClick(Sender: TObject);
var
  s: TComponent;
  n: String;
begin
  s := MakeSubcomponent(FParent.Owner, (Sender as TMenuItem).Tag);
  try
    n := Copy(s.ClassName, 2, Length(s.ClassName) - 1);
    s.Name := FDesigner.CreateUniqueComponentName(FParent.Name + n);
    AddSubcomponent(FParent, s);
    FDesigner.PropertyEditorHook.PersistentAdded(s, true);
    FDesigner.Modified;
    RefreshList;
  except
    s.Free;
    raise;
  end;
end;

procedure TMvComponentListEditorForm.MoveSelection(AStart, ADir: Integer);
var
  i: Integer;
begin
  if not ChildrenListBox.SelCount = 0 then exit;
  i := AStart - ADir;
  with ChildrenListBox do
    while InRange(i, 0, Count - 1) and InRange(i + ADir, 0, Count - 1) do begin
      if Selected[i] and not Selected[i + ADir] then begin
        with TMvIndexedComponent(Items.Objects[i]) do
          Index := Index + ADir;
        Items.Move(i, i + ADir);
        Selected[i + ADir] := true;
        Selected[i] := false;
      end;
      i -= ADir;
    end;
  FDesigner.Modified;
  SelectionChanged(true);
end;

procedure TMvComponentListEditorForm.OnComponentRenamed(AComponent: TComponent);
var
  i: Integer;
begin
  if AComponent = nil then exit;
  if FindChild(AComponent, i) then
    ChildrenListBox.Items[i] := AComponent.Name
  else if AComponent = FParent then
    BuildCaption;
end;

procedure TMvComponentListEditorForm.OnGetSelection(
  const ASelection: TPersistentSelectionList);
var
  i: Integer;
begin
  if ASelection = nil then exit;
  ASelection.Clear;
  with ChildrenListBox do
    for i := 0 to Items.Count - 1 do
      if Selected[i] then
        ASelection.Add(TPersistent(Items.Objects[i]));
end;

procedure TMvComponentListEditorForm.OnPersistentAdded(
  APersistent: TPersistent; ASelect: Boolean);
var
  s: TComponent;
begin
  if (APersistent = nil) or not (APersistent is ChildClass) then exit;
  s := APersistent as TComponent;
  if s.GetParentComponent <> FParent then exit;
  with ChildrenListBox do
    Selected[Items.AddObject(s.Name, s)] := ASelect;
end;

procedure TMvComponentListEditorForm.OnPersistentDeleting(
  APersistent: TPersistent);
var
  i, wasSelected: Integer;
begin
  if not FindChild(APersistent, i) then exit;
  with ChildrenListBox do begin
    wasSelected := ItemIndex;
    Items.Delete(i);
    ItemIndex := Min(wasSelected, Count - 1);
  end;
end;

procedure TMvComponentListEditorForm.OnSetSelection(
  const ASelection: TPersistentSelectionList);
var
  i, j: Integer;
begin
  if ASelection = nil then exit;
  ChildrenListBox.ClearSelection;
  for i := 0 to ASelection.Count - 1 do
    if FindChild(ASelection.Items[i], j) then
      ChildrenListBox.Selected[j] := true;
end;

procedure TMvComponentListEditorForm.RefreshList;
var
  ci: TStrings;
  i: Integer;
begin
  ci := ChildrenListBox.Items;
  try
    ci.BeginUpdate;
    ci.Clear;
    with GetChildrenList do
      for i := 0 to Count - 1 do
        ci.AddObject(TComponent(Items[i]).Name, TObject(Items[i]));
  finally
    ci.EndUpdate;
  end;
end;

procedure TMvComponentListEditorForm.SelectionChanged(AOrderChanged: Boolean);
var
  sel: TPersistentSelectionList;
begin
  if GlobalDesignHook=nil then exit;
  GlobalDesignHook.RemoveHandlerSetSelection(@OnSetSelection);
  try
    sel := TPersistentSelectionList.Create;
    sel.ForceUpdate := AOrderChanged;
    try
      OnGetSelection(sel);
      FDesigner.PropertyEditorHook.SetSelection(sel);
    finally
      sel.Free;
    end;
  finally
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  end;
end;

procedure TMvComponentListEditorForm.tbDeleteClick(Sender: TObject);
begin
  if ChildrenListBox.SelCount = 0 then exit;
  FDesigner.DeleteSelection;
  SelectionChanged;
end;

procedure TMvComponentListEditorForm.tbMoveDownClick(Sender: TObject);
begin
  MoveSelection(ChildrenListBox.Count - 1, 1);
end;

procedure TMvComponentListEditorForm.tbMoveUpClick(Sender: TObject);
begin
  MoveSelection(0, -1);
end;

{==============================================================================}


{ TMvPluginManagerComponentEditor }

function TMvPluginManagerComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'Edit plugin'
  else
    Result := '';
end;

function TMvPluginManagerComponentEditor.MakeEditorForm: TForm;
begin
  Result := TMvPluginManagerEditorForm.Create(Application, GetComponent, Self, nil);
end;


{ TMvPluginListPropertyEditor }

function TMvPluginListPropertyEditor.GetChildrenCount: Integer;
begin
  Result := (GetObjectValue as TMvPluginList).Count;
end;

function TMvPluginListPropertyEditor.MakeEditorForm: TForm;
begin
  with TMvPluginManagerEditorForm do
    Result := Create(Application, GetComponent(0) as TComponent, nil, Self);
end;


{ TMvPluginManagerEditorForm }

procedure TMvPluginManagerEditorForm.AddSubcomponent(AParent, AChild: TComponent);
begin
  (AChild as TMvCustomPlugin).PluginManager := (AParent as TMvPluginManager);
end;

procedure TMvPluginManagerEditorForm.BuildCaption;
begin
  Caption := 'Edit plugin - ' + Parent.Name;
end;

function TMvPluginManagerEditorForm.ChildClass: TComponentClass;
begin
  Result := TMvCustomPlugin;
end;

procedure TMvPluginManagerEditorForm.EnumerateSubcomponentClasses;
var
  i: Integer;
begin
  for i := 0 to PluginClassRegistry.Count - 1 do
    AddSubcomponentClass(PluginClassRegistry.GetCaption(i), i);
end;

function TMvPluginManagerEditorForm.GetChildrenList: TFPList;
begin
  Result := (Parent as TMvPluginManager).PluginList;
end;

function TMvPluginManagerEditorForm.MakeSubcomponent(AOwner: TComponent;
  ATag: Integer): TComponent;
begin
  Result := TMvCustomPluginClass(PluginClassRegistry.GetClass(ATag)).Create(AOwner);
end;


end.

