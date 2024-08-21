{******************************************************************************}
{                           Sanguine PJ Editor                                 }

{   Copyright (C) 2024
    La Serpiente y la Rosa Producciones.                                       }

{   This file is part of Sanguine PJ Editor.                                   }

{   Sanguine PJ Editor is free software: you can redistribute it
    and/or modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation, either version 3 of the License,
    or (at your option) any later version.                                     }

{   Sanguine PJ Editor is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public License for more details.                       }

{   You should have received a copy of the GNU General Public License
    along with Sanguine PJ Editor.
    If not, see <http://www.gnu.org/licenses/>.                                }
{******************************************************************************}

unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Menus, ActnList, ComCtrls, StdActns, ExtCtrls, Grids, Classes,
  SysUtils, FpJson, Controls, Buttons, Dialogs, GSet, GUtil, StdCtrls;

type

  TLessInt = specialize TLess<integer>;
  TIntegerSet = specialize TSet<integer, TLessInt>;

  { TfrmMain }

  TfrmMain = class(TForm)
    chkgrpTags: TCheckGroup;
    DataDiscardChanges: TAction;
    DataCommitChanges: TAction;
    imgInstructions: TImage;
    lbledModuleDescription: TLabeledEdit;
    lbledModuleKeywords: TLabeledEdit;
    lbledModuleManualURL: TLabeledEdit;
    lbledModuleModularGridURL: TLabeledEdit;
    lbledModuleName: TLabeledEdit;
    lbledModuleSlug: TLabeledEdit;
    mnuProgramOptions: TMenuItem;
    mnuProgram: TMenuItem;
    ProgramOptions: TAction;
    FileSafeOpen: TAction;
    FileSaveAs: TFileSaveAs;
    HelpOpenManifestManual: TAction;
    lbledPluginAuthor: TLabeledEdit;
    lbledPluginAuthorEmail: TLabeledEdit;
    lbledPluginAuthorURL: TLabeledEdit;
    lbledPluginBrand: TLabeledEdit;
    lbledPluginChangelogURL: TLabeledEdit;
    lbledPluginDescription: TLabeledEdit;
    lbledPluginDonateURL: TLabeledEdit;
    lbledPluginLicense: TLabeledEdit;
    lbledPluginManualURL: TLabeledEdit;
    lbledPluginMinRackVersion: TLabeledEdit;
    lbledPluginName: TLabeledEdit;
    lbledPluginSlug: TLabeledEdit;
    lbledPluginSourceURL: TLabeledEdit;
    lbledPluginURL: TLabeledEdit;
    lbledPluginVersion: TLabeledEdit;
    OpenDialog: TOpenDialog;
    scrlboxModuleInfo: TScrollBox;
    scrlboxModuleTags: TScrollBox;
    scrlboxPlugin: TScrollBox;
    Separator6: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    Separator5: TMenuItem;
    mnuHelpManifestManual: TMenuItem;
    Separator4: TMenuItem;
    mnuFileNew: TMenuItem;
    FileNew: TAction;
    ModuleRemove: TAction;
    ModuleAdd: TAction;
    HelpOpenAbout: TAction;
    HelpOpenLicense: TAction;
    ActionList: TActionList;
    EditCopy: TEditCopy;
    EditCut: TEditCut;
    EditPaste: TEditPaste;
    FileExit: TFileExit;
    MainMenu: TMainMenu;
    mnuHelpLicense: TMenuItem;
    pnlModule: TPanel;
    pgctrlPlugin: TPageControl;
    pnlModuleList: TPanel;
    Separator3: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuFileOpen: TMenuItem;
    Separator2: TMenuItem;
    mnuEditPaste: TMenuItem;
    Separator1: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFile: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    strgrdModules: TStringGrid;
    tabPlugin: TTabSheet;
    tabModules: TTabSheet;
    tbAddModule: TToolButton;
    tbarModuleList: TToolBar;
    tbRemoveModule: TToolButton;
    ToolBarCommon: TToolBar;
    tbOpen: TToolButton;
    tbCopy: TToolButton;
    tbCut: TToolButton;
    tbSeparator2: TToolButton;
    tbPaste: TToolButton;
    tbNew: TToolButton;
    tbSaveAs: TToolButton;
    tbSeparator1: TToolButton;
    tbSeparator3: TToolButton;
    tbSeparator4: TToolButton;
    tbOptions: TToolButton;
    ToolButton1: TToolButton;
    tbCommitChanges: TToolButton;
    tbDiscardChanges: TToolButton;
    procedure chkgrpTagsItemClick(Sender: TObject; Index: integer);
    procedure DataCommitChangesExecute(Sender: TObject);
    procedure DataDiscardChangesExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileSafeOpenExecute(Sender: TObject);
    procedure FileSaveAsAccept(Sender: TObject);
    procedure FileSaveAsBeforeExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HandleModuleEditorsChange(Sender: TObject);
    procedure HandlePluginEditorsChange(Sender: TObject);
    procedure HelpOpenAboutExecute(Sender: TObject);
    procedure HelpOpenLicenseExecute(Sender: TObject);
    procedure HelpOpenManifestManualExecute(Sender: TObject);
    procedure ModuleAddExecute(Sender: TObject);
    procedure ModuleRemoveExecute(Sender: TObject);
    procedure ProgramOptionsExecute(Sender: TObject);
    procedure StatusBarDrawPanel(TheStatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure strgrdModulesAfterSelection(Sender: TObject; aCol, aRow: integer);
    procedure strgrdModulesBeforeSelection(Sender: TObject; aCol, aRow: integer);
    procedure strgrdModulesCheckboxToggled(Sender: TObject;
      aCol, aRow: integer; aState: TCheckboxState);
    procedure strgrdModulesColRowDeleted(Sender: TObject; IsColumn: boolean;
      sIndex, tIndex: integer);
  protected
    procedure PrintParserError;
  private
    FErrorModules: TIntegerSet;
    FNoNameCount: integer;
    FConfigFileName: string;
    FDefaultAuthor: string;
    FDefaultLicense: string;
    FFileName: string;
    FChangesCommited: boolean;
    FInvalidPluginInfo: boolean;
    FModified: boolean;
    FModuleGridMutex: boolean;
    FPluginBase: TJSONObject;
    FRootWorking: TJSONObject;
    FModulesWorking: TJSONArray;
    procedure ChangePanelText(const PanelNum: integer; const PanelText: string);
    procedure ChangeWindowCaption(const TheFileName: string);
    function CheckCanDeleteModule: boolean;
    function CheckValidPlugin: boolean;
    function CheckValidModule: boolean;
    procedure ClearData;
    procedure ClearModuleInfo;
    procedure ClearPluginInfo;
    function DoModifiedQuery: boolean;
    procedure FillModuleData;
    procedure FillModuleList;
    procedure FillPluginData;
    procedure FillTags(Tags: TJSONArray);
    procedure FillTextBox(TheRoot: TJsonData; const Path: TJSONStringType;
      LabelEd: TLabeledEdit);
    function FindData(TheRoot: TJSONData; const Path: TJSONStringType;
      out Value: variant; const VariantKind: integer): boolean;
    procedure FreeObjects;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure SetModified(const IsModified: boolean);
    procedure SetTextBox(const Value: string; LabelEd: TLabeledEdit);
    procedure ToggleGUI(const EnableGUI: boolean);
    procedure UpdateBooleanField(TheRoot: TJSONObject; const Path: TJSONStringType;
      AValue: boolean);
    procedure UpdateStringField(TheRoot: TJSONObject; const Path: TJSONStringType;
      LabelEd: TLabeledEdit);
    procedure UpdateModuleCount;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Variants, ModuleTagging, LCLIntf, FormAbout, PJStrings, GUITools,
  FormNewPlugin, SanguineJSONReader, FormNewModule, BatJSONConf, LCLType,
  FormOptions, Graphics;

  {$R *.lfm}

const
  sURLLicense = 'http://www.gnu.org/licenses/gpl-3.0.html';
  sURLManifestManual = 'https://vcvrack.com/manual/Manifest';

  sApplicationName = 'Sanguine PJ Editor - %s';
  sDefaultFileName = 'noname%d.json';

  sModuleCount = 'Modules: %.2d';

  sConfigFileName = '.pjeditor.cfg';

  sConfigWindowX = 'wndx';
  sConfigWindowY = 'wndy';
  sConfigWindowHeight = 'wndheight';
  sConfigWindowWidth = 'wndwidth';
  sConfigWindowState = 'wndstate';
  sConfigModuleListWidth = 'modulelistwidth';
  sConfigTagsHeight = 'tagsheight';
  sConfigDefaultAuthor = 'defaultauthor';
  sConfigDefaultLicense = 'defaultlicense';

  cAsterisk = '*';

  cValueFalse = '0';
  cValueTrue = '1';
  ArrayCheckboxValues: array[boolean] of char = (
    cValueFalse,
    cValueTrue
    );

  ArrayChecboxBooleans: array[cbUnchecked..cbChecked] of boolean = (
    False,
    True
    );

  iStatusPanelState = 0;
  iStatusPanelErrors = 1;
  iStatusPanelModified = 2;
  iStatusPanelModuleCount = 3;

  iColumnSlug = 0;
  iColumnHidden = 1;

  { TfrmMain }

procedure TfrmMain.FileNewExecute(Sender: TObject);
var
  frmNewPlugin: TfrmNewPlugin;
  NewPluginData: TJSONObject;
begin
  if DoModifiedQuery then
  begin
    FModuleGridMutex := True;
    frmNewPlugin := TfrmNewPlugin.Create(nil);
    try
      frmNewPlugin.lbledPluginAuthor.Text := FDefaultAuthor;
      frmNewPlugin.lbledPluginLicense.Text := FDefaultLicense;
      if frmNewPlugin.ShowModal = mrOk then
      begin
        FreeObjects;
        FErrorModules := TIntegerSet.Create;
        ClearData;
        SetTextBox(frmNewPlugin.lbledPluginSlug.Text, lbledPluginSlug);
        SetTextBox(frmNewPlugin.lbledPluginName.Text, lbledPluginName);
        SetTextBox(frmNewPlugin.lbledPluginAuthor.Text, lbledPluginAuthor);
        SetTextBox(frmNewPlugin.lbledPluginVersion.Text, lbledPluginVersion);
        SetTextBox(frmNewPlugin.lbledPluginLicense.Text, lbledPluginLicense);
        NewPluginData := TJSONObject.Create([ArrayManifestKeywords[iSlug],
          lbledPluginSlug.Text, ArrayManifestKeywords[iName],
          lbledPluginName.Text, ArrayManifestKeywords[iAuthor],
          lbledPluginAuthor.Text, ArrayManifestKeywords[iVersion],
          lbledPluginVersion.Text, ArrayManifestKeywords[iLicense],
          lbledPluginLicense.Text]);
        FRootWorking := NewPluginData;
        FPluginBase := FRootWorking.Clone as TJSONObject;
        Inc(FNoNameCount);
        FFileName := Format(sDefaultFileName, [FNoNameCount]);
        ToggleGUI(True);
        ChangeWindowCaption(FFileName);
        SetModified(True);
        UpdateModuleCount;
        FInvalidPluginInfo := False;
        FChangesCommited := True;
      end;
    finally
      frmNewPlugin.Free;
    end;
    FModuleGridMutex := False;
  end;
end;

procedure TfrmMain.chkgrpTagsItemClick(Sender: TObject; Index: integer);
var
  i: integer;
  ModuleData: TJSONObject;
  Tags: TJSONArray;
begin
  if FModuleGridMutex then
    Exit;
  ModuleData := FModulesWorking.Items[strgrdModules.Row - 1] as TJSONObject;
  Tags := ModuleData.FindPath(ArrayManifestKeywords[iTags]) as TJSONArray;
  if Tags <> nil then
  begin
    ModuleData.Delete(ArrayManifestKeywords[iTags]);
    Tags := nil;
  end;

  Tags := TJSONArray.Create;

  for i := 0 to chkgrpTags.Items.Count - 1 do
    if chkgrpTags.Checked[i] then
      Tags.Add(chkgrpTags.Items[i]);

  if Tags.Count > 0 then
    ModuleData.Add(ArrayManifestKeywords[iTags], Tags);
  SetModified(True);
  FChangesCommited := False;
end;

procedure TfrmMain.DataCommitChangesExecute(Sender: TObject);
begin
  FModuleGridMutex := True;
  FPluginBase.Free;
  FPluginBase := FRootWorking.Clone as TJSONObject;
  FChangesCommited := True;
  FModuleGridMutex := False;
end;

procedure TfrmMain.DataDiscardChangesExecute(Sender: TObject);
begin
  FModuleGridMutex := True;
  ClearData;
  FRootWorking.Free;
  FRootWorking := FPluginBase.Clone as TJSONObject;
  FillPluginData;
  FModulesWorking := FRootWorking.FindPath(ArrayManifestKeywords[iModules]) as
    TJSONArray;
  if FModulesWorking <> nil then
  begin
    FillModuleList;
    strgrdModules.Row := 1;
    strgrdModulesAfterSelection(Self, strgrdModules.Col, strgrdModules.Row);
  end;
  UpdateModuleCount;
  FInvalidPluginInfo := False;
  FErrorModules.Free;
  FErrorModules := TIntegerSet.Create;
  FChangesCommited := True;
  SetModified(True);
  FModuleGridMutex := False;
end;

procedure TfrmMain.FileSafeOpenExecute(Sender: TObject);
var
  JData: TJSONData = nil;
begin
  if DoModifiedQuery then
  begin
    FModuleGridMutex := True;
    if OpenDialog.Execute then
    begin
      Cursor := crHourGlass;
      ChangePanelText(iStatusPanelState, rsStatusLoading);
      Application.ProcessMessages;
      ClearData;
      FreeObjects;
      FErrorModules := TIntegerSet.Create;
      ReadJSON(OpenDialog.FileName, JData, @PrintParserError);
      if JData <> nil then
        FRootWorking := JData as TJSONObject;
      if FRootWorking <> nil then
      begin
        ToggleGUI(True);
        FPluginBase := FRootWorking.Clone as TJSONObject;
        FillPluginData;
        FModulesWorking := FRootWorking.FindPath(ArrayManifestKeywords[iModules]) as
          TJSONArray;
        if FModulesWorking <> nil then
        begin
          FillModuleList;
          strgrdModules.Row := 1;
          strgrdModulesAfterSelection(Self, strgrdModules.Col, strgrdModules.Row);
        end;
        FFileName := OpenDialog.FileName;
        ChangeWindowCaption(FFileName);
        UpdateModuleCount;

      end;
      ChangePanelText(iStatusPanelState, rsStatusReady);
      Cursor := crDefault;
    end;
    FInvalidPluginInfo := False;
    FChangesCommited := True;
    SetModified(False);
    FModuleGridMutex := False;
  end;
end;

procedure TfrmMain.FileSaveAsAccept(Sender: TObject);
var
  OutString: string;
  MemoryStream: TMemoryStream;
begin
  Cursor := crHourGlass;
  ChangePanelText(iStatusPanelState, rsStatusSaving);
  Application.ProcessMessages;

  MemoryStream := TMemoryStream.Create;
  try
    OutString := FPluginBase.FormatJSON;
    MemoryStream.WriteBuffer(OutString[1], Length(OutString));
    MemoryStream.SaveToFile(FileSaveAs.Dialog.FileName);
    FFileName := FileSaveAs.Dialog.FileName;
    ChangeWindowCaption(FFileName);
    SetModified(False);
  finally
    MemoryStream.Free;
  end;

  ChangePanelText(iStatusPanelState, rsStatusReady);
  Cursor := crDefault;
end;

procedure TfrmMain.FileSaveAsBeforeExecute(Sender: TObject);
begin
  if FFileName <> EmptyStr then
  begin
    FileSaveAs.Dialog.InitialDir := ExtractFileDir(FFileName);
    FileSaveAs.Dialog.FileName := FFileName;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := DoModifiedQuery;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  chkgrpTags.Items.Clear;
  for i := 0 to Length(ModuleTags) - 1 do
    chkgrpTags.Items.Add(ModuleTags[i]);
  FNoNameCount := 0;
  ChangePanelText(iStatusPanelState, rsStatusReady);
  FFileName := '';
  FConfigFileName := IncludeTrailingPathDelimiter(GetUserDir) + sConfigFileName;
  if FileExists(FConfigFileName) then
  begin
    Position := poDesigned;
    LoadConfig;
  end
  else
    Position := poDefault;
  FErrorModules := TIntegerSet.Create;
  FChangesCommited := True;
  SetModified(False);
  ToggleGUI(False);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SaveConfig;
  FreeObjects;
  inherited;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if imgInstructions.Visible then
  begin
    imgInstructions.Left := (Width div 2) - (imgInstructions.Width div 2);
    imgInstructions.Top := (Height div 2) - (imgInstructions.Height div 2);
  end;
end;

procedure TfrmMain.HandleModuleEditorsChange(Sender: TObject);
var
  LabelEd: TLabeledEdit;
  ModuleData: TJSONObject;
begin
  if FModuleGridMutex then
    Exit;
  ModuleData := FModulesWorking.Items[strgrdModules.Row - 1] as TJSONObject;
  LabelEd := Sender as TLabeledEdit;
  UpdateStringField(ModuleData, ArrayManifestKeywords[LabelEd.Tag], LabelEd);
  CheckValidModule;
  SetModified(True);
  FChangesCommited := False;
end;

procedure TfrmMain.HandlePluginEditorsChange(Sender: TObject);
var
  LabelEd: TLabeledEdit;
begin
  LabelEd := Sender as TLabeledEdit;
  UpdateStringField(FRootWorking, ArrayManifestKeywords[LabelEd.Tag], LabelEd);
  CheckValidPlugin;
  SetModified(True);
  FChangesCommited := False;
end;

procedure TfrmMain.HelpOpenAboutExecute(Sender: TObject);
var
  frmAbout: TfrmAbout;
begin
  frmAbout := TfrmAbout.Create(nil);
  try
    frmAbout.ShowModal;
  finally
    frmAbout.Free;
  end;
end;

procedure TfrmMain.HelpOpenLicenseExecute(Sender: TObject);
begin
  OpenURL(sURLLicense);
end;

procedure TfrmMain.HelpOpenManifestManualExecute(Sender: TObject);
begin
  OpenURL(sURLManifestManual);
end;

procedure TfrmMain.ModuleAddExecute(Sender: TObject);
var
  ItemId: integer;
  frmNewModule: TfrmNewModule;
  JArray: TJSONArray;
  JObject: TJSONObject;
begin
  frmNewModule := TfrmNewModule.Create(nil);
  try
    if frmNewModule.ShowModal = mrOk then
    begin
      FModuleGridMutex := True;
      ClearModuleInfo;
      SetTextBox(frmNewModule.lbledModuleSlug.Text, lbledModuleSlug);
      SetTextBox(frmNewModule.lbledModuleName.Text, lbledModuleName);
      SetModified(True);
      FChangesCommited := False;
      if FModulesWorking = nil then
      begin
        JArray := TJSONArray.Create;
        ItemId := FRootWorking.Add(ArrayManifestKeywords[iModules], JArray);
        FModulesWorking := FRootWorking.Items[ItemId] as TJSONArray;
      end;
      JObject := TJSONObject.Create([ArrayManifestKeywords[iSlug],
        lbledModuleSlug.Text, ArrayManifestKeywords[iName], lbledModuleName.Text]);
      FModulesWorking.Add(JObject);
      strgrdModules.InsertRowWithValues(strgrdModules.RowCount,
        [lbledModuleSlug.Text, ArrayCheckboxValues[False]]);
      strgrdModules.Row := strgrdModules.RowCount - 1;
      UpdateModuleCount;
      FModuleGridMutex := False;
    end;
  finally
    frmNewModule.Free;
  end;
end;

procedure TfrmMain.ModuleRemoveExecute(Sender: TObject);
begin
  FModuleGridMutex := True;
  FModulesWorking.Delete(strgrdModules.Row - 1);
  strgrdModules.DeleteRow(strgrdModules.Row);
  if strgrdModules.Row > 0 then
    strgrdModulesAfterSelection(Self, strgrdModules.Col, strgrdModules.Row)
  else
    ClearModuleInfo;
  UpdateModuleCount;
  SetModified(True);
  FChangesCommited := False;
  FModuleGridMutex := False;
end;

procedure TfrmMain.ProgramOptionsExecute(Sender: TObject);
var
  frmOptions: TfrmOptions;
begin
  frmOptions := TfrmOptions.Create(nil);
  try
    frmOptions.lbledDefaultAuthor.Text := FDefaultAuthor;
    frmOptions.lbledDefaultLicense.Text := FDefaultLicense;
    if frmOptions.ShowModal = mrOk then
    begin
      FDefaultAuthor := frmOptions.lbledDefaultAuthor.Text;
      FDefaultLicense := frmOptions.lbledDefaultLicense.Text;
    end;
  finally
    frmOptions.Free;
  end;
end;

procedure TfrmMain.StatusBarDrawPanel(TheStatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
const
  sDefault = 'default';
begin
  if Panel.Index = iStatusPanelErrors then
  begin
    StatusBar.Canvas.Font.Name := sDefault;

    StatusBar.Canvas.Font.Color := clRed;
    StatusBar.Canvas.Font.Size := 0;
    StatusBar.Canvas.Font.Style := [fsBold];
    StatusBar.Canvas.Brush.Color := clDefault;
    StatusBar.Canvas.FillRect(Rect);
    if FInvalidPluginInfo then
      StatusBar.Canvas.TextRect(Rect, Rect.Left, Rect.Top + 2, rsErrorPlugin);
    if not FErrorModules.IsEmpty then
      StatusBar.Canvas.TextRect(Rect, Rect.Left, Rect.Top + 2, rsErrorModule);
  end;
end;

procedure TfrmMain.strgrdModulesAfterSelection(Sender: TObject; aCol, aRow: integer);
begin
  ClearModuleInfo;
  if strgrdModules.RowCount > 1 then
    FillModuleData;
  ModuleRemove.Enabled := CheckCanDeleteModule;
  FModuleGridMutex := False;
end;

procedure TfrmMain.strgrdModulesBeforeSelection(Sender: TObject; aCol, aRow: integer);
begin
  FModuleGridMutex := True;
end;

procedure TfrmMain.strgrdModulesCheckboxToggled(Sender: TObject;
  aCol, aRow: integer; aState: TCheckboxState);
var
  ModuleData: TJSONObject;
begin
  if FModuleGridMutex then
    Exit;
  if aCol = iColumnHidden then
  begin
    ModuleData := FModulesWorking.Items[strgrdModules.Row - 1] as TJSONObject;
    UpdateBooleanField(ModuleData, ArrayManifestKeywords[iHidden],
      ArrayChecboxBooleans[aState]);
  end;
  SetModified(True);
  FChangesCommited := False;
end;

procedure TfrmMain.strgrdModulesColRowDeleted(Sender: TObject;
  IsColumn: boolean; sIndex, tIndex: integer);
begin
  ModuleRemove.Enabled := CheckCanDeleteModule;
end;

procedure TfrmMain.PrintParserError;
begin
  ShowErrorBox(rsErrorTitle, rsErrorInvalidJSon);
end;

procedure TfrmMain.ChangePanelText(const PanelNum: integer; const PanelText: string);
begin
  StatusBar.Panels[PanelNum].Text := PanelText;
end;

procedure TfrmMain.ChangeWindowCaption(const TheFileName: string);
var
  NewCaption: string;
begin
  NewCaption := Format(sApplicationName, [TheFileName]);
  Caption := NewCaption;
  Application.Title := NewCaption;
end;

function TfrmMain.CheckCanDeleteModule: boolean;
begin
  Result := (strgrdModules.RowCount > 1) and (strgrdModules.Row > 0);
end;

function TfrmMain.CheckValidPlugin: boolean;
begin
  Result := (lbledPluginSlug.Text <> EmptyStr) and
    (lbledPluginName.Text <> EmptyStr) and (lbledPluginVersion.Text <> EmptyStr) and
    (lbledPluginLicense.Text <> EmptyStr) and (lbledPluginAuthor.Text <> EmptyStr);
  FInvalidPluginInfo := not Result;
  DataCommitChanges.Enabled := not FInvalidPluginInfo and (FErrorModules.Size = 0);
  FileSaveAs.Enabled := DataCommitChanges.Enabled;
  StatusBar.Invalidate;
end;

function TfrmMain.CheckValidModule: boolean;
begin
  Result := (lbledModuleName.Text <> EmptyStr) and (lbledModuleSlug.Text <> EmptyStr);
  if Result then
    FErrorModules.Delete(strgrdModules.Row)
  else
    FErrorModules.Insert(strgrdModules.Row);
  DataCommitChanges.Enabled := not FInvalidPluginInfo and (FErrorModules.Size = 0);
  FileSaveAs.Enabled := DataCommitChanges.Enabled;
  StatusBar.Invalidate;
end;

procedure TfrmMain.ClearData;
begin
  ClearPluginInfo;
  strgrdModules.Clear;
  strgrdModules.RowCount := 1;
  strgrdModules.Row := -1;
  ClearModuleInfo;
end;

procedure TfrmMain.ClearModuleInfo;
var
  i: integer;
begin
  for i := 0 to scrlboxModuleInfo.ControlCount - 1 do
    if scrlboxModuleInfo.Controls[i] is TLabeledEdit then
      (scrlboxModuleInfo.Controls[i] as TLabeledEdit).Text := EmptyStr;
  for i := 0 to chkgrpTags.ControlCount - 1 do
    chkgrpTags.Checked[i] := False;
end;

procedure TfrmMain.ClearPluginInfo;
var
  i: integer;
begin
  for i := 0 to scrlboxPlugin.ControlCount - 1 do
    if scrlboxPlugin.Controls[i] is TLabeledEdit then
      (scrlboxPlugin.Controls[i] as TLabeledEdit).Text := EmptyStr;
end;

function TfrmMain.DoModifiedQuery: boolean;
var
  CanContinue: boolean = True;
  ValidManifest: boolean;
  Choice: integer;
begin
  Result := True;
  ValidManifest := not (FInvalidPluginInfo or (FErrorModules.Size > 0));
  if FModified or not FChangesCommited then
  begin
    if not FChangesCommited then
    begin
      Choice := Application.MessageBox(PChar(rsQuestionUncommited),
        PChar(rsQuestionUncommitedTitle), MB_YESNO + MB_ICONQUESTION);
      if Choice = idYes then
        CanContinue := False;
    end;

    if CanContinue and not ValidManifest then
    begin
      CanContinue := False;
      Choice := Application.MessageBox(PChar(rsQuestionInvalidManifest),
        PChar(rsQuestionInvalidManifestTitle), MB_YESNO + MB_ICONERROR);
      if Choice = idYes then
        Exit(False);
    end;

    if CanContinue then
    begin
      Choice := Application.MessageBox(PChar(rsQuestionModified),
        PChar(rsQuestionModifedTitle), MB_YESNOCANCEL + MB_ICONQUESTION);
      if Choice = idYes then
        FileSaveAs.Execute;
      if Choice = idCancel then
        Result := False;
    end;
  end;
end;

procedure TfrmMain.FillModuleData;
var
  IsHidden: boolean = False;
  Value: variant;
  ModuleData: TJSONData;
  Tags: TJSONArray;
begin
  ModuleData := FModulesWorking.Items[strgrdModules.Row - 1];
  FillTextBox(ModuleData, ArrayManifestKeywords[iSlug], lbledModuleSlug);
  FillTextBox(ModuleData, ArrayManifestKeywords[iName], lbledModuleName);
  FillTextBox(ModuleData, ArrayManifestKeywords[iDescription], lbledModuleDescription);
  FillTextBox(ModuleData, ArrayManifestKeywords[iKeywords], lbledModuleKeywords);
  FillTextBox(ModuleData, ArrayManifestKeywords[iManualURL], lbledModuleManualURL);
  FillTextBox(ModuleData, ArrayManifestKeywords[iModularGridURL],
    lbledModuleModularGridURL);
  if FindData(ModuleData, ArrayManifestKeywords[iHidden], Value, varBoolean) then
    IsHidden := Value;
  strgrdModules.Cells[iColumnHidden, strgrdModules.Row] := ArrayCheckboxValues[IsHidden];
  Tags := ModuleData.FindPath(ArrayManifestKeywords[iTags]) as TJSONArray;
  FillTags(Tags);
end;

procedure TfrmMain.FillModuleList;
var
  IsHidden: boolean;
  Module: integer;
  HiddenValue: variant;
  Value: variant;
begin
  for Module := 0 to FModulesWorking.Count - 1 do
    if FindData(FModulesWorking.Items[Module], ArrayManifestKeywords[iSlug],
      Value, varString) then
    begin
      IsHidden := False;
      if FindData(FModulesWorking.Items[Module], ArrayManifestKeywords[iHidden],
        HiddenValue, varBoolean) then
        IsHidden := HiddenValue;
      strgrdModules.InsertRowWithValues(strgrdModules.RowCount,
        [VarToStr(Value), ArrayCheckboxValues[IsHidden]]);

    end;
end;

procedure TfrmMain.FillPluginData;
begin
  FillTextBox(FRootWorking, ArrayManifestKeywords[iSlug], lbledPluginSlug);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iName], lbledPluginName);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iVersion], lbledPluginVersion);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iLicense], lbledPluginLicense);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iBrand], lbledPluginBrand);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iDescription], lbledPluginDescription);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iAuthor], lbledPluginAuthor);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iAuthorEmail], lbledPluginAuthorEmail);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iAuthorURL], lbledPluginAuthorURL);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iPluginURL], lbledPluginURL);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iManualURL], lbledPluginManualURL);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iSourceURL], lbledPluginSourceURL);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iDonateURL], lbledPluginDonateURL);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iChangeLogURL],
    lbledPluginChangelogURL);
  FillTextBox(FRootWorking, ArrayManifestKeywords[iMinRackVersion],
    lbledPluginMinRackVersion);
end;

procedure TfrmMain.FillTags(Tags: TJSONArray);
var
  TagAlias: string;
  TagString: string;
  CurrentCheckBox: integer;
  CurrentTag: integer;
begin
  if Tags <> nil then
    if Tags.Count > 0 then
      for CurrentTag := 0 to Tags.Count - 1 do
      begin
        TagString := Tags.Items[CurrentTag].AsString;
        TagAlias := GetTagAlias(TagString);
        if TagAlias <> EmptyStr then
          TagString := TagAlias;
        for CurrentCheckBox := 0 to chkgrpTags.Items.Count - 1 do
          if AnsiCompareText(TagString, chkgrpTags.Items[CurrentCheckBox]) = 0 then
          begin
            chkgrpTags.Checked[CurrentCheckBox] := True;
            Break;
          end;
      end;
end;

procedure TfrmMain.FillTextBox(TheRoot: TJsonData; const Path: TJSONStringType;
  LabelEd: TLabeledEdit);
var
  Str: string;
  Value: variant;
begin
  if FindData(TheRoot, Path, Value, varString) then
  begin
    Str := VarToStr(Value);
    SetTextBox(Str, LabelEd);
  end;
end;

function TfrmMain.FindData(TheRoot: TJSONData; const Path: TJSONStringType;
  out Value: variant; const VariantKind: integer): boolean;
var
  Data: TJSONData;
begin
  Result := False;
  Data := TheRoot.FindPath(Path);
  if Data <> nil then
  begin
    Result := True;
    case VariantKind of
      varString: Value := Data.AsString;
      varInteger: Value := Data.AsInteger;
      varSingle: Value := Data.AsFloat;
      varBoolean: Value := Data.AsBoolean;
    end;
  end;
end;

procedure TfrmMain.FreeObjects;
begin
  if Assigned(FRootWorking) then
  begin
    FreeAndNil(FRootWorking);
    FModulesWorking := nil;
  end;
  if Assigned(FPluginBase) then
    FreeAndNil(FPluginBase);
  if Assigned(FErrorModules) then
    FreeAndNil(FErrorModules);
end;

procedure TfrmMain.LoadConfig;
var
  ConfigFile: TBatJSONConfig;
begin
  ConfigFile := TBatJSONConfig.Create(nil);
  try
    ConfigFile.Filename := FConfigFileName;

    Left := ConfigFile.GetValue(sConfigWindowX, Left);
    Top := ConfigFile.GetValue(sConfigWindowY, Top);
    Height := ConfigFile.GetValue(sConfigWindowHeight, Height);
    Width := ConfigFile.GetValue(sConfigWindowWidth, Width);
    WindowState := TWindowState(ConfigFile.GetValue(sConfigWindowState,
      Ord(WindowState)));
    pnlModuleList.Width := ConfigFile.GetValue(sConfigModuleListWidth,
      pnlModuleList.Width);
    scrlboxModuleTags.Height :=
      ConfigFile.GetValue(sConfigTagsHeight, scrlboxModuleTags.Height);
    FDefaultAuthor := UTF8Encode(ConfigFile.GetValue(sConfigDefaultAuthor, EmptyStr));
    FDefaultLicense := UTF8Encode(ConfigFile.GetValue(sConfigDefaultLicense, EmptyStr));
  finally
    ConfigFile.Free;
  end;
end;

procedure TfrmMain.SaveConfig;
var
  ConfigFile: TBatJSONConfig;
begin
  ConfigFile := TBatJSONConfig.Create(nil);
  try
    ConfigFile.SetValue(sConfigWindowX, Left);
    ConfigFile.SetValue(sConfigWindowY, Top);
    ConfigFile.SetValue(sConfigWindowHeight, Height);
    ConfigFile.SetValue(sConfigWindowWidth, Width);
    ConfigFile.SetValue(sConfigWindowState, Ord(WindowState));
    ConfigFile.SetValue(sConfigModuleListWidth, pnlModuleList.Width);
    ConfigFile.SetValue(sConfigTagsHeight, scrlboxModuleTags.Height);
    ConfigFile.SetValue(sConfigDefaultAuthor, FDefaultAuthor);
    ConfigFile.SetValue(sConfigDefaultLicense, FDefaultLicense);

    ConfigFile.Filename := FConfigFileName;
    ConfigFile.Flush;
  finally
    ConfigFile.Free;
  end;
end;

procedure TfrmMain.SetModified(const IsModified: boolean);
begin
  FModified := IsModified;
  FileSaveAs.Enabled := IsModified;
  case IsModified of
    True: ChangePanelText(iStatusPanelModified, cAsterisk);
    False: ChangePanelText(iStatusPanelModified, EmptyStr);
  end;
end;

procedure TfrmMain.SetTextBox(const Value: string; LabelEd: TLabeledEdit);
begin
  LabelEd.Text := Value;
end;

procedure TfrmMain.ToggleGUI(const EnableGUI: boolean);
begin
  pgctrlPlugin.Visible := EnableGUI;
  imgInstructions.Visible := not EnableGUI;
end;

procedure TfrmMain.UpdateBooleanField(TheRoot: TJSONObject;
  const Path: TJSONStringType; AValue: boolean);
begin
  if TheRoot.FindPath(Path) <> nil then
  begin
    if AValue then
      TheRoot.Elements[Path].AsBoolean := AValue
    else
      TheRoot.Delete(Path);
  end
  else if AValue then
    TheRoot.Add(Path, AValue);
end;

procedure TfrmMain.UpdateStringField(TheRoot: TJSONObject;
  const Path: TJSONStringType; LabelEd: TLabeledEdit);
begin
  if TheRoot.FindPath(Path) <> nil then
  begin
    if LabelEd.Text <> EmptyStr then
      TheRoot.Elements[Path].AsString := LabelEd.Text
    else
      TheRoot.Delete(Path);
  end
  else if LabelEd.Text <> EmptyStr then
    TheRoot.Add(Path, LabelEd.Text);
end;

procedure TfrmMain.UpdateModuleCount;
begin
  if FModulesWorking <> nil then
    ChangePanelText(iStatusPanelModuleCount, Format(sModuleCount,
      [FModulesWorking.Count]))
  else
    ChangePanelText(iStatusPanelModuleCount, Format(sModuleCount, [0]));
end;

end.
