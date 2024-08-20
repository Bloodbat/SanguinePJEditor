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
  SysUtils, FpJson, Controls, Buttons, Dialogs;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    imgInstructions: TImage;
    mnuProgramOptions: TMenuItem;
    mnuProgram: TMenuItem;
    ProgramOptions: TAction;
    FileSafeOpen: TAction;
    btnAcceptPluginChanges: TBitBtn;
    btnAcceptPluginChanges1: TBitBtn;
    btnCancelPluginChanges: TBitBtn;
    btnCancelPluginChanges1: TBitBtn;
    chkgrpTags: TCheckGroup;
    FileSaveAs: TFileSaveAs;
    HelpOpenManifestManual: TAction;
    lbledModuleDescription: TLabeledEdit;
    lbledModuleKeywords: TLabeledEdit;
    lbledModuleManualURL: TLabeledEdit;
    lbledModuleModularGridURL: TLabeledEdit;
    lbledModuleName: TLabeledEdit;
    lbledModuleSlug: TLabeledEdit;
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
    pnlModuleData: TPanel;
    pnlModuleDataControls: TPanel;
    pnlPluginDataControls: TPanel;
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
    ModuleDiscardChanges: TAction;
    ModuleAcceptChanges: TAction;
    PluginDiscardChanges: TAction;
    PluginAcceptChanges: TAction;
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
    procedure ModuleAcceptChangesExecute(Sender: TObject);
    procedure ModuleAddExecute(Sender: TObject);
    procedure ModuleDiscardChangesExecute(Sender: TObject);
    procedure ModuleRemoveExecute(Sender: TObject);
    procedure PluginAcceptChangesExecute(Sender: TObject);
    procedure PluginDiscardChangesExecute(Sender: TObject);
    procedure ProgramOptionsExecute(Sender: TObject);
    procedure strgrdModulesAfterSelection(Sender: TObject; aCol, aRow: integer);
    procedure strgrdModulesColRowDeleted(Sender: TObject; IsColumn: boolean;
      sIndex, tIndex: integer);
  protected
    procedure PrintParserError;
  private
    FNoNameCount: integer;
    FConfigFileName: string;
    FDefaultAuthor: string;
    FDefaultLicense: string;
    FFileName: string;
    FModified: boolean;
    FRoot: TJSONObject;
    FModules: TJSONArray;
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
    procedure UpdateField(TheRoot: TJSONObject; const Path: TJSONStringType;
      LabelEd: TLabeledEdit);
    procedure UpdateModuleCount;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Variants, ModuleTagging, LCLIntf, FormAbout, PJStrings, GUITools,
  FormNewPlugin, SanguineJSONReader, FormNewModule, BatJSONConf, LCLType, FormOptions;

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

  iStatusPanelState = 0;
  iStatusPanelModified = 1;
  iStatusPanelModuleCount = 2;

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
    frmNewPlugin := TfrmNewPlugin.Create(nil);
    try
      frmNewPlugin.lbledPluginAuthor.Text := FDefaultAuthor;
      frmNewPlugin.lbledPluginLicense.Text := FDefaultLicense;
      if frmNewPlugin.ShowModal = mrOk then
      begin
        FreeObjects;
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
        FRoot := NewPluginData;
        Inc(FNoNameCount);
        FFileName := Format(sDefaultFileName, [FNoNameCount]);
        ToggleGUI(True);
        ChangeWindowCaption(FFileName);
        SetModified(True);
        UpdateModuleCount;
      end;
    finally
      frmNewPlugin.Free;
    end;
  end;
end;

procedure TfrmMain.FileSafeOpenExecute(Sender: TObject);
var
  JData: TJSONData = nil;
begin
  if DoModifiedQuery then
  begin
    if OpenDialog.Execute then
    begin
      Cursor := crHourGlass;
      ChangePanelText(iStatusPanelState, rsStatusLoading);
      Application.ProcessMessages;
      ClearData;
      FreeObjects;
      ReadJSON(OpenDialog.FileName, JData, @PrintParserError);
      if JData <> nil then
        FRoot := JData as TJSONObject;
      if FRoot <> nil then
      begin
        ToggleGUI(True);
        FillPluginData;
        FModules := FRoot.FindPath(ArrayManifestKeywords[iModules]) as TJSONArray;
        if FModules <> nil then
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
    OutString := FRoot.FormatJSON;
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
begin
  ModuleAcceptChanges.Enabled := CheckValidModule;
end;

procedure TfrmMain.HandlePluginEditorsChange(Sender: TObject);
begin
  PluginAcceptChanges.Enabled := CheckValidPlugin;
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

procedure TfrmMain.ModuleAcceptChangesExecute(Sender: TObject);
var
  i: integer;
  LabelEd: TLabeledEdit;
  ModuleData: TJSONObject;
  Tags: TJSONArray;
  WantHidden: boolean = False;
begin
  ModuleData := FModules.Items[strgrdModules.Row - 1] as TJSONObject;

  for i := 0 to scrlboxModuleInfo.ControlCount - 1 do
    if scrlboxModuleInfo.Controls[i] is TLabeledEdit then
    begin
      LabelEd := scrlboxModuleInfo.Controls[i] as TLabeledEdit;
      UpdateField(ModuleData, ArrayManifestKeywords[LabelEd.Tag], LabelEd);
    end;

  WantHidden := strgrdModules.Cells[iColumnHidden, strgrdModules.Row][1] = cValueTrue;

  if ModuleData.FindPath(ArrayManifestKeywords[iHidden]) <> nil then
  begin
    case WantHidden of
      True: ModuleData.Elements[ArrayManifestKeywords[iHidden]].AsBoolean := WantHidden;
      False: ModuleData.Delete(ArrayManifestKeywords[iHidden]);
    end;
  end
  else if WantHidden then
    ModuleData.Add(ArrayManifestKeywords[iHidden], WantHidden);

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
      ClearModuleInfo;
      SetTextBox(frmNewModule.lbledModuleSlug.Text, lbledModuleSlug);
      SetTextBox(frmNewModule.lbledModuleName.Text, lbledModuleName);
      SetModified(True);
      if FModules = nil then
      begin
        JArray := TJSONArray.Create;
        ItemId := FRoot.Add(ArrayManifestKeywords[iModules], JArray);
        FModules := FRoot.Items[ItemId] as TJSONArray;
      end;
      JObject := TJSONObject.Create([ArrayManifestKeywords[iSlug],
        lbledModuleSlug.Text, ArrayManifestKeywords[iName], lbledModuleName.Text]);
      FModules.Add(JObject);
      strgrdModules.InsertRowWithValues(strgrdModules.RowCount,
        [lbledModuleSlug.Text, ArrayCheckboxValues[False]]);
      strgrdModules.Row := strgrdModules.RowCount - 1;
      UpdateModuleCount;
    end;
  finally
    frmNewModule.Free;
  end;
end;

procedure TfrmMain.ModuleDiscardChangesExecute(Sender: TObject);
begin
  ClearModuleInfo;
  FillModuleData;
end;

procedure TfrmMain.ModuleRemoveExecute(Sender: TObject);
begin
  FModules.Delete(strgrdModules.Row - 1);
  strgrdModules.DeleteRow(strgrdModules.Row);
  if strgrdModules.Row > 0 then
    strgrdModulesAfterSelection(Self, strgrdModules.Col, strgrdModules.Row)
  else
    ClearModuleInfo;
  UpdateModuleCount;
  SetModified(True);
end;

procedure TfrmMain.PluginAcceptChangesExecute(Sender: TObject);
var
  i: integer;
  LabelEd: TLabeledEdit;
  Modules: TJSONArray = nil;
  TmpModules: TJSONData;
begin
  // Keep modules after plugin info. Part 1
  TmpModules := FRoot.FindPath(ArrayManifestKeywords[iModules]) as TJSONArray;
  if TmpModules <> nil then
  begin
    Modules := TmpModules.Clone as TJSONArray;
    FRoot.Delete(ArrayManifestKeywords[iModules]);
  end;

  for i := 0 to scrlboxPlugin.ControlCount - 1 do
    if scrlboxPlugin.Controls[i] is TLabeledEdit then
    begin
      LabelEd := scrlboxPlugin.Controls[i] as TLabeledEdit;
      UpdateField(FRoot, ArrayManifestKeywords[LabelEd.Tag], LabelEd);
    end;

  // Keep modules after plugin info. Part 2
  if Modules <> nil then
    FRoot.Add(ArrayManifestKeywords[iModules], Modules);

  SetModified(True);
end;

procedure TfrmMain.PluginDiscardChangesExecute(Sender: TObject);
begin
  ClearPluginInfo;
  FillPluginData;
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

procedure TfrmMain.strgrdModulesAfterSelection(Sender: TObject; aCol, aRow: integer);
begin
  ClearModuleInfo;
  if strgrdModules.RowCount > 1 then
    FillModuleData;
  ModuleRemove.Enabled := CheckCanDeleteModule;
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
end;

function TfrmMain.CheckValidModule: boolean;
begin
  Result := (lbledModuleName.Text <> EmptyStr) and (lbledModuleSlug.Text <> EmptyStr);
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
  Choice: integer;
begin
  Result := True;
  if FModified then
  begin
    Choice := Application.MessageBox(PChar(rsQuestionModified),
      PChar(rsQuestionModifedTitle), MB_YESNOCANCEL + MB_ICONQUESTION);
    if Choice = idYes then
      FileSaveAs.Execute;
    if Choice = idCancel then
      Result := False;
  end;
end;

procedure TfrmMain.FillModuleData;
var
  IsHidden: boolean = False;
  Value: variant;
  ModuleData: TJSONData;
  Tags: TJSONArray;
begin
  ModuleData := FModules.Items[strgrdModules.Row - 1];
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
  for Module := 0 to FModules.Count - 1 do
    if FindData(FModules.Items[Module], ArrayManifestKeywords[iSlug],
      Value, varString) then
    begin
      IsHidden := False;
      if FindData(FModules.Items[Module], ArrayManifestKeywords[iHidden],
        HiddenValue, varBoolean) then
        IsHidden := HiddenValue;
      strgrdModules.InsertRowWithValues(strgrdModules.RowCount,
        [VarToStr(Value), ArrayCheckboxValues[IsHidden]]);

    end;
end;

procedure TfrmMain.FillPluginData;
begin
  FillTextBox(FRoot, ArrayManifestKeywords[iSlug], lbledPluginSlug);
  FillTextBox(FRoot, ArrayManifestKeywords[iName], lbledPluginName);
  FillTextBox(FRoot, ArrayManifestKeywords[iVersion], lbledPluginVersion);
  FillTextBox(FRoot, ArrayManifestKeywords[iLicense], lbledPluginLicense);
  FillTextBox(FRoot, ArrayManifestKeywords[iBrand], lbledPluginBrand);
  FillTextBox(FRoot, ArrayManifestKeywords[iDescription], lbledPluginDescription);
  FillTextBox(FRoot, ArrayManifestKeywords[iAuthor], lbledPluginAuthor);
  FillTextBox(FRoot, ArrayManifestKeywords[iAuthorEmail], lbledPluginAuthorEmail);
  FillTextBox(FRoot, ArrayManifestKeywords[iAuthorURL], lbledPluginAuthorURL);
  FillTextBox(FRoot, ArrayManifestKeywords[iPluginURL], lbledPluginURL);
  FillTextBox(FRoot, ArrayManifestKeywords[iManualURL], lbledPluginManualURL);
  FillTextBox(FRoot, ArrayManifestKeywords[iSourceURL], lbledPluginSourceURL);
  FillTextBox(FRoot, ArrayManifestKeywords[iDonateURL], lbledPluginDonateURL);
  FillTextBox(FRoot, ArrayManifestKeywords[iChangeLogURL], lbledPluginChangelogURL);
  FillTextBox(FRoot, ArrayManifestKeywords[iMinRackVersion], lbledPluginMinRackVersion);
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
  if Assigned(FRoot) then
  begin
    FreeAndNil(FRoot);
    FModules := nil;
  end;
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

procedure TfrmMain.UpdateField(TheRoot: TJSONObject; const Path: TJSONStringType;
  LabelEd: TLabeledEdit);
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
  if FModules <> nil then
    ChangePanelText(iStatusPanelModuleCount, Format(sModuleCount, [FModules.Count]))
  else
    ChangePanelText(iStatusPanelModuleCount, Format(sModuleCount, [0]));
end;

end.
