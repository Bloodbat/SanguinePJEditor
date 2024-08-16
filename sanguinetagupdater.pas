program SanguineTagUpdater;

{$IF (FPC_FULLVERSION < 30202)}
{$FATAL You need at least Free Pascal version 3.2.2 to compile Sanguine Tag Updater.}
{$ENDIF}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  FPJson,
  PJStrings,
  ModuleTagging,
  SanguineJSONReader;

resourcestring
  rsHelpDesc1 = 'A program to update deprecated tags for VCV Rack plugin manifests.';
  rsHelpUsage1 = 'Usage:';
  rsHelpUsage2 = '  %s (manifestfilename | -h)';
  rsHelpUsage3 = 'Parameters:';
  rsHelpUsage4 = '  manifestfilename    Mandatory. Manifest json file to be processed.';
  rsHelpUsage5 = '  -h                  Show this help screen.';
  rsHelpUsage6 = 'Parameters are case insensitive';

  rsErrorHeader = 'Error: ';
  rsErrorFileNotFound = '%s file not found.';
  rsErrorNoInFile = 'A manifest file must be specified.';
  rsErrorNoModuleKey = 'Modules key not found.';
  rsErrorNoModules = 'No modules found in manifest.';
  rsErrorTooManyInfiles = 'Only one input file can be specified.';

  rsWarningHeader = 'Warning: ';
  rsWarningNoSlug = 'No slug for module %.2d, skipping.';

  rsStatusFinished = 'Done updating %s';
  rsStatusStatistics =
    'Modules processed: %.2d. Tags processed: %.2d. Tags updated: %.2d';
  rsStatusStartUpdate = 'Updating %s';

type

  TOptionsResult = (orOk, orHelp, orError);

  { TSanguineTagUpdater }

  TSanguineTagUpdater = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure DoParserError; virtual;
    function ParseCommandLine: TOptionsResult; virtual;
    procedure PrintError(const ErrorMsg: string); virtual;
    procedure PrintHeader; virtual;
    procedure PrintHelp; virtual;
    procedure PrintWarning(const WarningMsg: string); virtual;
    function ProcessModules: boolean; virtual;
    procedure SaveManifest;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  private
    FInFileName: string;
    FModules: TJSONArray;
    FRoot: TJSONData;
  end;

  { TSanguineTagUpdater }

  procedure TSanguineTagUpdater.DoRun;
  begin
    PrintHeader;
    if ParseCommandLine = orOk then
    begin
      if FileExists(FInFileName) then
      begin
        WriteLn(Format(rsStatusStartUpdate, [FInFileName]));
        WriteLn;
        ReadJSon(FInFileName, FRoot, @DoParserError);
        if FRoot <> nil then
          if ProcessModules then
          begin
            SaveManifest;
            WriteLn(Format(rsStatusFinished, [FInFileName]));
          end;
      end
      else
        PrintError(Format(rsErrorFileNotFound, [FInFileName]));
    end;
    Terminate;
  end;

  procedure TSanguineTagUpdater.DoParserError;
  begin
    PrintError(rsErrorInvalidJSon);
  end;

  function TSanguineTagUpdater.ParseCommandLine: TOptionsResult;
  const
    cParamHelpShort = 'h';

    sAllShortParams = cParamHelpShort;
  var
    NonOpts: TStringArray;
    OptsError: string;
  begin
    CaseSensitiveOptions := False;

    if ParamCount < 1 then
    begin
      PrintError(rsErrorNoInFile);
      WriteLn;
      PrintHelp;
      Exit(orError);
    end;

    OptsError := CheckOptions(sAllShortParams, '');
    if OptsError <> EmptyStr then
    begin
      PrintError(OptsError);
      WriteLn;
      PrintHelp;
      Exit(orError);
    end;

    if HasOption(cParamHelpShort, '') then
    begin
      PrintHelp;
      Exit(orHelp);
    end;

    NonOpts := GetNonOptions(sAllShortParams, []);
    if Length(NonOpts) > 1 then
    begin
      PrintError(rsErrorTooManyInfiles);
      WriteLn;
      PrintHelp;
      Exit(orError);
    end;
    FInFileName := NonOpts[0];
    Result := orOk;
  end;

  procedure TSanguineTagUpdater.PrintError(const ErrorMsg: string);
  begin
    WriteLn(rsErrorHeader, ErrorMsg);
  end;

  procedure TSanguineTagUpdater.PrintHeader;
  const
    ArrayGNUNotice: array of string = (
      'This program comes with ABSOLUTELY NO WARRANTY.',
      'This is free software, and you are welcome to redistribute it',
      'under certain conditions; see LICENSE-GPLv3.txt for details.');
    ArrayProgramHeader: array of string = (
      'Sanguine Tag Updater V.1.0.',
      'Copyright 2024 Bloodbat.',
      'La Serpiente y la Rosa Producciones.'
      );
  var
    s: string;
  begin
    for s in ArrayProgramHeader do
      WriteLn(s);
    WriteLn;
    for s in ArrayGNUNotice do
      WriteLn(s);
    WriteLn;
  end;

  procedure TSanguineTagUpdater.PrintHelp;
  var
    ProgName: string;
  begin
    ProgName := ExtractFileName(ExeName);
    WriteLn(rsHelpDesc1);
    WriteLn;
    WriteLn(rsHelpUsage1);
    WriteLn(Format(rsHelpUsage2, [ProgName]));
    WriteLn(rsHelpUsage3);
    WriteLn(rsHelpUsage4);
    WriteLn(rsHelpUsage5);
    WriteLn(rsHelpUsage6);
    WriteLn;
  end;

  procedure TSanguineTagUpdater.PrintWarning(const WarningMsg: string);
  begin
    WriteLn(rsWarningHeader, WarningMsg);
  end;

  function TSanguineTagUpdater.ProcessModules: boolean;
  const
    sStatusConversion = '%s: %s -> %s';
  var
    CurrentTag: integer;
    Module: integer;
    ModuleCount: integer = 0;
    TagCount: integer = 0;
    UpdateCount: integer = 0;
    ModuleData: TJSONData;
    SlugData: TJSONData;
    Tags: TJSONArray;
    SlugName: string;
    TagAlias: string;
    TagString: string;
  begin
    Result := False;
    FModules := (FRoot.FindPath(sModules) as TJSONArray);
    if FModules <> nil then
    begin
      if FModules.Count > 0 then
      begin
        for Module := 0 to FModules.Count - 1 do
        begin
          ModuleData := FModules.Items[Module];
          SlugData := ModuleData.FindPath(sSlug);
          if SlugData <> nil then
          begin
            SlugName := SlugData.AsString;
            Tags := (ModuleData.FindPath(sTags) as TJSONArray);
            if Tags <> nil then
              if Tags.Count > 0 then
                for CurrentTag := 0 to Tags.Count - 1 do
                begin
                  TagString := Tags.Items[CurrentTag].AsString;
                  Inc(TagCount);
                  TagAlias := GetTagAlias(TagString);
                  if TagAlias <> EmptyStr then
                  begin
                    WriteLn(Format(sStatusConversion, [SlugName, TagString, TagAlias]));
                    Tags.Items[CurrentTag].AsString := TagAlias;
                    Inc(UpdateCount);
                  end;
                end;
            Inc(ModuleCount);
          end
          else
          begin
            PrintWarning(Format(rsWarningNoSlug, [Module]));
          end;
        end;
        WriteLn;
        WriteLn(Format(rsStatusStatistics, [ModuleCount, TagCount, UpdateCount]));
        WriteLn;
        Result := True;
      end
      else
        PrintError(rsErrorNoModules);
    end
    else
      PrintError(rsErrorNoModuleKey);
  end;

  procedure TSanguineTagUpdater.SaveManifest;
  var
    JString: string;
    MemoryStream: TMemoryStream;
  begin
    JString := FRoot.FormatJSON;
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.WriteBuffer(JString[1], Length(JString));
      MemoryStream.SaveToFile(FInFileName);
    finally
      MemoryStream.Free;
    end;
  end;

  constructor TSanguineTagUpdater.Create(TheOwner: TComponent);
  begin
    inherited;
    FInFileName := '';
    FRoot := nil;
    StopOnException := True;
  end;

  destructor TSanguineTagUpdater.Destroy;
  begin
    if Assigned(FRoot) then
      FreeAndNil(FRoot);
    inherited;
  end;

var
  Application: TSanguineTagUpdater;

  {$R *.res}

begin
  Application := TSanguineTagUpdater.Create(nil);
  Application.Title := 'Sanguine Tag Updater';
  Application.Run;
  Application.Free;
end.
