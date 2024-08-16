unit SanguineJSONReader;

{$mode ObjFPC}{$H+}

interface

uses
  FPJson;

type
  TErrorCallback = procedure of object;

procedure ReadJSON(const FileName: string; var Root: TJSONData;
  ErrorCallback: TErrorCallback);

implementation

uses
  Classes, SysUtils, JSONParser, JSONScanner;

procedure ReadJSON(const FileName: string; var Root: TJSONData;
  ErrorCallback: TErrorCallback);
var
  FileStream: TFileStream;
  JParser: TJSONParser;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    JParser := TJSONParser.Create(FileStream, []);
    try
      JParser.Options := JParser.Options + [joStrict];
      try
        Root := JParser.Parse;
      except
        ErrorCallback();
      end;
    finally
      JParser.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
