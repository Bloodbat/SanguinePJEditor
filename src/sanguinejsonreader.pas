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
