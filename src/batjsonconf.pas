(*                           Bloodbat's Code Library                          *)

(* Copyright 2018-2024 Bloodbat / La Serpiente y la Rosa Producciones         *)
(* Bloodbat's Code Library is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at your
   option) any later version.                                                 *)

(* Bloodbat's Code Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.                                           *)

(* You should have received a copy of the GNU General Public License along
   with Bloodbat's Code Library. If not, see <http://www.gnu.org/licenses/>.  *)

{ A unit to avoid annoying string coversion warnings under Lazarus }

unit BatJSONConf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONConf;

type

  { TBatJSONConfig }

  TBatJSONConfig = class(TJSONConfig)
  public
    function GetValue(const APath: UnicodeString;
      const ADefault: UnicodeString): RawByteString; overload;
  end;

implementation

{ TBatJSONConfig }

function TBatJSONConfig.GetValue(const APath: UnicodeString;
  const ADefault: UnicodeString): RawByteString;
begin
  Result := UTF8Encode(inherited GetValue(APath, ADefault));
end;

end.
