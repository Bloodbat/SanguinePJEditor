(*                           Bloodbat's Code Library                          *)

(* Copyright 2014-2024 Bloodbat
   La Serpiente y la Rosa Producciones.                                       *)
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

{ Provides an easy way to integrate a program's version information into an
  application.                                                                }

unit VersionInfo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  { Most of these need to be intialized to be actually useful, whether by
    hand or by calling InitVersionInfo. }
  IsSvn: boolean = False;
  StrProgramName: string = '';
  VersionControlString: string = 'SVN';
  VersionMajor: integer = 0;
  VersionMinor: integer = 0;
  VersionRevision: integer = 0;

{
  procedure InitVersionInfo(const TheProgramName: string;
    Major, Minor, Revision: integer; FromSvn: boolean);
  Inits program version constants.
}
procedure InitVersionInfo(const TheProgramName: string;
  Major, Minor, Revision: integer; FromSvn: boolean);

{ function GetProgramVersion: string;
  Returns the processed, complete program version as a string.
}
function GetProgramVersion: string;

implementation

procedure InitVersionInfo(const TheProgramName: string;
  Major, Minor, Revision: integer; FromSvn: boolean);
begin
  StrProgramName := TheProgramName;
  VersionMajor := Major;
  VersionMinor := Minor;
  VersionRevision := Revision;
  IsSvn := FromSvn;
end;

function GetProgramVersion: string;
const
  VersionString = '%d.%d.%d';
var
  Str: string;
begin
  Str := Format(VersionString, [VersionMajor, VersionMinor, VersionRevision]);
  if IsSVN then
  begin
    Str += ' ';
    Str += VersionControlString;
  end;
  Result := Str;
end;

end.
