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

unit FormAbout;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ExtCtrls, LCLIntf, Controls, Classes;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    bvlGPL: TBevel;
    bvlLazarus: TBevel;
    bvlMonsters: TBevel;
    bvlMutants: TBevel;
    bvlProductions: TBevel;
    bvlZDE: TBevel;
    imgGPL: TImage;
    imgLaz: TImage;
    imgLSRP: TImage;
    imgMonsters: TImage;
    imgMutants: TImage;
    imgProgram: TImage;
    lblBuildDate: TLabel;
    lblFPCVer: TLabel;
    lblLazVer: TLabel;
    lblProgramName: TLabel;
    memoAboutText: TMemo;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    procedure FormShow(Sender: TObject);
    procedure HandleSiteClicks(Sender: TObject);
  end;

const
  iMaxSites = 4;
  iIdxSiteZde = 0;
  iIdxSiteGpl = 1;
  iIdxSiteLazarus = 2;
  ArraySites: array[0..iMaxSites] of string = (
    'https://github.com/Bloodbat/SanguinePJEditor',
    'http://www.gnu.org/licenses/gpl-3.0.html',
    'http://www.lazarus-ide.org/',
    'https://github.com/Bloodbat/SanguineMutants',
    'https://github.com/Bloodbat/SanguineMonsters'
    );

implementation

uses
  SysUtils, LCLVersion, VersionInfo, PJStrings;

  {$R *.lfm}

  { TfrmAbout }

procedure TfrmAbout.FormShow(Sender: TObject);
const
  ProgramImageDistance = 10;
  sAboutBloodbatCredit = 'Bloodbat / La Serpiente y la Rosa Producciones.';
  sAboutProgVer = '%s %s';
var
  Day: integer = {$I %DATEDAY%};
  Month: integer = {$I %DATEMONTH%};
  Year: integer = {$I %DATEYEAR%};
begin
  lblBuildDate.Caption := Format(rsAboutDate, [Day, Month, Year]);
  lblFPCVer.Caption := Format(rsAboutFPCVer, [FPC_VERSION, FPC_RELEASE, FPC_PATCH]);
  lblFPCVer.Left := ClientRect.CenterPoint.X - (lblFPCVer.Width div 2);
  lblLazVer.Caption := Format(rsAboutLazVer, [lcl_version]);
  lblLazVer.Left := Width - lblLazVer.Width - 8;
  lblProgramName.Caption := Format(sAboutProgVer, [StrProgramName, GetProgramVersion]);
  lblProgramName.Left := ClientRect.CenterPoint.X - (lblProgramName.Width div 2);
  imgProgram.Left := lblProgramName.Left - imgProgram.Width - ProgramImageDistance;
  bvlZDE.Left := imgProgram.Left - 2;
  memoAboutText.Lines.AddStrings(
    [Format(sAboutProgVer, [StrProgramName, GetProgramVersion]),
    EmptyStr, rsAboutCopyYear, rsAboutSerpienteyRosa, EmptyStr,
    rsAboutCodeCreditHeader, sAboutBloodbatCredit, rsAboutRightsReserved,
    EmptyStr, rsAboutIconsCredit, EmptyStr, rsAboutGNU1, EmptyStr,
    rsAboutGNU2, EmptyStr, rsAboutGNU3]);
end;

procedure TfrmAbout.HandleSiteClicks(Sender: TObject);
var
  Control: TControl;
begin
  Control := Sender as TControl;
  OpenURL(ArraySites[Control.Tag]);
end;

end.
