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

program pjeditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  FormMain,
  ImagesModule,
  VersionInfo,
  LCLVersion;

  {$R *.res}

  {$I version.inc}

begin
  {$IF (FPC_FULLVERSION < 30202)}
  {$FATAL You need at least Free Pascal version 3.2.2 to compile Sanguine PJ Editor.}
  {$ENDIF}
  {$IF (lcl_fullversion < 3040000)}
  {$FATAL You need at least Lazarus 3.4.0.0 to compile Sanguine PJ Editor.}
  {$ENDIF}

  InitVersionInfo(ProgramName, ProgramMajor, ProgramMinor, ProgramRevision, SVN);

  RequireDerivedFormResource := True;
  Application.Title:='Sanguine PJ Editor';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TImagesDataModule, ImagesDataModule);
  Application.Run;
end.
