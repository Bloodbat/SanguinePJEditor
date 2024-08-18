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

unit FormNewPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Forms, Dialogs, ExtCtrls, Buttons, ActnList, ComCtrls, Menus,
  StdActns;

type

  { TfrmNewPlugin }

  TfrmNewPlugin = class(TForm)
    ActionList: TActionList;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    EditCopy: TEditCopy;
    EditCut: TEditCut;
    EditPaste: TEditPaste;
    lbledPluginAuthor: TLabeledEdit;
    lbledPluginLicense: TLabeledEdit;
    lbledPluginName: TLabeledEdit;
    lbledPluginSlug: TLabeledEdit;
    lbledPluginVersion: TLabeledEdit;
    MainMenu: TMainMenu;
    mnuEdit: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditPaste: TMenuItem;
    pnlControls: TPanel;
    scrlboxEditors: TScrollBox;
    Separator1: TMenuItem;
    tbCopy: TToolButton;
    tbCut: TToolButton;
    tbPaste: TToolButton;
    ToolBar: TToolBar;
    ToolButton2: TToolButton;
    procedure FormShow(Sender: TObject);
    procedure HandleEditorsChange(Sender: TObject);
  private
    function CheckValidPlugin: boolean;
  end;

implementation

{$R *.lfm}

{ TfrmNewPlugin }

procedure TfrmNewPlugin.FormShow(Sender: TObject);
begin
  lbledPluginSlug.SetFocus;
end;

procedure TfrmNewPlugin.HandleEditorsChange(Sender: TObject);
begin
  btnOk.Enabled := CheckValidPlugin;
end;

function TfrmNewPlugin.CheckValidPlugin: boolean;
begin
  Result := (lbledPluginSlug.Text <> EmptyStr) and
    (lbledPluginName.Text <> EmptyStr) and (lbledPluginVersion.Text <> EmptyStr) and
    (lbledPluginLicense.Text <> EmptyStr) and (lbledPluginAuthor.Text <> EmptyStr);
end;

end.
