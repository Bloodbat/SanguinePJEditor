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

{ Helpers, tools and encapsulation for GUI programs                            }

unit GUITools;

{$mode objfpc}{$H+}

interface

uses
  Forms, LCLType, Classes;

{
  function GetNextWidgetPos(const CurrentXY: TPoint; const WidgetDimensions: TPoint;
    const MinX: integer; const MaxX: integer; const Offset: integer = 0): TPoint;
  Calculates the position for the next widget in a container with many of the
  same widgets (e.g. An image browser). Can be used to keep widgets in a box
  without the need for horizontal scrollbars.
  MinX is the left position for the first widget in the next row.
  MaxX is the width of the container.
  Offset is an optional margin between widgets.
}

{ #note : This is a legacy function: use a TFlowPanel!! }
function GetNextWidgetPos(const CurrentXY: TPoint; const WidgetDimensions: TPoint;
  const MinX: integer; const MaxX: integer; const Offset: integer = 0): TPoint;

{
  function ShowBox(const Title: string; const Msg: string; Flags: longint = MB_OK): integer;
  Shows a messagebox with an an Ok button, flags can be set to change the icon
  and displayed buttons.
}
function ShowBox(const Title: string; const Msg: string;
  Flags: longint = MB_OK): integer;

{
  procedure ShowErrorBox(Title: string; Msg: string);
  Shows an error messagebox with an error icon and an Ok button.
}
procedure ShowErrorBox(const Title: string; const Msg: string);

{
  procedure ShowInformationBox(Title: string; Msg: string);
  Shows an information messagebox with an information icon and an Ok button
}
procedure ShowInformationBox(const Title: string; const Msg: string);

{
  procedure ShowQuestionBox(Title: string; Msg: string);
  Shows a question messagebox with a question icon and an Ok button
}
procedure ShowQuestionBox(const Title: string; const Msg: string);

{
  procedure ShowWarningBox(Title: string; Msg: string);
  Shows a warning messagebox with a warning icon and an Ok button
}
procedure ShowWarningBox(const Title: string; const Msg: string);

{
  procedure ShowYesNoQuestion(Title: string; Msg: string);
  Show a question messagebox with a question icon and Yes/No buttons
}
function ShowYesNoQuestion(const Title: string; const Msg: string): integer;

implementation

uses
  Dialogs;

function GetNextWidgetPos(const CurrentXY: TPoint; const WidgetDimensions: TPoint;
  const MinX: integer; const MaxX: integer; const Offset: integer = 0): TPoint;
var
  CalculatedPosition: TPoint;
begin
  CalculatedPosition.x := CurrentXY.x + WidgetDimensions.x + Offset;
  CalculatedPosition.y := CurrentXY.y;
  if (MaxX > 0) and (CalculatedPosition.x + WidgetDimensions.x + Offset >= MaxX) then
  begin
    CalculatedPosition.x := MinX;
    CalculatedPosition.y := CalculatedPosition.y + WidgetDimensions.y + Offset;
  end;
  Result := CalculatedPosition;
end;

function ShowBox(const Title: string; const Msg: string; Flags: longint): integer;
begin
  Result := Application.MessageBox(PChar(Msg), PChar(Title), Flags);
end;

procedure ShowErrorBox(const Title: string; const Msg: string);
begin
  ShowBox(Title, Msg, MB_ICONERROR);
end;

procedure ShowInformationBox(const Title: string; const Msg: string);
begin
  ShowBox(Title, Msg, MB_ICONINFORMATION);
end;

procedure ShowQuestionBox(const Title: string; const Msg: string);
begin
  ShowBox(Title, Msg, MB_ICONQUESTION);
end;

procedure ShowWarningBox(const Title: string; const Msg: string);
begin
  ShowBox(Title, Msg, MB_ICONWARNING);
end;

function ShowYesNoQuestion(const Title: string; const Msg: string): integer;
begin
  Result := MessageDlg(Title, Msg, mtConfirmation, [mbNo, mbYes], 0);
end;

end.
