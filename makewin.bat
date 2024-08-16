@echo off
REM                           Sanguine PJ Editor

REM Copyright (C) 2024
REM La Serpiente y la Rosa Producciones.

REM This file is part of Sanguine PJ Editor.

REM Sanguine PJ Editor is free software: you can redistribute it
REM and/or modify it under the terms of the GNU General Public License as
REM published by the Free Software Foundation, either version 3 of the License,
REM or (at your option) any later version.

REM Sanguine PJ Editor is distributed in the hope that it will be
REM useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
REM MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
REM See the GNU General Public License for more details.

REM You should have received a copy of the GNU General Public License
REM along with Sanguine PJ Editor.
REM If not, see <http://www.gnu.org/licenses/>.                                                                  

echo Preparing to build Sanguine PJ Editor.
echo lazbuild.exe must be present in your path!
echo.

:BuildTagUpdaterx64
echo Building Sanguine Tag Updater
lazbuild.exe --bm="Release Intel x64" sanguinetagupdater.lpi
if ERRORLEVEL 1 goto BuildErrorTagUpdater
echo Done building Sanguine Tag Updater
goto exit

:BuildErrorTagUpdater
echo.
echo Error building Sanguine Tag Updater
echo Something went really, really wrong.
echo Check the code for errors and your path for lazbuild.exe
goto exit

:exit
