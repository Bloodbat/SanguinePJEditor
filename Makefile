#                           Sanguine PJ Editor
#
# Copyright (C) 2024
# La Serpiente y la Rosa Producciones.
#
# This file is part of Sanguine PJ Editor.
#
# Sanguine PJ Editor is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# Sanguine PJ Editor is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Sanguine PJ Editor.
# If not, see <http://www.gnu.org/licenses/>.

ARCH_UNKNOWN = 1

COMPILERFLAGS = -MObjFPC -Scghim -CX -O3 -Xs -XX -l -vewnhibq -Fusrc -Fu. -FEbin

ifeq ($(OS),Windows_NT)
	ARCH_WIN = 1
	CPU_X64 = 1
	ARCH_UNKNOWN = 0
else
	UNAME_OS := $(shell uname -s)
	ifeq ($(UNAME_OS),Linux)
		ARCH_LIN = 1
		CPU_X64 = 1
		 ARCH_UNKNOWN = 0
	endif
	ifeq ($(UNAME_OS),Darwin)
		ARCH_MAC = 1
		ARCH_UNKNOWN = 0
	endif
endif

ifeq (ARCH_UNKNOWN,1)
$(error Could not determine machine type.)
endif

ifdef ARCH_WIN
LIBFOLDER = lib\x86_64-win64
COMPILERFLAGS += -Fi$(LIBFOLDER) -FU$(LIBFOLDER) -Px86_64 -CpCOREAVX -obin\sanguinetagupdater.exe
MKDIRCMD = md
endif

ifdef ARCH_LIN
LIBFOLDER = lib/x86_64-linux
COMPILERFLAGS += -Cg -Fi$(LIBFOLDER) -FU$(LIBFOLDER) -Px86_64 -CpCOREAVX -obin/sanguinetagupdater
MKDIRCMD = mkdir
endif

ifdef ARCH_MAC
LIBX64FOLDER = lib/x86_64-darwin
LIBARMFOLDER = lib/aarch64-darwin
COMPILERFLAGSX64 = -Px86_64 -CpCOREAVX -Fi$(LIBX64FOLDER) -FU$(LIBX64FOLDER) -obin/sanguinetagupdater_intel
COMPILERFLAGSARM = -Paarch64 -Fi$(LIBARMFOLDER) -FU$(LIBARMFOLDER) -obin/sanguinetagupdater_arm
endif

all: sanguinetagupdater rackpjeditor

ifndef ARCH_MAC
sanguinetagupdater:	outputfolders
	fpc $(COMPILERFLAGS) $(EXTRAFLAGS) sanguinetagupdater.pas

rackpjeditor: outputfolders
	lazbuild --bm="Release Intel x64" pjeditor.lpi

outputfolders:
	-$(MKDIRCMD) lib
	-$(MKDIRCMD) $(LIBFOLDER)
	-$(MKDIRCMD) bin
endif

ifdef ARCH_MAC
sanguinetagupdater: outputfolders
ifndef NOINTEL
	fpc $(COMPILERFLAGS) $(COMPILERFLAGSX64) $(EXTRAFLAGS) sanguinetagupdater.pas
endif
ifndef NOARM
	fpc $(COMPILERFLAGS) $(COMPILERFLAGSARM) $(EXTRAFLAGS) sanguinetagupdater.pas
endif
ifndef NOFAT
	lipo -create -output ./bin/sanguinetagupdater ./bin/sanguinetagupdater_arm ./bin/sanguinetagupdater_intel
	strip ./bin/sanguinetagupdater
endif
# MISSING!!! CODESIGN stuff!!!
	@echo codesign should be here!

rackpjeditor: outputfolders
ifndef NOINTEL
	lazbuild --bm="Release MacOS Intel" pjeditor.lpi
endif
ifndef NOARM
	lazbuild --bm="Release MacOS ARM" pjeditor.lpi
endif
ifndef NOFAT
	lipo -create -output ./bin/pjeditor ./bin/pjeditor_arm ./bin/pjeditor_intel
	strip ./bin/pjeditor
	cp ./bin/pjeditor ./bin/pjeditor.app/Contents/MacOS
	cp ./iconsrc/pj_editor_icon.icns ./bin/pjeditor.app/Contents/Resources
	cp ./info.plist ./bin/pjeditor.app/Contents
endif
# MISSING!!! CODESIGN stuff!!!
	@echo codesign should be here!

outputfolders:
	-mkdir lib
	-mkdir $(LIBX64FOLDER)
	-mkdir $(LIBARMFOLDER)
	-mkdir bin
	-mkdir ./bin/pjeditor.app
	-mkdir ./bin/pjeditor.app/Contents
	-mkdir ./bin/pjeditor.app/Contents/MacOS
	-mkdir ./bin/pjeditor.app/Contents/Resources
endif

clean:
	-rm -r lib

cleanall: clean
	-rm -r bin
