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

ifeq ($(OS),Windows_NT)
	ARCH_WIN = 1
	CPU_X64 = 1
	undefine ARCH_UNKNOWN
else
	UNAME_OS := $(shell uname -s)
	ifeq ($(UNAME_OS),Linux)
		ARCH_LIN = 1
		CPU_X64 = 1
		undefine ARCH_UNKNOWN
	endif
	ifeq ($(UNAME_OS),Darwin)
		ARCH_MAC = 1
		UNAME_CPU := $(shell uname -p)
		ifeq ($(UNAME_CPU),x86_64)
			CPU_X64 = 1
		endif
		ifneq ($(filter arm%,$(UNAME_P)),)
			CPU_ARM = 1
		endif
		undefine ARCH_UNKNOWN
	endif
endif

ifdef ARCH_UNKNOWN
$(error Could not determine machine type.)
endif

ifdef ARCH_WIN
PROJECT_FILE = Release Intel x64
endif

ifdef ARCH_LIN
PROJECT_FILE = Release Intel x64
endif

ifdef ARCH_MAC
ifdef CPU_X64
PROJECT_FILE = Release Mac Intel
endif
ifdef CPU_ARM
PROJECT_FILE = Release Mac ARM
endif
endif

all: sanguinetagupdater

sanguinetagupdater:
	lazbuild --bm="$(PROJECT_FILE)" sanguinetagupdater.lpi