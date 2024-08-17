#!/bin/bash
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

echo "Preparing to build Sanguine PJ Editor."
echo "lazbuild must be present in your path!"
echo "Building Sanguine Tag Updater"
lazbuild --bm="Release Intel x64" sanguinetagupdater.lpi
if [ $? != 0 ];
then
	echo "Error building Sanguine Tag Updater"
	echo "Something went really, really wrong."
    echo "Check the code for errors and make sure lazbuild is in your path."
	exit
else
	echo "Done building Sanguine PJ Editor"
fi