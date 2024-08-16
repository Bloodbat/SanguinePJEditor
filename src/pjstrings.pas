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

unit PJStrings;

{$mode ObjFPC}{$H+}

interface

const
  sAuthor = 'author';
  sAuthorEmail = 'authorEmail';
  sAuthorURL = 'authorUrl';
  sBrand = 'brand';
  sChangeLogURL = 'changelogUrl';
  sDescription = 'description';
  sDonateURL = 'donateUrl';
  sHidden = 'hidden';
  sKeywords = 'keywords';
  sLicense = 'license';
  sManualURL = 'manualUrl';
  sMinRackVersion = 'minRackVersion';
  sModularGridURL = 'modularGridUrl';
  sModules = 'modules';
  sName = 'name';
  sPluginURL = 'pluginUrl';
  sSlug = 'slug';
  sSourceURL = 'sourceUrl';
  sTags = 'tags';
  sVersion = 'version';

resourcestring
  rsAboutDate = 'Built on: %.2d/%.2d/%d';
  rsAboutFPCVer = 'FPC Version: %d.%d.%d';
  rsAboutLazVer = 'Lazarus Version: %s';
  rsAboutCopyYear = 'Copyright (C) 2024.';
  rsAboutSerpienteyRosa = 'La Serpiente y la Rosa Producciones.';
  rsAboutCodeCreditHeader = 'Coding and design:';
  rsAboutRightsReserved = 'All Rights Reserved.';
  rsAboutIconsCredit =
    'Uses icons from the Oxygen icon theme, copyright Oxygen icon theme authors.';
  rsAboutGNU1 =
    'Sanguine PJ Editor is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.';
  rsAboutGNU2 =
    'Sanguine PJ Editor is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.';
  rsAboutGNU3 = 'Click the "License" item in the "Help" menu for more information.';

  // Status
  rsReady = 'Ready';
  rsLoading = 'Loading...';
  rsSaving = 'Saving...';

  // Errors
  rsErrorTitle = 'Error!';
  rsErrorInvalidJSon = 'Invalid or corrupt json manifest.' + LineEnding + 'Aborting.';

implementation

end.
