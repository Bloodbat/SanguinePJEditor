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

  ArrayManifestKeywords: array of string = (
    'slug',
    'name',
    'modules',
    'tags',
    'author',
    'version',
    'description',
    'license',
    'authorEmail',
    'authorUrl',
    'pluginUrl',
    'manualUrl',
    'sourceUrl',
    'changelogUrl',
    'donateUrl',
    'minRackVersion',
    'brand',
    'keywords',
    'modularGridUrl',
    'hidden'
    );

  iSlug = 0;
  iName = 1;
  iModules = 2;
  iTags = 3;
  iAuthor = 4;
  iVersion = 5;
  iDescription = 6;
  iLicense = 7;
  iAuthorEmail = 8;
  iAuthorURL = 9;
  iPluginURL = 10;
  iManualURL = 11;
  iSourceURL = 12;
  iChangeLogURL = 13;
  iDonateURL = 14;
  iMinRackVersion = 15;
  iBrand = 16;
  iKeywords = 17;
  iModularGridURL = 18;
  iHidden = 19;

resourcestring
  rsAboutDate = 'Built on: %.2d/%.2d/%d';
  rsAboutFPCVer = 'FPC Version: %d.%d.%d';
  rsAboutLazVer = 'Lazarus Version: %s';
  rsAboutCopyYear = 'Copyright © 2024.';
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
  rsStatusApplying = 'Applying...';
  rsStatusLoading = 'Loading...';
  rsStatusNewManifest = 'New manifest...';
  rsStatusReady = 'Ready';
  rsStatusReverting = 'Reverting...';
  rsStatusSaving = 'Saving...';

  // Errors
  rsErrorTitle = 'Error!';
  rsErrorInvalidJSon = 'Invalid or corrupt json manifest.' + LineEnding + 'Aborting.';

  // Queries
  rsQuestionInvalidManifest =
    'The manifest cannot be saved.' + LineEnding +
    'If you continue, all changes will be lost' + LineEnding +
    'Cancel the operation so you can fix it?';
  rsQuestionInvalidManifestTitle = 'Invalid Manifest Found';
  rsQuestionModified = 'Save changes to manifest?';
  rsQuestionModifedTitle = 'Manifest Changed';
  rsQuestionUncommited = 'If you continue, the most recent changes will be lost.' +
    LineEnding + 'Cancel the operation so you can commit them?';
  rsQuestionUncommitedTitle = 'Uncommited Changes Found';

  // Info. Errors
  rsErrorModule = 'Invalid Module Info.';
  rsErrorPlugin = 'Invalid Plugin Info.';

implementation

end.
