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

unit ModuleTagging;

{$mode ObjFPC}{$H+}

interface

type
  TTagAlias = record
    TagName: string;
    DeprecatedTags: array of string;
  end;

const
  ModuleTags: array of string = (
    'Arpeggiator',
    'Attenuator',
    'Blank',
    'Chorus',
    'Clock generator',
    'Clock modulator',
    'Compressor',
    'Controller',
    'Delay',
    'Digital',
    'Distortion',
    'Drum',
    'Dual',
    'Dynamics',
    'Effect',
    'Envelope follower',
    'Envelope generator',
    'Equalizer',
    'Expander',
    'External',
    'Filter',
    'Flanger',
    'Function generator',
    'Granular',
    'Hardware clone',
    'Limiter',
    'Logic',
    'Low-frequency oscillator',
    'Low-pass gate',
    'MIDI',
    'Mixer',
    'Multiple',
    'Noise',
    'Oscillator',
    'Panning',
    'Phaser',
    'Physical modeling',
    'Polyphonic',
    'Quad',
    'Quantizer',
    'Random',
    'Recording',
    'Reverb',
    'Ring modulator',
    'Sample and hold',
    'Sampler',
    'Sequencer',
    'Slew limiter',
    'Speech',
    'Switch',
    'Synth voice',
    'Tuner',
    'Utility',
    'Visual',
    'Vocoder',
    'Voltage-controlled amplifier',
    'Waveshaper'
    );

  DeprecatedTags: array of TTagAlias = (
    (TagName: 'Clock generator'; DeprecatedTags: ('Clock')),
    (Tagname: 'Drum'; DeprecatedTags: ('Drums', 'Percussion')),
    (TagName: 'Equalizer'; DeprecatedTags: ('EQ')),
    (TagName: 'Filter'; DeprecatedTags: ('VCF', 'Voltage controlled filter')),
    (TagName: 'Hardware clone'; DeprecatedTags: ('Hardware')),
    (TagName: 'Low-frequency oscillator';
    DeprecatedTags: ('LFO', 'Low frequency oscillator')),
    (TagName: 'Low-pass gate'; DeprecatedTags: ('Low pass gate', 'Lowpass gate')),
    (TagName: 'Oscillator'; DeprecatedTags: ('VCO', 'Voltage controlled oscillator')),
    (TagName: 'Panning'; DeprecatedTags: ('Pan')),
    (TagName: 'Polyphonic'; DeprecatedTags: ('Poly')),
    (TagName: 'Sample and hold'; DeprecatedTags: ('S&H', 'Sample & hold')),
    (TagName: 'Voltage-controlled amplifier';
    DeprecatedTags: ('Amplifier', 'VCA', 'Voltage controlled amplifier'))
    );

function GetTagAlias(const Tag: string): string;

implementation

uses
  SysUtils;

function GetTagAlias(const Tag: string): string;
var
  Item: integer;
  TagAlias: TTagAlias;
  S: string;
begin
  Result := '';
  for Item := 0 to Length(DeprecatedTags) - 1 do
  begin
    TagAlias := DeprecatedTags[Item];
    for S in TagAlias.DeprecatedTags do
      if AnsiCompareText(Tag, S) = 0 then
        Exit(TagAlias.TagName);
  end;
end;

end.
