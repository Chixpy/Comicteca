unit uCTKEditorConst;
{< Comicteca Editor constants unit.

  This file is part of Comicteca Editor.

  Copyright (C) 2020 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const  
  krsCTKEditorIni = 'CTKEditor.ini';
  //< File for Comicteca Editor configuration.

  // Files
  kSeriesCBFile = 'Series.txt';
  kTitlesCBFile = 'Titles.txt';
  kPublishersCBFile = 'Publishers.txt';
  kEditorsCBFile = 'Editors.txt';
  kLanguageCBFile = 'Languages.txt';

  // Config INI sections
  kIniFilesKey = 'Files';
  kIniLastFolderKey = 'LastFolder';

implementation

end.

