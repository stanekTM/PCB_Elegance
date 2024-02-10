# PCB Elegance
PCB Elegance is a collection of circuit board design tools for Windows 7, 8 and 10.
- Symbol editor 
- Schematic editor 
- Geometry editor 
- Layout editor 
- Gerber file generation 
- View gerber (does not contain source code. ViewPlot 2.0 is a viewer only, more at http://www.viewplot.com)

It was commercial software from 1998-2012.
In 2012 it was released as open source software under the GPL.

PCB Elegance is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY,
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

Credits
PCB Elegance is maintained by Tim Worthington

http://www.pcbelegance.org

tim@pcbelegance.org

Copyright (C) 2012 Herman Morsink Vollenbroek

## My version PCB Elegance
This version is not installed, just extract the file folder to disk.
The key is the folder "PCB_Elegance_v3.52.1", here is also the latest version of the release.
By default, the programs are in "EN".

Other translations are only in "CZ".
Copy the path of the translation folder in advance, for example "C:\PCB_Elegance_v3.52.1\translations\CZ".
Then start the main program "Design manager" by right-clicking "run as administrator" and click on "Configure paths" in the "Edit" tab.
In the "Language path set" setting, switch to the "Other language" entry and enter the path of the translation folder, for example "C:\PCB_Elegance_v3.52.1\translations\CZ".
Confirm "OK" and restart the program.
All translations should be in Czech.

Remember that there are thousands of words in software tools and some are not ready yet.

Translations in "CZ" will by no means go with the original version from http://www.pcbelegance.org

## Instructions for building/compiling
Unzip the source code archive into am empty directory.

The source code can be compiled with Microsoft Visual Studio 2022 (version "Community" is free)        
https://visualstudio.microsoft.com/en/downloads

To support Windows 7, 8 and 10 :            
- SDK version : 10.0              
- Platform tools set : Visual Studio 2022 (v143)

Open the solution file "vc2010\pcbeleg.sln"

The solution contains all the projects for "PCB Elegance 3.52.1"

When doing a rebuild (debug/release) all executables/libraries are compiled.
