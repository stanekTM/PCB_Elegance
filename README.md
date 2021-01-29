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

# My edited version PCB Elegance
Somehow I didn't like "Kickad" and "EasyEda", I just prefer PCB Elegance which is closer to me.
This version of "PCB Elegance" is compatible with Windows 7, 8 and 10.
I want to say in advance that with my modified version I generate gerber files reliably as the original versions and have PCBs manufactured in China.
This version is not installed, just unzip the itself files folder to disk. The key is the "PCB_Elegance_v3.52.1" folder, here is also the latest version of the release.
By default, the programs are in "EN". Other translations are only in "CZ". If you are a proud Czech, copy the translation files from the "Translations\CZ" folder to the main "PCB_Elegance_v3.52.1" directory.
Keep in mind that there are thousands of words in software tools and some are not yet finished. 

Translations in "CZ" will by no means go with the original version from http://www.pcbelegance.org

# Instructions for building/compiling
Unzip the source code archive into am empty directory.

The source code can be compiled with Microsoft Visual Studio 2019 (version "Community" is free)        
https://visualstudio.microsoft.com/en/downloads

Download the package Microsoft Visual C++ Redistributable pro Visual Studio 2015, 2017 a 2019        
https://support.microsoft.com/cs-en/help/2977003/the-latest-supported-visual-c-downloads

Note : Visual C++ 2015, 2017 a 2019 shares the same redistributable files.
For example, installing redistributable software Visual C ++ 2019 affects programs created with Visual C ++ 2015 a 2017.
Installation distributable versions Visual C++ 2015 it does not replace newer versions of files with installed distributable versions Visual C++ 2017 a 2019.

To support Windows 7, 8 and 10 :            
- Version Windows : SDK 10.0              
- Platform tools set : Visual Studio 2019 - Windows 10 (v142)

To support Windows XP :              
- Version Windows : SDK 7.0              
- Platform tools set : Visual Studio 2017 - Windows XP (v141_xp)

Open the solution file "vc2010\pcbeleg.sln"

The solution contains all the projects for "PCB Elegance 3.52.1"

When doing a rebuild (Debug/release) all executables/libraries are compiled.
#
Jiri StanekTM
