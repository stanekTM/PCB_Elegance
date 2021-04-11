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

# My version PCB Elegance
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

# For "C" fans and programmers
### Thanks to the dev "zhouxs1023" for supporting x64 code!

Unfortunately, these tools are no longer evolving, the only mentioned thread speaks for itself https://www.eevblog.com/forum/eda/pcb-elegance-new-version/

My calls for a code pitbucket repair were never accepted, so I set out on my own.
That's how I got to "C" and started learning.
There are many features that I do not use in the production of printed circuit boards and could be removed.
For now, however, I leave these functions to others if they are interested.
This version of "PCB Elegance" is compatible with Windows 7, 8 and 10.
It is possible to compile for Windows XP, but you must change the compilation target in the IDE.
Until recently, I used this system successfully.
I have already stopped testing the XP system, I use Windows 10 for tests and work.
Testing takes place during the design of the project I am currently creating.
If I find a bug, I'll try to fix it.

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
