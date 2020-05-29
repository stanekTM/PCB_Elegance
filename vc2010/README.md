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

The solution contains all the projects for "PCB Elegance 3.52.0"

When doing a rebuild (Debug/release) all executables/libraries are compiled.

George StanekTM

