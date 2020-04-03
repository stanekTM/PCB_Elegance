PCB Elegance
=================

PCB Elegance is a collection of circuit board design tools for Microsoft Windows. Including schematic capture, board layout, and manufacturing file generation. It was commercial software from 1998-2012. In 2012 it was released as open source software under the GPL.

PCB Elegance is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

PCB Elegance is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.


Instructions for building/compiling
===================================

Unzip the source code archive into am empty directory. The source code can be compiled with either VC2015 (Microsoft Visual Studio 2015) or MingGW.

-----------

For VC2015 open the solution file vc2010\pcbeleg.sln. The solution contains all the projects for pcb elegance 3.5. When doing a rebuild (Debug/release) all executables/libraries are compiled.

-----------

For MingGW you need:

* The MingGW build system 
  Download from http://www.mingw.org

* The Microsoft HTML Help Workshop
  Download from http://www.microsoft.com
  Copy the htmlhelp.h file to <mingw path>\include
  Copy the htmlhelp.lib file to <mingw path>\lib


There are a few scripts that can be used to build the entire system:

build.cmd
---------
Assuming you have console window opened, and are in the main pcb elegenace directory, and have the mingw\bin directory added to your path.

Type "build -j 4"  

All executables/libraries are being build using up to 4 processor cores.

mingwbuild.cmd
--------------
Assuming you have console window opened, and are in the main pcb elegenace directory, and have the mingw\bin directory added to your path.

Type "mingwbuild -j 4"  

Every project (executables/libraries) is compiled using up to 4 processor cores. 
Building/compiling using this command is a little bit slower than using build.cmd.

For the mingw build environment you will have to work with the windows command shell. The standard windows command shell (cmd.exe), has its drawbacks. There are however replacements. Check the link http://sourceforge.net  (search for console)


Credits
=======

PCB Elegance is maintained by Tim Worthington.

tim@pcbelegance.org http://www.pcbelegance.org

Copyright (C) 2012  Herman Morsink Vollenbroek
