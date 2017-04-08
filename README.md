# hypo3d
HYPO3D is a computer program for determining hypocenter and magnitude
of local earthquakes in the 3-dimensional velocity model.
Author Milan Werl wrote this programm under the guidance of Petr Firbas in the years 1986-1988.
The HYPO3D is derived from the program HYPO71:

 http://www.jclahr.com/science/software/hypo71/index.html

The 3-dimensional velocity model is constructed from rectangular blocks
and the ray-tracing is solved in 1-dimensional layered model reduced from 3-d for each line
between the source and the receiver point.
The problem is solved in map coordinates S-JTSK Křovák EPSG:5513, but the unit is [km].

## Authors
* **Petr Firbas** - 1986 - Initial work.
* **Milan Werl** - 1986-1987 - Researcher and programmer for the most part of the code.
* **Vladimír Dvořák** and **Libor Vejmělek** - 1996 - Porting program of HP minicomputer platform to SUN workstation.

## Plans
This project is intended for maintenance of code and documentation.
HYPO3D program has been used for 30 years with minimal modifications. 
No further development is foreseen but for further use it is needed:

1. Reduce the ballast code for clarity.
1. Fix some known bugs.
1. Gather documentation of this software.

## Licence
HYPO3D is primarily designed for users in Institute of Physics of the Earth, Masaryk University in Brno.
Potential users or contributors are welcome. 
But note that this is the working repository and not an end-user release. 
