# hypo3d
HYPO3D is a computer program for determining hypocenter and magnitude
of local earthquakes in the 3-dimensional velocity model.
Author Milan Werl wrote this programm under the guidance of Petr Firbas in the years 1986-1988.
The HYPO3D is derived from the program HYPO71

 http://www.jclahr.com/science/software/hypo71/index.html

3-dimensional velocity model is constructed of rectangular blocks
and ray-tracing is solved in 1-dimensional layered model reduced of 3-d for each line between the source and the receiver point.
The problem is solved in map coordinates S-JTSK Křovák EPSG:5513, but the unit is kilometers, not meters.

## Authors
* **Petr Firbas** - 1986 - Initial work.
* **Milan Werl** - 1986-1987 - Researcher and programmer for the most part of the code.
* **Vladimír Dvořák** and **Libor Vejmělek** - 1996 - Porting program of HP minicomputer platform to SUN workstation.

## Licence
HYPO3D is primarily designed for users in Institute of Physics of the Earth, Masaryk University in Brno.
Potential users or contributors are welcome. 
But note that this is working repository and this is not an end-user release. 
