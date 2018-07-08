
# hypo3d
HYPO3D is a computer program for determining hypocenter and magnitude of local earthquakes.
The program is an attempt to solve the localization of the earthquake in a 3-dimensional velocity model.
Author Milan Werl wrote this program under the guidance of Petr Firbas in the years 1986-1988.
It was used in the Institute of Physics of the Earth, Masaryk University in Brno.

## Program features and limitations

The program is an implementation of the classical Geiger’s method for earthquake location.
Another inspiration was the HYPO71 (http://www.jclahr.com/science/software/hypo71/index.html),
especially when programming forward modeling.

### Velocity model
The 3-dimensional velocity model is constructed from rectangular homogeneous blocks.
The space of the model is divided into blocks in three perpendicular directions by parallel planes with irregular spacings.
The number of dividing planes (and the number of blocks) is limited, now it is 15x15x40.
The velocities of longitudinal seismic waves in units of [km/s] are assigned to these blocks.

### Forward modelling
The program reflects the 3-dimensional model only in a limited way.
The ray-tracing is solved in 1-dimensional layered model reduced from 3-d for each section 
between the source and the receiver point.
This procedure applies the linearization scheme used in tomography but not in full.
As a result, it is difficult to estimate the exact effect of the model, it must be tested.
The program works as expected in the 1-dimensional model, it can handle a simple model like a vertical 
velocity interface but it is not suitable for working with a complex 3-dimensional model.

### Coordinates
The map coordinate system S-JTSK Křovák EPSG:5513 is used for program input and output
(but the unit is [km]).
This coordinate system is defined only for Bohemia, Slovakia, and near border regions.

The velocity model is defined in local coordinates.
Most of the computations is made in an internal coordinate system connected with the velocity model.
This brings some limitations, especially in the case of fixing some coordinates.

### Weighting
The program uses a in seismology well-established weight codes for phase arrivals measurements
(from HYPO71, HYPOINVERSE).
Weight codes are:
```
 0 - full weight
 1 - 3/4 weight
 2 - half weight
 3 - 1/4 weight
 4 - no weight
```
The question is how to convert weight codes into weighting factors. 
Original HYPO3D to version 1.65 compute weighting factors linearly (according to HYPO71). 
From HYPO3D version 1.66 it is changed and weights are applied quadratically (according to HYPOINVERSE).

## Authors

|     |      |               |
| --- | ---- | ------------- |
|   Petr Firbas   | 1986 | Initial work. |
| **Milan Werl** | 1986-1987 | Researcher and programmer for the most of the code. |
| Vladimír Dvořák and Libor Vejmělek | 1996 | Porting program of HP minicomputer platform to SUN workstation. |

Luděk Klimeš contributed to the initial code.
The program includes parts of the library FITPACK (coded by Alan Kaylor Cline) and
subroutines of IBM SSP (Scientific Subroutine Package).

## Plans
This project is intended for maintenance of code and documentation.
HYPO3D program has been used for 30 years with minimal modifications. 
No further development is foreseen but for further use it is needed:

1. Reduce the ballast code for clarity.
1. Fix some known bugs.
1. Gather documentation of this software.

## Upgrades

There are only minor changes compared to the original version 1.50, 
which does not change the features of the computer program.

1. To clear the code, about 9,000 rows of code were reduced.
   Deleting the ballast (inaccessible or unusable) code 
   did not affect the functionality.
2. The parameter reading_error was originally hard coded.
   Now the default value can be overwritten
   as a second optional argument in the velocity model file (line number 5th).
3. The coordinates x,y of the epicenter and their estimate errors dxer,dyer
   were presented in different coordinate systems.
   (Epicenter in map coordinates Křovák EPSG:5513 but errors in model internal coordinates).
   Now it is consistently in map coordinates Křovák.  
4. The azimuth of rays emerging from the focus and the angle
   of rotation of the error ellipse is counted with respect
   to meridian convergence of Křovák coordinates at the focal point.
5. From version 1.66 the weights of measurements are applied quadratically, not linearly.


## Licence
HYPO3D is primarily designed for users in Institute of Physics of the Earth, Masaryk University in Brno.
Potential users or contributors are welcome. 
But note that this is the working repository and not an end-user release. 
