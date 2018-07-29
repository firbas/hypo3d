
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
The velocities of longitudial seismic waves in units of [km/s] are assigned to these blocks.

<p align="center">
<img src="doc/img/panelak.png" alt="Cross-wall structure" style="max-width:35%;" />
<br>
Cross-wall structure of the 3-d velocity model
</p>

### Forward modelling
The program HYPO3D reflects the 3-dimensional model only in a limited way.
The ray-tracing is solved in 1-dimensional layered model reduced from 3-d for each section 
between the source and the receiver point.   
The travel-time is calculated for each source-receiver pair in the following steps:  
1) An vertical plane cross-section of the 3-d velocity model is made between the source and the receiver.  
2) The conversion procedure for the 1-d model consists in preserving horizontal layers (the floor in the cross-wall structure of the 3-d model) and the seismic velocities in each layer are merged into one velocity with the equivalent propagation time in this layer between the source and the receiver.   
3) An ray is computed in the 1-d velocity model, using two-point fast ray-tracing scheme taken from HYPO71.   
4) The travel-time is then calculated by integrating in the 3-d velocity model along the ray path approximately calculated in the previous step.

The authors of the computer program justify this approach by reference to [[Romanov 1972](#romanov1972)],
[Firbas 1984](#firbas1984)] and consider this solution to be a linearization approach.

The described solution process has features of the perturbation method.
This method provides an approximate forward modeling solution whose accuracy depends on the complexity of the velocity model and the type of seismic phases. 
It is difficult to estimate the exact effect of the model, it must be tested.
It is known [[Ryaboy 2001](#ryaboy2001)] that this procedure does not guarantee sufficient accuracy 
for refracted or head waves that pass along curved or sloping interfaces.
<!-- [[Ryaboy 2001](#ryaboy2001)] writes that
the linearization approach, which can be successfully applied in seismic tomography,
is not directly applicable to our problem because it does not guarantee the accuracy needed for regional phases,
whose ray paths are passing close to the curved surface. -->


This method can only be accurate enough if the ray path perturbances have little effect on the total travel-time.


### Coordinates
The map coordinate system S-JTSK Křovák EPSG:5513 is used for program input and output
(but the unit is [km]).
This coordinate system is defined only for Bohemia, Slovakia, and near border regions.

The velocity model is defined in local coordinates.
For most internal calculations, the model local coordinate system is used.
This brings some limitations, especially in the case of fixing hypocenter in some coordinates.

### Weighting
The program uses a in seismology well-established weight codes for phase arrivals measurements
(from HYPO71, HYPOINVERSE).
Weight codes are:
```
 0 - full weight, 1 - 3/4 weight, 2 - half weight, 3 - 1/4 weight, 4 - no weight
```
The question is how to convert weight codes into weighting factors. 
Original HYPO3D to version 1.65 compute weighting factors linearly (according to HYPO71). 
From HYPO3D version 1.66 it is changed and weights are applied quadratically (according to HYPOINVERSE).

## Authors

| | | |
|-|-|-|
|   Petr Firbas   | 1986 | Initial work. |
| [**Milan Werl**](https://cz.linkedin.com/in/milan-werl-a174357?trk=org-employees_mini-profile_cta) | 1986-1987 | Researcher and programmer for the most of the code. |
| Vladimír Dvořák and Libor Vejmělek | 1996 | Porting program of HP minicomputer platform to SUN workstation. |

Luděk Klimeš contributed to the initial code.
The program includes parts of the library FITPACK (coded by Alan Kaylor Cline) and
subroutines of IBM SSP (Scientific Subroutine Package).

## Plans
HYPO3D program has been used for 30 years with minimal modifications. 
This project is intended for maintenance of code and documentation and
to test and document the limitations of this program.
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
   (Epicenter in map coordinates Křovák EPSG:5513 but errors in model local coordinates).
   Now it is consistently in map coordinates Křovák.  
4. The azimuth of rays emerging from the focus and the angle
   of rotation of the error ellipse is counted with respect
   to meridian convergence of Křovák coordinates at the focal point.
5. From version 1.66 the weights of measurements are applied quadratically, not linearly.

## Documentation

The original documentation of the program is in the report
[Firbas P., Werl M.: HYPO3D, rev. 9.00. Lokalizace ve 3D blokovém prostředí. Etapová zpráva. Geofyzika n.p., Brno 1988](https://github.com/firbas/hypo3d/blob/master/doc/hypo3d_Werl.pdf)

Additional information
([starting the program](https://github.com/firbas/hypo3d/wiki#starting-the-hypo3d-program), formats)
is available at the wiki site:   
https://github.com/firbas/hypo3d/wiki

## Licence
HYPO3D is primarily designed for users in Institute of Physics of the Earth, Masaryk University in Brno.
The computer program is free to use. 
But note that this is the working repository and not an end-user release
and consider the limited capabilities of the program.

## Literature

<a name="romanov1972"></a>Романов В. Г. (1972). Некоторые обратные задачи для уравнений гиперболического типа. Издательство "Наукa", Новосибирск, 1972

<a name="firbas1984"></a>Firbas, P. & Vaněk, J. (1984). Travel time curves for complex inhomogeneous slightly anisotropic media. Stud Geophys Geod (1984) 28: 393-406.
DOI https://doi.org/10.1007/BF01642992

<a name="ryaboy2001"></a>Ryaboy V., Baumgardt D.R., Firbas P., Dainty A.M. (2001). Application of 3-D crustal and upper mantle velocity model of North America for Location of Regional Seismic Events. Pure appl. geophys. 158 (2001) 79-103. Birkhäuser Verlag, Basel, 2001.
DOI https://doi.org/10.1007/PL00001169

