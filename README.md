
# hypo3d

HYPO3D is a software tool designed to determine the hypocenter and magnitude
of local earthquakes.
The program aims to solve earthquake location within a 3-dimensional velocity model.
Authored by Milan Werl under the guidance of Petr Firbas between 1986 and 1988,
this software has been utilized at the Institute of Physics of the Earth,
Masaryk University in Brno.

## Program Features and Limitations

HYPO3D implements the classical Geiger’s method for earthquake location, with
inspiration drawn from HYPO71 \[[Lee and Lahr 1975](#lee1975)\].
Ray tracing is based on a modified TRVDRV subroutine developed by \[[Eaton 1969](#eaton1969)\].

### Velocity Model

The 3-dimensional velocity model consists of rectangular homogeneous blocks
of rectilinear structure with irregular spacing in three perpendicular directions.
The model is limited to 100x100x100 blocks,
with seismic wave velocities assigned in units of [km/s].

<p align="center">
<img src="doc/img/panelak.png" alt="Rectilinear structure of the 3-d velocity model" width="40%" />
<br>
Rectilinear structure of the 3-d velocity model
</p>

The upper boundary of the model is defined by a surface, interpolated using splines.

### Forward Modeling

HYPO3D reflects the 3-dimensional model by solving ray tracing in a 1-dimensional
layered model for each section between the earthquake source and receiver point.
The travel-time computation involves several steps:

1. Creating a vertical plane cross-section of the 3-dimensional velocity model
   between the source and receiver.
2. Converting the 3-dimensional model into a 1-dimensional model, preserving
   horizontal layers.
3. Computing the ray trajectory in the 1-dimensional velocity model, utilizing
   a two-point fast ray-tracing scheme similar to HYPO71.
4. Calculating the travel-time by integrating along the ray path
   in the 3-dimensional velocity model.

The authors of the computer program justify this approach by reference to
\[[Romanov 1972](#romanov1972), [Firbas 1984](#firbas1984)\]
and consider this solution to be a linearization approach.

### Applicability Limits

The method assumes regularity, making it suitable for some seismic phases but not for all.
It is known that this approach may not provide sufficient accuracy for refracted
or head waves passing along curved or sloping interfaces, as discussed by \[[Ryaboy 2001](#ryaboy2001)\].

<p align="center">
<img src="doc/img/downdip.png" alt="Hypo3d ray tracing" width="60%" />
<br>
Hypo3d approximate ray tracing along the dipping planar interface.
</p>

Originally designed for short-distance direct phases, HYPO3D may have limitations
in locating regional-distance earthquakes within complex 3-dimensional velocity models.

### Coordinates

The map coordinate system S-JTSK Křovák EPSG:5513 is utilized for program input
and output (with units in [km]).
The velocity model, however, is defined in local coordinates, which may impose
limitations, particularly in fixing hypocenters.

### Weighting

The weighted least squares algorithm considers weight codes for phase arrivals
measurements.
These weight codes range from full weight (0) to no weight (4),
following the conventions of HYPO71 and HYPOINVERSE.

## Authors

<table>
<tbody>
<tr class="odd">
<td>Petr Firbas, Luděk Klimeš</td>
<td>1986</td>
<td>Initial work.</td>
</tr>
<tr class="even">
<td><a href="https://cz.linkedin.com/in/milan-werl-a174357"><strong>Milan Werl</strong></a></td>
<td>1986-1987</td>
<td>Researcher and programmer for the most of the code.</td>
</tr>
<tr class="odd">
<td>Vladimír Dvořák and Libor Vejmělek</td>
<td>1996</td>
<td>Porting program of HP minicomputer platform to SUN workstation.</td>
</tr>
</tbody>
</table>

The program includes parts of the library FITPACK (coded by Alan Kaylor Cline) and
subroutines of IBM SSP (Scientific Subroutine Package).

## Plans

The project aims to maintain code and documentation, and to identify program limitations.
No further development is intended.

## Upgrades

Minor changes compared to the original version were primarily focused
on code clarity and bug fixes.
Further upgrades enhanced functionality, addressing issues such as coordinate
consistency and model limitations.

1. To streamline the code, approximately 9,000 lines of program code were removed.
   Removing unreachable or redundant code had no impact on functionality.
2. Originally, the parameter reading_error was hardcoded.
   Now, its default value can be overridden as a second optional argument
   in the velocity model file (line number 5).
   (v10.50)
3. The coordinates (x, y) of the epicenter and their estimated errors (dxer, dyer)
   were previously presented in different coordinate systems
   (Epicenter in map coordinates Křovák EPSG:5513 but errors in model local coordinates).
   Now, they are consistently presented in map coordinates.
   (v10.62)
4. In cases of coordinate fixation,
   the calculation of the error covariance matrix is not reduced in dimensions
   unless the hypocenter is on or near the surface.
   (v10.73)
5. The azimuth of rays emerging from the focus and the angle
   of rotation of the error ellipse are now calculated with respect
   to the meridian convergence of Křovák coordinates at the focal point.
   (v10.64)
6. The limitation on the number of blocks in the velocity model has been extended
   from the original 15x15x40 to 100x100x100.
   (v10.73)
7. Previously,
   the a posteriori estimate of the arrival variance was not unbiased
   because the individual residuals were first adjusted
   based on the a priori determined reading error.
   Now, the arrival variance estimate is calculated first
   and then adjusted according to the value of the reading error.
   (v10.70)
8. If a small number of measurements does not provide any degree of freedom,
   only the 'a priori' part of the standard deviation (parameter reading error)
   is used to calculate the error ellipse.
   If the error interval cannot be estimated, NaN is printed.
   (v10.73)
9. Previously, the seismic location program assumed a constant ratio between
   P-wave and S-wave velocities. With the update to version 10.79, the program
   now allows the input of two velocity models, enabling seismic location based
   on a more accurate relationship between P-wave and S-wave velocities.
   (v10.79)
10. Station corrections can be entered separately for both P-wave and S-wave
   seismic phases.
   (v10.80)


## Installation

HYPO3D is adapted for the Linux environment and the gfortran compiler.
Compilation is facilitated through the supplied Makefile, resulting in the binary
hypo3d, which can be manually copied to the binary path.

## Documentation

The original documentation of the program is available in the report by
Firbas P. and Werl M. ([PDF](https://github.com/firbas/hypo3d/blob/master/doc/hypo3d_Werl.pdf)).
Additional information, including running the program and formats, is provided
in the [wiki](https://github.com/firbas/hypo3d/wiki).

## License

HYPO3D is freely available, primarily for users at the Institute of Physics of the Earth,
Masaryk University in Brno.
However, it's a working repository, not intended for end-user release.

## Literature

<a name="eaton1969"></a>Eaton, J. P. (1969). HYPOLAYR, a computer program for determining hypocenters of local earthquakes in an earth consisting of uniform flat layers over a half space, Open File Report, U.S. Geological Survey, 155 pp.

<a name="firbas1984"></a>Firbas, P. & Vaněk, J. (1984). Travel time curves for complex inhomogeneous slightly anisotropic media. Stud Geophys Geod (1984) 28: 393-406.
DOI https://doi.org/10.1007/BF01642992

<a name="lee1975"></a>Lee, W. H. K. and J. C. Lahr (1975). HYP071 (Revised): A computer program for determining hypocenter, magnitude, and first motion pattern of local earthquakes, U. S. Geological Survey Open File Report 75-311, 113 pp.

<a name="romanov1972"></a>Романов В. Г. (1972). Некоторые обратные задачи для уравнений гиперболического типа. Издательство "Наукa", Новосибирск, 1972

<a name="ryaboy2001"></a>Ryaboy V., Baumgardt D.R., Firbas P., Dainty A.M. (2001). Application of 3-D crustal and upper mantle velocity model of North America for Location of Regional Seismic Events. Pure appl. geophys. 158 (2001) 79-103. Birkhäuser Verlag, Basel, 2001.
DOI https://doi.org/10.1007/PL00001169

