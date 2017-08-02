# POVRayRender For Mathematica
======

POVRayRender is a *Mathematica* package that uses POVRay to ray-trace 3D graphics inline.

Features
--------

* Ray-trace 3D graphics inline in Mathematica notebook.
* Preserve the color and smoothness of the 3D object.

Requirements
------------

* [*Mathematica*][mma].
* [*POVRay*][povray].

Installation
------------

Copy the POVRayRender folder to your `$UserBaseDirectory`. 

> On Windows system should be this directory ```FileNameJoin[{$InstallationDirectory, "AddOns/ExtraPackages"}]```


Usage
-----

`POVRayRender[graphics3D]`

Usage example:

`POVRayRender@Plot3D[Sin[x + y^2], {x, -3, 3}, {y, -2, 2}, ColorFunction -> Hue, Mesh -> None, PlotPoints -> 50]`

Rendered image examples:


<table border="0">
  <tr>
    <th><img src="http://i.imgur.com/viskRMw.png" height="300"/></th>
    <th><img src="http://i.imgur.com/2riqmTx.png" height="300"/></th>
    <th><img src="http://i.imgur.com/0UPD116.png" height="300"/></th>
  </tr>
</table>


For more examples, see documentation (search "POVRayRender" in Wolfram Lauguage Documentation Center).

[mma]:http://www.wolfram.com/mathematica/
[povray]:http://www.povray.org/
