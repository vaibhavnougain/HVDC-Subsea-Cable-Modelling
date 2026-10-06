PSCAD LINE CONSTANTS CASE - 525 kV ARMOURED HVDC SUBMARINE CABLE
================================================================
Mustafa Al-Fahham, School of Engineering, University of Aberdeen
Supervisor: Dr Vaibhav Nougain

WHAT THIS FOLDER IS
-------------------
* PSCAD calculates the cable's series impedance Z and shunt admittance
  Y directly from the geometry and the material data.
* These are the reference values that the analytical model is compared
  against in the paper.
* Nothing here is run as a time-domain simulation.  You only press
  "Solve Constants".  It takes a few seconds.


WHAT YOU NEED
-------------
* PSCAD 4.6.2 or later.  The free Educational edition is enough; this
  case is far below its size limit.
* No Fortran compiler is needed for this folder.  The companion
  analytical folder does need one; this one does not.


FILES IN THIS FOLDER
--------------------
* HVDC_cable_LCP.pscx        the PSCAD case.  This is the only file
                             you open.
* 525Kv_Subsea_Cable.cli     PSCAD's echo of the data it was given.
* 525Kv_Subsea_Cable.out     the results.
* 525Kv_Subsea_Cable.log     run log, including the solver version.
* 525Kv_Subsea_Cable.clo     conductor layout echo.

The last four are the output of the run described below.  They are
included so you can compare without running anything.


STEP BY STEP
------------
 1. Start PSCAD.

 2. File -> Open...  Select HVDC_cable_LCP.pscx in this folder.

 3. The project appears in the Workspace panel on the left, under
    Projects, named HVDC_cable_LCP.

 4. In the Workspace panel expand Main.  You will see one component,
    Cable_1 '525Kv_Subsea_Cable'.

 5. Double-click Main to open the schematic.  One cable component is
    on the canvas.

 6. Double-click the cable component.  The cable definition canvas
    opens, showing the cross-section drawing and three information
    boxes.  Check the drawing is labelled, from the centre outwards:

        0.03    0.0598   0.063   0.0672   0.0732   0.0798   [m]

    Do NOT open the "Coax Cable Cross-Section" dialog.  See WARNING
    below.

 7. The Cables tab appears on the ribbon while this canvas is open.
    Click it.

 8. Click "Solve Constants".

 9. Wait a few seconds.  The Build Messages panel should report
    0 Errors.

10. Results are now in a folder called HVDC_cable_LCP.gf42, created
    next to the .pscx file.  Open it and read

        525Kv_Subsea_Cable.out

    with any text editor.

    You can also read the same output inside PSCAD: at the bottom of
    the cable canvas click the "Output" tab.

11. On closing, PSCAD asks "Do you want to save changes in workspace
    'Untitled'?".  Answer No.  The workspace is only a list of open
    projects; it is not part of this case.

12. The HVDC_cable_LCP.gf42 folder can be deleted afterwards.  It is
    regenerated every time you solve.


WHERE THE NUMBERS ARE
---------------------
In 525Kv_Subsea_Cable.out, find the heading

    PHASE DOMAIN DATA @    50.000 Hz:

Under it are two 3x3 matrices.  Each entry is printed as a pair,
real,imaginary.  The conductor order is [core, sheath, armour].

    SERIES IMPEDANCE MATRIX (Z)  [ohms/m]
    SHUNT ADMITTANCE MATRIX (Y)  [mhos/m]

The sequence-component matrices printed below them are derived from
these two and are not used in the paper.


CHECK YOU GOT THE RIGHT ANSWER
------------------------------
The first row of Z should read

    0.664234459E-04,0.550265204E-03
    0.538612607E-04,0.494471738E-03
    0.522315744E-04,0.461272744E-03

and the first entry of Y should read

    0.000000000E+00,0.705320625E-07

If those match, the run is correct.


UNITS
-----
PSCAD prints per metre.  The paper uses per kilometre.

    R [ohm/km]  = Re(Z) x 1000
    L [mH/km]   = Im(Z) / (2 x pi x 50) x 1000 x 1000
    C [uF/km]   = Im(Y) / (2 x pi x 50) x 1000 x 1e6

Example: Z(1,1) = 0.664234459E-04 ohm/m gives R = 0.0664234459 ohm/km
and L = 1.75154854 mH/km.


WARNING - THE CROSS-SECTION DIALOG
----------------------------------
* Opening the "Coax Cable Cross-Section" dialog and pressing Ok can
  change the geometry, even if you type nothing.
* It adds the 1.8 mm outer semiconducting screen on top of the first
  insulator's outer radius, moving it from 0.0598 to 0.0616, and
  shifts the layers outside it by 0.7 mm.
* The sheath wall then becomes 1.4 mm instead of 3.2 mm and the sheath
  self-resistance roughly doubles.
* If you ever suspect this has happened, open 525Kv_Subsea_Cable.cli
  and check these five lines:

        Insulator 1 Outer Radius = 0.0598
        Sheath Outer Radius      = 0.063
        Insulator 2 Outer Radius = 0.0672
        Armour Outer Radius      = 0.0732
        Insulator 3 Outer Radius = 0.0798

  If Insulator 1 reads 0.0616, close PSCAD without saving and start
  again from the file as supplied.


NOTES ON THE RECORDED SETTINGS
------------------------------
* Seabed resistivity 0.67 ohm.m, burial depth 1.0 m, both recorded in
  the .cli file as GroundResistivity and Depth below ground surface.
* Underground earth-return method is Direct Numerical Integration.
  This is why the analytical model, which uses the Wedepohl-Wilcox
  logarithmic approximation, shows a small constant offset in every
  entry of Z.
* "Frequency for Calculation = 50.0" is the extraction frequency used
  in the paper.  "Steady State Frequency = 50.0" is the EMTDC
  initialisation frequency and plays no part in this calculation.
* Layer thicknesses are entered as radii measured from the centre of
  the cable, not as individual thicknesses.
* Conductors to eliminate is set to none, so the full 3x3 primitive
  matrices are reported.  Ideal cross-bonding is disabled.
* Solver: Line Constants Program for PSCAD X4, build 2016.08.11,
  recorded in 525Kv_Subsea_Cable.log.


THE COMPANION FOLDER
--------------------
The analytical model that these values are compared against is in the
HVDC_Analytical_Blocks folder.  Its README describes how to build and
run it.
