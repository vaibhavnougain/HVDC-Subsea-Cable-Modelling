HVDC SUBMARINE CABLE - ANALYTICAL PARAMETER MODEL IN PSCAD
==========================================================
Mustafa Al-Fahham, School of Engineering, University of Aberdeen
Supervisor: Dr Vaibhav Nougain


WHAT THIS IS
------------
The analytical derivation of the primitive R, L, C and G matrices of a
525 kV single-core armoured HVDC submarine cable, written as Fortran that
is compiled and run inside PSCAD as five computation blocks, and compared
against PSCAD's own Line Constants Program at 50 Hz.

Both routes run in the same program from the same input set, so the
comparison tests the FORMULATION, not the transfer of numbers between
tools.

It is not a time-domain simulation.  It computes once, writes a report,
and stops.

The Line Constants case it is compared against is in the companion folder
"PSCAD LCP files".


FILES
-----
Analytical_Check.pscx   the PSCAD case.  This is what you open.
analytical_rlc.f        the source code PSCAD compiles.
analytical_rlc.txt      byte-identical copy, so the code can be read or
                        diffed without opening PSCAD.  If the .f and the
                        .txt ever differ, one has been tampered with.
analytical_all_out.txt  the output of the run described below, included
                        so you can compare without running anything.
PROCESS_DIAGRAM.txt     what each of the five blocks does, step by step.

Not included: the Analytical_Check.gf42 build folder, the .psmx window
layout and the .bakx auto-backup.  PSCAD regenerates all three, and the
build folder holds absolute paths from the machine it was built on.


WHAT YOU NEED
-------------
PSCAD 4.6 or later with its bundled GFortran 4.2.1 compiler.  The free
Educational edition is enough.  Nothing else - no Excel, no external
libraries.


BEFORE YOU START - READ THIS ONE
--------------------------------
* The project stores the FULL PATH to analytical_rlc.f, and that path is
  the one on the author's machine.
* On your machine it will be wrong, and you must correct it in step 3.
* If you do not, PSCAD still reports 0 Errors, still builds, still runs,
  and produces NOTHING.  There is no error message of any kind.
* Step 6 is the check that catches this.  Do not skip it.


HOW TO RUN
----------
 1. Copy this whole folder somewhere on your own machine.

 2. Start PSCAD.  File -> Open...  Select Analytical_Check.pscx.
    The project appears in the Workspace panel on the left.

 3. Point the project at YOUR copy of the source:
      * Project tab -> Project Settings
      * Fortran tab
      * Under "Additional Source files", click Browse
      * Select analytical_rlc.f in this folder
      * OK

 4. Double-click Main in the Workspace panel.  You should see five
    blocks on the canvas, labelled BLOCK 1 to BLOCK 5.

 5. Home tab -> Build.
      * Expect 0 Errors.
      * Around 84 warnings is normal.  Only the error count matters.

 6. Home tab -> Run.  It finishes in about a second.

 7. CHECK IT ACTUALLY RAN.  Open analytical_all_out.txt in this folder
    and read the top two lines:

        RUN AT   : <date and time>
        VERDICT  : GOOD RUN.  All 9 structural checks PASS.

      * RUN AT must be the time you pressed Run.  If it is older, the
        source was not compiled in - go back to step 3.
      * VERDICT must say GOOD RUN.  If it says PROBLEM, one of the
        structural checks failed, and SECTION 6 says which.

 8. Read the results in either place:
      * analytical_all_out.txt in this folder - everything, at full
        precision, SECTION 0 to SECTION 7.
      * PSCAD's RUNTIME MESSAGES tab - the same summary on screen.
        Use Runtime Messages, NOT Build Messages; Build Messages only
        reports compilation.

 9. On closing, PSCAD may ask "Do you want to save changes in workspace
    'Untitled'?".  Answer No.  The workspace is only a list of open
    projects.

10. The Analytical_Check.gf42 folder that appears can be deleted.  It is
    the build output and is regenerated every time you build.


CHECK YOU GOT THE RIGHT ANSWER
------------------------------
The headline block at the top of the output should read:

    R  diff [%]  : mean  0.170761   max  0.208586
    L  diff [%]  : mean  0.019985   max  0.021184
    Y  diff [%]  : mean  9.9955E-08 max  4.2751E-07

and SECTION 4 should begin

    Z(1,1)   6.6532394068253453E-02   5.5016937200280625E-01


THE FIVE BLOCKS
---------------
The blocks on the Main canvas follow the dependency order of the model:

                   BLOCK 2  Conductor impedances  --,
                  /                                  \
    BLOCK 1  --------- BLOCK 3  Magnetic and seabed ---+-- BLOCK 4
    Geometry and      \                                    Series
    materials          `-- BLOCK 5  Shunt matrix C G Y     matrix

Every block holds its own data.  Double-click one to see and edit it:

    BLOCK 1   22 inputs   Core and frequency / Sheath / Armour / Seabed
    BLOCK 2    2 inputs   Wedepohl constants 0.733 and 0.3179, Eq. (4)
    BLOCK 3    4 inputs   Magnetic regions
    BLOCK 4   12 inputs   PSCAD reference R and L
    BLOCK 5   12 inputs   Geometry / Permittivity / Loss tangents /
                          PSCAD reference Y

Edit a value, then Build and Run again, and the whole chain recomputes.

Blocks 4 and 5 hold the PSCAD Line Constants reference values.  These are
transcribed from the Line Constants output; they are not computed here,
and they only set the percentages printed in SECTION 7.  Changing them
does not change any analytical result.  Only the independent entries are
entered, since the matrices are symmetric and Ycs = -Ycc and Yca = 0
follow from the structure.

The blocks may execute in any order.  Each returns immediately if its own
work is done, or if what it depends on has not run yet, so the chain
completes within the first few timesteps whichever order PSCAD uses.
Moving the blocks around the canvas cannot break the model.


RESULTS AT 50 Hz
----------------
Differences from PSCAD Line Constants, computed in the code as
100 * |analytical - PSCAD| / |PSCAD|:

                        over 9 entries        over 6 unique entries
    Resistance R        mean 0.1708 %         mean 0.1529 %
                        max  0.2086 %         max  0.2086 %
    Inductance L        mean 0.0200 %         mean 0.0198 %
                        max  0.0212 %         max  0.0212 %
    Admittance Y        mean 1.00e-7 %        (7 non-zero entries)
                        max  4.28e-7 %

Both averaging conventions are printed because they give different
numbers.  Any paper quoting these must say which one it uses.

Nine structural checks are evaluated in the code and must all PASS.
They test correctness rather than agreement: a broken implementation
fails them even when its numbers look plausible.


WHAT THE OUTPUT FILE CONTAINS
-----------------------------
    verdict banner   run time, GOOD RUN or PROBLEM, headline percentages
    SECTION 0        inputs, and which block dialog owns each one
    SECTION 1        equivalent material properties, penetration factors
    SECTION 2        internal, surface and transfer impedances
    SECTION 3        magnetic regions and seabed return
    SECTION 4        primitive Z, R and L matrices
    SECTION 5        potential coefficients, C, G and Y
    SECTION 6        structural checks, PASS or FAIL
    SECTION 7        comparison against PSCAD Line Constants

Conductor order is [core, sheath, armour] everywhere.  R is in ohm/km,
L in mH/km, C in uF/km, G and Y in S/m.

Z and Y are not separate results from R, L and C.  They are the same
quantities written two ways, and both forms are printed:

    Z = R + j.omega.L     so  R = Re(Z),  L = Im(Z)/omega
    Y = G + j.omega.C     so  G = Re(Y),  C = Im(Y)/omega

G is identically zero here because all three loss tangents are zero.
That is verified as a structural check, not assumed.

The shunt side is derived independently of the series side, from an
electrostatic problem.  Y is NOT the inverse of Z.


WHY THERE IS A SMALL DIFFERENCE
-------------------------------
All nine entries of Z differ from PSCAD by the same constant complex
amount, to a spread of 1e-5.  The only term common to all of them is the
seabed return impedance zE.  Subtract it and the conductor terms agree to
2e-7 %.

The cause is a formulation difference, not an error.  This model uses the
Wedepohl-Wilcox logarithmic approximation to the earth-return integral;
Line Constants evaluates the exact Pollaczek integral.  Evaluating that
integral for this geometry predicts the observed resistance difference
with no fitted parameter.

A residual of 0.018 % on the reactance is not explained by any input or
by the earth-return formulation.  It is reported as a bounded limitation.


IF SOMETHING GOES WRONG
-----------------------
* Build reports 0 Errors but the output file is old
      -> the source path in step 3 is wrong.

* The output file is not in this folder
      -> the code could not write here and fell back to the build
         folder, Analytical_Check.gf42.  The header of the file says so
         and gives the error number.

* VERDICT says PROBLEM
      -> read SECTION 6 and see which of the nine checks failed.

* Build fails with linker errors about duplicate subroutine names
      -> more than one copy of analytical_rlc.f is attached to the
         project.  Only one is allowed.


NOTES IF YOU MODIFY THE CODE
----------------------------
* The subroutines ANBLK1..ANBLK5, ANCOTH and ANCSCH are the verified
  computation, carried over unchanged.  Everything else - the wrappers
  ANB1..ANB5, the shared module, the reporting - sits around them, never
  inside them.

* PSCAD compiles this source with GFortran 4.2.1 using
      -c -ffree-form -fdefault-real-8
  so it must stay FREE form: "!" comments, trailing "&" continuations.
  Never REAL*8 or D0 literals; -fdefault-real-8 promotes DOUBLE PRECISION
  to sixteen bytes and it will not match REAL(8) dummies.

* PSCAD writes Main.f itself and compiles THAT as FIXED form.  So in the
  block segments: column 1 must be blank, continuations put a character
  in column 6, and anything past column 72 is discarded silently.  The
  segments are indented and split two arguments per line for this reason.
  The longest generated line is 50 columns.

* A component definition with no instance on the canvas generates no code
  at all, and the build still succeeds.  The Workspace tree shows the
  count in brackets: AN_Block1 (1) means one instance exists.

* The output file path is hard-coded in SUBROUTINE ANREPT, in FOUT, and
  must be built with CHAR(92), never as a literal backslash string.
  GFortran treated backslashes as escapes by default until GCC 4.3, and
  PSCAD ships 4.2.1, so "\a" in a literal path becomes a BEL character
  and the OPEN fails.  A modern GFortran behaves the opposite way, so
  this cannot be reproduced by testing the file outside PSCAD.

* Around 84 build warnings are normal.  -Wconversion fires on almost
  every mixed-type expression once -fdefault-real-8 is in play.  0 Errors
  is the number that matters.


SCOPE
-----
50 Hz is a common extraction point chosen for this benchmark.  It is not
an HVDC operating fundamental.

This is implementation consistency between two computational routes
sharing one input set.  It is not independent physical validation; no
measurement is involved.
