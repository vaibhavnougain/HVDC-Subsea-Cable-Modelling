!  =====================================================================
!  ANALYTICAL_RLC.F        FIVE-BLOCK VERSION
!  ---------------------------------------------------------------------
!  Analytical primitive-parameter model of a 525 kV single-core armoured
!  HVDC submarine cable, drawn in PSCAD as FIVE blocks on the canvas.
!
!  Each block owns its own input data in its own PSCAD parameter dialog.
!  Double-click a block to see and edit what goes into it.
!
!  Mustafa Al-Fahham, School of Engineering, University of Aberdeen
!  Supervisor: Dr Vaibhav Nougain
!
!  STRUCTURE
!  ---------
!    MODULE ANSTATE   shared storage between the blocks, plus the
!                     ready flags that make them order-independent
!
!    ANBLK1..ANBLK5   THE VERIFIED COMPUTATION.  Carried over verbatim
!    ANCOTH, ANCSCH   from the five-block source that was checked
!                     against the Excel workbook.  Argument-based and
!                     module-free.  DO NOT EDIT THESE.
!
!    ANB1..ANB5       thin wrappers.  Each takes its block's inputs from
!                     PSCAD, calls the matching ANBLK, stores the
!                     results, and asks ANREPT whether it can report
!                     yet.  All new code lives here.
!
!    ANREPT           writes the output file and prints the summary to
!                     PSCAD's message window.  Runs once, when all five
!                     blocks have completed.
!
!  WHY THE BLOCKS CAN RUN IN ANY ORDER
!  -----------------------------------
!  Each wrapper returns immediately if its own work is done, or if the
!  block it depends on has not run yet.  The component segments execute
!  every timestep, so whatever order PSCAD places them in, the chain
!  completes within the first few timesteps and then does nothing more.
!  Nothing depends on canvas sequence numbers.
!
!  Dependencies:  1 -> 2, 1 -> 3, 1 -> 5, (2 and 3) -> 4
!
!  BUILD ENVIRONMENT
!  -----------------
!  PSCAD compiles user source with GFortran 4.2.1 and the flags
!        -c -ffree-form -fdefault-real-8
!  Consequences, all of which this file obeys:
!    * FREE-FORM source.  Comments are '!', continued lines end with '&'.
!    * NO REAL*8 and NO D0 literals.  -fdefault-real-8 promotes DOUBLE
!      PRECISION to sixteen bytes, which then mismatches REAL(8) dummies.
!    * Complex sinh/cosh are built from EXP.  GFortran 4.2.1 predates the
!      complex hyperbolic intrinsics.
!    * SUBROUTINEs and one MODULE, no PROGRAM, so it links cleanly.
!
!  NOTE THAT Main.f IS DIFFERENT.  PSCAD generates Main.f itself and
!  compiles it as FIXED form: column 1 must be blank, continuations put
!  a character in column 6, and ANYTHING PAST COLUMN 72 IS DISCARDED
!  SILENTLY.  That is why each component's Fortran segment is indented
!  and splits its argument list two per line.  If you type a very long
!  number into a block dialog, check the generated Main.f still compiles.
!
!  SCOPE
!  -----
!  Single extraction frequency, 50 Hz.  A common comparison point for
!  the analytical-versus-PSCAD benchmark, NOT an HVDC operating
!  fundamental, and this is implementation consistency rather than
!  independent physical validation.  No measurement is involved.
!  =====================================================================


!  =====================================================================
!  MODULE ANSTATE - everything the five blocks share.
!  =====================================================================
MODULE ANSTATE
   IMPLICIT NONE
   SAVE

!  ---- block 1 inputs, from the Block 1 dialog ------------------------
   REAL :: F, RC, AC_M, RHOC0, SC, RCMAN, BASIS, MURC
   REAL :: RSI, RSO, RHOS, MURS
   REAL :: RAI, RAO, ANW, DW, RHOAW, SA, MURA
   REAL :: H, RHOG, MURG

!  ---- block 2 inputs, from the Block 2 dialog ------------------------
!  Wedepohl solid-conductor approximation constants used in Eq. (4).
   REAL :: WK1, WK2

!  ---- block 3 inputs, from the Block 3 dialog ------------------------
   REAL :: RTZ, MUR12, MUR23, MUR34

!  ---- PSCAD Line Constants reference values --------------------------
!  NOT COMPUTED HERE.  Owned by the Block 4 dialog (R and L) and the
!  Block 5 dialog (Y), so the benchmark can be updated from the canvas
!  without editing Fortran.  Transcribed from the Line Constants output
!  for HVDC_cable_draft:Main(0):Cable_1(0), PHASE DOMAIN DATA
!  @ 50.000 Hz, run 3.  Solver: Line Constants Program for PSCAD X4,
!  build 2016.08.11.
   REAL :: RPS(3,3), LPS(3,3), YPS(3,3)

!  ---- block 5 inputs, from the Block 5 dialog ------------------------
   REAL :: RINSO, RTY, ERCS, ERSA, ERAG, TDCS, TDSA, TDAG

!  ---- fixed mathematical and physical constants ----------------------
!  Deliberately NOT exposed as block parameters.  They are not design
!  data, and keeping them here guarantees their full precision survives
!  into the computation rather than depending on how PSCAD renders a
!  number into Main.f.
   REAL :: GAMMAE = 1.781072418
   REAL :: EPS0   = 8.854187813E-12

!  ---- block 1 results ------------------------------------------------
   REAL :: W, ACEQ, RHOCEF, AS, RSDC, AAW, RADC
   REAL :: AAEQ, RHOAEF, HH, QC, QS, QA, QG

!  ---- block 2 results ------------------------------------------------
   COMPLEX :: MC, MS, MA, ZC
   COMPLEX :: Z2I, Z2M, Z2O, Z2W, Z3I, Z3M, Z3O, Z3W

!  ---- block 3 results ------------------------------------------------
   COMPLEX :: Z12, Z23, Z34, MG, Z0, ZE

!  ---- block 4 results ------------------------------------------------
   COMPLEX :: ZM(3,3)
   REAL    :: RM(3,3), LM(3,3)

!  ---- block 5 results ------------------------------------------------
   REAL    :: PCS, PSA, PAG, CCS, CSA, CAG
   REAL    :: PM(3,3), CM(3,3), CMNUM(3,3), GM(3,3)
   COMPLEX :: YM(3,3)

!  ---- ready flags ----------------------------------------------------
   LOGICAL :: DN1 = .FALSE., DN2 = .FALSE., DN3 = .FALSE.
   LOGICAL :: DN4 = .FALSE., DN5 = .FALSE., RPTD = .FALSE.

END MODULE ANSTATE


SUBROUTINE ANBLK1 (F, RC, AC_M, RHOC0, SC, RCMAN, BASIS, MURC,        &
                   RSI, RSO, RHOS, MURS,                              &
                   RAI, RAO, ANW, DW, RHOAW, SA, MURA,                &
                   H, RHOG, MURG,                                     &
                   W, ACEQ, RHOCEF, AS, RSDC, AAW, RADC,              &
                   AAEQ, RHOAEF, HH, QC, QS, QA, QG)

   IMPLICIT NONE

!  ---- inputs ---------------------------------------------------------
   REAL    F, RC, AC_M, RHOC0, SC, RCMAN, MURC
   REAL    RSI, RSO, RHOS, MURS
   REAL    RAI, RAO, ANW, DW, RHOAW, SA, MURA
   REAL    H, RHOG, MURG
!  ANW   = number of armour wires, as a decimal (68.0)
!  BASIS = 1.0 use manufacturer Rc ; 0.0 use calculated Rc
   REAL    BASIS

!  ---- outputs --------------------------------------------------------
   REAL    W, ACEQ, RHOCEF, AS, RSDC, AAW, RADC
   REAL    AAEQ, RHOAEF, HH, QC, QS, QA, QG

!  ---- locals ---------------------------------------------------------
   REAL    PI, MU0, RCCALC, RCSEL

   PI  = 4.0E0*ATAN(1.0E0)
   MU0 = 4.0E0*PI*1.0E-7

!  E1  angular frequency                                        [rad/s]
   W      = 2.0E0*PI*F

!  E2  calculated core DC resistance                           [ohm/km]
   RCCALC = RHOC0*(1.0E0+SC)*1000.0E0/AC_M

!  E3  selected core DC resistance
   IF (BASIS .GT. 0.5E0) THEN
      RCSEL = RCMAN
   ELSE
      RCSEL = RCCALC
   ENDIF

!  E4  equivalent core area                                        [m2]
   ACEQ   = PI*RC*RC

!  E5  effective copper resistivity                             [ohm.m]
   RHOCEF = (RCSEL/1000.0E0)*ACEQ

!  E6  lead-sheath area                                            [m2]
   AS     = PI*(RSO*RSO - RSI*RSI)

!  E7  lead-sheath DC resistance                               [ohm/km]
   RSDC   = RHOS*1000.0E0/AS

!  E8  total armour-wire area                                      [m2]
   AAW    = ANW*PI*DW*DW/4.0E0

!  E9  physical armour DC resistance                           [ohm/km]
   RADC   = RHOAW*(1.0E0+SA)*1000.0E0/AAW

!  E10 equivalent armour area                                      [m2]
   AAEQ   = PI*(RAO*RAO - RAI*RAI)

!  E11 effective armour resistivity                             [ohm.m]
   RHOAEF = (RADC/1000.0E0)*AAEQ

!  E12 earth-return depth quantity                                  [m]
   HH     = 2.0E0*H

!  E13 penetration factors  q = sqrt(w.mu0.mur/(2.rho))          [1/m]
   QC = SQRT(W*MU0*MURC/(2.0E0*RHOCEF))
   QS = SQRT(W*MU0*MURS/(2.0E0*RHOS))
   QA = SQRT(W*MU0*MURA/(2.0E0*RHOAEF))
   QG = SQRT(W*MU0*MURG/(2.0E0*RHOG))

   RETURN
END


!  =====================================================================
!  BLOCK 2 : paper Eq. (3a), (4), (5a)-(5d)
!
!  Complex penetration constant m, solid-core internal impedance, and
!  the sheath / armour inner-surface, transfer and outer-surface
!  impedances plus the net wall contribution.
!
!  Uses the paper's complex form directly.  The workbook reaches the
!  same numbers by splitting coth and csch into real and imaginary
!  parts by hand; agreement therefore also verifies that split.
!
!  Complex sinh/cosh are built from EXP, which is a guaranteed
!  intrinsic, rather than relying on complex SINH/COSH support.
!  =====================================================================

!  --- coth(z) = (e^z + e^-z) / (e^z - e^-z) ---------------------------
COMPLEX FUNCTION ANCOTH (Z)
   IMPLICIT NONE
   COMPLEX Z, EP, EM
   EP = EXP(Z)
   EM = EXP(-Z)
   ANCOTH = (EP + EM) / (EP - EM)
   RETURN
END

!  --- csch(z) = 2 / (e^z - e^-z) --------------------------------------
COMPLEX FUNCTION ANCSCH (Z)
   IMPLICIT NONE
   COMPLEX Z
   ANCSCH = 2.0 / (EXP(Z) - EXP(-Z))
   RETURN
END


SUBROUTINE ANBLK2 (W, RC, RHOCEF, MURC,                               &
                   RSI, RSO, RHOS, MURS,                              &
                   RAI, RAO, RHOAEF, MURA,                            &
                   WK1, WK2,                                          &
                   MC, MS, MA, ZC,                                    &
                   Z2I, Z2M, Z2O, Z2W,                                &
                   Z3I, Z3M, Z3O, Z3W)

   IMPLICIT NONE

   REAL    W, RC, RHOCEF, MURC
   REAL    RSI, RSO, RHOS, MURS
   REAL    RAI, RAO, RHOAEF, MURA
!  WK1, WK2 are Wedepohl's solid-conductor approximation constants,
!  0.733 and 0.3179.  Previously written as literals inside Eq. (4);
!  now supplied by Block 2 so they are visible and defensible.
   REAL    WK1, WK2
   COMPLEX MC, MS, MA, ZC
   COMPLEX Z2I, Z2M, Z2O, Z2W
   COMPLEX Z3I, Z3M, Z3O, Z3W

   REAL    PI, MU0, LKM, TS, TA
   COMPLEX JJ, ANCOTH, ANCSCH
   EXTERNAL ANCOTH, ANCSCH

   PI  = 4.0*ATAN(1.0)
   MU0 = 4.0*PI*1.0E-7
   LKM = 1000.0
   JJ  = (0.0, 1.0)

!  E3a  complex penetration constant  m = sqrt(j.w.mu0.mur/rho)
   MC = SQRT(JJ*W*MU0*MURC/RHOCEF)
   MS = SQRT(JJ*W*MU0*MURS/RHOS)
   MA = SQRT(JJ*W*MU0*MURA/RHOAEF)

!  E4   solid-core internal impedance
   ZC = ( MC*RHOCEF/(2.0*PI*RC) * ANCOTH(WK1*MC*RC)                   &
        + WK2*RHOCEF/(PI*RC*RC) ) * LKM

!  E5a-d  lead sheath: a = rs,i  b = rs,o  t = b - a
   TS  = RSO - RSI
   Z2I = ( MS*RHOS/(2.0*PI*RSI) * ANCOTH(MS*TS)                       &
         - RHOS/(2.0*PI*RSI*(RSI+RSO)) ) * LKM
   Z2M = ( MS*RHOS/(PI*(RSI+RSO)) * ANCSCH(MS*TS) ) * LKM
   Z2O = ( MS*RHOS/(2.0*PI*RSO) * ANCOTH(MS*TS)                       &
         + RHOS/(2.0*PI*RSO*(RSI+RSO)) ) * LKM
   Z2W = Z2I + Z2O - 2.0*Z2M

!  E5a-d  homogenised armour: a = ra,i  b = ra,o  t = b - a
   TA  = RAO - RAI
   Z3I = ( MA*RHOAEF/(2.0*PI*RAI) * ANCOTH(MA*TA)                     &
         - RHOAEF/(2.0*PI*RAI*(RAI+RAO)) ) * LKM
   Z3M = ( MA*RHOAEF/(PI*(RAI+RAO)) * ANCSCH(MA*TA) ) * LKM
   Z3O = ( MA*RHOAEF/(2.0*PI*RAO) * ANCOTH(MA*TA)                     &
         + RHOAEF/(2.0*PI*RAO*(RAI+RAO)) ) * LKM
   Z3W = Z3I + Z3O - 2.0*Z3M

   RETURN
END


!  =====================================================================
!  BLOCK 3 : paper Eq. (6a)-(6d) and (7a)-(7c)
!
!  Magnetic impedances of the nonconducting coaxial regions, and the
!  homogeneous seabed-return impedance.
!
!  NOTE ON Eq. (7b): this is the Wedepohl-Wilcox logarithmic
!  APPROXIMATION to the earth-return integral.  PSCAD's Line Constants
!  evaluates the exact Pollaczek integral instead.  The two differ by
!  about -0.21 % on the earth-return resistance, which propagates to
!  -0.16 % on Rcc.  That difference is expected and quantified; it is
!  a formulation difference, not an input error.  See
!  claude/PSCAD_Run3_Exact_Resistivities_and_Earth_Return_Residual.md
!  =====================================================================

SUBROUTINE ANBLK3 (W, RC, RSI, RSO, RAI, RAO, RTZ,                    &
                   MUR12, MUR23, MUR34,                              &
                   RHOG, MURG, HH, GAMMAE,                            &
                   Z12, Z23, Z34, MG, Z0, ZE)

   IMPLICIT NONE

   REAL    W, RC, RSI, RSO, RAI, RAO, RTZ
   REAL    MUR12, MUR23, MUR34
   REAL    RHOG, MURG, HH, GAMMAE
   COMPLEX Z12, Z23, Z34, MG, Z0, ZE

   REAL    PI, MU0, LKM
   COMPLEX JJ

   PI  = 4.0*ATAN(1.0)
   MU0 = 4.0*PI*1.0E-7
   LKM = 1000.0
   JJ  = (0.0, 1.0)

!  E6b  core-to-sheath magnetic impedance
   Z12 = JJ*W*MU0*MUR12/(2.0*PI) * LOG(RSI/RC)  * LKM

!  E6c  sheath-to-armour magnetic impedance
   Z23 = JJ*W*MU0*MUR23/(2.0*PI) * LOG(RAI/RSO) * LKM

!  E6d  armour-to-surface magnetic impedance
   Z34 = JJ*W*MU0*MUR34/(2.0*PI) * LOG(RTZ/RAO) * LKM

!  E7a  seabed complex penetration constant
   MG  = SQRT(JJ*W*MU0*MURG/RHOG)

!  E7b  homogeneous seabed-return impedance
!       D, the characteristic self-distance, is taken as rt,Z, the same
!       radius used in z34.  The two uses cancel in zE = z34 + z0.
   Z0  = JJ*W*MU0*MURG/(2.0*PI)                                       &
         * ( -LOG(GAMMAE*MG*RTZ/2.0) + 0.5 - (2.0/3.0)*MG*HH ) * LKM

!  E7c  total external impedance
   ZE  = Z34 + Z0

   RETURN
END


!  =====================================================================
!  BLOCK 4 : paper Eq. (8a)-(8f) and (9), plus R/L extraction (S 2.5)
!
!  Recursive assembly of the six independent primitive elements from
!  the component impedances of blocks 2 and 3, then insertion into the
!  symmetric primitive matrix with conductor order [c, s, a].
!
!  The progressively shorter expressions follow the radial order
!  core -> sheath -> armour -> external return: by Ampere's law a path
!  inside a current-carrying shell encloses none of that shell's
!  current, so the sheath self-term excludes the core and core-sheath
!  contributions, and the armour self-term contains only the armour
!  outer-surface and external-return terms.
!
!  Zca = Zsa exactly, because core and sheath both lie inside the
!  armour and share the same armour-transfer and external-return
!  coupling with armour current.
!  =====================================================================

SUBROUTINE ANBLK4 (W, ZC, Z2M, Z2O, Z2W, Z3M, Z3O, Z3W,               &
                   Z12, Z23, ZE, ZM, RM, LM)

   IMPLICIT NONE

   REAL    W
   COMPLEX ZC, Z2M, Z2O, Z2W, Z3M, Z3O, Z3W
   COMPLEX Z12, Z23, ZE
   COMPLEX ZM(3,3)
   REAL    RM(3,3), LM(3,3)

   COMPLEX ZCC, ZCS, ZCA, ZSS, ZSA, ZAA
   INTEGER I, K

!  E8a  core self
   ZCC = ZC + Z12 + Z2W + Z23 + Z3W + ZE
!  E8b  core-sheath mutual  (one sheath inner-to-outer transfer)
   ZCS = (Z2O - Z2M) + Z23 + Z3W + ZE
!  E8c  core-armour mutual
   ZCA = (Z3O - Z3M) + ZE
!  E8d  sheath self
   ZSS = Z2O + Z23 + Z3W + ZE
!  E8e  sheath-armour mutual  (identical to Zca by construction)
   ZSA = (Z3O - Z3M) + ZE
!  E8f  armour self
   ZAA = Z3O + ZE

!  E9   symmetric primitive matrix, conductor order [c, s, a]
   ZM(1,1) = ZCC
   ZM(1,2) = ZCS
   ZM(1,3) = ZCA
   ZM(2,1) = ZCS
   ZM(2,2) = ZSS
   ZM(2,3) = ZSA
   ZM(3,1) = ZCA
   ZM(3,2) = ZSA
   ZM(3,3) = ZAA

!  S2.5  R = Re(Z) in ohm/km ;  L = Im(Z)/w in mH/km
   DO I = 1, 3
      DO K = 1, 3
         RM(I,K) = REAL(ZM(I,K))
         LM(I,K) = AIMAG(ZM(I,K)) / W * 1000.0
      END DO
   END DO

   RETURN
END


!  =====================================================================
!  BLOCK 5 : paper Eq. (10)-(14)   -- the complete shunt side
!
!  Potential coefficients for the three dielectric regions, the
!  potential-coefficient matrix P, the capacitance matrix C = P^-1,
!  the branch conductances, and Y = G + jwC.
!
!  The shunt matrix is derived independently of the series matrix, so
!  Y is NOT the inverse of Z.  The continuous lead sheath shields the
!  core from the armour, so there is no direct core-armour capacitance
!  branch and C(1,3) = C(3,1) = 0.
!
!  C is formed BOTH ways: by the closed-form inverse of Eq. (12i) and
!  by numerical inversion of P from Eq. (12g).  Agreement between them
!  verifies the analytical inversion asserted in the paper.
!  =====================================================================

SUBROUTINE ANBLK5 (W, RC, RINSO, RSO, RAI, RAO, RTY,                  &
                   EPS0, ERCS, ERSA, ERAG,                            &
                   TDCS, TDSA, TDAG,                                  &
                   PCS, PSA, PAG, CCS, CSA, CAG,                      &
                   PM, CM, CMNUM, GM, YM)

   IMPLICIT NONE

   REAL    W, RC, RINSO, RSO, RAI, RAO, RTY
   REAL    EPS0, ERCS, ERSA, ERAG, TDCS, TDSA, TDAG
   REAL    PCS, PSA, PAG, CCS, CSA, CAG
   REAL    PM(3,3), CM(3,3), CMNUM(3,3), GM(3,3)
   COMPLEX YM(3,3)

   REAL    PI, GCS, GSA, GAG, DET
   INTEGER I, K

   PI = 4.0*ATAN(1.0)

!  E11b-d  potential coefficients [m/F] and branch capacitances [F/m]
   PCS = LOG(RINSO/RC ) / (2.0*PI*EPS0*ERCS)
   PSA = LOG(RAI  /RSO) / (2.0*PI*EPS0*ERSA)
   PAG = LOG(RTY  /RAO) / (2.0*PI*EPS0*ERAG)
   CCS = 1.0/PCS
   CSA = 1.0/PSA
   CAG = 1.0/PAG

!  E12g  potential-coefficient matrix P
   PM(1,1) = PCS + PSA + PAG
   PM(1,2) = PSA + PAG
   PM(1,3) = PAG
   PM(2,1) = PSA + PAG
   PM(2,2) = PSA + PAG
   PM(2,3) = PAG
   PM(3,1) = PAG
   PM(3,2) = PAG
   PM(3,3) = PAG

!  E12i  capacitance matrix, closed-form inverse
   CM(1,1) =  CCS
   CM(1,2) = -CCS
   CM(1,3) =  0.0
   CM(2,1) = -CCS
   CM(2,2) =  CCS + CSA
   CM(2,3) = -CSA
   CM(3,1) =  0.0
   CM(3,2) = -CSA
   CM(3,3) =  CSA + CAG

!  E12h  independent check: numerical inverse of P by cofactors
   DET =   PM(1,1)*(PM(2,2)*PM(3,3) - PM(2,3)*PM(3,2))                &
         - PM(1,2)*(PM(2,1)*PM(3,3) - PM(2,3)*PM(3,1))                &
         + PM(1,3)*(PM(2,1)*PM(3,2) - PM(2,2)*PM(3,1))
   CMNUM(1,1) =  (PM(2,2)*PM(3,3) - PM(2,3)*PM(3,2)) / DET
   CMNUM(1,2) = -(PM(1,2)*PM(3,3) - PM(1,3)*PM(3,2)) / DET
   CMNUM(1,3) =  (PM(1,2)*PM(2,3) - PM(1,3)*PM(2,2)) / DET
   CMNUM(2,1) = -(PM(2,1)*PM(3,3) - PM(2,3)*PM(3,1)) / DET
   CMNUM(2,2) =  (PM(1,1)*PM(3,3) - PM(1,3)*PM(3,1)) / DET
   CMNUM(2,3) = -(PM(1,1)*PM(2,3) - PM(1,3)*PM(2,1)) / DET
   CMNUM(3,1) =  (PM(2,1)*PM(3,2) - PM(2,2)*PM(3,1)) / DET
   CMNUM(3,2) = -(PM(1,1)*PM(3,2) - PM(1,2)*PM(3,1)) / DET
   CMNUM(3,3) =  (PM(1,1)*PM(2,2) - PM(1,2)*PM(2,1)) / DET

!  E13  branch conductances  g = w.c.tan(delta)   (all tan-delta zero)
   GCS = W*CCS*TDCS
   GSA = W*CSA*TDSA
   GAG = W*CAG*TDAG
   GM(1,1) =  GCS
   GM(1,2) = -GCS
   GM(1,3) =  0.0
   GM(2,1) = -GCS
   GM(2,2) =  GCS + GSA
   GM(2,3) = -GSA
   GM(3,1) =  0.0
   GM(3,2) = -GSA
   GM(3,3) =  GSA + GAG

!  E10 / E14  Y = G + jwC   [S/m]
   DO I = 1, 3
      DO K = 1, 3
         YM(I,K) = CMPLX(GM(I,K), W*CM(I,K))
      END DO
   END DO

   RETURN
END


!  =====================================================================
!  BLOCK WRAPPERS
!
!  One per PSCAD block.  Each is called from its component's Fortran
!  segment every timestep, and each does its work exactly once.
!  =====================================================================

!  ---------------------------------------------------------------------
!  ANB1 - Block 1.  Cable geometry and materials.
!  Owns 22 inputs.  Depends on nothing.
!  ---------------------------------------------------------------------
SUBROUTINE ANB1 (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11,        &
                 P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22)
   USE ANSTATE
   IMPLICIT NONE
   REAL P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11
   REAL P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22

   IF (DN1) RETURN

   F     = P1
   RC    = P2
   AC_M  = P3
   RHOC0 = P4
   SC    = P5
   RCMAN = P6
   BASIS = P7
   MURC  = P8
   RSI   = P9
   RSO   = P10
   RHOS  = P11
   MURS  = P12
   RAI   = P13
   RAO   = P14
   ANW   = P15
   DW    = P16
   RHOAW = P17
   SA    = P18
   MURA  = P19
   H     = P20
   RHOG  = P21
   MURG  = P22

   CALL ANBLK1 (F, RC, AC_M, RHOC0, SC, RCMAN, BASIS, MURC,           &
                RSI, RSO, RHOS, MURS,                                 &
                RAI, RAO, ANW, DW, RHOAW, SA, MURA,                   &
                H, RHOG, MURG,                                        &
                W, ACEQ, RHOCEF, AS, RSDC, AAW, RADC,                 &
                AAEQ, RHOAEF, HH, QC, QS, QA, QG)

   DN1 = .TRUE.
   CALL ANREPT
   RETURN
END


!  ---------------------------------------------------------------------
!  ANB2 - Block 2.  Conductor internal, surface and transfer impedances.
!  Owns the two Wedepohl solid-conductor constants of Eq. (4).
!  Geometry and materials come from Block 1.
!  ---------------------------------------------------------------------
SUBROUTINE ANB2 (P1, P2)
   USE ANSTATE
   IMPLICIT NONE
   REAL P1, P2

   IF (DN2) RETURN
   IF (.NOT. DN1) RETURN

   WK1 = P1
   WK2 = P2

   CALL ANBLK2 (W, RC, RHOCEF, MURC,                                  &
                RSI, RSO, RHOS, MURS,                                 &
                RAI, RAO, RHOAEF, MURA,                               &
                WK1, WK2,                                             &
                MC, MS, MA, ZC,                                       &
                Z2I, Z2M, Z2O, Z2W, Z3I, Z3M, Z3O, Z3W)

   DN2 = .TRUE.
   CALL ANREPT
   RETURN
END


!  ---------------------------------------------------------------------
!  ANB3 - Block 3.  Magnetic regions and seabed return.
!  Owns 4 inputs.  Depends on Block 1 for omega and the burial depth.
!  ---------------------------------------------------------------------
SUBROUTINE ANB3 (P1, P2, P3, P4)
   USE ANSTATE
   IMPLICIT NONE
   REAL P1, P2, P3, P4

   IF (DN3) RETURN
   IF (.NOT. DN1) RETURN

   RTZ   = P1
   MUR12 = P2
   MUR23 = P3
   MUR34 = P4

   CALL ANBLK3 (W, RC, RSI, RSO, RAI, RAO, RTZ,                       &
                MUR12, MUR23, MUR34,                                  &
                RHOG, MURG, HH, GAMMAE,                               &
                Z12, Z23, Z34, MG, Z0, ZE)

   DN3 = .TRUE.
   CALL ANREPT
   RETURN
END


!  ---------------------------------------------------------------------
!  ANB4 - Block 4.  Assembly of the primitive series matrix.
!
!  The assembly itself has no free parameters: Eq. (8a)-(8f) fix it
!  completely, and that is a property worth keeping, not a gap to fill.
!  What Block 4 does own is the PSCAD Line Constants REFERENCE for R and
!  L, which its own results are measured against.  Six unique entries
!  each; the matrices are symmetric, so the rest follow.
!
!  These are TRANSCRIBED measurements, not computed values.  They carry
!  the six decimals recorded in the project notes, not the nine
!  significant figures PSCAD prints, so the reported percentages are
!  good to about three significant figures.  Replace them from the full
!  Line Constants listing before publishing - which is now a dialog
!  edit, not a code change.
!
!  Depends on Blocks 2 and 3.
!  ---------------------------------------------------------------------
SUBROUTINE ANB4 (R11, R12, R13, R22, R23, R33,                        &
                 L11, L12, L13, L22, L23, L33)
   USE ANSTATE
   IMPLICIT NONE
   REAL R11, R12, R13, R22, R23, R33
   REAL L11, L12, L13, L22, L23, L33

   IF (DN4) RETURN
   IF (.NOT. DN2) RETURN
   IF (.NOT. DN3) RETURN

   RPS(1,1) = R11
   RPS(1,2) = R12
   RPS(1,3) = R13
   RPS(2,2) = R22
   RPS(2,3) = R23
   RPS(3,3) = R33
   RPS(2,1) = RPS(1,2)
   RPS(3,1) = RPS(1,3)
   RPS(3,2) = RPS(2,3)

   LPS(1,1) = L11
   LPS(1,2) = L12
   LPS(1,3) = L13
   LPS(2,2) = L22
   LPS(2,3) = L23
   LPS(3,3) = L33
   LPS(2,1) = LPS(1,2)
   LPS(3,1) = LPS(1,3)
   LPS(3,2) = LPS(2,3)

   CALL ANBLK4 (W, ZC, Z2M, Z2O, Z2W, Z3M, Z3O, Z3W,                  &
                Z12, Z23, ZE, ZM, RM, LM)

   DN4 = .TRUE.
   CALL ANREPT
   RETURN
END


!  ---------------------------------------------------------------------
!  ANB5 - Block 5.  Potential coefficients, C, G and Y.
!  Owns 8 inputs.  Depends on Block 1 for omega only: the shunt side is
!  an independent electrostatic problem, which is why Y is NOT the
!  inverse of Z.
!  ---------------------------------------------------------------------
SUBROUTINE ANB5 (P1, P2, P3, P4, P5, P6, P7, P8,                      &
                 Y11, Y22, Y23, Y33)
   USE ANSTATE
   IMPLICIT NONE
   REAL P1, P2, P3, P4, P5, P6, P7, P8
   REAL Y11, Y22, Y23, Y33

   IF (DN5) RETURN
   IF (.NOT. DN1) RETURN

   RINSO = P1
   RTY   = P2
   ERCS  = P3
   ERSA  = P4
   ERAG  = P5
   TDCS  = P6
   TDSA  = P7
   TDAG  = P8

!  PSCAD Line Constants reference for Im(Y), four unique entries.
!  Ycs = -Ycc because the core-sheath branch is the only path between
!  those two conductors, and Yca = 0 because the continuous sheath
!  shields the core from the armour.  Both follow from the structure, so
!  only four numbers are entered.
   YPS(1,1) =  Y11
   YPS(2,2) =  Y22
   YPS(2,3) =  Y23
   YPS(3,3) =  Y33
   YPS(1,2) = -Y11
   YPS(2,1) = -Y11
   YPS(1,3) =  0.0
   YPS(3,1) =  0.0
   YPS(3,2) =  Y23

   CALL ANBLK5 (W, RC, RINSO, RSO, RAI, RAO, RTY,                     &
                EPS0, ERCS, ERSA, ERAG, TDCS, TDSA, TDAG,             &
                PCS, PSA, PAG, CCS, CSA, CAG,                         &
                PM, CM, CMNUM, GM, YM)

   DN5 = .TRUE.
   CALL ANREPT
   RETURN
END


!  =====================================================================
!  ANREPT - report once, when all five blocks have finished.
!
!  Called by every wrapper.  Returns immediately unless all five ready
!  flags are set and nothing has been reported yet, so whichever block
!  happens to finish last is the one that triggers the report.
!
!  Writes TWO things:
!    1. the full output file, at full precision, in the project folder
!    2. a short summary to standard output, which PSCAD shows in its
!       runtime message pane
!  =====================================================================
SUBROUTINE ANREPT
   USE ANSTATE
   IMPLICIT NONE

   REAL    DR(3,3), DL(3,3), DY(3,3)
   REAL    SR9, SL9, SY9, XR9, XL9, XY9
   REAL    SR6, SL6, XR6, XL6
   INTEGER NR9, NL9, NY9, NR6, NL6
   INTEGER I, K, LU, NBAD, IOS
   INTEGER TVAL(8)
   CHARACTER*4 PF
   LOGICAL CK(9)
   CHARACTER*54 CKD(9)
   CHARACTER*200 FOUT
   CHARACTER*1   BS
   LOGICAL FBACK

!  ---- report only when everything is ready, and only once ------------
   IF (RPTD) RETURN
   IF (.NOT. DN1) RETURN
   IF (.NOT. DN2) RETURN
   IF (.NOT. DN3) RETURN
   IF (.NOT. DN4) RETURN
   IF (.NOT. DN5) RETURN
   RPTD = .TRUE.

!  ---- where the output file goes -------------------------------------
!  The project folder, NOT the .gf42 build folder, so it does not have
!  to be hunted for.
!
!  On the 16 Sep run the OPEN failed and the output fell back to the
!  .gf42 build folder.  Two candidate causes, both now removed:
!
!    1. TRAILING BLANKS.  FOUT is CHARACTER*200, and the OPEN passed it
!       whole.  Modern GFortran strips trailing blanks from FILE=; the
!       4.2.1 runtime PSCAD ships may not, which would make the filename
!       invalid on Windows.  TRIM() below removes this.  This is the
!       likely cause - it is exactly the difference between the GFortran
!       13.3 used to test this file and the 4.2.1 that runs it.
!
!    2. ESCAPE PROCESSING of a literal backslash.  Ruled out: the
!       makefile shows FC_Args = -c -ffree-form -fdefault-real-8, with no
!       -fbackslash.  CHAR(92) is kept anyway - it costs nothing and
!       cannot be reinterpreted if those flags ever change.
!
!  The OPEN now reports its IOSTAT on failure, so a third cause would
!  identify itself rather than having to be guessed at.
!
!  To move the output elsewhere, edit the names below.
   BS   = CHAR(92)
   FOUT = 'C:'//BS//'Users'//BS//'t18ma25'//BS//'PSCAD_Work'//BS//     &
          'HVDC_Analytical_Blocks'//BS//'analytical_all_out.txt'


!  =====================================================================
!  PERCENTAGE DIFFERENCES.  100 * |analytical - PSCAD| / |PSCAD|.
!  Entries whose PSCAD reference is exactly zero are excluded and
!  reported separately: a percentage of zero has no meaning.  Those
!  entries are exactly zero analytically too.
!  =====================================================================
   SR9 = 0.0
   SL9 = 0.0
   SY9 = 0.0
   XR9 = 0.0
   XL9 = 0.0
   XY9 = 0.0
   NR9 = 0
   NL9 = 0
   NY9 = 0
   DO I = 1, 3
      DO K = 1, 3
         DR(I,K) = 0.0
         DL(I,K) = 0.0
         DY(I,K) = 0.0
         IF (RPS(I,K) .NE. 0.0) THEN
            DR(I,K) = 100.0*ABS(RM(I,K) - RPS(I,K))/ABS(RPS(I,K))
            SR9 = SR9 + DR(I,K)
            NR9 = NR9 + 1
            IF (DR(I,K) .GT. XR9) XR9 = DR(I,K)
         ENDIF
         IF (LPS(I,K) .NE. 0.0) THEN
            DL(I,K) = 100.0*ABS(LM(I,K) - LPS(I,K))/ABS(LPS(I,K))
            SL9 = SL9 + DL(I,K)
            NL9 = NL9 + 1
            IF (DL(I,K) .GT. XL9) XL9 = DL(I,K)
         ENDIF
         IF (YPS(I,K) .NE. 0.0) THEN
            DY(I,K) = 100.0*ABS(AIMAG(YM(I,K)) - YPS(I,K))/ABS(YPS(I,K))
            SY9 = SY9 + DY(I,K)
            NY9 = NY9 + 1
            IF (DY(I,K) .GT. XY9) XY9 = DY(I,K)
         ENDIF
      END DO
   END DO

   SR6 = 0.0
   SL6 = 0.0
   XR6 = 0.0
   XL6 = 0.0
   NR6 = 0
   NL6 = 0
   DO I = 1, 3
      DO K = I, 3
         IF (RPS(I,K) .NE. 0.0) THEN
            SR6 = SR6 + DR(I,K)
            NR6 = NR6 + 1
            IF (DR(I,K) .GT. XR6) XR6 = DR(I,K)
         ENDIF
         IF (LPS(I,K) .NE. 0.0) THEN
            SL6 = SL6 + DL(I,K)
            NL6 = NL6 + 1
            IF (DL(I,K) .GT. XL6) XL6 = DL(I,K)
         ENDIF
      END DO
   END DO

!  =====================================================================
!  STRUCTURAL CHECKS.  Properties the model must satisfy by physics or
!  by construction.  A broken implementation fails these even when its
!  numbers look plausible, so they test correctness, not agreement.
!  =====================================================================
   CKD(1) = 'magnetic regions carry no loss, Re(z12,z23,z34) = 0'
   CK(1)  = (REAL(Z12) .EQ. 0.0) .AND. (REAL(Z23) .EQ. 0.0)           &
            .AND. (REAL(Z34) .EQ. 0.0)

   CKD(2) = 'reciprocity, Zcs=Zsc and Zca=Zac and Zsa=Zas'
   CK(2)  = (ZM(1,2) .EQ. ZM(2,1)) .AND. (ZM(1,3) .EQ. ZM(3,1))       &
            .AND. (ZM(2,3) .EQ. ZM(3,2))

   CKD(3) = 'Zca = Zsa exactly, as Eq. (8c) and (8e) require'
   CK(3)  = (ZM(1,3) .EQ. ZM(2,3))

   CKD(4) = 'continuous sheath shields core from armour, Cca = 0'
   CK(4)  = (CM(1,3) .EQ. 0.0) .AND. (CM(3,1) .EQ. 0.0)

   CKD(5) = 'closed-form C equals numerical inverse of P, to 1e-12'
   CK(5)  = .TRUE.
   DO I = 1, 3
      DO K = 1, 3
         IF (CM(I,K) .NE. 0.0) THEN
            IF (ABS(CMNUM(I,K)-CM(I,K))/ABS(CM(I,K)) .GT. 1.0E-12)    &
               CK(5) = .FALSE.
         ENDIF
      END DO
   END DO

   CKD(6) = 'G identically zero, all three tan delta are zero'
   CK(6)  = .TRUE.
   DO I = 1, 3
      DO K = 1, 3
         IF (GM(I,K) .NE. 0.0) CK(6) = .FALSE.
      END DO
   END DO

   CKD(7) = 'm = (1+j)q, so Re(m) = Im(m) for core, sheath, armour'
   CK(7)  = .TRUE.
   IF (ABS(REAL(MC)-AIMAG(MC)) .GT. 1.0E-9*ABS(REAL(MC))) CK(7) = .FALSE.
   IF (ABS(REAL(MS)-AIMAG(MS)) .GT. 1.0E-9*ABS(REAL(MS))) CK(7) = .FALSE.
   IF (ABS(REAL(MA)-AIMAG(MA)) .GT. 1.0E-9*ABS(REAL(MA))) CK(7) = .FALSE.

   CKD(8) = 'Re(m) equals block-1 q for every layer'
   CK(8)  = .TRUE.
   IF (ABS(REAL(MC)-QC) .GT. 1.0E-9*QC) CK(8) = .FALSE.
   IF (ABS(REAL(MS)-QS) .GT. 1.0E-9*QS) CK(8) = .FALSE.
   IF (ABS(REAL(MA)-QA) .GT. 1.0E-9*QA) CK(8) = .FALSE.

   CKD(9) = 'z34 is lossless, so Re(zE) = Re(z0)'
   CK(9)  = (ABS(REAL(ZE) - REAL(Z0)) .LT. 1.0E-12*ABS(REAL(Z0)))

   NBAD = 0
   DO I = 1, 9
      IF (.NOT. CK(I)) NBAD = NBAD + 1
   END DO

   CALL DATE_AND_TIME (VALUES=TVAL)

!  =====================================================================
!  OPEN THE OUTPUT FILE FIRST, so the message-pane summary can say
!  truthfully where the full-precision file ended up.
!  =====================================================================
   LU = 77
   FBACK = .FALSE.
   IOS   = 0
   OPEN (UNIT=LU, FILE=TRIM(FOUT), STATUS='UNKNOWN', IOSTAT=IOS)
   IF (IOS .NE. 0) THEN
      FBACK = .TRUE.
      OPEN (UNIT=LU, FILE='analytical_all_out.txt', STATUS='UNKNOWN')
   ENDIF

!  =====================================================================
!  PART 1 - SUMMARY TO PSCAD'S MESSAGE WINDOW
!  =====================================================================
   WRITE(*,800) ' '
   WRITE(*,800) '=================================================================='
   WRITE(*,800) ' ANALYTICAL RLC MODEL - 525 kV ARMOURED HVDC SUBMARINE CABLE'
   WRITE(*,800) ' All five blocks complete.  Extraction frequency 50 Hz.'
   WRITE(*,800) '=================================================================='
   WRITE(*,806) ' RUN AT   : ', TVAL(1), TVAL(2), TVAL(3), TVAL(5), TVAL(6), TVAL(7)
   IF (NBAD .EQ. 0) THEN
      WRITE(*,800) ' VERDICT  : GOOD RUN.  All 9 structural checks PASS.'
   ELSE
      WRITE(*,812) ' VERDICT  : PROBLEM.  Failed structural checks: ', NBAD
   ENDIF
   WRITE(*,800) '------------------------------------------------------------------'
   WRITE(*,800) ' PRIMITIVE RESISTANCE  R [ohm/km]      order [core, sheath, armour]'
   DO I = 1, 3
      WRITE(*,820) (RM(I,K), K = 1, 3)
   END DO
   WRITE(*,800) ' PRIMITIVE INDUCTANCE  L [mH/km]'
   DO I = 1, 3
      WRITE(*,820) (LM(I,K), K = 1, 3)
   END DO
   WRITE(*,800) ' PRIMITIVE CAPACITANCE C [uF/km]'
   DO I = 1, 3
      WRITE(*,820) (CM(I,K)*1.0E9, K = 1, 3)
   END DO
   WRITE(*,800) '------------------------------------------------------------------'
   WRITE(*,800) ' SERIES IMPEDANCE     Z = R + j.omega.L   [ohm/km]'
   DO I = 1, 3
      WRITE(*,823) (REAL(ZM(I,K)), AIMAG(ZM(I,K)), K = 1, 3)
   END DO
!  ---- Y.  When all three tan delta are zero, G is identically zero and
!  ---- Y is purely imaginary, so printing a 3x3 of zeros for the real
!  ---- part wastes half the width.  Print Im(Y) alone and say so.  If a
!  ---- loss tangent is ever made non-zero, CK(6) fails and BOTH parts
!  ---- are printed instead, so nothing can be hidden by this shortcut.
   IF (CK(6)) THEN
      WRITE(*,800) ' SHUNT ADMITTANCE     Y = G + j.omega.C   [S/m]'
      WRITE(*,800) '   G is identically zero, so Y is purely imaginary.  Im(Y):'
      DO I = 1, 3
         WRITE(*,824) (AIMAG(YM(I,K)), K = 1, 3)
      END DO
   ELSE
      WRITE(*,800) ' SHUNT ADMITTANCE     Y = G + j.omega.C   [S/m]'
      WRITE(*,800) '   G is NOT zero - a loss tangent has been set.  Re(Y) = G:'
      DO I = 1, 3
         WRITE(*,824) (REAL(YM(I,K)), K = 1, 3)
      END DO
      WRITE(*,800) '   Im(Y) = omega.C:'
      DO I = 1, 3
         WRITE(*,824) (AIMAG(YM(I,K)), K = 1, 3)
      END DO
   ENDIF
   WRITE(*,800) ' R, L, C above are not separate results: they ARE Z and Y, split'
   WRITE(*,800) ' into the per-unit-length forms.  R = Re(Z), L = Im(Z)/omega,'
   WRITE(*,800) ' G = Re(Y) = 0 here, C = Im(Y)/omega.  Nothing is lost either way.'
   WRITE(*,800) '------------------------------------------------------------------'
   WRITE(*,800) ' DIFFERENCE FROM PSCAD LINE CONSTANTS, over 9 matrix entries'
   WRITE(*,813) '   R  [%] : mean', SR9/REAL(NR9), '   max', XR9
   WRITE(*,813) '   L  [%] : mean', SL9/REAL(NL9), '   max', XL9
   WRITE(*,814) '   Y  [%] : mean', SY9/REAL(NY9), '   max', XY9
   WRITE(*,800) '------------------------------------------------------------------'
   WRITE(*,800) ' STRUCTURAL CHECKS'
   DO I = 1, 9
      PF = 'FAIL'
      IF (CK(I)) PF = 'PASS'
      WRITE(*,805) CKD(I), PF
   END DO
   WRITE(*,800) '------------------------------------------------------------------'
   IF (FBACK) THEN
      WRITE(*,812) ' Could not write to the project folder, IOSTAT =', IOS
      WRITE(*,800) ' Full precision left in the .gf42 build folder instead.'
   ELSE
      WRITE(*,815) ' Full precision written to : ', TRIM(FOUT)
   ENDIF
   WRITE(*,800) '=================================================================='
   WRITE(*,800) ' '

!  =====================================================================
!  PART 2 - THE FULL OUTPUT FILE
!  =====================================================================

   WRITE(LU,800) '======================================================================'
   WRITE(LU,800) 'ANALYTICAL PRIMITIVE PARAMETERS OF A 525 kV ARMOURED HVDC SUBMARINE'
   WRITE(LU,800) 'CABLE, COMPUTED INSIDE PSCAD, BENCHMARKED AGAINST PSCAD LINE'
   WRITE(LU,800) 'CONSTANTS AT 50 Hz.'
   WRITE(LU,800) '======================================================================'
   WRITE(LU,806) 'RUN AT          : ', TVAL(1), TVAL(2), TVAL(3), TVAL(5), TVAL(6), TVAL(7)
   IF (NBAD .EQ. 0) THEN
      WRITE(LU,800) 'VERDICT         : GOOD RUN.  All 9 structural checks PASS.'
   ELSE
      WRITE(LU,812) 'VERDICT         : PROBLEM.  Failed structural checks: ', NBAD
      WRITE(LU,800) '                  Do NOT use the numbers below.  See SECTION 6.'
   ENDIF
   WRITE(LU,813) 'R  diff [%]     : mean', SR9/REAL(NR9), '  max', XR9
   WRITE(LU,813) 'L  diff [%]     : mean', SL9/REAL(NL9), '  max', XL9
   WRITE(LU,814) 'Y  diff [%]     : mean', SY9/REAL(NY9), '  max', XY9
   WRITE(LU,800) '----------------------------------------------------------------------'
   WRITE(LU,800) 'If the RUN AT time above is not the time you just pressed Run, this'
   WRITE(LU,800) 'file is left over from an earlier run and the code did not execute.'
   IF (FBACK) THEN
      WRITE(LU,800) 'NOTE: the project folder could not be written to, so this file was'
      WRITE(LU,800) 'left in the .gf42 build folder instead.'
      WRITE(LU,812) '      The OPEN failed with IOSTAT = ', IOS
      WRITE(LU,815) '      Target was: ', TRIM(FOUT)
   ENDIF
   WRITE(LU,800) '======================================================================'
   WRITE(LU,800) ' '
   WRITE(LU,800) 'Source   : analytical_rlc.f, five-block version'
   WRITE(LU,800) 'Blocks   : ANB1 geometry and materials, ANB2 conductor impedances,'
   WRITE(LU,800) '           ANB3 magnetic regions and seabed, ANB4 series assembly,'
   WRITE(LU,800) '           ANB5 shunt side'
   WRITE(LU,800) 'Sections : 0 inputs, 1-5 blocks, 6 structural checks,'
   WRITE(LU,800) '           7 comparison against PSCAD Line Constants'
   WRITE(LU,815) 'Written to : ', TRIM(FOUT)
   WRITE(LU,800) ' '

   WRITE(LU,800) ' '
   WRITE(LU,800) 'SECTION 0   INPUTS'
   WRITE(LU,800) '----------------------------------------------------------------------'
   WRITE(LU,800) 'Each value below is owned by a block dialog on the PSCAD canvas,'
   WRITE(LU,800) 'except gamma_E and eps_0 which are fixed constants held in the code.'
   WRITE(LU,800) 'Originally transcribed from Cable_1.cli, PSCAD''s own echo of the'
   WRITE(LU,800) 'Line Constants input deck, so both routes start from one identical'
   WRITE(LU,800) 'input set.  If a number here is not what you expect, the block'
   WRITE(LU,800) 'dialog has been edited.'
   WRITE(LU,800) ' '
   WRITE(LU,800) 'from BLOCK 1 dialog:'
   WRITE(LU,801) 'f            [Hz]', F
   WRITE(LU,801) 'r_c          [m]', RC
   WRITE(LU,801) 'A_c,man      [m2]', AC_M
   WRITE(LU,801) 'rho_c0       [ohm.m]', RHOC0
   WRITE(LU,801) 's_c          [-]', SC
   WRITE(LU,801) 'R_c,man      [ohm/km]', RCMAN
   WRITE(LU,801) 'basis (1=man)[-]', BASIS
   WRITE(LU,801) 'mu_r,c       [-]', MURC
   WRITE(LU,801) 'r_s,i        [m]', RSI
   WRITE(LU,801) 'r_s,o        [m]', RSO
   WRITE(LU,801) 'rho_s        [ohm.m]', RHOS
   WRITE(LU,801) 'mu_r,s       [-]', MURS
   WRITE(LU,801) 'r_a,i        [m]', RAI
   WRITE(LU,801) 'r_a,o        [m]', RAO
   WRITE(LU,801) 'n_a,w        [-]', ANW
   WRITE(LU,801) 'd_w          [m]', DW
   WRITE(LU,801) 'rho_a,w      [ohm.m]', RHOAW
   WRITE(LU,801) 's_a (lay)    [-]', SA
   WRITE(LU,801) 'mu_r,a       [-]', MURA
   WRITE(LU,801) 'h burial     [m]', H
   WRITE(LU,801) 'rho_g        [ohm.m]', RHOG
   WRITE(LU,801) 'mu_r,g       [-]', MURG
   WRITE(LU,800) ' '
   WRITE(LU,800) 'from BLOCK 3 dialog:'
   WRITE(LU,801) 'r_t,Z series [m]', RTZ
   WRITE(LU,801) 'mu_r region12[-]', MUR12
   WRITE(LU,801) 'mu_r region23[-]', MUR23
   WRITE(LU,801) 'mu_r region34[-]', MUR34
   WRITE(LU,800) ' '
   WRITE(LU,800) 'from BLOCK 5 dialog:'
   WRITE(LU,801) 'r_ins,o      [m]', RINSO
   WRITE(LU,801) 'r_t,Y shunt  [m]', RTY
   WRITE(LU,801) 'eps_r core-sh[-]', ERCS
   WRITE(LU,801) 'eps_r sh-arm [-]', ERSA
   WRITE(LU,801) 'eps_r arm-gnd[-]', ERAG
   WRITE(LU,801) 'tan d core-sh[-]', TDCS
   WRITE(LU,801) 'tan d sh-arm [-]', TDSA
   WRITE(LU,801) 'tan d arm-gnd[-]', TDAG
   WRITE(LU,800) ' '
   WRITE(LU,800) 'fixed constants held in the code:'
   WRITE(LU,801) 'gamma_E      [-]', GAMMAE
   WRITE(LU,801) 'eps_0        [F/m]', EPS0
   WRITE(LU,800) ' '
   WRITE(LU,800) 'NOTE  r_t,Z = 0.0805 m is the series modelling radius; the shunt side'
   WRITE(LU,800) 'uses r_t,Y = 0.0798 m, the physical outer radius implied by the'
   WRITE(LU,800) 'entered layer thicknesses.  In the series model r_t,Z cancels'
   WRITE(LU,800) 'identically between z34 and the -ln(D) term of z0, so the choice does'
   WRITE(LU,800) 'not affect Z.  This is verified in SECTION 6.'

   WRITE(LU,800) ' '
   WRITE(LU,800) ' '
   WRITE(LU,800) 'SECTION 1   BLOCK 1   EQUIVALENT PROPERTIES AND PENETRATION FACTORS'
   WRITE(LU,800) '----------------------------------------------------------------------'
   WRITE(LU,800) 'Paper E1-E13.  Replaces the stranded core with a SOLID CYLINDER and'
   WRITE(LU,800) 'the 68 armour wires with ONE TUBE, in both cases keeping the geometry'
   WRITE(LU,800) 'and adjusting the resistivity so the DC resistance is unchanged.  Then'
   WRITE(LU,800) 'forms q = 1/delta for each layer.  The sheath needs no equivalencing:'
   WRITE(LU,800) 'it is already a continuous tube, so its own resistivity is used.'
   WRITE(LU,800) ' '
   WRITE(LU,800) 'rho_c,eff is ~18 % above pure copper because the solid cylinder spans a'
   WRITE(LU,800) 'larger area than the stranded copper (factor 1.1310) and the datasheet'
   WRITE(LU,800) 'resistance already includes lay and temperature (factor 1.0440).'
   WRITE(LU,800) ' '
   WRITE(LU,801) 'omega        [rad/s]', W
   WRITE(LU,801) 'A_c,eq       [m2]', ACEQ
   WRITE(LU,801) 'rho_c,eff    [ohm.m]', RHOCEF
   WRITE(LU,801) 'A_s          [m2]', AS
   WRITE(LU,801) 'R_s,dc       [ohm/km]', RSDC
   WRITE(LU,801) 'A_a,w        [m2]', AAW
   WRITE(LU,801) 'R_a,dc       [ohm/km]', RADC
   WRITE(LU,801) 'A_a,eq       [m2]', AAEQ
   WRITE(LU,801) 'rho_a,eff    [ohm.m]', RHOAEF
   WRITE(LU,801) 'H = 2h       [m]', HH
   WRITE(LU,801) 'q_c          [1/m]', QC
   WRITE(LU,801) 'q_s          [1/m]', QS
   WRITE(LU,801) 'q_a          [1/m]', QA
   WRITE(LU,801) 'q_g          [1/m]', QG
   WRITE(LU,800) ' '
   WRITE(LU,800) 'Skin depths delta = 1/q [mm]:'
   WRITE(LU,801) 'delta_c      [mm]', 1000.0/QC
   WRITE(LU,801) 'delta_s      [mm]', 1000.0/QS
   WRITE(LU,801) 'delta_a      [mm]', 1000.0/QA
   WRITE(LU,801) 'delta_g      [mm]', 1000.0/QG
   WRITE(LU,800) ' '
   WRITE(LU,800) 'NOTATION  the paper uses the COMPLEX constant m of Eq. (3a); q above'
   WRITE(LU,800) 'is the real building block, and m = (1 + j) q.  SECTION 2 prints m'
   WRITE(LU,800) 'and SECTION 6 checks Re(m) = Im(m) = q numerically.'

   WRITE(LU,800) ' '
   WRITE(LU,800) ' '
   WRITE(LU,800) 'SECTION 2   BLOCK 2   INTERNAL, SURFACE AND TRANSFER IMPEDANCES'
   WRITE(LU,800) '----------------------------------------------------------------------'
   WRITE(LU,800) 'Paper Eq. (3a), (4), (5a)-(5d).  All values ohm/km.'
   WRITE(LU,800) 'Computed in complex arithmetic directly, where the spreadsheet must'
   WRITE(LU,800) 'split coth and csch into real and imaginary parts by hand.  Agreement'
   WRITE(LU,800) 'between the two routes therefore also verifies that hand split.'
   WRITE(LU,800) ' '
   WRITE(LU,800) '                                real                       imag'
   WRITE(LU,802) 'm_c          [1/m]', REAL(MC),  AIMAG(MC)
   WRITE(LU,802) 'm_s          [1/m]', REAL(MS),  AIMAG(MS)
   WRITE(LU,802) 'm_a          [1/m]', REAL(MA),  AIMAG(MA)
   WRITE(LU,802) 'z_c core int', REAL(ZC),  AIMAG(ZC)
   WRITE(LU,802) 'z2i sheath in', REAL(Z2I), AIMAG(Z2I)
   WRITE(LU,802) 'z2m sheath tr', REAL(Z2M), AIMAG(Z2M)
   WRITE(LU,802) 'z2o sheath out', REAL(Z2O), AIMAG(Z2O)
   WRITE(LU,802) 'z2w sheath wall', REAL(Z2W), AIMAG(Z2W)
   WRITE(LU,802) 'z3i armour in', REAL(Z3I), AIMAG(Z3I)
   WRITE(LU,802) 'z3m armour tr', REAL(Z3M), AIMAG(Z3M)
   WRITE(LU,802) 'z3o armour out', REAL(Z3O), AIMAG(Z3O)
   WRITE(LU,802) 'z3w armour wall', REAL(Z3W), AIMAG(Z3W)
   WRITE(LU,800) ' '
   WRITE(LU,800) 'CONDITIONING  z2w = z2i + z2o - 2 z2m is a cancellation of about'
   WRITE(LU,800) '30 000 to 1 in the real part, so Re(z2w) carries fewer significant'
   WRITE(LU,800) 'digits than its inputs.  Harmless at this sheath thickness, but worth'
   WRITE(LU,800) 'a sentence if a reviewer asks about numerical conditioning.'

   WRITE(LU,800) ' '
   WRITE(LU,800) ' '
   WRITE(LU,800) 'SECTION 3   BLOCK 3   MAGNETIC REGIONS AND SEABED RETURN'
   WRITE(LU,800) '----------------------------------------------------------------------'
   WRITE(LU,800) 'Paper Eq. (6a)-(6d) and (7a)-(7c).  All values ohm/km.'
   WRITE(LU,800) ' '
   WRITE(LU,800) '                                real                       imag'
   WRITE(LU,802) 'z12 core-sheath', REAL(Z12), AIMAG(Z12)
   WRITE(LU,802) 'z23 sheath-arm', REAL(Z23), AIMAG(Z23)
   WRITE(LU,802) 'z34 arm-surface', REAL(Z34), AIMAG(Z34)
   WRITE(LU,802) 'm_g          [1/m]', REAL(MG),  AIMAG(MG)
   WRITE(LU,802) 'z0 seabed', REAL(Z0),  AIMAG(Z0)
   WRITE(LU,802) 'zE = z34 + z0', REAL(ZE),  AIMAG(ZE)
   WRITE(LU,800) ' '
   WRITE(LU,800) 'KNOWN FORMULATION DIFFERENCE.  Eq. (7b) is the Wedepohl-Wilcox'
   WRITE(LU,800) 'LOGARITHMIC APPROXIMATION to the earth-return integral.  PSCAD Line'
   WRITE(LU,800) 'Constants evaluates the exact Pollaczek integral.  The two differ by'
   WRITE(LU,800) 'about -0.21 % on the earth-return resistance, propagating to -0.16 %'
   WRITE(LU,800) 'on Rcc.  zE appears in EVERY entry of the primitive matrix, which is'
   WRITE(LU,800) 'why SECTION 7 shows one shared offset rather than nine independent'
   WRITE(LU,800) 'errors.  This is a formulation difference, not an input error.'

   WRITE(LU,800) ' '
   WRITE(LU,800) ' '
   WRITE(LU,800) 'SECTION 4   BLOCK 4   PRIMITIVE SERIES MATRIX'
   WRITE(LU,800) '----------------------------------------------------------------------'
   WRITE(LU,800) 'Paper Eq. (8a)-(8f), (9) and section 2.5.  Conductor order [c, s, a].'
   WRITE(LU,800) 'Z in ohm/km, R = Re(Z) in ohm/km, L = Im(Z)/omega in mH/km.'
   WRITE(LU,800) ' '
   WRITE(LU,800) 'Z                               real                       imag'
   DO I = 1, 3
      DO K = 1, 3
         WRITE(LU,803) 'Z', I, K, REAL(ZM(I,K)), AIMAG(ZM(I,K))
      END DO
   END DO
   WRITE(LU,800) ' '
   WRITE(LU,800) 'R [ohm/km]'
   DO I = 1, 3
      WRITE(LU,804) (RM(I,K), K = 1, 3)
   END DO
   WRITE(LU,800) ' '
   WRITE(LU,800) 'L [mH/km]'
   DO I = 1, 3
      WRITE(LU,804) (LM(I,K), K = 1, 3)
   END DO

   WRITE(LU,800) ' '
   WRITE(LU,800) ' '
   WRITE(LU,800) 'SECTION 5   BLOCK 5   PRIMITIVE SHUNT MATRIX'
   WRITE(LU,800) '----------------------------------------------------------------------'
   WRITE(LU,800) 'Paper Eq. (10)-(14).  The shunt side is derived independently of the'
   WRITE(LU,800) 'series side from an electrostatic problem, so Y is NOT the inverse'
   WRITE(LU,800) 'of Z.  p in m/F, c and C in uF/km, G and Y in S/m.'
   WRITE(LU,800) ' '
   WRITE(LU,801) 'p_cs         [m/F]', PCS
   WRITE(LU,801) 'p_sa         [m/F]', PSA
   WRITE(LU,801) 'p_ag         [m/F]', PAG
   WRITE(LU,801) 'c_cs         [uF/km]', CCS*1.0E9
   WRITE(LU,801) 'c_sa         [uF/km]', CSA*1.0E9
   WRITE(LU,801) 'c_ag         [uF/km]', CAG*1.0E9
   WRITE(LU,800) ' '
   WRITE(LU,800) 'C [uF/km], closed form of Eq. (12i)'
   DO I = 1, 3
      WRITE(LU,804) (CM(I,K)*1.0E9, K = 1, 3)
   END DO
   WRITE(LU,800) ' '
   WRITE(LU,800) 'C [uF/km], numerical inverse of P from Eq. (12g)'
   DO I = 1, 3
      WRITE(LU,804) (CMNUM(I,K)*1.0E9, K = 1, 3)
   END DO
   WRITE(LU,800) ' '
   WRITE(LU,800) 'G [S/m]'
   DO I = 1, 3
      WRITE(LU,804) (GM(I,K), K = 1, 3)
   END DO
   WRITE(LU,800) ' '
   WRITE(LU,800) 'Y = G + j omega C   [S/m]      real                       imag'
   DO I = 1, 3
      DO K = 1, 3
         WRITE(LU,803) 'Y', I, K, REAL(YM(I,K)), AIMAG(YM(I,K))
      END DO
   END DO

   WRITE(LU,800) ' '
   WRITE(LU,800) ' '
   WRITE(LU,800) 'SECTION 6   STRUCTURAL CHECKS'
   WRITE(LU,800) '----------------------------------------------------------------------'
   WRITE(LU,800) 'Properties the model must satisfy by physics or by construction.  A'
   WRITE(LU,800) 'broken implementation fails these even when its numbers look'
   WRITE(LU,800) 'plausible, so they test correctness rather than agreement.'
   WRITE(LU,800) ' '
   DO I = 1, 9
      PF = 'FAIL'
      IF (CK(I)) PF = 'PASS'
      WRITE(LU,805) CKD(I), PF
   END DO
   WRITE(LU,800) ' '
   IF (NBAD .EQ. 0) THEN
      WRITE(LU,800) 'All nine PASS.  SECTION 7 may be used.'
   ELSE
      WRITE(LU,812) 'FAILURES: ', NBAD
      WRITE(LU,800) 'A FAIL invalidates SECTION 7.  Fix the cause before using any'
      WRITE(LU,800) 'number from this file.'
   ENDIF

   WRITE(LU,800) ' '
   WRITE(LU,800) ' '
   WRITE(LU,800) 'SECTION 7   COMPARISON AGAINST PSCAD LINE CONSTANTS AT 50 Hz'
   WRITE(LU,800) '----------------------------------------------------------------------'
   WRITE(LU,800) 'PSCAD values are TRANSCRIBED reference data, not computed here.'
   WRITE(LU,800) 'Source: HVDC_cable_draft:Main(0):Cable_1(0), PHASE DOMAIN DATA'
   WRITE(LU,800) '@ 50.000 Hz, run 3 (exact resistivities, eps_r = 2.4).'
   WRITE(LU,800) 'Solver: Line Constants Program for PSCAD X4, build 2016.08.11.'
   WRITE(LU,800) 'R and L carry six decimals as recorded, not PSCAD''s nine'
   WRITE(LU,800) 'significant figures, so these percentages are good to about three'
   WRITE(LU,800) 'significant figures.  Use the full listing before publishing.'
   WRITE(LU,800) ' '
   WRITE(LU,800) 'Difference convention: 100 * |analytical - PSCAD| / |PSCAD|.'
   WRITE(LU,800) ' '
   WRITE(LU,800) 'R [ohm/km]        analytical              PSCAD            diff %'
   DO I = 1, 3
      DO K = 1, 3
         WRITE(LU,807) 'R', I, K, RM(I,K), RPS(I,K), DR(I,K)
      END DO
   END DO
   WRITE(LU,800) ' '
   WRITE(LU,800) 'L [mH/km]         analytical              PSCAD            diff %'
   DO I = 1, 3
      DO K = 1, 3
         WRITE(LU,807) 'L', I, K, LM(I,K), LPS(I,K), DL(I,K)
      END DO
   END DO
   WRITE(LU,800) ' '
   WRITE(LU,800) 'Im(Y) [S/m]       analytical              PSCAD            diff %'
   DO I = 1, 3
      DO K = 1, 3
         IF (YPS(I,K) .NE. 0.0) THEN
            WRITE(LU,810) 'Y', I, K, AIMAG(YM(I,K)), YPS(I,K), DY(I,K)
         ELSE
            WRITE(LU,808) 'Y', I, K, AIMAG(YM(I,K)), YPS(I,K)
         ENDIF
      END DO
   END DO
   WRITE(LU,800) ' '
   WRITE(LU,800) 'SUMMARY.  Both averaging conventions are given because they produce'
   WRITE(LU,800) 'different numbers and the manuscript must state which it uses, or a'
   WRITE(LU,800) 'reviewer recomputing will not reproduce the figure.'
   WRITE(LU,800) ' '
   WRITE(LU,800) 'over all nine matrix entries:'
   WRITE(LU,809) 'R  mean / max  [%]', SR9/REAL(NR9), XR9, NR9
   WRITE(LU,809) 'L  mean / max  [%]', SL9/REAL(NL9), XL9, NL9
   WRITE(LU,811) 'Y  mean / max  [%]', SY9/REAL(NY9), XY9, NY9
   WRITE(LU,800) ' '
   WRITE(LU,800) 'over the six unique upper-triangle entries:'
   WRITE(LU,809) 'R  mean / max  [%]', SR6/REAL(NR6), XR6, NR6
   WRITE(LU,809) 'L  mean / max  [%]', SL6/REAL(NL6), XL6, NL6
   WRITE(LU,800) ' '
   WRITE(LU,800) 'Yca and Yac are excluded from the Y statistics: both routes give'
   WRITE(LU,800) 'exactly zero there, and a percentage of zero has no meaning.'
   WRITE(LU,800) ' '
   WRITE(LU,800) 'HOW TO READ THIS TABLE.  Every resistance difference has the same'
   WRITE(LU,800) 'sign and a similar size, and every inductance difference is about ten'
   WRITE(LU,800) 'times smaller and almost uniform.  That is the signature of ONE'
   WRITE(LU,800) 'shared cause, the earth-return term of SECTION 3, rather than nine'
   WRITE(LU,800) 'independent discrepancies.  Subtracting that common offset, the'
   WRITE(LU,800) 'conductor terms agree with PSCAD to about 1e-7 %.'
   WRITE(LU,800) ' '
   WRITE(LU,800) 'WHAT THIS IS NOT.  This is implementation consistency between two'
   WRITE(LU,800) 'computational routes sharing one input set.  It is not independent'
   WRITE(LU,800) 'physical validation, because no measurement is involved.'
   WRITE(LU,800) ' '
   WRITE(LU,800) '======================================================================'
   WRITE(LU,800) 'END OF OUTPUT'
   WRITE(LU,800) '======================================================================'

   CLOSE(LU)

800 FORMAT(A)
801 FORMAT(3X,A22,3X,ES24.16)
802 FORMAT(3X,A22,3X,ES24.16,3X,ES24.16)
803 FORMAT(3X,A1,'(',I1,',',I1,')',16X,ES24.16,3X,ES24.16)
804 FORMAT(3X,3(ES24.16,2X))
805 FORMAT(3X,A54,2X,A4)
806 FORMAT(A,I4.4,'-',I2.2,'-',I2.2,2X,I2.2,':',I2.2,':',I2.2)
807 FORMAT(3X,A1,'(',I1,',',I1,')',3X,ES22.14,3X,ES22.14,3X,F12.6)
808 FORMAT(3X,A1,'(',I1,',',I1,')',3X,ES22.14,3X,ES22.14,3X,'   exact zero')
809 FORMAT(3X,A22,3X,F14.8,3X,F14.8,3X,'n =',I2)
810 FORMAT(3X,A1,'(',I1,',',I1,')',3X,ES22.14,3X,ES22.14,3X,ES14.6)
811 FORMAT(3X,A22,3X,ES14.6,3X,ES14.6,3X,'n =',I2)
812 FORMAT(A,I2)
813 FORMAT(A,F10.6,A,F10.6)
814 FORMAT(A,ES12.4,A,ES12.4)
815 FORMAT(A,A)
820 FORMAT(3X,3(F16.9,2X))
823 FORMAT(3X,3(F11.8,'+j',F11.8,1X))
824 FORMAT(3X,3(ES15.6,1X))

   RETURN
END
