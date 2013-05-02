GFORTRAN module version '6' created from src/ode/ode.f90 on Thu Mar  7 15:13:45 2013
MD5:ee7251ac4d0fa3cc0e057786e7a07559 -- If you edit this, you'll get what you deserve.

(() () () () () () () () () () () () () () () () () () () () () () () ()
() () ())

()

()

()

()

()

(2 'kmax' 'ode' 'kmax' 1 ((PARAMETER UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
IMPLICIT-SAVE 0 0) (INTEGER 4 0 0 INTEGER ()) 0 0 () (CONSTANT (INTEGER
4 0 0 INTEGER ()) 0 '15') () 0 () () () 0 0)
3 'ode' 'ode' 'ode' 1 ((MODULE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0) (UNKNOWN 0 0 0 UNKNOWN ()) 0 0 () () 0 () () () 0 0)
4 'rho_bs_k' 'ode' 'rho_bs_k' 1 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN IMPLICIT-SAVE 0 0) (INTEGER 4 0 0 INTEGER ()) 0 0 () () 0 () ()
() 0 0)
5 'rho_bs_step' 'ode' 'rho_bs_step' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE ALWAYS_EXPLICIT) (UNKNOWN 0 0 0
UNKNOWN ()) 6 0 (7 8 9 10 11) () 0 () () () 0 0)
12 'rho_dt_suggested' 'ode' 'rho_dt_suggested' 1 ((VARIABLE
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0) (REAL 8 0 0 REAL
()) 0 0 () () 0 () () () 0 0)
13 'rho_error_control' 'ode' 'rho_error_control' 1 ((VARIABLE
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0) (LOGICAL 4 0 0
LOGICAL ()) 0 0 () () 0 () () () 0 0)
14 'rho_euler_step' 'ode' 'rho_euler_step' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE ALWAYS_EXPLICIT) (UNKNOWN 0 0 0
UNKNOWN ()) 15 0 (16 17 18 19 20) () 0 () () () 0 0)
21 'rho_leapfrog_step' 'ode' 'rho_leapfrog_step' 1 ((PROCEDURE
UNKNOWN-INTENT MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE ALWAYS_EXPLICIT)
(UNKNOWN 0 0 0 UNKNOWN ()) 22 0 (23 24 25 26 27 28) () 0 () () () 0 0)
29 'rho_mm_step' 'ode' 'rho_mm_step' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE ALWAYS_EXPLICIT) (UNKNOWN 0 0 0
UNKNOWN ()) 30 0 (31 32 33 34 35 36) () 0 () () () 0 0)
37 'rho_norm_truncation' 'ode' 'rho_norm_truncation' 1 ((VARIABLE
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0) (REAL 8 0 0 REAL
()) 0 0 () () 0 () () () 0 0)
38 'rho_order' 'ode' 'rho_order' 1 ((VARIABLE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0) (REAL 8 0 0 REAL ()) 0 0 () () 0
() () () 0 0)
39 'rho_reject_step' 'ode' 'rho_reject_step' 1 ((VARIABLE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0) (LOGICAL 4 0 0 LOGICAL ()) 0 0 ()
() 0 () () () 0 0)
40 'rho_rg4_step' 'ode' 'rho_rg4_step' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE ALWAYS_EXPLICIT) (UNKNOWN 0 0 0
UNKNOWN ()) 41 0 (42 43 44 45 46) () 0 () () () 0 0)
47 'rho_scalar' 'ode' 'rho_scalar' 1 ((VARIABLE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0) (REAL 8 0 0 REAL ()) 0 0 () () 0
() () () 0 0)
48 'rho_scale' 'ode' 'rho_scale' 1 ((VARIABLE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0 ALLOCATABLE DIMENSION) (REAL 8 0
0 REAL ()) 0 0 () (1 0 DEFERRED () ()) 0 () () () 0 0)
49 'rho_tolerance' 'ode' 'rho_tolerance' 1 ((VARIABLE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0) (REAL 8 0 0 REAL ()) 0 0 () () 0
() () () 0 0)
16 'time_value' '' 'time_value' 15 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
17 'time_step' '' 'time_step' 15 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
18 'solution_old' '' 'solution_old' 15 ((VARIABLE IN UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
19 'solution_new' '' 'solution_new' 15 ((VARIABLE OUT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
20 'source_function' '' 'source_function' 15 ((PROCEDURE UNKNOWN-INTENT
DUMMY-PROC BODY UNKNOWN 0 0 DIMENSION DUMMY FUNCTION ALWAYS_EXPLICIT) (
REAL 8 0 0 REAL ()) 50 0 (51 52) (1 0 EXPLICIT (CONSTANT (INTEGER 4 0 0
INTEGER ()) 0 '1') (FUNCTION (INTEGER 4 0 0 INTEGER ()) 0 53 (('' (
VARIABLE (REAL 8 0 0 REAL ()) 1 51 ((ARRAY (FULL 0))))) ('' ()) ('' ()))
'' 0 'size')) 20 () () () 0 0)
25 'solution_old' '' 'solution_old' 22 ((VARIABLE IN UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
26 'solution_mid' '' 'solution_mid' 22 ((VARIABLE IN UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
27 'solution_new' '' 'solution_new' 22 ((VARIABLE OUT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
28 'source_function' '' 'source_function' 22 ((PROCEDURE UNKNOWN-INTENT
DUMMY-PROC BODY UNKNOWN 0 0 DIMENSION DUMMY FUNCTION ALWAYS_EXPLICIT) (
REAL 8 0 0 REAL ()) 54 0 (55 56) (1 0 EXPLICIT (CONSTANT (INTEGER 4 0 0
INTEGER ()) 0 '1') (FUNCTION (INTEGER 4 0 0 INTEGER ()) 0 57 (('' (
VARIABLE (REAL 8 0 0 REAL ()) 1 55 ((ARRAY (FULL 0))))) ('' ()) ('' ()))
'' 0 'size')) 28 () () () 0 0)
42 'time_value' '' 'time_value' 41 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
43 'time_step' '' 'time_step' 41 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
44 'solution_old' '' 'solution_old' 41 ((VARIABLE IN UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
45 'solution_new' '' 'solution_new' 41 ((VARIABLE OUT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
46 'source_function' '' 'source_function' 41 ((PROCEDURE UNKNOWN-INTENT
DUMMY-PROC BODY UNKNOWN 0 0 DIMENSION DUMMY FUNCTION ALWAYS_EXPLICIT) (
REAL 8 0 0 REAL ()) 58 0 (59 60) (1 0 EXPLICIT (CONSTANT (INTEGER 4 0 0
INTEGER ()) 0 '1') (FUNCTION (INTEGER 4 0 0 INTEGER ()) 0 61 (('' (
VARIABLE (REAL 8 0 0 REAL ()) 1 59 ((ARRAY (FULL 0))))) ('' ()) ('' ()))
'' 0 'size')) 46 () () () 0 0)
32 'time_step' '' 'time_step' 30 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
33 'solution_old' '' 'solution_old' 30 ((VARIABLE IN UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
34 'solution_new' '' 'solution_new' 30 ((VARIABLE OUT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
35 'source_function' '' 'source_function' 30 ((PROCEDURE UNKNOWN-INTENT
DUMMY-PROC BODY UNKNOWN 0 0 DIMENSION DUMMY FUNCTION ALWAYS_EXPLICIT) (
REAL 8 0 0 REAL ()) 62 0 (63 64) (1 0 EXPLICIT (CONSTANT (INTEGER 4 0 0
INTEGER ()) 0 '1') (FUNCTION (INTEGER 4 0 0 INTEGER ()) 0 65 (('' (
VARIABLE (REAL 8 0 0 REAL ()) 1 63 ((ARRAY (FULL 0))))) ('' ()) ('' ()))
'' 0 'size')) 35 () () () 0 0)
36 'nmax' '' 'nmax' 30 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
65 'size' '(intrinsic)' 'size' 62 ((PROCEDURE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 FUNCTION) (REAL 8 0 0 REAL ()) 0 0 () ()
65 () () () 0 0)
63 'vector' '' 'vector' 62 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
64 'time' '' 'time' 62 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
7 'time_value' '' 'time_value' 6 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
8 'time_step' '' 'time_step' 6 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
9 'solution_old' '' 'solution_old' 6 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
10 'solution_new' '' 'solution_new' 6 ((VARIABLE OUT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
11 'source_function' '' 'source_function' 6 ((PROCEDURE UNKNOWN-INTENT
UNKNOWN-PROC BODY UNKNOWN 0 0 DIMENSION DUMMY FUNCTION ALWAYS_EXPLICIT)
(REAL 8 0 0 REAL ()) 66 0 (67 68) (1 0 EXPLICIT (CONSTANT (INTEGER 4 0 0
INTEGER ()) 0 '1') (FUNCTION (INTEGER 4 0 0 INTEGER ()) 0 69 (('' (
VARIABLE (REAL 8 0 0 REAL ()) 1 67 ((ARRAY (FULL 0))))) ('' ()) ('' ()))
'' 0 'size')) 11 () () () 0 0)
53 'size' '(intrinsic)' 'size' 50 ((PROCEDURE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 FUNCTION) (REAL 8 0 0 REAL ()) 0 0 () ()
53 () () () 0 0)
51 'vector' '' 'vector' 50 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
52 'time' '' 'time' 50 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
23 'time_value' '' 'time_value' 22 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
24 'time_step' '' 'time_step' 22 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
57 'size' '(intrinsic)' 'size' 54 ((PROCEDURE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 FUNCTION) (REAL 8 0 0 REAL ()) 0 0 () ()
57 () () () 0 0)
55 'vector' '' 'vector' 54 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
56 'time' '' 'time' 54 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
61 'size' '(intrinsic)' 'size' 58 ((PROCEDURE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 FUNCTION) (REAL 8 0 0 REAL ()) 0 0 () ()
61 () () () 0 0)
59 'vector' '' 'vector' 58 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
60 'time' '' 'time' 58 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
31 'time_value' '' 'time_value' 30 ((VARIABLE IN UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
69 'size' '(intrinsic)' 'size' 66 ((PROCEDURE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 FUNCTION) (REAL 8 0 0 REAL ()) 0 0 () ()
69 () () () 0 0)
67 'vector' '' 'vector' 66 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 DIMENSION DUMMY) (REAL 8 0 0 REAL ()) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') ()) 0 () () ()
0 0)
68 'time' '' 'time' 66 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DUMMY) (REAL 8 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
)

('kmax' 0 2 'ode' 0 3 'rho_bs_k' 0 4 'rho_bs_step' 0 5 'rho_dt_suggested'
0 12 'rho_error_control' 0 13 'rho_euler_step' 0 14 'rho_leapfrog_step'
0 21 'rho_mm_step' 0 29 'rho_norm_truncation' 0 37 'rho_order' 0 38
'rho_reject_step' 0 39 'rho_rg4_step' 0 40 'rho_scalar' 0 47 'rho_scale'
0 48 'rho_tolerance' 0 49)
