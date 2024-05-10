# Numerical
This will contain a collection of Ada (and possibly lisp, if I get really ambitious) packages for numerical operations.

A rough outline of what I plan is below.  Obviously, this will be subject to change.  I expect that most, if not all
of these packages will be generic so that they can be used on different numerical types.

##  Complex Numbers
In general, you should probably use the complex package in the Ada standard
numerics library.  This is more of a practice run for me.  The one difference
is that complex numbers are a tagged record so that object notation can be used
for some of the operations, if that's important to you.

##  Integration
Implemented algorithms are:
* Composite trapezoid
* Composite Simpson's
* Adaptive Simpson's

##  Differentiation

##  Differential Equations

##  Root Finding
These are used to find roots (zero crossings) of functions.  There are a number
of different methods with diffrent pros and cons.  It is important to understand
the nature of the function before selecting the root finding algorithm.

The implemented algorithms are:
* Bisection
* Secant
* Mueller

##  Quaternions
Some basic operations are implemented.

##  Regression
Simple linear regression is implemented.
