# Numerical
This will contain a collection of Ada (and possibly lisp, if I get really
ambitious) packages for numerical operations.

# Warning
Don't blindly trust these or any other numerical methods.  You need to
understand your problem and how computers do arithmatic.

A rough outline of what I plan is below.  Obviously, this will be subject
to change.  I expect that most, if not all of these packages will be generic
so that they can be used on different numerical types.

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

##  Numerical Derivatives
These operations can easily cause trouble if you don't understand your
problem and computer arithmatic.  The core of taking a derivative involves
subtracting two nearly equal numbers and dividing by a small number.  This
has the effect of amplifying round-off errors.  Avoid these operations, if
you can.
* 2 point method
* 3 point methods
* 5 point methods

##  Differential Equations
These are basic algorithms for finding numerical solutions to differential
equations.

the implemented algorithms are:
* Euler's method
* 4th order Runge-Kutta
* 4/5th order Runge-Kutta-Fehlberg
* 4th order Adams-Bashforth/Adams-Moulton
* 4th order Runge-Kutta for systems of differential equations

##  Root Finding
These are used to find roots (zero crossings) of functions.  There are a number
of different methods with diffrent pros and cons.  It is important to understand
the nature of the function before selecting the root finding algorithm.

The implemented algorithms are:
* Bisection
* Secant
* Mueller (versions for real roots and complex roots)

##  Polynomials
A polynomial is implemented as an array with the lower index value set to 0.  The index represents
the exponent for the independant variable and the array value at that index is the coefficient.

Implemented operations include
* Addition and subtraction of polynomials
* Multiplication of polynomial by a scalar
* Multiplication of polynomial by polynomial
* Division of polynomial by polynomial
* Evaluation of polynomial
* Integral of polynomial
* Derivative of polynomial

##  Quaternions
Some basic operations are implemented.

##  Regression
Simple linear regression is implemented.
