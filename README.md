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

##  Differential Equations
These are basic algorithms for finding numerical solutions to differential
equations.

the implemented algorithms are:
* Euler's method
* 4th order Runge-Kutta
* 4/5th order Runge-Kutta-Fehlberg
* 4th order Adams-Bashforth/Adams-Moulton
* 4th order Runge-Kutta for systems of differential equations
* Stormer/Verlet method for conservative 2nd order differential equations

## Filtering
This provides an averaging filter to help smooth data.  More routines may
be added later.

##  Functions
These are special functions not included with Ada's elementary functions
that are useful or needed by other routines here.
* gamma2n - Divides the positive integer argument by two and computes the gamma function.  Used to support chi^2.
* lngamma2n - Natural log of gamma2n.  Allows larger values of n without overflow.
* lngamma - Natural log of gamma.  This allows positive real arguments
* gammaP - Regularized incomplete upper Gamma function.
* gammaQ - Regularized incomplete lower Gamma function.
* erf - Error function.  This is computed using gammaP
* erfc - Complementary error function.  This is computed using gammaQ.
* beta - The beta function
* lnbeta - The natural log of the beta function
* factorial - Computes the factorial of n.
* lnfact - Computes the natural log of the factorial of n allowing larger values of n without overflow.
* nChoosek - Computes binomial coefficent based on n and k.

##  Integration
Implemented algorithms are:
* Midpoint method
* Composite trapezoid
* Adaptive trapezoid
* Composite Simpson's
* Adaptive Simpson's
* Romberg's method

## Interpolation
These methods construct a polynomial that passes through the provided points.
Generally these are most accurate within the range of the provided points.
There is a danger of oscillation with higher order polynomials.

Implemented algorithms are:
* Lagrange 2 point (linear)
* Lagrange 3 point (quadratic)
* Lagrange 4 point (cubic)
* Lagrange 5 point (quartic)

##  Numerical Derivatives
These operations can easily cause trouble if you don't understand your
problem and computer arithmatic.  The core of taking a derivative involves
subtracting two nearly equal numbers and dividing by a small number.  This
has the effect of amplifying round-off errors.  Avoid these operations, if
you can.
* 2 point method
* 3 point methods
* 5 point methods

## Plotting
Routines are now available for drawing linear scaled plots.  The output
is a file in either SVG or LaTeX (with tikz) format.

##  Polynomials (real and complex)
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

## Random Numbers
Pseud-Random Number Generators implemented are
* Linear Congruent Generator with settable parameters
* Mersenne Twister (MT19937 algorithm)

##  Regression
Simple linear regression is implemented.

##  Root Finding
These are used to find roots (zero crossings) of functions.  There are a number
of different methods with diffrent pros and cons.  It is important to understand
the nature of the function before selecting the root finding algorithm.

The implemented algorithms are:
* Bisection
* Secant
* Mueller (versions for real roots and complex roots)

##  Quaternions
Some basic operations are implemented.

## Statistics
Some basic functions are implemented for analyzing data
*  Mean
*  Variance
*  Limits

The following probability distribution are implemented
* Chi^2 (PDF and CDF)
* Exponential (PDF and CDF)
* Normal (PDF and CDF)
* Poisson (PMF)
* Student's T (PDF and CDF)

