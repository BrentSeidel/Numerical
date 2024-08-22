with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
--
with BBS;
with BBS.Numerical;
with BBS.Numerical.derivative;
with BBS.Numerical.filter;
with BBS.Numerical.functions;
with BBS.Numerical.integration_real;
with BBS.Numerical.interpolation;
with BBS.Numerical.ode;
with BBS.Numerical.polynomial_complex;
with BBS.Numerical.polynomial_real;
with BBS.Numerical.random;
with BBS.Numerical.regression;
with BBS.Numerical.roots_complex;
with BBS.Numerical.roots_real;
with BBS.Numerical.Statistics;
with BBS.Numerical.plot_svg_linear;
with BBS.Numerical.plot_latex_linear;
package test_cases is
   subtype real is Float;
   --
   package float_io is new Ada.Text_IO.Float_IO(real);
   package uint32_io is new Ada.Text_IO.Modular_IO(BBS.uint32);
   package elem is new Ada.Numerics.Generic_Elementary_Functions(real);
   package cmplx is new Ada.Numerics.Generic_Complex_Types(real);
   use type cmplx.Complex;
   --
   package derive is new BBS.Numerical.derivative(real);
   package filter is new BBS.Numerical.filter(real);
   package funct is new BBS.Numerical.functions(real);
   package integ is new BBS.Numerical.integration_real(real);
   package interp is new BBS.Numerical.interpolation(real);
   package ode is new BBS.Numerical.ode(real);
   package cpoly is new BBS.Numerical.polynomial_complex(cmplx);
   use type cpoly.poly;
   package poly is new BBS.Numerical.polynomial_real(real);
   use type poly.poly;
   package croot is new BBS.Numerical.roots_complex(cmplx);
   package root is new BBS.Numerical.roots_real(real);
   package linreg is new BBS.Numerical.regression(real);
   package stat is new BBS.Numerical.Statistics(real);

   procedure test_derive;
   procedure test_filter;
   procedure test_func;
   procedure test_integ;
   procedure test_interp;
   procedure test_ode;
   procedure test_poly_cmplx;
   procedure test_poly;
   procedure test_random;
   procedure test_root;
   procedure test_stats;
   procedure test_regression;
   procedure test_plot;
private
   procedure cmplx_put(n : cmplx.Complex; fore, aft, exp : Natural);

end;
