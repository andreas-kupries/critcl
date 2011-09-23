# -*- tcl -*- (critcl actually, Tcl + embedded C)
# rnmath.tcl --
#
#	Low-level functions for the generation of random numbers
#	following various distributions (Poisson, Gaussian/Normal,
#	etc.).
#
# Math pulled out of and derived from tcllib/modules/simulation/random.tcl
# Copyright (c) 2007 by Arjen Markus <arjenmarkus@users.sourceforge.net>
#     Note:
#     Several formulae and algorithms come from "Monte Carlo Simulation"
#     by C. Mooney (Sage Publications, 1997)
#
# Critcl code generation and setup
# Copyright (c) 2011 by Andreas Kupries <andreas_kupris@users.sourceforge.net>
#
# Example of how to EXPORT a C-level stubs API through critcl v3.

# # ## ### ##### ######## ############# #####################
## Requirements

package require Tcl 8.4
package require critcl 3 ; # First providing stubs functionality

# # ## ### ##### ######## ############# #####################
## Configuration

critcl::license \
    {Arjen Markus, Andreas Kupries} \
    {BSD licensed.}

critcl::summary {C-level functions for the generation of random numbers.}

critcl::description {
    This package implements functions for the generation
    of random values following various known probability
    distribution. No Tcl-binding is provided. See package
    'random' for that.
}

critcl::subject {random numbers}
# plus the distributions, see inside of 'generator'

# # ## ### ##### ######## ############# #####################
## Code generation helper command converting a simple RNG declaration
## into the necessary C code.

proc generator {name parameters body rtypes} {
    # Generator results are returned through pointer arguments coming
    # after the generator parameters.
    foreach {t r} $rtypes {
	lappend parameters ${t}* $r
    }

    set fname      rnmath_$name
    set cparameters [join [critcl::argcsignature $parameters] {, }]

    # Low-level math function generating the numbers.

    lappend map @fname@ $fname
    lappend map @param@ $cparameters
    lappend map @body@  $body
    critcl::ccode [string map $map {void @fname@ (@param@) {@body@}}]

    # Exported through a stubs table.
    critcl::api function void $fname $parameters

    # Extend the meta data.
    critcl::subject "$name probability distribution"
    critcl::subject "probability distribution $name"
    critcl::subject "distribution $name"
    return
}

# # ## ### ##### ######## ############# #####################
## Intro and shared/common/fixed code.

critcl::ccode {
    /* -*- c -*- */

#include <math.h>

#ifndef M_PI
#define M_PI (3.141592653589793238462643)
#endif

    static double
    RANDOM (void)
    {
	/* Random numbers in range [0,1) */
#ifdef WIN32
	return ((unsigned int)  rand   ()) / 2147483648.0;
#else
	return ((unsigned long) random ()) / 2147483648.0;
#endif
    }
}

# # ## ### ##### ######## ############# #####################
## Generators ...

# Bernoulli --
#     Produce random numbers with a Bernoulli distribution
#
# Arguments:
#     p         Probability that the outcome will be 1
#
# Result:
#     Name of a procedure that returns a Bernoulli-distributed random number
#

generator bernoulli {double p} {
    *v = (RANDOM () < p) ? 1 : 0;
} {int v}

# Uniform --
#     Produce random numbers with a uniform distribution in a given range
#
# Arguments:
#     min       Minimum value
#     max       Maximum value
#
# Result:
#     Name of a procedure that returns a uniformly distributed
#     random number
#

generator uniform {double min double max} {
    *v = min + (max-min) * RANDOM ();
} {double v}

# Exponential --
#     Produce random numbers with an exponential distribution with given mean
#
# Arguments:
#     min       Minimum value
#     mean      Mean value
#
# Result:
#     Name of a procedure that returns an exponentially distributed
#     random number
#

generator exponential {double min double mean} {
    *v = min + (mean-min)*log(RANDOM ());
} {double v}

# Discrete --
#     Produce random numbers with a uniform but discrete distribution
#
# Arguments:
#     n         Outcome is an integer between 0 and n-1
#
# Result:
#     Name of a procedure that returns such a random number
#

generator discrete {int n} {
    *v = (int) (n*RANDOM ());
} {int v}

# Poisson --
#     Produce random numbers with a Poisson distribution
#
# Arguments:
#     lambda    The one parameter of the Poisson distribution
#
# Result:
#     Name of a procedure that returns such a random number
#

generator poisson {double lambda} {
    double r       = RANDOM ();
    int    number  = 0;
    double sum     = exp(-lambda);
    double rfactor = sum;

    while (r > sum) {
	rfactor *= lambda / (number + 1);
	sum += rfactor;
	number ++;
    }

    *v = number;
} {int v}

# Normal --
#     Produce random numbers with a normal distribution
#
# Arguments:
#     mean      Mean of the distribution
#     stdev     Standard deviation of the distribution
#
# Result:
#     Name of a procedure that returns such a random number
#
# Note:
#     Use the Box-Mueller method to generate a normal random number
#

generator normal {double mean double sigma} {
    /* Note: RANDOM () in [0,1); log < 0 for that interval */
    double rad = sqrt (-2 * log (RANDOM ()));
    double phi = 2 * M_PI * RANDOM ();
    double r   = rad * cos (phi);

    *v = mean + r*sigma;
} {double v}

# Pareto --
#     Produce random numbers with a Pareto distribution
#
# Arguments:
#     min       Minimum value for the distribution
#     steep     Steepness of the descent (> 0!)
#
# Result:
#     Name of a procedure that returns a Pareto-distributed number
#

generator pareto {double min double steepness} {
    *v = min * pow (1. - RANDOM (), 1./steepness);
} {double v}

# Gumbel --
#     Produce random numbers with a Gumbel distribution
#
# Arguments:
#     min       Minimum value for the distribution
#     f         Factor to scale the value
#
# Result:
#     Name of a procedure that returns a Gumbel-distributed number
#
# Note:
#     The chance P(v) = exp( -exp( f*(v-min) ) )
#

generator gumbel {double min double f} {
    *v = min + log ( -log (1. - RANDOM ()) / f);
} {double v}

# chiSquared --
#     Produce random numbers with a chi-squared distribution
#
# Arguments:
#     df        Degrees of freedom
#
# Result:
#     Name of a procedure that returns a chi-squared distributed number
#     with mean 0 and standard deviation 1
#

generator chisquared {int df} {
    double y = 0;
    int i;

    for (i = 0; i < df; i++) {
      double rad = sqrt (-log (RANDOM ()));
      double phi = 2 * M_PI * RANDOM ();
      double r   = rad * cos (phi);
      /* So far like a normal distribution */
      y += r * r;
    }

    /* http://www.dsplog.com/2008/07/28/chi-square-random-variable/ */

    *v = (y - df)/sqrt (2*df);
} {double v}

# Disk --
#     Produce random numbers with a uniform distribution of points on a disk
#
# Arguments:
#     rad       Radius of the disk
#
# Result:
#     Name of a procedure that returns the x- and y-coordinates of
#     such a random point
#

generator disk {double radius} {
    double rad = radius * sqrt (RANDOM ());
    double phi = 2 * M_PI * RANDOM ();

    *x   = rad * cos (phi);
    *y   = rad * sin (phi);
} {double x double y}

# Ball --
#     Produce random numbers with a uniform distribution of points within a ball
#
# Arguments:
#     rad       Radius of the ball
#
# Result:
#     Name of a procedure that returns the x-, y- and z-coordinates of
#     such a random point
#

generator ball {double radius} {
    double rad   = radius * pow (RANDOM (), 1./3.);
    double phi   = 2 * M_PI * RANDOM ();
    double theta = acos ( 2 * RANDOM () - 1);

    *x   = rad * cos (phi) * cos (theta);
    *y   = rad * sin (phi) * cos (theta);
    *z   = rad * sin (theta);
} {double x double y double z}

# Sphere --
#     Produce random numbers with a uniform distribution of points on the surface
#     of a sphere
#
# Arguments:
#     rad       Radius of the sphere
#
# Result:
#     Name of a procedure that returns the x-, y- and z-coordinates of
#     such a random point
#

generator sphere {double radius} {
    double phi   = 2 * M_PI * RANDOM ();
    double theta = acos ( 2 * RANDOM () - 1);

    *x   = radius * cos (phi) * cos (theta);
    *y   = radius * sin (phi) * cos (theta);
    *z   = radius * sin (theta);
} {double x double y double z}

# Rectangle --
#     Produce random numbers with a uniform distribution of points in a rectangle
#
# Arguments:
#     length    Length of the rectangle (x-direction)
#     width     Width of the rectangle (y-direction)
#
# Result:
#     Name of a procedure that returns the x- and y-coordinates of
#     such a random point
#

generator rectangle {double length double width} {
    *x   = length * RANDOM ();
    *y   = width  * RANDOM ();
} {double x double y}

# Block --
#     Produce random numbers with a uniform distribution of points in a block
#
# Arguments:
#     length    Length of the block (x-direction)
#     width     Width of the block (y-direction)
#     depth     Depth of the block (y-direction)
#
# Result:
#     Name of a procedure that returns the x-, y- and z-coordinates of
#     such a random point
#

generator block {double length double width double depth} {
    *x   = length * RANDOM ();
    *y   = width  * RANDOM ();
    *z   = depth  * RANDOM ();
} {double x double y double z}


# # ## ### ##### ######## ############# #####################
## Finalization; drop the helper command, and provide the package.

rename generator {}

package provide rnmath 1
return
