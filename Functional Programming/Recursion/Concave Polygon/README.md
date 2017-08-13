# Concave Polygon

## Problem
http://www.hackerrank.com/challenges/lambda-march-concave-polygon

We have N points in the plane, N from {3,..,1000}, (x_i, y_i) from {0,..,1000}^2,
which are the border points of a polygon.
The algorithm has to decide if this polygon is concave (output: YES or NO).

## Solution
[This Wikipedia article](http://en.wikipedia.org/wiki/Concave_polygon) notes that 
a concave polygon has at least one reflex interior angle, i.e. an angle larger than 180 degrees. 
My solutions checks this property.

The article also notes that a non-concave polygon is convex. This gives another
approach, using convex hull calculation combined with a check that all given points
are in the convex hull.

