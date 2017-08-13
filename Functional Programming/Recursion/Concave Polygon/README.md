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
approach, using convex hull calculation combined with a check that all given points are in the convex hull.

If N=3 we have nothing to do, this polygon can not be concave.

### Sorting
Otherwise the given points need sorting, this is were the O(n log n) complexity shows up.
We sort the points by calculating the center point (cx, cy) (see ``midpoint/2``), 
translating the origin of the coordinate system to that center point, and using the angle between
the x-axis and the point from the (new) origin to the point for sorting (see ``sort/3``).

An alternative might be the sorting strategy used by the
[Graham Scan](http://en.wikipedia.org/wiki/Graham_scan).

### Calculating angles
For angle calculation over all four quadrants we use the 
[atan2 function](http://en.wikipedia.org/wiki/Atan2), 
which calculates the angles from 0 to pi and 0 to -pi and handles
the cases where x vanishes. As we like to have angles from 0 to 2pi and more, we have to
perform some case distinctions (see ``angle/2``).

### Checking the interior angles of the polygon
If N>3 we then start checking the interior angle at (x_2, y_2), for which we need the previous 
point (x_1, x_2) and next point (x_3, y_3) as well. 
To have less hassle when we deal with the interior angles at (x_N, y_N) and (x_1, y_1), we have 
appended the list of points with (x_1, y_1) and (x_2, y_2).

To calculate the interior angle (see ``relative_angle/4``), we calculate the difference 
of the two angles and extend the angles range beyond 2pi where needed.

We check the interior angles until we find a reflex one or have checked all of them.

## Where the effort went
Getting the signs right, to get correct angles for all quadrants.

## Erlang Insights
Some floating point maths, including using ``math:atan2/2`` and ``math:pi/0``.

## Related to
* [Convex Hull](http://github.com/mvw/hackerrank/tree/master/Functional%20Programming/Recursion/Convex%20Hull)
