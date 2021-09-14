# TODO

## Streamline plane handling
The set of input faces commpletely determine the set of planes in the tree. Splitting a polygon does not change its plane,
nor does forming a bounding face from extending a plane outward to a clipping boundary cube. Because of this, we can create
a table of all planes, dedup them and reference them by index, removing a ton of headache from floating-point issues
that arise from frequent plane calculation and comparison.
For instance, currently planes are generated from the normal of the triangle formed from the first 3 points of a polygon. Splitting the polygon
should result in identical planes, but due to calculating the normal from a different set of inputs, we can run into floating point
rounding error that causes the representation planes to deviate slightly. This can break our invariants and is a major pain to reason about.
By instead calculating the planes exactly once up front, we eliminate an entire category of possible edge cases.

## Sort neighbor portals before matching them up
When building a neighborhood, we group up all the LHS and RHS portal faces, then test each lhs against each rhs to find intersecting pairs.
This is the simplest solution and has O(n^2) behavior, despite my suspiscion that most faces do not overlap. Better would be to pick an arbitrary axis on the plane of the neighborhood, perhaps by taking the line of the first side of the first face, and mapping each face to its nearest and furthest point along the axis.
Then we can sort the points in O(nlogn), and walk the list of points from nearest to furthest. Any point found between the outermost near point and its matching far point belongs to an intersecting face. If intersections are sparse, this should perform much better.

## Use original unsplit faces on the 2nd pass (aka after removing outside faces).
If we keep track of when a face splits, then after culling outside faces we can re-unify split faces when building our second bsp tree.
If all subfaces of a whole face still remain in the tree, the original whole face can be used instead.

