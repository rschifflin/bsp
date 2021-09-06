# BSP

Short for binary space partitioning, a technique for dividing up 3D space into practical pieces
This repository is a guile scheme implementation of bsp generation, capable of importing arbitrary polygon soups and exporting equivalent convex volumes.
Read below for theory and the module layout.

# Theory
## The problem
3D spaces can be defined by any arbitrary number of polygons, but the resulting polygon soup has no order
or simple way to navigate within it or operate over it. We want a datastructure with the following nice properties:

1. The space can be divided into a set of non-overlapping convex volumes
2. An efficient back-to-front ordering of these volumes can be found for any given axis
2. volumes can be looked up efficiently for any given point

Convex volumes are useful in general to work with because any point within a convex volumes is reachable from any other point.
This greatly simplifies a number of problems when working with volumes. Back-to-front orderings are very useful for drawing
a scene, as we can draw the furthest volumes first and draw the nearer volumes on top of them (the painter's algorithm). Volume
lookup is useful to locate point entities and their nearby surroundings within a scene.

## The solution
Binary space partitioning! The theory goes likes this:

Given the list of arbitrary polygons, enforce that they are all planar and convex. This can be handled by triangulating
any non-conforming polygons, as triangles are always planar and convex. Give each polygon a winding to determine which side of the polygon is forward and which is backward. Call these polygons 'faces'.

Next, label all faces as candidates for a partitioning split.
Then, choose a candidate for a partition. The plane of the chosen face will be the splitting plane.
Separate all polygons into those left of the splitting plane and those right of the splitting plane.
Clip intersecting polygons in half, and separate its left half and right half.
For polygons with planes equal to the splitting plane, separate those which face opposite the splitting plane to the left, and those which face the same direction as the splitting plane to the right.
Finally, remove all of the above equivalent-plane faces (including the chosen candidate face) from the candidate pool.
Repeat the process for the two separate halves, forming a tree structure where branches are nodes with splitting planes and whose leaves contain fewer and fewer candidates.
When no candidates remain, the surviving faces form a convex volume. This convex volume is a leaf node in the tree.

This process generates convex volumes because we start with a tacit convex volume containing all polygons, and continuously cut the volume in straight halves.
Straight cuts across convex volumes by definition ensure the two resulting half-volumes remain convex. The leaf nodes can't be separated any further and actually
represent a minimal convex volume where all faces lie on the convex hull. Any face that would intercede would be a splitting candidate and would eventually
separate the volume into two convex half-volumes with the interceding face now resting on the hull of a half volume, until the property holds.

## Using the tree
The resulting tree datastructure (called a bsp tree) is used for spatial queries. Any point in 3D space can be efficiently mapped to its containing convex volume
by walking the tree from the root, comparing which side of the splitting plane the point lies on, and descending that half of the branch until reaching a leaf.
Back-to-front orderings can be obtained from any given point in the tree by realizing all faces on your side of a split are closer than any face on the other
side of a split. By walking the tree back up to the root, a path is generated that can visit all volumes in order from back to front.

## Adjacency, portals and neighborhoods
The last concept to introduce in the datastructure is adjacency. Although the convex volumes generated in a BSP define a convex hull, the hull isn't completely
covered by faces- any number of gaps and spaces may be present. If two convex volumes are adjacent and connected by gaps, it would be useful to know the volumes
are joined, in a sense. To generate this information, we first generate the completely convered convex volumes.
This is done by using the tree structure to insert new faces called portals. One portal is generated for each unique face in the tree. A portal is defined as the face plane, bounded on all sides by the furthest point in a scene. This forms a polygon that is a strict superset of any polygon of that plane in the tree.
By inserting these portal polygons into the tree, they get split and partitioned into their respective volumes and fully cover their plane of the convex hull.
Whenever the portal's plane encounters a _coincident_ splitting plane, we've identified a potential boundary line between any volumes that share that plane.
In this case we generate what's called a _neighborhood_, a collection of volumes which may lie adjacent to one another.

Neighborhoods are formed whenever the splitting plane equals the portal plane. Neighborhoods hold two sets of neighbors, the left neighbors and the right neighbors.
The portal continues down both halves of the tree. Whichever volumes the left portal is partitioned into become the left neighbors, and whichever volumes
the right portal is partitioned into become the right neighbors. Then, all left neighbors are checked against all right neighbors for any overlap. Overlap
represents the two volumes sitting adjacent along the plane. Finally, any overlapping neighbors check to see if the overlap is covered by existing faces or if
a gap exists. If the volume has existing faces that completely cover the portal, it's closed off from its neighbor and not adjacent. But if a gap exists,
we mark the two neighbors as adjacent.

Adjaceny information is very powerful. Any spatial query that spans multiple volumes can quickly check adjacent volumes without needing to re-navigate the tree. And
by hopping from neighbor to neighbor, volumes can be linked together to form a single topological space. We use this property to mark the 'inside' of a bsp tree- a point
whose volume and neighbors combine to form a single contiguous space. If we put a garden hose into the scene at the given point, only the inside space would fill up with water. This topological distinction can help trim the scene- if we only care about entities 'inside' the space, we can cull anything lying outside where it would never
be relevant.

# The modules
## Sewer
The sewer is the submodule for all the low-level helper libraries. They represent plumbing and infrastructure
that aren't directly relevant to the problem space:
- `(bsp sewer list)` provides list helpers
- `(bsp sewer fn)` provides functional helpers
- `(bsp sewer alist)` provides asociation list helpers
- `(bsp sewer plist)` provides property list helpers
- `(bsp sewer tree)` provides a tree datastructure

## Geo
The linear algebra submodule, which logically groups all the geometric primitive modules:
- `(bsp geo face)` defines faces as convex planar lists of ordered points
- `(bsp geo line)` defines lines as a point on the line and a direction normal vector
- `(bsp geo plane)` defines planes as a point on the plane and a facing normal vector
- `(bsp geo vec3)` defines 3-element vectors
- `(bsp geo bounds)` to generate a bounding cube of a given length

## Main
The bsp module itself contains a few directly relevant submodules
- `(bsp lib)` exports the main `make-bsp` function and the various operations on it.
- `(bsp clip)` handles clipping faces
- `(bsp portal)` handles portal generation

An example script, `bsp.scm`, shows the use cases for `(bsp lib)`. By default,
it expects a file `concave.json` and produces `convex.json` and `portal.json`
