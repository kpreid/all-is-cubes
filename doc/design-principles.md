# Design Principles for All is Cubes

This document contains information intended to explain the design choices in All is Cubes’s code, and guide future changes.

See also [`CONTRIBUTING.md`](../CONTRIBUTING.md) for information about how code should be written.

## All is Cubes is a hobby project

All is Cubes is, above all else, Kevin Reid’s hobby.
The scope of the project is what Kevin finds fun to work on.
Despite this fundamental frivolity, the components of the system are intended to be, in principle, well-written libraries that could plausibly be used in other projects.

## Art style and rendering

This section covers two deeply entangled topics: the type of art the All is Cubes engine supports (that is, what shapes and materials can be expressed using `Block`, `Atom`, `Space`, etc.), and the architectural choices of the renderers which create images of this data.

### All is cubes (we mean it)

In All is Cubes, there are no spheres, cylinders, or other such shapes.
Not only that, there are also no cubes that are rotated.
This is both an artistic constraint and a property which makes the renderer much easier to implement.

This is not to say that there are no circles or cases where Euclidean distance applies at all;
in particular, render distance and fog is determined by Euclidean distance, and of course, the camera can rotate.

I have not yet decided whether there may be particle effects that involve rotation or curves.

### Visual stability

Both of All is Cubes’s renderers (the CPU-only raytracer and the GPU mesh renderer) follow the principle that the image they produce is **deterministic and noise-free**, outside of certain exceptions. This has the following consequences:

* No approximate screen-space effects are used, except for bloom;
  no screen-space ambient occlusion (SSAO),
  no screen-space reflections,
  and no pseudo-antialiasing (FXAA, etc.).
  This ensures that the image content is not affected by the accidental shapes of pixel-scale details (shimmering/flickering), or the boundaries of the image (except for bloom, which is somewhat intended to have that result).

* No temporal accumulation is used.
  Therefore, everything looks as good in motion as it does stationary.

* Global illumination is performed in world space and results are stored in the world, so while it can be stale, information is never lost as a result of camera movement.

* Meshes are carefully built to have no “T-junctions”, since T-junctions cause single-pixel gaps.
  In general, single-pixel errors are considered to be just as bad as larger ones, at least when they introduce shapes that should not be there, as opposed to merely slightly changing the outline of a real shape.

Exceptions:

* In the future, we may support path tracing techniques on an *opt-in* basis, for offline rendering and “reference” comparison with our block-scale GI.

* Mesh data and illumination are calculated asynchronously, so that some frames may contain stale shapes and stale light.

See also “The Sane Rendering Manifesto” ([text](https://gist.github.com/bazhenovc/c0aa56cdf50df495fda84de58ef1de5e), [video](https://www.youtube.com/watch?v=KwiwIbjcjW4)) by Kirill / baz! / 
bazhenovc, which is a similar perspective arrived at independently.

## Modularity

All is Cubes is broken up into several library crates.
The divisions are chosen to serve two purposes:

* Minimizing rebuild time during development.
* In theory, supporting the use of All is Cubes libraries for various purposes without bringing in
  undesirable additional dependencies and assumptions.

For the latter purpose (and for testability and other such considerations),
most of All is Cubes’s code is designed to be “sans-IO” —
to refrain from interacting with the outside world except as directed via callbacks and trait implementations.
For example:

* The `all-is-cubes` crate manages the simulation and data storage, but does not concern itself with rendering, UI, audio output, or disk IO.
* The `all-is-cubes-mesh` crate contains algorithms for producing meshes from `all-is-cubes` content, but does not interact with any graphics API or prescribe a mesh data format.
* The `all-is-cubes-gpu` crate uses `wgpu` to communicate with the GPU but does not create, or require, a window.

Whenever possible, each crate is `no_std` compatible.