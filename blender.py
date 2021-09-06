# Blender export currently selected mesh as json
import sys
import json
def export_geometry(filename):
    def mesh2dict(mesh):
        matrix = mesh.matrix_world
        data = mesh.data
        return {"vertices": list(map(lambda v: (matrix @ v.co).to_tuple(), data.vertices)),
                "faces": list(map(lambda f: list(f.vertices), data.polygons))}
    inside = D.collections['Entities'].objects['inside'].location.to_tuple()
    geometry = D.collections['Geometry']
    mesh_objects = filter(lambda obj: obj.type == "MESH", geometry.objects);
    meshes = list(map(mesh2dict, mesh_objects))
    export = {"inside": inside,
              "meshes": meshes }
    file = open(filename, "w")
    file.write(json.dumps(export))
    file.close()

# Blender export currently selected mesh as plane
import sys
import json
def write_plane_sexp():
    name = C.object.name
    loc = C.object.location
    norm = C.object.data.vertices[0].normal
    sexp = f"""(define {name}
                   (make-plane
                       (make-vec3 {loc[0]} {loc[1]} {loc[2]})
                       (make-vec3 {norm[0]} {norm[1]} {norm[2]})))"""
    file = open("polygon.sexp", "w")
    file.write(sexp)
    file.close()

# Blender import json as new mesh objects
import sys
import json
import bpy

def import_geometry(filename):
    file = open(filename, "r")
    obj = json.loads(file.read())
    file.close()
    for n, convex in enumerate(obj):
        bpy.ops.object.add(type='MESH', enter_editmode=False, location=(0,0,0))
        mesh = C.object
        mesh.name = "Mesh #" + str(n)
        mesh.data.from_pydata(convex["vertices"], [], convex["faces"])
        mesh.data.update()
