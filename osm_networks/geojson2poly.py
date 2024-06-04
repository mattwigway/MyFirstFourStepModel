import json
from sys import argv

with open(argv[1]) as inf:
    gj = json.load(inf)

assert len(gj["features"]) == 1
assert gj["features"][0]["geometry"]["type"] == "Polygon"
poly = gj["features"][0]["geometry"]["coordinates"]
if len(poly) != 1:
    print("WARN: holes will be ignored")
coords = poly[0]

with open(argv[2], "w") as outf:
    outf.write("none\n")
    outf.write("1\n")
    for coord in coords:
        outf.write(f"    {coord[0]:e}    {coord[1]:e}\n")
    outf.write("END\n")
    outf.write("END\n")
