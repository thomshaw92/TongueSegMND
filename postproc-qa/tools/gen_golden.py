"""Write a synthetic phantom and the expected output of each operation to tools/golden/."""
import json
import os
import sys

import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "reference"))
import postproc_ref as R

OUT = os.path.join(os.path.dirname(__file__), "golden")
os.makedirs(OUT, exist_ok=True)
rng = np.random.default_rng(0)

NX, NY, NZ = 22, 26, 24
ZOOMS = (1.0, 0.8, 1.2)                       # anisotropic, to exercise EDT spacing

# nested label phantom: concentric shells around a centre = a crude tongue
zz, yy, xx = np.meshgrid(np.arange(NX), np.arange(NY), np.arange(NZ), indexing="ij")
cx, cy, cz = NX/2, NY/2, NZ*0.55
r = np.sqrt(((xx-cx)*ZOOMS[0])**2 + ((yy-cy)*ZOOMS[1])**2 + ((zz-cz)*ZOOMS[2])**2)
L = np.zeros((NX, NY, NZ), dtype=np.int32)
L[r < 9.5] = R.SUPLONG          # outer shell
L[r < 7.5] = R.VERTICAL
L[r < 4.5] = R.GENIO
L[(np.abs(xx-cx) < 2.2) & (r < 8.5) & (r > 4.5)] = R.INFLONG   # a paired-ish strip
# a detached sup-long speckle (to exercise largest-CC cleanup)
L[2:4, 2:4, 2:4] = R.SUPLONG

# image: tissue ~1.0 inside, gap ~0.3 (dark) outside, + light noise
img = 0.3 + 0.7 * (L > 0).astype(np.float64)
img += 0.02 * rng.standard_normal(img.shape)
img = img.astype(np.float32)


def dump(name, arr):
    a = np.ascontiguousarray(arr)
    json.dump({"dims": list(a.shape), "data": a.ravel().tolist()},
              open(os.path.join(OUT, name + ".json"), "w"))


dump("phantom_label", L)
dump("phantom_image", img)
json.dump({"zooms": list(ZOOMS)}, open(os.path.join(OUT, "meta.json"), "w"))

# real NIfTI phantoms (nibabel) to test the JS reader's Fortran->C transpose
import nibabel as nib
aff = np.diag(list(ZOOMS) + [1.0])
nib.save(nib.Nifti1Image(L.astype(np.uint8), aff), os.path.join(OUT, "phantom_label.nii"))
nib.save(nib.Nifti1Image(img.astype(np.float32), aff), os.path.join(OUT, "phantom_image.nii"))

# primitives
dump("erode6", R.erode6(L > 0).astype(np.int32))
dump("dilate6", R.dilate6(L > 0).astype(np.int32))
dump("largest_cc", R.largest_cc(L == R.SUPLONG).astype(np.int32))
dump("edt", R.edt(L > 0, ZOOMS))

# operations
lo, hi = R.tissue_band(img, L)
json.dump({"lo": lo, "hi": hi}, open(os.path.join(OUT, "tissue_band.json"), "w"))
dump("thin", R.thin_suplong(L, ZOOMS, 0.88))
dump("inferior_clip", R.suplong_inferior_clip(L))
dump("surface_trim", R.surface_trim(L, img, lo, hi))
dump("clean", R.clean_labels(L, 1))
dump("smooth", R.smooth_labels(L, 0.8))
post = L.copy()
post[:, :, int(cz) + 6:] = 0
dump("follow_post", post)
dump("follow_dorsum", R.follow_dorsum(L, post))
dump("pipeline_gentle", R.run_pipeline(L, img, ZOOMS, {
    "thin": True, "inferior_clip": True, "surface_trim": True,
    "clean": True, "follow_dorsum": True, "smooth": True}))

# midline: an elongated, rotated tongue-like ellipsoid with an off-centre genioglossus core
ia, ja, ka = np.meshgrid(np.arange(NX), np.arange(NY), np.arange(NZ), indexing="ij")
M = np.zeros((NX, NY, NZ), dtype=np.int32)
M[((ia - 10.5) / 5) ** 2 + ((ja - 12.5) / 11) ** 2 + ((ka - 11.5) / 8) ** 2 < 1] = R.VERTICAL
M[(M > 0) & (np.abs(ia - 11.5) < 1.6) & (ka < 11)] = R.GENIO
th, ph = np.radians(9), np.radians(-6)
rot = np.array([[np.cos(th), -np.sin(th), 0], [np.sin(th), np.cos(th), 0], [0, 0, 1]]) @ \
      np.array([[np.cos(ph), 0, np.sin(ph)], [0, 1, 0], [-np.sin(ph), 0, np.cos(ph)]])
AFF = np.eye(4)
AFF[:3, :3] = rot @ np.diag(ZOOMS)
AFF[:3, 3] = [-12.3, 4.5, 30.2]
dump("midline_label", M)
res = {"affine": AFF.tolist()}
for mode in ("gg", "tilt"):
    p, n = R.midline_plane(M, AFF, mode)
    pa, na = R.adjust_plane(p, n, 1.5, 7.0, -4.0)
    res[mode] = {"point": p.tolist(), "normal": n.tolist(), "counts": R.lateral_counts(M, AFF, p, n),
                 "adj_point": pa.tolist(), "adj_normal": na.tolist(), "adj_counts": R.lateral_counts(M, AFF, pa, na)}
json.dump(res, open(os.path.join(OUT, "midline.json"), "w"))

print("golden written to", OUT)
print("phantom:", (NX, NY, NZ), "labels present:", sorted(np.unique(L).tolist()))
