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
# qform only, rotated and reflected (qfac -1), to test the reader's quaternion affine
q = nib.Nifti1Image(L.astype(np.uint8), None)
qa = np.array([[0, 0, -1.2, 40.0], [1.0, 0, 0, 12.5], [0, 0.8, 0, -7.0], [0, 0, 0, 1]])
q.set_qform(qa, code=1); q.set_sform(None, code=0)
nib.save(q, os.path.join(OUT, "phantom_qform.nii"))
json.dump({"affine": q.get_qform().tolist()}, open(os.path.join(OUT, "phantom_qform.json"), "w"))

# primitives
dump("erode6", R.erode6(L > 0).astype(np.int32))
dump("major_cc", R.major_cc(L == R.SUPLONG).astype(np.int32))
P = np.zeros((NX, NY, NZ), dtype=np.int32)
P[3:9, 5:12, 6:12] = 1
P[13:18, 5:12, 6:12] = 1
P[2:3, 20:21, 2:4] = 1
dump("pair_mask", P)
dump("major_cc_pair", R.major_cc(P > 0).astype(np.int32))
dump("edt", R.edt(L > 0, ZOOMS))

# operations
lo, hi = R.tissue_band(img, L)
json.dump({"lo": lo, "hi": hi}, open(os.path.join(OUT, "tissue_band.json"), "w"))
dump("thin", R.thin_suplong(L, ZOOMS, 0.88))
dump("inferior_clip", R.suplong_inferior_clip(L))
dump("surface_trim", R.surface_trim(L, img, lo, hi))
dump("clean", R.clean_labels(L, ZOOMS))
dump("smooth", R.smooth_labels(L, ZOOMS, 1.0))
json.dump({k: list(R.muscle_band(img, L, k, 2.0)) for k in (R.SUPLONG, R.INFLONG)}, open(os.path.join(OUT, "muscle_band.json"), "w"))
O = L.copy()
O[(O == R.SUPLONG) & (np.arange(NX)[:, None, None] < 6)] = R.INFLONG
dump("order_label", O)
I2 = img.copy()
I2[(L == R.INFLONG) & ~R.erode6(L > 0)] *= 0.5
dump("il_image", I2)
dump("il_trim", R.surface_trim(L, I2, *R.muscle_band(I2, L, R.INFLONG, 2.0), iters=3, which=(R.INFLONG,)))
dump("order_inflong", R.order_inflong(O))
G = L.copy()
G[0:2, 10:14, 6:12] = R.GENIO
G[10:12, 12:14, 18:20] = R.GENIO
dump("genio_label", G)
dump("genio_rules", R.genio_rules(G, ZOOMS))
assert (R.genio_rules(G, ZOOMS) != G).any() and (R.genio_rules(L, ZOOMS) == L).all()
B = np.zeros((6, 32, 8), dtype=np.int32)
B[0:3, 2:28, 4:7], B[0:3, 2:13, 7], B[0:3, 2:32, 1:4] = R.VERTICAL, R.SUPLONG, R.GENIO
B[3:6, 2:10, 4:7], B[3:6, 2:31, 7], B[3:6, 2:21, 1:4] = R.VERTICAL, R.SUPLONG, R.GENIO
dump("genio_front_label", B)
dump("genio_front", R.genio_rules(B, ZOOMS))
gf = R.genio_rules(B, ZOOMS)
assert (gf[0:3] != B[0:3]).any() and (gf[3:6] != B[3:6]).any(), "both front rules act"
gd = np.indices((24, 24, 28))
DL = np.where(((gd - 12) ** 2).sum(0) <= 81, 1, 0).astype(np.int32)
DL[9:15, 8:16, 20:27] = R.SUPLONG
DI = np.where(DL > 0, 100.0, 5.0) + rng.normal(0, 5, DL.shape)
DI[9:15, 8:16, 21:27] = 20.0
dump("dark_label", DL); dump("dark_image", DI)
dc = R.dark_cut(DL, DI, 3.0)
dump("dark_cut", dc)
assert (dc[9:15, 8:16, 21:27] == 0).mean() > 0.3 and (dc[DL == 1] == 1).mean() > 0.99, "the cut acts on the lump, ball kept"
TP = np.where(L > 0, 1.0, 0.0)
TP[(L > 0) & (np.arange(NX)[:, None, None] < 7)] = 0.5
dump("tongue_prob", TP)
dump("shave_side", R.shave_side(L))
assert (R.shave_side(L) != L).any()
post = L.copy()
post[:, :, int(cz) + 6:] = 0
dump("follow_post", post)
dump("follow_dorsum", R.follow_dorsum(L, post))
C = np.zeros((NX, NY, NZ), dtype=np.int32)
C[3:19, 4:22, 4:17] = R.VERTICAL
C[4:19, 4:22, 14:17] = R.SUPLONG
C[4:19, 20:22, 12:17] = R.SUPLONG              # a thicker edge: the cap uses the median thickness
C[4:19, 11:13, 14:17] = R.VERTICAL
C[8:10, 16:18, 14:17] = R.VERTICAL
dump("cap_label", C)
dump("sl_cap", R.sl_cap(C))
dump("pipeline_gentle", R.run_pipeline(L, img, ZOOMS, {
    "thin": True, "inferior_clip": True, "sl_cap": True, "sl_trim": 2, "il_trim": 3, "trim_sd": 2.0,
    "follow_dorsum": True, "order": True, "smooth": True, "sigma": 1.0, "genio": True, "clean": True}))
cuts = R.run_pipeline(L, img, ZOOMS, {"inferior_clip": True, "sl_trim": 2, "il_trim": 3, "trim_sd": 2.0, "follow_dorsum": True,
                                     "order": True, "dark_sd": 3.0, "prob_cut": 0.99, "smooth": True, "sigma": 1.0, "genio": True, "shave": True, "clean": True}, TP)
dump("pipeline_cuts", cuts)
MV = {"move_mm": [1, -2, 1], "move_deg": [3, -4, 2]}
moved, moved_p = R.move_volumes([L, TP], ZOOMS, R.move_rotation(MV["move_deg"]), MV["move_mm"], R.tongue_centre(L, ZOOMS))
dump("move_label", moved)
dump("move_prob", moved_p)
dump("pipeline_move", R.run_pipeline(L, img, ZOOMS, {"inferior_clip": True, "sl_trim": 2, "il_trim": 3, "trim_sd": 2.0, "follow_dorsum": True,
     "order": True, "dark_sd": 3.0, "prob_cut": 0.99, "smooth": True, "sigma": 1.0, "genio": True, "shave": True, "clean": True, **MV}, TP))
assert ((cuts > 0) & (np.arange(NX)[:, None, None] < 7)).sum() < ((L > 0) & (np.arange(NX)[:, None, None] < 7)).sum() / 2, "unsure side cut"

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

# mirror cut: an unsure vertical lump below one side of the phantom
L2, TP2 = L.copy(), np.ones(L.shape, np.float32)
L2[15:19, 11:15, 2:7] = R.VERTICAL
TP2[15:19, 11:15, 2:7] = 0.99
TP2[15:19, 11:15, 2] = 0.9999
L2[15:19, 3:6, 2:5] = R.VERTICAL
plane = {"affine": AFF.tolist(), "point": (AFF[:3, :3] @ [11, 0, 0] + AFF[:3, 3]).tolist(), "normal": (AFF[:3, 0] / ZOOMS[0]).tolist()}
mc = R.mirror_cut(L2, ZOOMS, AFF, plane["point"], plane["normal"], TP2)
assert (mc != L2).any() and ((mc != L2) <= ((L2 == R.VERTICAL) & (TP2 < 1))).all(), "only the unsure lump is cut"
assert (mc[15:19, 11:15, 2] == 0).all() and (mc[15:19, 11:15, 6] != 0).any() and (mc[15:19, 3:6, 2:5] == R.VERTICAL).all() and (mc[L == R.VERTICAL] == R.VERTICAL).all()
dump("mirror_label", L2); dump("mirror_prob", TP2); dump("mirror_cut", mc)
dump("pipeline_mirror", R.run_pipeline(L2, img, ZOOMS, {"shave": True, "clean": True, "mirror": True}, TP2, {**plane, "affine": AFF}))
json.dump(plane, open(os.path.join(OUT, "mirror_plane.json"), "w"))

print("golden written to", OUT)
print("phantom:", (NX, NY, NZ), "labels present:", sorted(np.unique(L).tolist()))
