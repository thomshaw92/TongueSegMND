"""The prior-refinement contract, especially that the prior never adds foreground."""
from __future__ import annotations

import numpy as np

from lats.utils.prior import refine_with_prior


def _scene(shape=(40, 40, 40)):
    """A clean 2-muscle block plus a strong prior that matches it."""
    dl = np.zeros(shape, dtype=np.int16)
    dl[10:30, 10:20, 10:30] = 1        # muscle 1 (anterior half)
    dl[10:30, 20:30, 10:30] = 2        # muscle 2 (posterior half)
    priors = np.zeros(shape + (2,), dtype=np.float32)
    priors[10:30, 10:20, 10:30, 0] = 0.95
    priors[10:30, 20:30, 10:30, 1] = 0.95
    return dl, priors


def test_debris_outside_envelope_is_removed():
    dl, priors = _scene()
    # A blob the network invented far from any prior support.
    dl[2:6, 2:6, 2:6] = 1
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2))
    assert refined[2:6, 2:6, 2:6].sum() == 0, "debris should be demoted to background"
    assert info["debris_removed_voxels"] == 4 ** 3


def test_prior_never_fills_atrophy():
    # Atlas expects muscle where the DL sees background: must stay background.
    dl, priors = _scene()
    hole = (slice(12, 18), slice(12, 18), slice(12, 18))
    dl[hole] = 0                        # atrophied-away tissue
    priors[hole + (0,)] = 0.95          # atlas still expects muscle 1 here
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2))
    assert refined[hole].sum() == 0, "prior must NOT resurrect atrophied tissue"
    assert info["promoted_voxels"] == 0


def test_refined_foreground_is_subset_of_dl():
    # With dilate_mm=0, refined foreground can only ever shrink.
    dl, priors = _scene()
    refined, _ = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2))
    assert np.all((refined > 0) <= (dl > 0))


def test_boundary_is_arbitrated_toward_prior():
    # DL labels a muscle-1 strip as muscle 2; the prior should pull it back.
    dl, priors = _scene()
    dl[10:30, 18:20, 10:30] = 2         # two-voxel strip of wrong label at the seam
    priors[10:30, 18:20, 10:30, 0] = 0.95
    priors[10:30, 18:20, 10:30, 1] = 0.05
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2),
                                      prior_weight=0.6)
    # the strip should now be mostly label 1
    strip = refined[10:30, 18:20, 10:30]
    assert (strip == 1).sum() > (strip == 2).sum()
    assert info["muscle_relabeled_voxels"] > 0


def test_interior_is_preserved():
    # Deep inside each muscle, DL evidence dominates and nothing should change.
    dl, priors = _scene()
    refined, _ = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2),
                                   prior_weight=0.3)
    assert np.all(refined[14:26, 12:18, 14:26] == 1)
    assert np.all(refined[14:26, 22:28, 14:26] == 2)


def test_no_prior_support_anywhere_is_all_debris():
    dl, priors = _scene()
    priors[:] = 0.0                     # empty prior
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2))
    assert (refined > 0).sum() == 0
    assert info["debris_removed_voxels"] == info["dl_foreground_voxels"]


def test_dilate_allows_bounded_promotion():
    # With an explicit dilation, promotion is allowed but bounded to the rim.
    dl, priors = _scene()
    # extend the prior one voxel beyond the DL on the +x face
    priors[30:31, 10:20, 10:30, 0] = 0.95
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2),
                                      dilate_mm=1.0)
    assert info["promoted_voxels"] > 0
    # but still bounded to the dilation shell, not the whole prior
    assert info["promoted_voxels"] < 2000


# --- posterior path: DL softmax probabilities as the likelihood -----------------

def _probs_from_labels(dl, fg_ids=(1, 2), conf=0.9):
    """A near-one-hot softmax consistent with the hard labels; channel [bg, *fg_ids]."""
    C = len(fg_ids) + 1
    p = np.full(dl.shape + (C,), (1.0 - conf) / (C - 1), dtype=np.float32)
    idx = np.zeros(dl.shape, dtype=int)              # bg -> channel 0
    for j, fid in enumerate(fg_ids, start=1):
        idx[dl == fid] = j
    np.put_along_axis(p, idx[..., None], conf, axis=-1)
    return p


def test_probs_path_reports_source_and_preserves_interior():
    dl, priors = _scene()
    probs = _probs_from_labels(dl, conf=0.95)
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2), dl_probs=probs)
    assert info["likelihood_source"] == "dl_probs"
    assert np.all(refined[14:26, 12:18, 14:26] == 1)
    assert np.all(refined[14:26, 22:28, 14:26] == 2)
    assert info["promoted_voxels"] == 0


def test_probs_never_fills_atrophy():
    # Atlas expects muscle where the DL confidently sees background.
    dl, priors = _scene()
    hole = (slice(12, 18), slice(12, 18), slice(12, 18))
    dl[hole] = 0
    priors[hole + (0,)] = 0.95
    probs = _probs_from_labels(dl, conf=0.95)        # bg confident in the hole
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2), dl_probs=probs)
    assert refined[hole].sum() == 0
    assert info["promoted_voxels"] == 0


def test_probs_candidate_mask_still_clamps():
    # A DL-background voxel stays background even if probs and prior say muscle.
    dl, priors = _scene()
    probs = _probs_from_labels(dl, conf=0.9)
    probs[5, 5, 5] = [0.05, 0.90, 0.05]              # DL (wrongly) says muscle 1
    priors[5, 5, 5, 0] = 0.95                        # atlas says muscle 1
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2),
                                      dl_probs=probs, dilate_mm=0.0)
    assert refined[5, 5, 5] == 0
    assert info["promoted_voxels"] == 0


def test_probs_bg_channel_demotes_debris():
    dl, priors = _scene()
    dl[2:6, 2:6, 2:6] = 1                            # DL debris with no prior support
    probs = _probs_from_labels(dl, conf=0.6)
    refined, _ = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2), dl_probs=probs)
    assert refined[2:6, 2:6, 2:6].sum() == 0


def test_probs_boundary_arbitrated_toward_prior():
    dl, priors = _scene()
    strip = (slice(10, 30), slice(18, 20), slice(10, 30))
    dl[strip] = 2                                    # DL says muscle 2 at the seam
    probs = _probs_from_labels(dl, conf=0.9)
    probs[strip] = [0.05, 0.45, 0.50]               # but DL is nearly 50/50 there
    priors[strip + (0,)] = 0.95                      # atlas strongly muscle 1
    priors[strip + (1,)] = 0.05
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2),
                                      dl_probs=probs, prior_weight=0.6)
    s = refined[strip]
    assert (s == 1).sum() > (s == 2).sum()
    assert info["muscle_relabeled_voxels"] > 0


def test_probs_confident_interior_beats_soft_prior():
    # DL confident muscle 1, atlas mildly favours muscle 2: the likelihood wins.
    dl, priors = _scene()
    interior = (slice(14, 26), slice(12, 18), slice(14, 26))
    priors[interior + (0,)] = 0.4
    priors[interior + (1,)] = 0.6
    probs = _probs_from_labels(dl, conf=0.95)
    refined, _ = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2),
                                   dl_probs=probs, prior_weight=0.3)
    assert np.all(refined[interior] == 1)


def test_probs_exact_zeros_do_not_nan():
    dl, priors = _scene()
    probs = _probs_from_labels(dl, conf=1.0)         # exact zeros in off-channels
    priors[:] = 0.0                                   # zeros in the prior too
    refined, info = refine_with_prior(dl, priors, (1, 1, 1), fg_ids=(1, 2), dl_probs=probs)
    assert (refined > 0).sum() == 0                  # no prior support -> all debris
    assert info["likelihood_source"] == "dl_probs"
