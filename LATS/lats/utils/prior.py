"""Per-voxel refinement of a DL segmentation with soft atlas priors."""
from __future__ import annotations

import numpy as np

EPS = 1e-6


def _smooth_onehot(labels: np.ndarray, k: int, sigma_vox: float) -> np.ndarray:
    from scipy import ndimage

    oh = (labels == k).astype(np.float32)
    if sigma_vox > 0:
        oh = ndimage.gaussian_filter(oh, sigma_vox)
    return oh


def refine_with_prior(
    dl_labels: np.ndarray,
    priors: np.ndarray,
    zooms,
    fg_ids=(1, 2, 3, 4),
    prior_weight: float = 0.3,
    debris_prior_threshold: float = 0.05,
    likelihood_smooth_mm: float = 1.5,
    dilate_mm: float = 0.0,
    dl_probs: np.ndarray | None = None,
) -> tuple[np.ndarray, dict]:
    """Per-voxel MAP combine of DL labels and soft priors; returns (labels, info)."""
    from scipy import ndimage

    dl = np.asarray(dl_labels).astype(int)
    K = len(fg_ids)
    if priors.shape[-1] != K:
        raise ValueError(f"priors has {priors.shape[-1]} channels, expected {K}")

    mean_zoom = float(np.mean(zooms[:3]))
    sig_like = likelihood_smooth_mm / max(mean_zoom, 1e-6)

    # Hard clamp: only the (optionally grown) DL foreground can become foreground.
    dl_fg = dl > 0
    if dilate_mm and dilate_mm > 0:
        r = int(round(dilate_mm / max(mean_zoom, 1e-6)))
        candidate = ndimage.binary_dilation(dl_fg, iterations=max(r, 1))
    else:
        candidate = dl_fg

    fg_prior = np.clip(priors, 0, None)                        # (X, Y, Z, K)
    total_fg_prior = fg_prior.sum(axis=-1)
    fg_id_arr = np.array(fg_ids)
    refined = np.zeros_like(dl)

    if dl_probs is not None:
        # Proper posterior: DL softmax as the likelihood over {bg, *fg_ids}.
        p = np.clip(np.asarray(dl_probs, dtype=np.float32), 0, None)
        if p.shape[-1] != K + 1:
            raise ValueError(
                f"dl_probs has {p.shape[-1]} channels, expected {K + 1} ([bg, *fg_ids])")
        p = p / (p.sum(axis=-1, keepdims=True) + EPS)          # renorm after interp
        # Background prior derived as 1 - sum of the foreground priors.
        bg_prior = np.clip(1.0 - total_fg_prior, 0.0, 1.0)
        log_like = np.log(p + EPS)                             # (X, Y, Z, K+1)
        log_prior_full = np.concatenate(
            [np.log(bg_prior[..., None] + EPS), np.log(fg_prior + EPS)], axis=-1)
        score = log_like + prior_weight * log_prior_full       # (X, Y, Z, K+1)
        win = np.argmax(score, axis=-1)                        # 0 = bg, j>=1 -> fg_ids[j-1]
        inside = candidate & (win >= 1)
        refined[inside] = fg_id_arr[win[inside] - 1]
        likelihood_source = "dl_probs"
    else:
        # Fallback (no probabilities supplied): soft one-hot of each hard DL label.
        like = np.stack([_smooth_onehot(dl, fid, sig_like) for fid in fg_ids], axis=-1)
        score = np.log(like + EPS) + prior_weight * np.log(fg_prior + EPS)  # (X, Y, Z, K)
        winner = np.argmax(score, axis=-1)                     # index into fg_ids
        refined[candidate] = fg_id_arr[winner[candidate]]
        likelihood_source = "smoothed_onehot"

    # Debris demotion (both paths): candidate voxels outside the anatomical envelope.
    debris = candidate & (total_fg_prior < debris_prior_threshold)
    refined[debris] = 0

    # Bookkeeping.
    removed = int(debris.sum())
    both_fg = (dl > 0) & (refined > 0)
    relabeled = int((dl[both_fg] != refined[both_fg]).sum())
    info = {
        "prior_weight": prior_weight,
        "debris_prior_threshold": debris_prior_threshold,
        "dilate_mm": dilate_mm,
        "likelihood_source": likelihood_source,
        "dl_foreground_voxels": int(dl_fg.sum()),
        "refined_foreground_voxels": int((refined > 0).sum()),
        "debris_removed_voxels": removed,
        "muscle_relabeled_voxels": relabeled,
        "promoted_voxels": int(((dl == 0) & (refined > 0)).sum()),  # must be 0 if dilate_mm==0
    }
    return refined.astype(dl_labels.dtype if hasattr(dl_labels, "dtype") else np.int16), info
