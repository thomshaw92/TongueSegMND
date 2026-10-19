# Tongue-QC rating scale

**Source of truth: Shaw et al. 2025, *Computers in Biology and Medicine* —
"Segmentation of the Human Tongue Musculature Using MRI: Field Guide and Validation in
Motor Neuron Disease", Supplementary Materials, Appendix One, Table One.**

The anchors below are that table verbatim. Do not paraphrase them — ratings made against
reworded anchors are not comparable with the published work. Grades run **1–5**; there is
no 0, because "unusable / tongue not visible" is already grade 1.

Two independent scales per scan:

- **MRI quality** — the raw image in the tongue region.
- **Seg quality** — the 4-label tongue segmentation.

Rate them independently: a segmentation that is as good as a bad image allows still gets its
own honest seg score — the MRI scale is where the image is penalised.

---

## Seg quality (1–5)

| | Anchor | Disposition |
|---|---|---|
| **1** | **Unusable Quality.** Either completely unusable segmentations or labels that do not fit the anatomy. | **Fail** |
| **2** | **Poor Quality.** Missing large sections of the required labels or having labels in incorrect locations. Alternatively, this level can also include many (5+) small errors in segmentation. | **Fail** |
| **3** | **Moderate Quality.** More than three errors, but these errors are not so severe as to make the segmentation unusable. The errors could be minor misplacements or inaccuracies in labelling. Despite these errors, the segmentation is generally accurate. | **Pass** |
| **4** | **Good Quality.** The overall quality of the segmentation is good, with either minor errors present or perhaps one large error. However, these errors do not substantially impact the overall utility of the segmentation. | **Pass** |
| **5** | **Excellent Quality.** Virtually error-free. It represents the anatomy accurately without any noticeable mistakes. | **Pass** |

## MRI quality (1–5)

| | Anchor | Disposition |
|---|---|---|
| **1** | **Unacceptable Quality.** The tongue is not visible, there are large artifacts obscuring the tongue, extreme motion artifacts are present, or there's a complete lack of contrast or signal. | **Fail** |
| **2** | **Poor Quality.** Smaller artifacts like those described in 1, or minor motion artifacts. However, the tongue is still identifiable despite these issues. | **Fail or check** |
| **3** | **Moderate Quality.** The tongue is shown with adequate quality, but with noticeable imperfections. These may include moderate artifacts, slight motion blur, or some issues with contrast. The overall structure and some of the internal structures of the tongue are discernible, but the image is not ideal for detailed segmentation. | **Pass + check** |
| **4** | **Good Quality.** Minor errors in image quality are present in one or more of the domains (CNR, SNR, ringing, etc.). However, these do not significantly impair the visibility of the tongue structure. | **Pass** |
| **5** | **Excellent Quality.** Perfect image quality across all domains, including CNR, SNR, absence of ringing, and other artefacts. The entire tongue is visible, and the image is ideal for segmentation. | **Pass** |

The disposition column is exported as `mri_disposition` / `seg_disposition`, so the exclusion
rule travels with the ratings instead of being re-derived downstream.

---

## What to look for — landmarks from Appendix Two

The montage shows six frames per plane, sagittal / coronal / axial, with the plain image on
top and the overlay beneath. Hold **`h`** to blink the overlay off — the fastest way to judge
a boundary. Press **`o`** for outlines instead of fill.

Label colours follow the **pipeline**, not the paper's figures:

| | Muscle | Colour here |
|---|---|---|
| 1 | vertical / transverse | red |
| 2 | superior longitudinal | blue |
| 3 | inferior longitudinal | green |
| 4 | genioglossus | amber |

> Appendix Two's figures use yellow for superior longitudinal and green for
> transverse/vertical. This tool matches the container's own `qc.png` instead, so the two are
> consistent with each other but not with the paper's figure colours.

Shape cues from the guide, all checkable on these frames:

- **Axial** — the superior longitudinal typically shows a **"W" shape**, becoming a larger
  sheet as you move superiorly, since the top of the tongue is superior longitudinal alone.
- **Coronal** — look for the **"M" shape** of the superior longitudinal and transverse/vertical.
- **Sagittal** — superior longitudinal is a band across the top quarter of the tongue,
  beginning at the anterior-most portion of the palate. The transition to transverse/vertical
  below is where the band becomes speckled, or a bright line separates them. The air void of
  the oral cavity proper, between tongue and palate, **should not be labelled**.
- The **inferior longitudinal** lies alongside the submandibular and sublingual salivary
  glands and the mylohyoid.
- The **mylohyoid** forms the floor of the mouth and is *not* a tongue muscle — labelling it
  is an error. Styloglossus and palatoglossus are deliberately outside the protocol.

---

## Tooth artefact — press `t`

Dental and orthodontic metal (fillings, crowns, brackets) produces a signal void with a bright
rim, geometric distortion of the adjacent tongue border, and dropout following the dental arch
— most obvious on axial and coronal.

`t` is a **separate axis from the two scales**. Still grade MRI and Seg on what you actually
see; `t` records the *cause*, so it can be excluded later. After the second grade lands you get
two questions:

1. **Volume affected?** — yes if the artefact makes the segmentation include or omit tissue.
2. **Shape affected?** — yes if it distorts the tongue border or its left/right symmetry, even
   where the volume looks about right.

These export as independent columns, so a scan can be dropped from the volume analysis and kept
for the shape analysis, or the reverse.

---

## When in doubt

Score the **worst** defect you can see, not the average. If you are torn between two grades,
take the lower one and leave a note (`n`) — a note costs seconds and makes the borderline cases
adjudicable later.
