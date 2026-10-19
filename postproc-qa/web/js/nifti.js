// Minimal NIfTI-1 read/write. readNifti takes a decompressed ArrayBuffer and returns
// voxel data in C order (z fastest), as postproc.js and reference/postproc_ref.py expect.

const DT = { 2: Uint8Array, 4: Int16Array, 8: Int32Array, 16: Float32Array, 64: Float64Array, 256: Int8Array, 512: Uint16Array, 768: Uint32Array };

export function readNifti(buf) {
  const dv = new DataView(buf);
  // sizeof_hdr; the typed-array voxel read below only handles little-endian
  if (dv.getInt32(0, true) !== 348) throw new Error('not a little-endian NIfTI-1 file (big-endian and NIfTI-2 files are not supported)');
  const g16 = (o) => dv.getInt16(o, true), gf = (o) => dv.getFloat32(o, true);
  const nx = g16(42), ny = g16(44), nz = Math.max(1, g16(46));
  const datatype = g16(70);
  const zooms = [gf(80), gf(84), gf(88)].map(v => v || 1);
  const voxOffset = Math.round(gf(108));
  const sclSlope = gf(112) || 1, sclInter = gf(116) || 0;
  const Arr = DT[datatype];
  if (!Arr) throw new Error('unsupported NIfTI datatype ' + datatype);
  const nVox = nx * ny * nz;
  const raw = new Arr(buf, voxOffset, nVox);       // file order: x fastest
  const dataC = new Float32Array(nVox);            // C order: z fastest
  let f = 0;
  for (let z = 0; z < nz; z++) for (let y = 0; y < ny; y++) for (let x = 0; x < nx; x++) {
    dataC[(x * ny + y) * nz + z] = raw[f++] * sclSlope + sclInter;
  }
  return { dims: [nx, ny, nz], zooms, dataC, header: new Uint8Array(buf.slice(0, voxOffset)) };
}

// Write a label volume (uint8) reusing the input header, patched to datatype 2.
export function writeNiftiLabels(header, dataC, dims) {
  const [nx, ny, nz] = dims, nVox = nx * ny * nz;
  const voxOffset = header.length;
  const buf = new ArrayBuffer(voxOffset + nVox);
  new Uint8Array(buf).set(header, 0);
  const dv = new DataView(buf);
  dv.setInt16(70, 2, true);          // datatype UINT8
  dv.setInt16(72, 8, true);          // bitpix
  dv.setFloat32(112, 1, true);       // scl_slope
  dv.setFloat32(116, 0, true);       // scl_inter
  const out = new Uint8Array(buf, voxOffset, nVox);
  let f = 0;
  for (let z = 0; z < nz; z++) for (let y = 0; y < ny; y++) for (let x = 0; x < nx; x++)
    out[f++] = dataC[(x * ny + y) * nz + z];       // C order -> file (Fortran) order
  return new Uint8Array(buf);
}

export async function gunzipIfNeeded(arrayBuffer) {
  const b = new Uint8Array(arrayBuffer);
  if (b.length > 2 && b[0] === 0x1f && b[1] === 0x8b) {
    const ds = new DecompressionStream('gzip');
    const stream = new Blob([arrayBuffer]).stream().pipeThrough(ds);
    return await new Response(stream).arrayBuffer();
  }
  return arrayBuffer;
}
