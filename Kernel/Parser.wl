BeginPackage["CoffeeLiqueur`Workshop`SplatMesh`Parser`"]

ImportSPZ::usage = "ImportSPZ[_ByteArray | _NumericArray | _List] _Association"
ExportSPZ::usage = "ExportSPZ[_Association] _ByteArray"

RegisterFormat;

Begin["`Private`"]


(* ====================================================================
   SPZ Reader — Fast (packed + compiled)
   Supports SPZ v1, v2, v3 (gzip ok; pass inflated bytes)
   Mostly generated using OpenAI GPT5 Thinking based on spark.js source files 
   ==================================================================== *)

(* ---------- utilities ---------- *)

toPackedU8[bytes_] := Developer`ToPackedArray @ If[Head[bytes] === ByteArray, Normal[bytes], bytes];

(* 32-bit LE from 4 bytes, masked to unsigned *)
u32LEFrom4C = Compile[{{b, _Integer, 1}},
  BitAnd[
    b[[1]] + BitShiftLeft[b[[2]], 8] + BitShiftLeft[b[[3]], 16] + BitShiftLeft[b[[4]], 24],
    4294967295
  ],
  CompilationTarget -> "WVM", RuntimeOptions -> "Speed"
];

(* 16-bit LE from 2 bytes *)
u16LEFrom2C = Compile[{{b, _Integer, 1}},
  b[[1]] + BitShiftLeft[b[[2]], 8],
  CompilationTarget -> "WVM", RuntimeOptions -> "Speed"
];

(* signed 24-bit little-endian for an {n,3} array of bytes -> vector length n *)
s24TriplesC = Compile[{{t, _Integer, 2}},
  Module[{u = t[[All, 1]] + BitShiftLeft[t[[All, 2]], 8] + BitShiftLeft[t[[All, 3]], 16]},
    u - 2^24*UnitStep[u - 2^23]
  ],
  CompilationTarget -> "WVM", RuntimeOptions -> "Speed"
];

(* half -> real (vector), no symbolic Infinity/Indeterminate in compiled code *)
halfToRealC = Compile[{{h, _Integer, 1}},
  Module[{n = Length[h], out = Table[0., {Length[h]}], s, e, f, sign, i},
    Do[
      s = BitAnd[BitShiftRight[h[[i]], 15], 1];
      e = BitAnd[BitShiftRight[h[[i]], 10], 31];
      f = BitAnd[h[[i]], 1023];
      sign = If[s == 1, -1., 1.];

      out[[i]] =
        If[e == 0 && f == 0,
          0.,
        If[e == 0,                              (* subnormal *)
          sign * 2.^(-14) * (f/1024.),
        If[e == 31 && f == 0,                   (* +/- Infinity -> large finite sentinel *)
          sign * $MaxMachineNumber,
        If[e == 31 && f != 0,                   (* NaN -> quiet NaN *)
          0./0.,
          sign * 2.^(e - 15) * (1. + f/1024.)   (* normal *)
        ]]]];

      ,
      {i, 1, n}
    ];
    out
  ],
  CompilationTarget -> "WVM", RuntimeOptions -> "Speed"
];

(* v1/v2 quaternions: xyz in [-1,1] -> {x,y,z,w}, elementwise, compiled *)
quatXYZtoQuatC = Compile[{{xyz, _Real, 2}},
  Module[{n = Length[xyz], out = ConstantArray[0., {Length[xyz], 4}],
          x, y, z, w2, i},
    Do[
      x = xyz[[i, 1]];  y = xyz[[i, 2]];  z = xyz[[i, 3]];
      w2 = 1. - x*x - y*y - z*z;
      If[w2 < 0., w2 = 0.];
      out[[i, 1]] = x;
      out[[i, 2]] = y;
      out[[i, 3]] = z;
      out[[i, 4]] = Sqrt[w2];
      ,
      {i, 1, n}
    ];
    out
  ],
  CompilationTarget -> "WVM",
  RuntimeOptions -> "Speed"
];


(* v3 quat from packed u32 — keep WVM to avoid rare external eval hiccups *)
quatFromU32C = Compile[{{u32, _Integer, 1}},
  Module[{n = Length[u32], out = ConstantArray[0., {Length[u32], 4}],
          valueMask = 2^9 - 1, maxValue = 1./Sqrt[2.],
          combined, largest, remain, val, sgn, idx, q0, q1, q2, q3, sumsq, pos, nn},
    Do[
      combined = BitAnd[u32[[i]], 4294967295];
      largest  = BitShiftRight[combined, 30];
      remain   = BitAnd[combined, 2^30 - 1];
      q0 = 0.; q1 = 0.; q2 = 0.; q3 = 0.;
      sumsq = 0.;
      Do[
        If[idx =!= largest,
          val = BitAnd[remain, valueMask];
          sgn = BitAnd[BitShiftRight[remain, 9], 1];
          remain = BitShiftRight[remain, 10];
          pos = idx + 1;
          If[pos == 1, q0 = maxValue*(val/valueMask); If[sgn == 1, q0 = -q0]; sumsq += q0*q0;];
          If[pos == 2, q1 = maxValue*(val/valueMask); If[sgn == 1, q1 = -q1]; sumsq += q1*q1;];
          If[pos == 3, q2 = maxValue*(val/valueMask); If[sgn == 1, q2 = -q2]; sumsq += q2*q2;];
          If[pos == 4, q3 = maxValue*(val/valueMask); If[sgn == 1, q3 = -q3]; sumsq += q3*q3;];
        ],
        {idx, 3, 0, -1}
      ];
      If[largest + 1 == 1, q0 = Sqrt[Max[0., 1. - sumsq]]];
      If[largest + 1 == 2, q1 = Sqrt[Max[0., 1. - sumsq]]];
      If[largest + 1 == 3, q2 = Sqrt[Max[0., 1. - sumsq]]];
      If[largest + 1 == 4, q3 = Sqrt[Max[0., 1. - sumsq]]];

      (* normalize this row explicitly *)
      nn = Sqrt[q0^2 + q1^2 + q2^2 + q3^2];
      If[nn == 0., nn = 1.];
      out[[i, 1]] = q0/nn; out[[i, 2]] = q1/nn; out[[i, 3]] = q2/nn; out[[i, 4]] = q3/nn;
      ,
      {i, 1, n}
    ];
    out
  ],
  CompilationTarget -> "WVM",  (* robust across versions *)
  RuntimeOptions -> "Speed"
];


SHC0 = 0.28209479177387814;  (* 1/sqrt(4π) *)
degToVecs = <|1 -> 3, 2 -> 8, 3 -> 15|>;

ImportSPZ[na_NumericArray] := ImportSPZ[Normal @ na];
ImportSPZ[na_ByteArray] :=    ImportSPZ[Normal @ na];

ImportSPZ[bytesIn_List] := Module[
  {b = toPackedU8[bytesIn], p = 1, n = Length @ toPackedU8[bytesIn],
   takeK, bytes, q, version, numSplats, shDegree, fractionalBits, flags, antiAlias,
   centers, alphas, rgbs, scales, quats, scaleC, fixed,
   shDegreeCnt, shBytes, off, sh1 = Null, sh2 = Null, sh3 = Null,
   expected, rgbBytes, scaleBytes, quatBytes, u32s, halfs, s24raw, xyzBytes, xyz,
   remaining, neededSH},

  takeK[k_Integer?NonNegative] := Module[{q2 = p + k - 1},
    If[q2 > n, Throw["eof"]];
    bytes = b[[p ;; q2]]; p = q2 + 1; bytes
  ];

  Check[
    (* header *)
    expected = FromDigits[Reverse @ ToCharacterCode["NGSP"], 256];
    If[u32LEFrom4C[takeK[4]] =!= expected,
      Message[ImportSpz::badmagic, IntegerString[%, 16, 8]]; Return[$Failed]
    ];

    version         = u32LEFrom4C @ takeK[4];
    If[! MemberQ[{1, 2, 3}, version], Message[ImportSpz::unsup, version]; Return[$Failed]];
    numSplats       = u32LEFrom4C @ takeK[4];
    shDegree        = First @ takeK[1];
    fractionalBits  = First @ takeK[1];
    flags           = First @ takeK[1];
    antiAlias       = BitAnd[flags, 1] =!= 0;
    (* reserved *) takeK[1];

    (* centers *)
    Which[
      version == 1,
        (* 3 half-floats per splat *)
        halfs   = Partition[u16LEFrom2C /@ Partition[takeK[2*3*numSplats], 2], 3];
        centers = Partition[halfToRealC @ Flatten[halfs], 3],
      True,
        (* 3 signed 24-bit per splat *)
        fixed   = 2.^fractionalBits;
        s24raw  = Partition[takeK[9*numSplats], 3];
        s24raw  = Partition[s24TriplesC @ s24raw, 3];
        centers = N[s24raw/fixed, MachinePrecision]
    ];

    (* alpha *)
    alphas = takeK[numSplats]/255.;

    (* DC/RGB *)
    rgbBytes = Partition[takeK[3*numSplats], 3];
    scaleC = SHC0/0.15;
    rgbs = (rgbBytes*(1./255.) - 0.5) * scaleC + 0.5;

    (* scales *)
    scaleBytes = Partition[takeK[3*numSplats], 3];
    scales = Exp[scaleBytes*(1./16.) - 10.];

    (* quaternions *)
    If[version == 3,
      quatBytes = Partition[takeK[4*numSplats], 4];
      u32s = u32LEFrom4C /@ quatBytes;
      quats = quatFromU32C[u32s],
      (* v1/v2: xyz bytes -> [-1,1], reconstruct w *)
      xyzBytes = Partition[takeK[3*numSplats], 3];
      xyz = xyzBytes/127.5 - 1.;
      quats = quatXYZtoQuatC[xyz]
    ];

    (* SH (spherical harmonics) *)
    shDegreeCnt = If[shDegree >= 1, degToVecs[shDegree]*3, 0];

    If[shDegreeCnt > 0,
      remaining = n - p + 1;
      neededSH  = numSplats*shDegreeCnt;
      If[remaining < neededSH,
        Message[ImportSpz::short, "SPZ SH block"]; Return[$Failed];
      ];

      shBytes = takeK[neededSH];
      off = 0;

      If[shDegree >= 1,
        sh1 = ArrayReshape[
                (shBytes[[off + 1 ;; off + 9*numSplats]] - 128)/128.,
                {numSplats, 9}
              ];
        off += 9*numSplats;
      ];

      If[shDegree >= 2,
        sh2 = ArrayReshape[
                (shBytes[[off + 1 ;; off + 15*numSplats]] - 128)/128.,
                {numSplats, 15}
              ];
        off += 15*numSplats;
      ];

      If[shDegree >= 3,
        sh3 = ArrayReshape[
                (shBytes[[off + 1 ;; off + 21*numSplats]] - 128)/128.,
                {numSplats, 21}
              ];
        off += 21*numSplats;
      ];
    ];
    ,
    Message[ImportSpz::short, "SPZ data"]; Return[$Failed];
  ];

  <|
    "Version" -> version,
    "NumSplats" -> numSplats,
    "ShDegree" -> shDegree,
    "FractionalBits" -> fractionalBits,
    "Flags" -> flags,
    "AntiAliasFlagQ" -> antiAlias,
    "Centers" -> centers,      (* {n,3} *)
    "Opacities" -> alphas,     (* {n} *)
    "RGB" -> rgbs,             (* {n,3} in [0,1] (DC scaled) *)
    "Scales" -> scales,        (* {n,3} *)
    "Quaternions" -> quats,    (* {n,4} *)
    "SH1" -> sh1,              (* or Null *)
    "SH2" -> sh2,              (* or Null *)
    "SH3" -> sh3               (* or Null *)
  |>
];

(* ---------- optional post-pass: map sentinels back to symbols ---------- *)
FixHalfSpecials[data_Association] := Module[{map},
  map = # /. {
      x_ /; Abs[x] >= 0.5 $MaxMachineNumber :> Sign[x] Infinity,
      y_ /; ! NumericQ[y] :> Indeterminate
    } &;
  data /. {
    "Centers" -> c_ :> c,  (* centers may include half->real; leave as-is or map per-field *)
    "Quaternions" -> q_ :> q
  }
];


Options[ExportSPZ] = {
  "FractionalBits" -> 12,
  "AntiAlias"      -> True,
  "ShDegree"       -> Automatic  (* inferred from SH arrays when Automatic *)
};

(* constants *)
SHC0 = 0.28209479177387814; (* 1/sqrt(4π) *)
FLAGANTIALIASED = 1;
degToVecs = <|1 -> 3, 2 -> 8, 3 -> 15|>;

(* ---------- helpers (packed + vectorized) ---------- *)

toPackedU8Numeric[ints_List] := ByteArray[NumericArray[ints, "UnsignedInteger8", "ClipAndRound"] ];

u32LEBytes1[u_Integer] := Module[{x = BitAnd[u, 4294967295]},
  {
    BitAnd[x, 255],
    BitAnd[BitShiftRight[x, 8], 255],
    BitAnd[BitShiftRight[x, 16], 255],
    BitAnd[BitShiftRight[x, 24], 255]
  }
];

u32LEBytesVec[u_List] := Module[{x = BitAnd[u, 4294967295]},
  Flatten @ Transpose @ {
    BitAnd[x, 255],
    BitAnd[BitShiftRight[x, 8], 255],
    BitAnd[BitShiftRight[x, 16], 255],
    BitAnd[BitShiftRight[x, 24], 255]
  }
];

(* signed 24-bit two's complement to bytes (vectorized over a flat integer list) *)
s24LEBytesVec[intFlat_List] := Module[{u = Mod[intFlat, 2^24]},
  Flatten @ Transpose @ {
    BitAnd[u, 255],
    BitAnd[BitShiftRight[u, 8], 255],
    BitAnd[BitShiftRight[u, 16], 255]
  }
];

(* v3 quaternion compression (smallest three) -> u32 per row (compiled) *)
quatV3U32C = Compile[{{q, _Real, 2}},
  Module[{n = Length[q], out = Table[0, {Length[q]}],
          maxValue = 1./Sqrt[2.], x, y, z, w, nrm,
          iLargest, av, ai, negate, comp, j, temp, negbit, mag},
    Do[
      x = q[[i, 1]]; y = q[[i, 2]]; z = q[[i, 3]]; w = q[[i, 4]];
      nrm = Sqrt[x*x + y*y + z*z + w*w];
      If[nrm == 0., x = 0.; y = 0.; z = 0.; w = 1., x /= nrm; y /= nrm; z /= nrm; w /= nrm];

      (* find index of largest abs component (1..4) *)
      iLargest = 1; av = Abs[x];
      ai = Abs[y]; If[ai > av, iLargest = 2; av = ai];
      ai = Abs[z]; If[ai > av, iLargest = 3; av = ai];
      ai = Abs[w]; If[ai > av, iLargest = 4; av = ai];

      negate = If[Which[iLargest == 1, x, iLargest == 2, y, iLargest == 3, z, True, w] < 0., 1, 0];
      comp = iLargest - 1; (* 0..3 in the top bits, then 3x{sign,9bit} packed high->low *)

      Do[
        If[j =!= iLargest,
          temp = Which[j == 1, x, j == 2, y, j == 3, z, True, w];
          negbit = BitXor[If[temp < 0., 1, 0], negate];
          mag = Round[(2^9 - 1) * (Abs[temp]/maxValue)];
          If[mag < 0, mag = 0]; If[mag > 2^9 - 1, mag = 2^9 - 1];
          comp = BitOr[BitShiftLeft[comp, 10], BitOr[BitShiftLeft[negbit, 9], mag]];
        ],
        {j, 1, 4}
      ];

      out[[i]] = comp;
      ,
      {i, 1, n}
    ];
    out
  ],
  CompilationTarget -> "WVM",  (* robust across versions *)
  RuntimeOptions -> "Speed"
];

scaleRgbToByteVec[rgbMat_?MatrixQ] := Module[
  {scaleC = SHC0/0.15, bytes},
  bytes = ((rgbMat - 0.5)/scaleC + 0.5)*255.;
  Developer`ToPackedArray @ Round @ Clip[Flatten @ bytes, {0., 255.}]
];

scalesToByteVec[scMat_?MatrixQ] := Module[{bytes},
  bytes = (Log[scMat] + 10.)*16.;
  Developer`ToPackedArray @ Round @ Clip[Flatten @ bytes, {0., 255.}]
];

opacToByteVec[opac_List] :=
  Developer`ToPackedArray @ Round @ Clip[opac*255., {0., 255.}];

(* SH quantization: bits=5 for SH1, bits=4 for SH2/SH3 (as in your writer) *)
quantizeSHMat[shMat_?MatrixQ, bits_Integer] := Module[{bucket = 2^(8 - bits)},
  Developer`ToPackedArray @ Clip[
    bucket * Round[(Round[shMat*128] + 128)/bucket],
    {0, 255}
  ]
];

(* ---------- main writer ---------- *)

ExportSPZ[spz_Association, OptionsPattern[]] := Module[
  {
    centers = N @ spz["Centers"],        (* {n,3} *)
    opac    = N @ spz["Opacities"],      (* {n}   *)
    rgbs    = N @ spz["RGB"],            (* {n,3} *)
    scales  = N @ spz["Scales"],         (* {n,3} *)
    quats   = N @ spz["Quaternions"],    (* {n,4} *)

    sh1 = spz["SH1"], sh2 = spz["SH2"], sh3 = spz["SH3"],

    numSplats, shDegree, fracBits, flags,
    fraction, centersIntRaw, centersIntClip, clippedMask, clippedCount,
    centersBytes, opacBytes, rgbBytes, scaleBytes, quatU32, quatBytes,
    shBytes = {}, header
  },

  numSplats = If[MatrixQ[centers], Length[centers], 0];

  shDegree = Replace[OptionValue["ShDegree"], {
    Automatic :> Which[
      MatrixQ[sh3], 3,
      MatrixQ[sh2], 2,
      MatrixQ[sh1], 1,
      True, 0
    ],
    x_Integer :> x
  }];

  fracBits = OptionValue["FractionalBits"];
  flags    = If[TrueQ@OptionValue["AntiAlias"], FLAGANTIALIASED, 0];

  (* ---- Header ---- *)
  header = Join[
    u32LEBytes1[16^^5053474E],     (* magic "NGSP" little-endian *)
    u32LEBytes1[3],                (* SPZ version = 3 *)
    u32LEBytes1[numSplats],
    {shDegree, fracBits, flags, 0} (* 4 single bytes *)
  ];

  (* ---- Centers: 24-bit signed fixed-point ---- *)
  fraction = 2.^fracBits;
  centersIntRaw  = Round[centers * fraction];                      (* {n,3} integers *)
  centersIntClip = Clip[centersIntRaw, {-2^23, 2^23 - 1}];         (* {n,3} *)
  centersBytes   = s24LEBytesVec @ Flatten @ Mod[centersIntClip, 2^24];

  (* ---- Opacity ---- *)
  opacBytes = opacToByteVec @ opac;

  (* ---- DC/RGB ---- *)
  rgbBytes = scaleRgbToByteVec @ rgbs;

  (* ---- Scales ---- *)
  scaleBytes = scalesToByteVec @ scales;

  (* ---- Quaternions (v3 smallest-three) ---- *)
  quatU32   = quatV3U32C[quats];
  quatBytes = u32LEBytesVec @ quatU32;

  (* ---- SH bands ---- *)
  If[shDegree >= 1 && MatrixQ[sh1],
    shBytes = Join[shBytes, Flatten @ quantizeSHMat[sh1, 5]];
  ];
  If[shDegree >= 2 && MatrixQ[sh2],
    shBytes = Join[shBytes, Flatten @ quantizeSHMat[sh2, 4]];
  ];
  If[shDegree >= 3 && MatrixQ[sh3],
    shBytes = Join[shBytes, Flatten @ quantizeSHMat[sh3, 4]];
  ];

  ExportByteArray[toPackedU8Numeric @ Join[
      header,
      centersBytes,
      opacBytes,
      rgbBytes,
      scaleBytes,
      quatBytes,
      shBytes
  ], {"GZIP", "Byte"}]
];

(* Contributed by: Sean Cheren *)

RegisterFormat[fmt_String?StringQ, 
   assoc : <|
     Repeated[
      "MIMETypes" | "Extensions" -> _?StringQ | {_?StringQ ..}, {1, 
       2}]|>] :=
  Block[
   	{$ext, $mime}
   	,
   	$ext = 
    Replace[Developer`ToList @ Lookup[assoc, "Extensions", {}], 
     s_String?(StringFreeQ["*"]) :> "*." <> s , {1}];
   	$mime = Developer`ToList @ Lookup[assoc, "MIMETypes", {}];
   	
   	If[Positive @ Length[$ext],
    		(
       			
       If[Lookup[System`ConvertersDump`ExtensionMappings, #] =!= fmt,
         				
         AppendTo[System`ConvertersDump`ExtensionMappings, # -> fmt]
         			] &
       		) /@ $ext;
    	];
   	
   	If[MemberQ[FileFormatDump`$FILEFORMATS, fmt],
    		If[Positive @ Length @ $ext,
     			FileFormatDump`$FILEFORMATMATRIX[fmt] = 
       				ReplacePart[
        					FileFormatDump`$FILEFORMATMATRIX[fmt], 
        					{7 -> 
          DeleteDuplicates @ 
           Join[FileFormatDump`$FILEFORMATMATRIX[fmt][[7]], $ext]}
        				];
     		];
    		If[Positive @ Length @ $mime,
     			FileFormatDump`$FILEFORMATMATRIX[fmt] =
       				ReplacePart[
        					FileFormatDump`$FILEFORMATMATRIX[fmt], 
        					{8 -> 
          DeleteDuplicates @ 
           Join[FileFormatDump`$FILEFORMATMATRIX[fmt][[8]], $mime]}
        				];
     		];
    		,
    		FileFormatDump`$FILEFORMATS = 
     Insert[FileFormatDump`$FILEFORMATS, fmt, -2];
    		FileFormatDump`$FILEFORMATMATRIX[fmt] =
     			{fmt, True, False, False, False, False, $ext, $mime, None, {}};
    	];
   	Success["RegisterFormatSuccess", <|
     		"MessageTemplate" -> "Format successfully registered."
     	|>]
   ];


End[]
EndPackage[]