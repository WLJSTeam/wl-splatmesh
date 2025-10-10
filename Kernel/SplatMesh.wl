BeginPackage["CoffeeLiqueur`Workshop`SplatMesh`", {
    "CoffeeLiqueur`Workshop`SplatMesh`Parser`"
}]

SplatMesh::usage = "Representation of packed or unpacked gaussian splatting mesh.\r\nUse Import expression to load .spz files into SplatMesh"
SplatMeshPack::usage = "SplatMeshPack[s_SplatMesh] returns an packed SplatMesh"
SplatMeshUnpack::usage = "SplatMeshUnpack[s_SplatMesh] returns an unpacked SplatMesh ready for post-processing"
SplatMeshPackedQ::usage = "SplatMeshPackedQ[s_SplatMesh] returns True if SplatMesh is packed"

SplatMeshResample::usage = "SplatMeshResample[s_SplatMesh, n_Integer] resamples SplatMesh s using binned method down to n splats"
TransformedSplat::usage = "TransformedSplat[s_SplatMesh, func_] applies func on s\r\nTransformedSplat[s_SplatMesh, matrix_] applies 3x3 matrix to centers of splat\r\nTransformedSplat[s_SplatMesh, {m,v}] applies 3x3 m matrix and translates by vector v"
SplatMeshFilter::usage = "SplatMeshFilter[s_SplatMesh, func_] filters splats based on func criteria"

Begin["`Private`"]

(* Packing/Unpacking *)

SplatMeshPackedQ[ SplatMesh["SPZ", "Packed", __] ] := True
SplatMeshPackedQ[ _ ] := False

SplatMeshPack[s: SplatMesh["SPZ", "Packed", __] ] := s
SplatMeshUnpack[s: SplatMesh["SPZ", "Unpacked", __] ] := s

SplatMeshPack[SplatMesh["SPZ", "Unpacked", a_Association, meta_] ] := SplatMesh["SPZ", "Packed", ExportSPZ[a], meta]
SplatMeshUnpack[SplatMesh["SPZ", "Packed", b_ByteArray, meta_] ] := SplatMesh["SPZ", "Unpacked", ImportSPZ[ ImportByteArray[b, {"GZIP", "Byte"}] ], meta]

SplatMeshPack[_] := $Failed
SplatMeshUnpack[_] := $Failed
SplatMesh[_, _, $Failed, _] := $Failed


(* might be slow actually *)
splatUnit[s_] := MatchQ[s, {
        {_?NumericQ, _?NumericQ, _?NumericQ}, 
        {_?NumericQ, _?NumericQ, _?NumericQ}, 
        {_?NumericQ, _?NumericQ, _?NumericQ, _?NumericQ}, 
        _?NumericQ, 
        {_?NumericQ, _?NumericQ, _?NumericQ}
}]

(* Transformations *)

TransformedSplat[s: SplatMesh["SPZ", "Packed", __], o_] := SplatMeshPack[TransformedSplat[SplatMeshUnpack[s], o] ]

TransformedSplat[   SplatMesh["SPZ", "Unpacked", a_, _], matrix_?MatrixQ] := With[{
    nmatrix = N[matrix]
}, {
    data = Append[a, "Centers" -> Map[Function[p, With[{r = (nmatrix.(With[{c = p}, {c[[3]], -c[[1]], c[[2]]}]) )},  {-r[[2]], r[[3]], r[[1]]}] ], a["Centers"] ] ]
},
    SplatMesh["SPZ", "Unpacked", data, generateMeta[data] ]
]

TransformedSplat[   SplatMesh["SPZ", "Unpacked", a_, _], {matrix_?MatrixQ, v_?VectorQ}] := With[{
    nmatrix = N[matrix],
    nvector = N[nvector]
}, {
    data = Append[a, "Centers" -> Map[Function[p, With[{r = (nmatrix.(With[{c = p}, {c[[3]], -c[[1]], c[[2]]}]) + nvector)},  {-r[[2]], r[[3]], r[[1]]}] ], a["Centers"] ] ]
},
    SplatMesh["SPZ", "Unpacked", data, generateMeta[data] ]
]

TransformedSplat::badfunction = "Transformation function does not return association";

TransformedSplat[ SplatMesh["SPZ", "Unpacked", a_, _], function_] := Module[{
    test = function[1, {0.,0.,0.}, {1.,1.,1.}, {1.,1.,1.,1.}, 1., {1.,1.,1.}]
},
    If[!MatchQ[test, {
        {_?NumericQ, _?NumericQ, _?NumericQ}, 
        {_?NumericQ, _?NumericQ, _?NumericQ}, 
        {_?NumericQ, _?NumericQ, _?NumericQ, _?NumericQ}, 
        _?NumericQ, 
        {_?NumericQ, _?NumericQ, _?NumericQ}
    }], Message[TransformedSplat::badfunction]; Return[$Failed] ];
                                                        (* RUB to normal one and back *)
    With[{t = Transpose[MapThread[(function[#1, With[{c = #2}, {c[[3]], -c[[1]], c[[2]]}], #3, #4, #5, #6]), {Range[Length[a["Centers"] ] ], a["Centers"], a["Scales"], a["Quaternions"], a["Opacities"], a["RGB"]}] ]},
        With[{
            new = Join[a, Association["Centers"->({-#[[2]], #[[3]], #[[1]]}&/@t[[1]]), "Opacities"->t[[4]], "RGB"->t[[5]], "Scales"->t[[2]], "Quaternions"->t[[3]]] ]
        },
            SplatMesh["SPZ", "Unpacked", new, generateMeta[new] ]
        ]
    ]
]

SplatMeshFilter[s: SplatMesh["SPZ", "Packed", __], o_] := SplatMeshPack[SplatMeshFilter[SplatMeshUnpack[s], o] ]

SplatMeshFilter[ SplatMesh["SPZ", "Unpacked", a_, _], function_] := Module[{
},
                                (* this sucks. JIT won't be involved *) (* RUB to normal one and back *)
    With[{indexes = MapThread[If[TrueQ[function[#1, With[{c = #2}, {c[[3]], -c[[1]], c[[2]]}], #3, #4, #5, #6 ] ], #1, Nothing]&, {Range[Length[a["Centers"] ] ], a["Centers"], a["Scales"], a["Quaternions"], a["Opacities"], a["RGB"]}]},
        With[{
            new = Join[a, Association[
                "Centers"->a["Centers"][[indexes]], 
                "Opacities"->a["Opacities"][[indexes]], 
                "RGB"->a["RGB"][[indexes]], 
                "Scales"->a["Scales"][[indexes]], 
                "Quaternions"->a["Quaternions"][[indexes]],
                "SH1" -> If[a["SH1"] === Null, Null, a["SH1"][[indexes]]],
                "SH2" -> If[a["SH2"] === Null, Null, a["SH3"][[indexes]]],
                "SH3" -> If[a["SH3"] === Null, Null, a["SH3"][[indexes]]],

                "NumSplats" -> Length[indexes]
            ] ]
        },
            SplatMesh["SPZ", "Unpacked", new, generateMeta[new] ]
        ]
    ]
]

(* resampling *)


SplatMeshResample[s_SplatMesh, n_Integer] := (Message[SplatMeshResample::toolow]; $Failed) /; n < 3000

SplatMeshResample[s: SplatMesh["SPZ", "Packed", a_, _], n_Integer] := SplatMeshPack[SplatMeshResample[SplatMeshUnpack[s], n] ]
SplatMeshResample[s: SplatMesh["SPZ", "Unpacked", a_, _], n_Integer] := Module[{data = a},

    data["Centers"] = ArrayResample[data["Centers"], {n, 3}];
    data["Scales"] = ArrayResample[data["Scales"], {n, 3}];
    data["Quaternions"] = ArrayResample[data["Quaternions"], {n, 4}];
    data["Opacities"] = ArrayResample[data["Opacities"], n];
    data["RGB"] = ArrayResample[data["RGB"], {n, 3}];
    data["SH1"] = If[data["SH1"] === Null, Null, ArrayResample[data["SH1"], {n, Length[data["SH1"][[1]]]}] ];
    data["SH2"] = If[data["SH2"] === Null, Null, ArrayResample[data["SH2"], {n, Length[data["SH2"][[1]]]}] ];
    data["SH3"] = If[data["SH3"] === Null, Null, ArrayResample[data["SH3"], {n, Length[data["SH3"][[1]]]}] ];
    data["NumSplats"] = n;

    SplatMesh["SPZ", "Unpacked", data, generateMeta[data] ]
]

(* Push new splats *)

SplatMesh /: Append[s_SplatMesh, data_] := appendSplats[s, data]

appendSplats::badsplats = "Provided data is not a splat or list of splats"

appendSplats[s: SplatMesh["SPZ", "Packed", __], props_] := SplatMeshPack[appendSplats[SplatMeshUnpack[s], props] ]
appendSplats[s: SplatMesh["SPZ", "Unpacked", a_, meta_], splat_] := appendSplats[s, {splat}] /; (Length[splat]===5 && Length[splat[[1]]]===3)
appendSplats[SplatMesh["SPZ", "Unpacked", a_, meta_], splats_] := With[{transposed = Check[Transpose[splats], $Failed]},
    If[FailureQ[transposed], Message[appendSplats::badsplats]; Return[$Failed] ];
    With[{newData = Check[Join[a, <|
        "Centers" -> Join[a["Centers"], transposed[[1]]], 
        "Scales" -> Join[a["Scales"], transposed[[2]]], 
        "Quaternions" -> Join[a["Quaternions"], transposed[[3]]], 
        "Opacities" -> Join[a["Opacities"], transposed[[4]]], 
        "RGB" -> Join[a["RGB"], transposed[[5]]],
        "NumSplats" -> a["NumSplats"] + Length[transposed[[1]]],
        "SH1" -> If[a["SH1"] === Null, Null, Join[a["SH1"], Table[0., {Length[transposed[[1]]]}, {Length[a["SH1"][[1]]]} ] ] ],      
        "SH2" -> If[a["SH2"] === Null, Null, Join[a["SH2"], Table[0., {Length[transposed[[1]]]}, {Length[a["SH2"][[1]]]} ] ] ],           
        "SH3" -> If[a["SH3"] === Null, Null, Join[a["SH3"], Table[0., {Length[transposed[[1]]]}, {Length[a["SH3"][[1]]]} ] ] ]         
    |>], $Failed]},
        (* Return[newData]; *)
        If[FailureQ[newData], Message[appendSplats::badsplats]; Return[$Failed] ];
        SplatMesh["SPZ", "Unpacked", newData, generateMeta[newData] ]
    ]
]


(* Mutation of properties *)

append[s: SplatMesh["SPZ", "Packed", __], props_List | props_Rule] := SplatMeshPack[append[SplatMeshUnpack[s], props] ]

append[SplatMesh["SPZ", "Unpacked", a_, meta_], r_Rule] := SplatMesh["SPZ", "Unpacked", Join[a, Association[r] ], meta]
append[SplatMesh["SPZ", "Unpacked", a_, meta_], {r__Rule}] := SplatMesh["SPZ", "Unpacked", Join[a, Association[r] ], meta]
append[_, _] := $Failed

SplatMesh[s_SplatMesh] := s

SplatMesh[s_SplatMesh, p_Rule] := append[s, p]
SplatMesh[s_SplatMesh, p:{__Rule}] := append[s, p]

(* Preview *)

generatePreview[meta_] := Graphics[{
    PointSize[0.003],ColorData[97][2], 
    GraphicsComplex[
        meta["Preview"]
    , 
        Point[ meta["Preview"]//Length//Range ]
    ]
}, ImageSize->50, "Controls"->False, ImagePadding->None];


SplatMesh /: MakeBoxes[obj : SplatMesh[format_, state_, r_, meta_], StandardForm] := 
    Module[{above, below},
        above = { 
          {BoxForm`SummaryItem[{"Container: ", Style[format, Bold]}]},
          {BoxForm`SummaryItem[{"Format: ", Style[state, Bold, If[state === "Unpacked", Red, Black] ]}]},
          {BoxForm`SummaryItem[{"Size: ", SetPrecision[Round[UnitConvert[Quantity[Switch[Head[r], ByteArray, Length[r], _, ByteCount[r] ], "Bytes"], "Conventional"],1], 1]}]},
          Sequence @@ Map[Function[k, {
            {BoxForm`SummaryItem[{k, meta[k]}]}
          }], Complement[Keys[meta], {"Preview"}] ]
        };

        BoxForm`ArrangeSummaryBox[
           SplatMesh, (* head *)
           obj,      (* interpretation *)
           generatePreview[meta],    (* icon, use None if not needed *)
           (* above and below must be in a format suitable for Grid or Column *)
           above,    (* always shown content *)
           Null (* expandable content. Currently not supported!*)
        ]
];

(* Properties READ-ONLY *)

getProperty[s: SplatMesh["SPZ", "Packed",   __], p: ("Centers" | "Opacities" | "RGB" | "Scales" | "Quaternions" | "SH1" | "SH2" | "SH3" | "Preview" | "PreviewGeometry")] := getProperty[SplatMeshUnpack[s], p]
getProperty[s: SplatMesh["SPZ", "Unpacked", a_Association, _], p: ("Centers" | "Opacities" | "RGB" | "Scales" | "Quaternions" | "SH1" | "SH2" | "SH3")] := a[p]
getProperty[s: SplatMesh["SPZ", _, _, meta_Association], p_String] := meta[p]
getProperty[s: SplatMesh["SPZ", _, _, meta_Association], "Properties"] := Join[Keys[meta], {"Container", "Format", "RawData", "Centers", "Opacities", "RGB", "Scales", "Quaternions", "SH1", "SH2", "SH3", "Preview", "PreviewGeometry"}]
getProperty[s: SplatMesh["SPZ", _, r_, meta_Association], "RawData"] := r
getProperty[s: SplatMesh["SPZ", _, _, meta_Association], "Container"] := "SPZ"
getProperty[s: SplatMesh["SPZ", f_, _, meta_Association], "Format"] := f
getProperty[s: SplatMesh["SPZ", f_, r_, meta_Association], "Size"] := UnitConvert[Quantity[Switch[Head[r], ByteArray, Length[r], _, ByteCount[r] ], "Bytes"], "Conventional"]

getProperty[s: SplatMesh["SPZ", "Unpacked", a_Association, meta_Association], "PreviewGeometry"] := Module[{
    p
},
    p = Transpose[{
        ArrayResample[s["Centers"], {3000, 3}], 
        Mean /@ ArrayResample[s["Scales"], {3000, 3}], 
        ArrayResample[s["Opacities"], 3000], 
        ArrayResample[s["RGB"], {3000, 3}]
    }];

    p = SortBy[p, Function[x, ColorConvert[RGBColor @@ (x[[4]]), "Hue"]//First  ] ];
    p = Transpose /@ Partition[p, 10];

    p = Map[Function[group, {
        {#[[3]], -#[[1]], #[[2]]} &/@ group[[1]], 
        Mean[group[[2]]], 
        Mean[group[[3]]], 
        group[[4,1]]
    }], p];

    p[[All, 2]] = Rescale[p[[All, 2]], {0,1}];

    With[{g = Table[{
            PointSize[ 0.01 ], Opacity[part[[3]]], RGBColor @@ (part[[4]]), 
            GraphicsComplex[part[[1]], Point[Range[Length[part[[1]] ] ] ] ]
        }, {part, p}]},
        
        g
    ]
]


getProperty[s: SplatMesh["SPZ", "Unpacked", a_Association, meta_Association], "Preview"] :=  Graphics3D[getProperty[s, "PreviewGeometry"] ]

SplatMesh[args__][property_String] := getProperty[SplatMesh[args], property]


(* Import/Export SPZ *)

projectionPlane = (*GB[*){{1/(*SqB[*)Sqrt[2](*]SqB*)(*|*),(*|*)-1/(*SqB[*)Sqrt[2](*]SqB*)(*|*),(*|*)0}(*||*),(*||*){1/(*SqB[*)Sqrt[6](*]SqB*)(*|*),(*|*)1/(*SqB[*)Sqrt[6](*]SqB*)(*|*),(*|*)-2/(*SqB[*)Sqrt[6](*]SqB*)}}(*||*)(*1:eJxTTMoPSmNkYGAo5gUSYZmp5S6pyflFiSX5RcFsQBHfxJKizAoAs04KOA==*)(*]GB*)//N;
generateMeta[spz_Association] := Join[KeySelect[spz, Function[m, MemberQ[{"Version","NumSplats","ShDegree","FractionalBits","Flags","AntiAliasFlagQ"}, m]]], <|"Preview"->((-projectionPlane.#) &/@ArrayResample[spz["Centers"], {300, 3}])|>]

ImportExport`RegisterImport[
 "SPZ",
  SPZExportFormat`Import
]

SPZExportFormat`Import[src_String, options___] :=
 Module[{data, strm, raw},
  raw = ReadByteArray[src];
  data = raw // ImportSPZ;
  SplatMesh["SPZ", "Packed", ExportByteArray[raw, {"GZIP", "Byte"}], generateMeta[data] ]
]

ImportExport`RegisterExport[
    "SPZ",
    exporter 
]

exporter[filename_, data_SplatMesh, opts___] :=
 Module[{strm, mesh = data},
  If[mesh[[1]] =!= "SPZ", Return[$Failed] ];
  If[mesh[[2]] === "Unpacked", mesh = SplatMeshPack[mesh] ];
  If[FailureQ[mesh], Return[$Failed] ];

  With[{out = mesh[[3]]},
    If[FailureQ[out], Return[$Failed] ];

    strm = OpenWrite[filename, BinaryFormat->True];
    BinaryWrite[strm, out];
    Close[strm]
  ]
]

(* RegisterFormat["SPZ", <|"Extensions" -> "spz"|>]; *) (* does not work in WL14  )*)

End[]
EndPackage[]