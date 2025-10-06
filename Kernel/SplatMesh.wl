BeginPackage["CoffeeLiqueur`Workshop`SplatMesh`", {
    "CoffeeLiqueur`Workshop`SplatMesh`Parser`"
}]

SplatMesh::usage = "Representation of packed or unpacked gaussian splatting mesh.\r\nUse Import expression to load .spz files into SplatMesh"
SplatMeshPack::usage = "SplatMeshPack[s_SplatMesh] returns an packed SplatMesh"
SplatMeshUnpack::usage = "SplatMeshUnpack[s_SplatMesh] returns an unpacked SplatMesh ready for post-processing"
SplatMeshPackedQ::usage = "SplatMeshPackedQ[s_SplatMesh] returns True if SplatMesh is packed"

TransformedSplat::usage = "TransformedSplat[s_SplatMesh, func_] applies func on s\r\nTransformedSplat[s_SplatMesh, matrix_] applies 3x3 matrix to centers of splat\r\nTransformedSplat[s_SplatMesh, {m,v}] applies 3x3 m matrix and translates by vector v"

Begin["`Private`"]

Unprotect[SplatMesh]
Unprotect[getProperty]
ClearAll[getProperty]
ClearAll[SplatMesh]

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
    data = AssociateTo[a, "Centers" -> Map[Function[p, nmatrix.p], a["Centers"] ] ]
},
    SplatMesh["SPZ", "Unpacked", data, generateMeta[data] ]
]

TransformedSplat[   SplatMesh["SPZ", "Unpacked", a_, _], {matrix_?MatrixQ, v_?VectorQ}] := With[{
    nmatrix = N[matrix],
    nvector = N[nvector]
}, {
    data = AssociateTo[a, "Centers" -> Map[Function[p, nmatrix.p + nvector], a["Centers"] ] ]
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

    With[{t = Transpose[MapThread[function, {Range[Length[a["Centers"] ] ], a["Centers"], a["Scales"], a["Quaternions"], a["Opacities"], a["RGB"]}] ]},
        With[{
            new = Join[a, Association["Centers"->t[[1]], "Opacities"->t[[4]], "RGB"->t[[5]], "Scales"->t[[2]], "Quaternions"->t[[3]]] ]
        },
            SplatMesh["SPZ", "Unpacked", new, generateMeta[new] ]
        ]
    ]
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

getProperty[s: SplatMesh["SPZ", "Packed",   __], p: ("Centers" | "Opacities" | "RGB" | "Scales" | "Quaternions" | "SH1" | "SH2" | "SH3")] := getProperty[SplatMeshUnpack[s], p]
getProperty[s: SplatMesh["SPZ", "Unpacked", a_Association, _], p: ("Centers" | "Opacities" | "RGB" | "Scales" | "Quaternions" | "SH1" | "SH2" | "SH3")] := a[p]
getProperty[s: SplatMesh["SPZ", _, _, meta_Association], p_String] := meta[p]
getProperty[s: SplatMesh["SPZ", _, _, meta_Association], "Properties"] := Join[Keys[meta], {"Container", "Format", "RawData", "Centers", "Opacities", "RGB", "Scales", "Quaternions", "SH1", "SH2", "SH3"}]
getProperty[s: SplatMesh["SPZ", _, r_, meta_Association], "RawData"] := r
getProperty[s: SplatMesh["SPZ", _, _, meta_Association], "Container"] := "SPZ"
getProperty[s: SplatMesh["SPZ", f_, _, meta_Association], "Format"] := f
getProperty[s: SplatMesh["SPZ", f_, r_, meta_Association], "Size"] := UnitConvert[Quantity[Switch[Head[r], ByteArray, Length[r], _, ByteCount[r] ], "Bytes"], "Conventional"]

SplatMesh[args__][property_String] := getProperty[SplatMesh[args], property]

Protect[SplatMesh]
Protect[getProperty]

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