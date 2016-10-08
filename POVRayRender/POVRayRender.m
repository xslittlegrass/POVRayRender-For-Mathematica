(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: POVRayRender *)
(* :Context: POVRayRender` *)
(* :Author: xslittlegrass *)
(* :Date: 2016-08-29 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 xslittlegrass *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["POVRayRender`"];
(* Exported symbols added here with SymbolName::usage *)

POVRayRender::usage = "POVRayRender[3Dgraphics] render a 3D graphics using POVRay.";
ConfigurePOVRayRender::usage = "ConfigurePOVRayRender[\"POVRayPath\"->\"path\"] configures the POVRay path.";
ToPOVRayScene::usage = "ToPOVRayScene[3Dgraphics] generates POVRay scence file content in string.";

Begin["`Private`"];


(* Load and check configuration *)

$applicationDataDirectory = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "POVRayRender"}];
If[Not@DirectoryQ[$applicationDataDirectory], CreateDirectory[$applicationDataDirectory]];

$configFile = FileNameJoin[{$applicationDataDirectory, "config.m"}];

(* Load configuration, if it exists *)
If[FileExistsQ[$configFile], $POVRayPath = Import[$configFile, "Package"], $POVRayPath=""];
If[$POVRayPath==""||!FileExistsQ[$POVRayPath],

  Print["POVRay Path not configured. Please use ConfigurePOVRayRender[\"POVRayPath\"->\"path to POVRay executable\"] to configure the path."];
];


Options[ConfigurePOVRayRender]={"POVRayPath"->$POVRayPath};
ConfigurePOVRayRender[OptionsPattern[]]:=Module[{povrayPath},
  povrayPath=OptionValue["POVRayPath"];
  If[povrayPath==""||!FileExistsQ[povrayPath],Print["path not correct"];Abort[]];

  Export[$configFile, povrayPath, "Package"];
  $POVRayPath = povrayPath;
  povrayPath

];




globalStr="

global_settings {
  max_trace_level 5
  assumed_gamma 1.0
  radiosity {
    pretrace_start 0.08
    pretrace_end   0.01
    count 35
    nearest_count 5
    error_bound 1.8
    recursion_limit 2
    low_error_factor .5
    gray_threshold 0.0
    minimum_reuse 0.015
    brightness 1
    adc_bailout 0.01/2
  }
}

#default {
  texture {
    pigment {rgb 1}
    finish {
      ambient 0.0
      diffuse 0.6
      specular 0.6 roughness 0.001
      reflection { 0.0 1.0 fresnel on }
      conserve_energy
    }
  }
}


light_source { <50,-50,50> 1 }
background { rgb <0,.25,.5> }
";

generatePovrayString[p_Graphics3D]:=Module[{povStr,replaceStr1,replaceStr2,replaceStr3},

  replaceStr1="finish {ambient color rgb 1}";(*remove the default light*)
  replaceStr2="finish {ambient rgb 1}";(* remove additional light*)
  replaceStr3="pigment {color rgb <0., 0., 0.>}";(*remove the default color*)

  If[ByteCount[p]<200||p[[1]]==={},(*if the graphics is empty*)
    povStr=globalStr;,
    povStr=StringJoin[globalStr,StringReplace[StringDrop[#,Last@Flatten@StringPosition[#,"/*ViewPoint*/"]]&@ExportString[p,"pov"],{replaceStr1->"",replaceStr2->"",replaceStr3->""}]]];
  povStr
];
(* export povray file to string from Graphics3D object *)


(* graphicsComplexToMesh2 convert a GraphicsComplex object into mesh2 object in povray.
Right now only support one object in GraphicsComplex.*)

graphicsComplexToMesh2[gc_]:=Module[{scform,NoVertex,vertex,vertexNormals,NoFaces,faceIndex,vertexStr,vertexNormalsStr,faceIndexStr,vertexColors,vertexColorsStr},
  scform[x_]:=ScientificForm[x,8,NumberFormat->(Row[If[#3=="",{#1},{#1,"e",#3}]]&)];
  vertex=gc[[1]];
  vertexNormals=First@Cases[gc,Rule[VertexNormals,x__]:>x,\[Infinity]];
  faceIndex=First@Cases[gc,GraphicsGroup[{Polygon[x__],___}]|GraphicsGroup[Annotation[{Polygon[x__], ___}, ___]]:>x,\[Infinity]]-1;
  (*The pattern with Annotation is for the bug introduced in Mathematica version 11.01*)
  NoVertex=ToString@Length[vertex];
  NoFaces=ToString@Length[faceIndex];
  vertexStr=StringRiffle[Apply["<" <> ToString[#1] <> "," <> ToString[#2] <> "," <> ToString[#3] <> ">" &, Map[ToString[scform[#]] &, vertex, {-1}], {1}], ",\n"];
  vertexNormalsStr=StringRiffle[Apply["<" <> ToString[#1] <> "," <> ToString[#2] <> "," <> ToString[#3] <> ">" &,Map[ToString[scform[#]] &, vertexNormals, {-1}],{1}],",\n"];

  If[
    Cases[gc,Rule[VertexColors,x_]:>x,\[Infinity]]=={},

  (* no color in the original plot*)
    faceIndexStr=StringRiffle[Apply[StringTemplate["<`1`,`2`,`3`>"],faceIndex,{1}],",\n"];
    StringTemplate["
	mesh2{
		vertex_vectors{
			``,
			``
		}
		normal_vectors{
			``,
			``
		}
		face_indices{
			``,
			``
		}
	}
"][NoVertex,vertexStr,NoVertex,vertexNormalsStr,NoFaces,faceIndexStr],

  (* color presented in the original plot*)

    faceIndexStr=StringRiffle[Apply["<" <> ToString[#1] <> "," <> ToString[#2] <> "," <> ToString[#3] <> ">" <> "," <> ToString[#1] <> "," <> ToString[#2] <> "," <> ToString[#3] &, faceIndex, {1}], ",\n"];
    (* this is the same as StringRiffle[Apply[StringTemplate["<`1`,`2`,`3`>,`1`,`2`,`3`"], faceIndex, {1}], ",\n"], but StringTemplate has a huge performance issue.*)
    vertexColors=First@Cases[Replace[gc,Line[x__]:>{},\[Infinity]],Rule[VertexColors,x__]:>x,\[Infinity]];
    If[!VectorQ[vertexColors,NumberQ],vertexColors=ColorConvert[vertexColors,"RGB"]/.RGBColor->List];
    (*sometimes the color is stored as VertexColors\[Rule]{Hue[...],Hue[...],...}, this tries to convert to RGB numbers.*)
    vertexColorsStr=StringRiffle[Apply["texture{pigment{rgb <" <> ToString[#1] <> "," <> ToString[#2] <> "," <> ToString[#3] <> ">}}" &, Map[ToString[scform[#]] &, vertexColors, {-1}], {1}], "\n"];

    StringTemplate["
	mesh2{
		vertex_vectors{
			``,
			``
		}
		normal_vectors{
			``,
			``
		}
		texture_list{
			``,
			``
		}
		face_indices{
			``,
			``
		}
	}
"][NoVertex,vertexStr,NoVertex,vertexNormalsStr,NoVertex,vertexColorsStr,NoFaces,faceIndexStr]
  ]
];


generatePovrayStringUsingMesh[p_Graphics3D]:=Module[{gc,camera},
  gc=First@Cases[p,_GraphicsComplex];
  camera=First@StringCases[ExportString[p/.GraphicsComplex[___]:>GraphicsComplex[{{0,0,0}},{Sphere[1]}],"POV"],"camera {"~~Shortest[x__]~~"}"];
  (*This gets only the camera information. We dump the content of the 3d Graphics so that the export can be much faster.*)
  povStr=StringJoin[globalStr,camera,graphicsComplexToMesh2[gc]];
  povStr
];

povrayExec[inputPath_,outputPath_,povpath_,povrayOptions_]:=Module[{renderCmd},
  renderCmd=povpath<>" "<>povrayOptions<>""<>outputPath<>" "<>inputPath;
  Run[renderCmd]
];
(* call povray exectuable to render the graphics *)



(* put together the subroutines *)

Options[POVRayRender]={"Method"->"Mesh","OutputPath"->None,"ImageSize"->{800,600},"RenderOptions"->"+A0.001 -J"};

Block[{$POVRayPath},
POVRayRender[p_Graphics3D,povpath_String:$POVRayPath,OptionsPattern[]]:=Module[{imageWidth,imageHeight,tracingOptions,inputPath,outputPath,povrayOptions,tempFileStream,povraySceneStr,temp},

(*-----path for povray executables and options -------------*)
  {imageWidth,imageHeight}=OptionValue["ImageSize"];
  tracingOptions=OptionValue["RenderOptions"];
  povrayOptions=StringTemplate["+W`1` +H`2` `3` +O"][imageWidth,imageHeight,tracingOptions];

(*	temp=AbsoluteTime[];*)

  povraySceneStr=If[OptionValue["Method"]=="Mesh",
    generatePovrayStringUsingMesh[p],
    generatePovrayString[p]];

(*	Print["parse Graphics3D"];
	Print[AbsoluteTime[]-temp];
	temp=AbsoluteTime[];*)

  (*----generate temporary pov file from Grpahics3D object----*)
  (* this is intent to generate uniqu filename each time *)
  inputPath=$TemporaryDirectory<>"/"<>DateString[{ToString@$KernelID,"Year","-","MonthNameShort","Day","-","Hour24","Minute","SecondExact"}]<>StringJoin@@ToString/@RandomInteger[{1,1000},{10}]<>".pov";
  tempFileStream=OpenWrite[inputPath];
  WriteString[inputPath,povraySceneStr];
  Close[tempFileStream];

  outputPath=$TemporaryDirectory<>"/"<>DateString[{ToString@$KernelID,"Year","-","MonthNameShort","Day","-","Hour24","Minute","SecondExact"}]<>StringJoin@@ToString/@RandomInteger[{1,1000},{10}]<>".png";
  (*------call povray to render and output rendered file------*)

(*	Print["writting to disk"];
	Print[AbsoluteTime[]-temp];
	temp=AbsoluteTime[];*)


  povrayExec[inputPath,outputPath,povpath,povrayOptions];

(*	Print["POVRay render"];
	Print[AbsoluteTime[]-temp];
	temp=AbsoluteTime[];*)

  If[OptionValue["OutputPath"]===None,
    Import[outputPath],
    CopyFile[outputPath,OptionValue["OutputPath"]];
    OptionValue["OutputPath"]
  ]
]];


Options[ToPOVRayScene]={"Method"->"Mesh"};

ToPOVRayScene[p_Graphics3D,OptionsPattern[]]:=Module[{},
  If[OptionValue["Method"]=="Mesh",
    generatePovrayStringUsingMesh[p],
    generatePovrayString[p]]

];











End[]; (* `Private` *)

EndPackage[]
