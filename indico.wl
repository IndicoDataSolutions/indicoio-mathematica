(* ::Package:: *)

(* ::Section:: *)
(*Package Setup*)


(* ::Subsection::Closed:: *)
(*begin package*)


(* begin package context *)
BeginPackage["indico`"];


(* define any Needs *)


(* ::Subsection:: *)
(*messages*)


(* ::Subsubsection::Closed:: *)
(*apiKey*)


apiKey::usage = "Variable to specify the indico key";
apiKey::keyNotSpecified = "Please either set the apiKey variable or include the \"apiKey\" option to your function call";
apiKey::keyIsNotString = "Your indico apiKey must be a string";
apiKey::keyLengthIncorrect = "Your indico apiKey must be 32 characters long";


(* ::Subsubsection::Closed:: *)
(*Text Analysis*)


indico::usage = "\!\(\*
StyleBox[\"Provides\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"native\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"Mathematica\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"access\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"to\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"the\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"indico\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\".\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"io\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"predictive\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"APIs\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\".\",\nFontFamily->\"Consolas\",\nFontWeight->\"Bold\"]\)

indico[\"name\", data]
\tClassifies text or images using API \"name\".
indico[\"name\", data, \"Probabilities\"]
\tProbabilities of classification using API \"name\".

Evaluate indico[] to see a list of all possible values for \"name\".
Possible values of \"name\" include:
\t\"Sentiment\" - classifies sentiment of text,
\t\"Political\" - classifies political leaning of text,
\t\"TextTags\" - classifies topics discussed in text,
\t\"Language\", - classifies the language of text,
\t\"FacialEmotionRecognition\", - classifies the emotion expressed in an image of a person's face,
\t\"ImageFeatures\", - returns a feature vector of an image

Possible input types for data using text analysis functions:
\tString - \"input string\"
\tList of Strings - {\"first input string\", \"second input string\"}

Possible input types for data using image analysis functions:
\tImage - Image[data] or Image[Graphics[]]
\tList of Images - {Image[data], Image[data]}

Text Analysis Examples:
   In[1] := text1 = \"Hawaii is the best vacation destination!\";

   In[2] := text2 = \"That restauran had terrible service and disgusting food!\";

   In[3] := indico[\"Sentiment\", text1]
Out[3] := \"Positive\"

   In[4] := indico[\"Sentiment\", {text1, text2}]
Out[4] := {\"Positive\", \"Negative\"}

   In[5] := indico[\"Sentiment\", text1, \"Probabilities\"]
Out[5] := <|\"Positive\" -> 0.96, \"Negative\" -> 0.04|>

   In[6] := indico[\"Sentiment\", {text1, text2}, \"Probabilities\"]
Out[4] := {<|\"Positive\" -> 0.96, \"Negative\" -> 0.04|>, <|\"Positive\" -> 0.04, \"Negative\" -> 0.96|>}

Image Analysis Examples:
   In[1] := image = ExampleData[{\"TestImage\", \"Tiffany\"}];

   In[2] := indico[\"FacialEmotionRecognition\", image]
Out[2] := \"Happy\"

   In[3] := indico[\"FacialEmotionRecognition\", {image, image}]
Out[3] := {\"Happy\", \"Happy\"}

   In[4] := indico[\"FacialEmotionRecognition\", image, \"Probabilities\"]
Out[4] := \[LeftAssociation]\"Sad\" \[Rule] 0.267075220332238`, \"Angry\" \[Rule] 0.11075824301423644`, \"Surprise\" \[Rule] 0.005966154897323556`, \"Fear\" \[Rule] 0.11908871969391857`, \"Neutral\" \[Rule] 0.1073345177595749`, \"Happy\" \[Rule] 0.38977714430270854`\[RightAssociation]

   In[5] := indico[\"FacialEmotionRecognition\", {image, image}, \"Probabilities\"]
Out[5] := {\[LeftAssociation]\"Sad\" \[Rule] 0.267075220332238`, \"Angry\" \[Rule] 0.11075824301423644`, \"Surprise\" \[Rule] 0.005966154897323556`, \"Fear\" \[Rule] 0.11908871969391857`, \"Neutral\" \[Rule] 0.1073345177595749`, \"Happy\" \[Rule] 0.38977714430270854`\[RightAssociation], \[LeftAssociation]\"Sad\" \[Rule] 0.267075220332238`, \"Angry\" \[Rule] 0.11075824301423644`, \"Surprise\" \[Rule] 0.005966154897323556`, \"Fear\" \[Rule] 0.11908871969391857`, \"Neutral\" \[Rule] 0.1073345177595749`, \"Happy\" \[Rule] 0.38977714430270854`\[RightAssociation]}

   In[6] := indico[\"ImageFeatures\", image]
Out[6] := {0.710356, 0., 0. ,0. , 0.383978, <---2038 omitted--->, 0.0758799, 0., 0., 0., 2.16368}";
indico::apiErrorMessage = "`1`";
sentiment::usage = "sentiment";
textTags::usage = "text tags";
political::usage = "political";
language::usage = "language";


(* ::Subsubsection::Closed:: *)
(*Image Analysis*)


imageFeatures::usage = "image features";
fer::usage = "facial emotion recognition";


(* ::Subsection::Closed:: *)
(*initial values*)


If[
	Head @ apiKey =!= String,
	apiKey = None
];


(* ::Subsection::Closed:: *)
(*begin private*)


(* begin Private context *)
Begin["`Private`"];


(* ::Section:: *)
(*General*)


(* ::Subsection::Closed:: *)
(*languageLookupTable*)


languageLookupTable = {
	"Tagalog" -> Entity["Language","Tagalog"],
	"Portuguese" -> Entity["Language","Portuguese"],
	"Esperanto" -> Entity["Language","Esperanto"],
	"Thai" -> Entity["Language","Thai"],
	"Swedish" -> Entity["Language","Swedish"],
	"Lithuanian" -> Entity["Language","Lithuanian"],
	"French" -> Entity["Language","French"],
	"Russian" -> Entity["Language","Russian"],
	"Czech" -> Entity["Language","Czech"],
	"Norwegian" -> Entity["Language","Norwegian"],
	"Bulgarian" -> Entity["Language","Bulgarian"],
	"German" -> Entity["Language","German"],
	"Slovak" -> Entity["Language","Slovak"],
	"Arabic" -> Entity["Language","Arabic"],
	"Chinese" -> Entity["Language","ChineseMandarin"],
	"Romanian" -> Entity["Language","Romanian"],
	"Dutch" -> Entity["Language","Dutch"],
	"English" -> Entity["Language","English"],
	"Polish" -> Entity["Language","Polish"],
	"Korean" -> Entity["Language","Korean"],
	"Finnish" -> Entity["Language","Finnish"],
	"Indonesian" -> Entity["Language","Indonesian"],
	"Danish" -> Entity["Language","Danish"],
	"Japanese" -> Entity["Language","Japanese"],
	"Turkish" -> Entity["Language","Turkish"],
	"Hebrew" -> Entity["Language","Hebrew"],
	"Hungarian" -> Entity["Language","Hungarian"],
	"Vietnamese" -> Entity["Language","Vietnamese"],
	"Italian" -> Entity["Language","Italian"],
	"Persian (Farsi)" -> Entity["Language","FarsiEastern"],
	"Greek" -> Entity["Language","Greek"],
	"Spanish" -> Entity["Language","Spanish"],
	"Latin" -> Entity["Language","Latin"]
}


(* ::Subsection::Closed:: *)
(*validKeyQ*)


validKeyQ[key_] := 
Module[
	{},
	Which[
		key === None, 
		Message[apiKey::keyNotSpecified];
		False,
		Head @ key =!= String,
		Message[apiKey::keyIsNotString];
		False,
		StringLength @ key =!= 32,
		Message[apiKey::keyLengthIncorrect];
		False,
		True,
		True
	]
]


(* ::Subsection::Closed:: *)
(*sendToIndico*)


sendToIndico[api_, data_] := 
URLFetch[
	api, 
	"Body" -> ExportString[{"data" -> data}, "JSON", "Compact" -> True], 
	"Method" -> "POST"
]


(* ::Subsection::Closed:: *)
(*batchOrNot*)


batchOrNot[input_, apiName_, type_] := 
If[
	Head @ input === type,
	batchQ = False;
	apiName,
	batchQ = True;
	apiName <> "/batch"
]


(* ::Subsection:: *)
(*formatResult*)


(* ::Subsubsection:: *)
(*value*)


(* ::Text:: *)
(*This is for base results of the form "value"*)


formatResult["Sentiment" | "SentimentHQ", results: List[_Rule], output_] :=
Module[
	{finalResult, preResult},
	If[
		results[[1,1]] === "results",
		Which[
			batchQ && output === "Probabilities",
			preResult = results[[1, 2]];
			<|"Positive" -> #, "Negative" -> 1 - #|> & /@ preResult,
			!batchQ && output === "Probabilities",
			preResult = results[[1, 2]];
			<|"Positive" -> preResult, "Negative" -> 1 - preResult|>,
			batchQ,
			preResult = results[[1, 2]];
			If[TrueQ[# > .5], "Positive", "Negative"]& /@ preResult,
			True,
			preResult = results[[1, 2]];
			If[TrueQ[preResult > .5], "Positive", "Negative"]
		],
		Message[indico::apiErrorMessage, results[[1,2]]]
	]
]


(* ::Subsubsection::Closed:: *)
(*{key -> value, key -> value}*)
(*with Entity lookup*)


(* ::Text:: *)
(*This is for base results of the form { "key" -> value } and includes the normalization to Entity["Language"]*)


formatResult["Language", results: List[_Rule], output_] :=
Module[
	{finalResult, preResult},
	If[
		results[[1,1]] === "results",
		Which[
			batchQ && output === "Probabilities",
			preResult = Association @@@ results[[1, 2]];
			Map[KeyMap[ReplaceAll[#, languageLookupTable]&, #]&, preResult],
			!batchQ && output === "Probabilities",
			preResult = Association @@ results[[1, 2]];
			KeyMap[ReplaceAll[#, languageLookupTable]&, preResult],
			batchQ,
			preResult = Association @@@ results[[1, 2]];
			preResult = Map[First @ Keys @ Reverse @ Sort @ # &, preResult];
			preResult /. languageLookupTable,
			True,
			preResult = Association @@ results[[1, 2]];
			preResult = First @ Keys @ Reverse @ Sort @ preResult;
			preResult /. languageLookupTable
		],
		Message[indico::apiErrorMessage, results[[1,2]]]
	]
]


(* ::Subsubsection::Closed:: *)
(*{key -> value, key -> value}*)


(* ::Text:: *)
(*This is for base results of the form { "key" -> value }*)


formatResult["Political" | "TextTags" | "FacialEmotionRecognition", results: List[_Rule], output_] :=
Module[
	{finalResult, preResult},
	If[
		results[[1,1]] === "results",
		Which[
			batchQ && output === "Probabilities",
			preResult = Association @@@ results[[1, 2]];
			preResult,
			!batchQ && output === "Probabilities",
			preResult = Association @@ results[[1, 2]];
			preResult,
			batchQ,
			preResult = Association @@@ results[[1, 2]];
			preResult = Map[First @ Keys @ Reverse @ Sort @ # &, preResult];
			preResult,
			True,
			preResult = Association @@ results[[1, 2]];
			preResult = First @ Keys @ Reverse @ Sort @ preResult;
			preResult
		],
		Message[indico::apiErrorMessage, results[[1,2]]]
	]
]


(* ::Subsubsection::Closed:: *)
(*{value, value}*)


(* ::Text:: *)
(*This is for base results of the form {value1, value2}*)


formatResult["ImageFeatures", results: List[_Rule], "TopResult"] :=
Module[
	{finalResult, preResult},
	If[
		results[[1,1]] === "results",
		Which[
			batchQ,
			preResult = results[[1, 2]];
			preResult,
			True,
			preResult = results[[1, 2]];
			preResult
		],
		Message[indico::apiErrorMessage, results[[1,2]]]
	]
]


(* ::Section:: *)
(*API functions*)


(* ::Subsection::Closed:: *)
(*Default Options*)


Options[indico] = {"apiKey" :> apiKey};


indico[] := {
	"Sentiment",
	"Language",
	"Political",
	"TextTags",
	"FacialEmotionRecognition",
	"ImageFeatures"
}


(* ::Subsection::Closed:: *)
(*Text Analysis*)


indico[apiName: "Language" | "Sentiment" | "SentimentHQ" | "TextTags" | "Political", input: _String | List[__String], output: "Probabilities" | "TopResult" : "TopResult", OptionsPattern[]] := 
Module[
	{apiURL, key, api, results},
	apiURL = 
		"http://apiv2.indico.io/"
		<>
		batchOrNot[input, ToLowerCase @ apiName, String]
		<> 
		"?key=";
	key = OptionValue["apiKey"];
	If[!validKeyQ[key], Return[]];
	api = apiURL <> key;
	results = sendToIndico[api, input];
	results = ImportString[results, "JSON"];
	formatResult[apiName, results, output]
]


sentiment[input: _String | List[__String], output: "Probabilities" | "TopResult" : "TopResult", opts:OptionsPattern[]] := 
indico[
	"Sentiment", 
	input, 
	output,
	opts
]


sentimentHQ[input: _String | List[__String], output: "Probabilities" | "TopResult" : "TopResult", opts:OptionsPattern[]] := 
indico[
	"SentimentHQ", 
	input, 
	output,
	opts
]


language[input: _String | List[__String], output: "Probabilities" | "TopResult" : "TopResult", opts:OptionsPattern[indico]] := 
indico[
	"Language", 
	input, 
	output,
	opts
]


textTags[input: _String | List[__String], output: "Probabilities" | "TopResult" : "TopResult", opts:OptionsPattern[]] := 
indico[
	"TextTags", 
	input, 
	output,
	opts
]


political[input: _String | List[__String], output: "Probabilities" | "TopResult" : "TopResult", opts:OptionsPattern[indico]] := 
indico[
	"Political", 
	input, 
	output,
	opts
]


(* ::Subsection::Closed:: *)
(*Image Analysis*)


indico[apiName: "FacialEmotionRecognition" | "ImageFeatures", input: _Image | List[__Image], output: "Probabilities" | "TopResult" : "TopResult", OptionsPattern[]] := 
Module[
	{apiURL, key, api, data, results},
	apiURL = "http://apiv2.indico.io/"
	<>
	batchOrNot[input, ToLowerCase[apiName /. {"FacialEmotionRecognition" -> "fer"}], Image]
	<> 
	"?key=";
	key = OptionValue["apiKey"];
	If[!validKeyQ[key], Return[]];
	api = apiURL <> key;
	SetDirectory[$TemporaryDirectory];
	If[
		Head @ input === Image,
		Export["temp.jpg.b64", input];
		data = Import["temp.jpg.b64", "Text"];
		DeleteFile["temp.jpg.b64"];,
		MapIndexed[Export["temp"<> ToString @ First @ #2 <>".jpg.b64", #1]&, input];
		data = MapIndexed[Import["temp"<> ToString @ First @ #2 <>".jpg.b64", "Text"]&, input];
		DeleteFile[MapIndexed["temp"<> ToString @ First @ #2 <>".jpg.b64"&, input]];
	];
	ResetDirectory[];
	results = sendToIndico[api, data];
	results = ImportString[results, "JSON"];
	formatResult[apiName, results, output]
]


imageFeatures[input: _Image | List[__Image], opts:OptionsPattern[indico]] := 
indico[
	"ImageFeatures", 
	input, 
	"TopResult",
	opts
]


fer[input: _Image | List[__Image], output: "Probabilities" | "TopResult" : "TopResult", opts:OptionsPattern[indico]] := 
indico[
	"FacialEmotionRecognition", 
	input, 
	output,
	opts
]


(* ::Section::Closed:: *)
(*Package Termination*)


End[];
EndPackage[];
