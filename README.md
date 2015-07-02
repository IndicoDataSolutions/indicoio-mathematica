# indicoio-mathematica
**Description:** Mathematica package for accessing predictive APIs from https://indico.io 

**Version:** 0.6

**Author:** Kyle Keane <physics@kylekeane.com>

**License:** file LICENSE

## Details
	indico["name", data]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Classifies text or images using API `"name"`. 

	indico["name", data, "Probabilities"]

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Probabilities of classification using API `"name"`.

###Inputs
Evaluate `indico[]` to see a list of all possible values for `"name"`. 

Possible values of `"name"` include:

* `"Sentiment"` - classifies sentiment of text
* `"Political"` - classifies political leaning of text
* `"TextTags"` - classifies topics discussed in text
* `"Language"` - classifies the language of text
* `"FacialEmotionRecognition"` - classifies the emotion expressed in an image of a person's face
* `"ImageFeatures"` - returns a feature vector of an image

Possible input types for data using text analysis functions:

* String:`"input string"`
* List of Strings:`{"first input string", "second input string"}`

Possible input types for `data` using image analysis functions:

* Image:`Image[data]` or `Image[Graphics[]]`
* List of Images:`{Image[data], Image[data]}`

###Text Analysis Examples

	In[1] := text1 = "Hawaii is the best vacation destination!";
	In[2] := text2 = "That restaurant had terrible service and disgusting food!";

	In[3] := indico["Sentiment", text1]
	Out[3] := "Positive"

	In[4] := indico["Sentiment", {text1, text2}]
	Out[4] := {"Positive", "Negative"}

	In[5] := indico["Sentiment", text1, "Probabilities"]
	Out[5] := <|"Positive" -> 0.96, "Negative" -> 0.04|>

	In[6] := indico["Sentiment", {text1, text2}, "Probabilities"]
	Out[4] := {<|"Positive" -> 0.96, "Negative" -> 0.04|>, <|"Positive" -> 0.04, "Negative" -> 0.96|>}

###Image Analysis Examples:
	In[1] := image = ExampleData[{"TestImage", "Tiffany"}];

	In[2] := indico["FacialEmotionRecognition", image]
	Out[2] := "Happy"

	In[3] := indico["FacialEmotionRecognition", {image, image}]
	Out[3] := {"Happy", "Happy"}

	In[4] := indico["FacialEmotionRecognition", image, "Probabilities"]
	Out[4] := <|"Sad" -> 0.267075220332238`, "Angry" -> 0.11075824301423644`, "Surprise" -> 0.005966154897323556`, "Fear" -> 0.11908871969391857`, "Neutral" -> 0.1073345177595749`, "Happy" -> 0.38977714430270854`|>

	In[5] := indico["FacialEmotionRecognition", {image, image}, "Probabilities"]
	Out[5] := {<|"Sad" -> 0.267075220332238`, "Angry" -> 0.11075824301423644`, "Surprise" -> 0.005966154897323556`, "Fear" -> 0.11908871969391857`, "Neutral" -> 0.1073345177595749`, "Happy" -> 0.38977714430270854`|>, <|"Sad" -> 0.267075220332238`, "Angry" -> 0.11075824301423644`, "Surprise" -> 0.005966154897323556`, "Fear" -> 0.11908871969391857`, "Neutral" -> 0.1073345177595749`, "Happy" -> 0.38977714430270854`|>}

	In[6] := indico["ImageFeatures", image]
	Out[6] := {0.710356, 0., 0. ,0. , 0.383978, <---2038 omitted--->, 0.0758799, 0., 0., 0., 2.16368}