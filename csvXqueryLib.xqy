(:
	TODO: Add template support
	TODO: Add special char support for code points/joinCodes
:)

xquery version "1.0-ml";

(:
 : Lib Name:    csvXqueryLib
 : Date:        04/25/2019
 : Author:      Gary Hott
 : Does:        CSV things
 : Inspired By: (Dave Cassell)  - https://github.com/dmcassel/blog-code/blob/master/src/app/models/csv-lib.xqy
 :              (Matthew Royal) - https://github.com/masyukun/correct-csv-parser-xquery
 :)

module namespace csvXqLib = "http://marklogic.com/ghott/xquery/lib/csvXqueryLib";

import module namespace functx = "http://www.functx.com"          at "/MarkLogic/functx/functx-1.0-nodoc-2007-01.xqy";
import module namespace mem    = "http://xqdev.com/in-mem-update" at "/MarkLogic/appservices/utils/in-mem-update.xqy";
import module namespace sem    = "http://marklogic.com/semantics" at "/MarkLogic/semantics.xqy";

declare namespace json = "http://marklogic.com/xdmp/json";

(: use json:object() here vice map:map() to maintain key order :)
declare private variable $headersMap          := json:object();
declare private variable $createdDocsMap      := json:object();
declare private variable $removeNonAsciiChars := 
	"var inText; inText.toString().replace(/[^\x00-\x7F]/g, '');";


(:~
 Takes an XML structure and parses it to gather all of the element names 
 and attribute names witin, then creates and stores a CSV file with those 
 element/attribute names as the CSV headers (first row).
 
 @param $xmlStruct   - xml document structure
 @param $fileName    - (optional) file name to include in the document uri
 @param $includePath - (optional) whether or not to include the full path (dot notation)
                       for each element within the csv header names
 @return             - doc uri for the created/stored blank (headers only) csv
 :)
declare function csvXqLib:createCsvFromXml(
	$xmlStruct   as document-node(),
	$fileName    as xs:string?,
	$includePath as xs:boolean?
) as xs:string {
	(: set the fileName, uri, collections, and permissions for the csv doc :)
	let $fileName :=
		if (fn:empty($fileName) or fn:normalize-space($fileName) eq "") then
			fn:concat("csvFile_", sem:uuid-string(), ".csv")
		else
			if (fn:contains($fileName, ".csv")) then $fileName
			else fn:concat($fileName, ".csv")
	let $uri       := fn:concat("/blank/csvFromXml/", $fileName)
	let $colls     := (("csvFromXml", "csv", "blank"), xdmp:default-collections())
	let $perms     := xdmp:default-permissions()

	let $includePath := if (fn:empty($includePath)) then fn:false() else $includePath
	let $headerRow   := ""

	(: populate the private global $headersMap variable with the 
	 : element/attributes from the $xmlStruct :)
	let $_ := csvXqLib:populateHeadersMap($xmlStruct/*, $includePath)

	(: populate the header row :)
	let $_ := (
		for $header in map:keys($headersMap)
		let $count := xs:int(map:get($headersMap, $header))
		(: build out the comma separated header row for the csv :)
		return
			if ($count eq 1) then 
				xdmp:set($headerRow, fn:concat($headerRow, $header, ","))
			else
				for $i in 1 to $count
				let $value := fn:concat($header, "-", xs:string($i))
				return xdmp:set($headerRow, fn:concat($headerRow, $value, ","))
	)
    
    (: remove last trailing comma and wrap in document tag :)
    let $docInput := 
    	document { 
    		fn:substring($headerRow, 1, (fn:string-length($headerRow) - 1))
    	}

    (: insert the csv and return the uri :)
    return (
    	xdmp:document-insert($uri, $docInput, $perms, $colls),
    	$uri
    )
};

(:~
 Get a single CSV document. This function supports an optional base64Enc boolean
 query parameter.  If true, a base64 encoded string is returned (defaults to 
 false if no value is specified).
 
 @param $csvUri    - document uri for the csv file
 @param $base64Enc - (optional) should the returned document be base64 encoded?
 @return           - csv document (or base64 encoded string)
 :)
declare function csvXqLib:getCsvDoc(
    $csvUri    as xs:string,
    $base64Enc as xs:boolean?
) as document-node() {
    let $base64Enc := ($base64Enc, fn:false())[1]
    let $fileExt   :=
    	if (fn:contains($csvUri, ".")) then
        	fn:lower-case(xs:string(functx:substring-after-last($csvUri, ".")))
    	else ""
    
    return
    	(: validation :)
    	if ($fileExt ne "csv") then
    		document { "Must specify a CSV document" }
    	else if (fn:doc-available($csvUri) eq fn:false()) then
    		document { "The specified CSV document URI cannot be found" }
    	(: return the csv document :)
		else
			if ($base64Enc) then
				let $doc := fn:doc($csvUri)
    			return
    				try {
	    				document { 
	    					xs:base64Binary(xs:hexBinary($doc/node())) cast as xs:string 
	    				}
	    			} catch ($err) {
	        			document { xdmp:base64-encode(xdmp:quote($doc)) }
					}
			else
	    		(: set response headers (useful if responding to a REST call) :)
	        	let $_ := (
	        		xdmp:set-response-content-type("text/csv"),
	            	xdmp:add-response-header(
	            		"Content-Disposition", 
	            		'attachment; filename="download.csv"'
	            	)
	        	)
    			return fn:doc($csvUri)
};

(:~
 Takes a CSV document and creates/stores XML document(s) based on the data within.
 This function supports an optional $xmlTemplate parameter which allows a user to
 specify the XML structure for various attributes and/or elements. 
 NOTES:
   * If no $xmlTemplate is defined, a mostly flat XML structure is created with a root 
     element of <xmlFromCsv> and two sub-elements containing the data <elements> and
     <attributes> (depending on if the header name is prefixed with an "@").
   * If an $xmlTemplate is defined and this function encounters unmapped CSV headers, 
     those headers and their respective values will be placed as elements within an 
     <unmappedHeaders> element directly under the XML root.

 @param $csvInput     - the csv document
 @param $xmlTemplate  - (optional) xml template/structure
 @param $csvFileName  - (optional) file name to include in the csv document uri
 @param $xmlDirectory - (optional) directory to include in the xml document uris
 @return              - xml response containing data points for the csv upload/parse
 :)
declare function csvXqLib:createXmlFromCsv(
	$csvInput     as document-node(),
	$xmlTemplate  as document-node()?,
	$csvFileName  as xs:string?,
	$xmlDirectory as xs:string?
) as document-node() {
	(: get the input types :)
	let $csvInputType := csvXqLib:getInputType($csvInput)
	let $xmlInputType := 
		if (fn:not(fn:empty($xmlTemplate))) then csvXqLib:getInputType($xmlTemplate)
		else ()

	return
		(: validation :)
		if ($csvInputType ne "text") then 
			"Unsupported CSV input type"
		else if ($xmlInputType ne "xml" or fn:not(fn:empty($xmlInputType))) then 
			"Unsupported XML template input type"
		else
			(: remove non ascii characters :)
			let $csvInput  := 
				document { 
					if (fn:not(fn:empty($csvInput))) then
						xdmp:javascript-eval($removeNonAsciiChars, ("inText", $csvInput/text()))
					else ()
				}
			let $batchUuid := sem:uuid-string()
			(: set the fileName, uri, collections, and permissions for the csv doc :)
			let $csvFileName :=
				if (fn:empty($csvFileName) or fn:normalize-space($csvFileName) eq "") then
					fn:concat("csvFile_", sem:uuid-string(), ".csv")
				else
					if (fn:contains($csvFileName, ".csv")) then $csvFileName
					else fn:concat($csvFileName, ".csv")
			let $csvUri   := fn:concat("/filled/csvToXml/", $batchUuid, "/", $csvFileName)
			let $csvColls := (("csvToXml", "csv", "filled", $batchUuid), xdmp:default-collections())
			let $csvPerms := xdmp:default-permissions()

			(: set the base uri, collections, and permissions for the xml doc :)
			let $xmlDirectory := 
				if (fn:empty($xmlDirectory) or fn:normalize-space($xmlDirectory) eq "") then "/"
				else fn:concat("/", $xmlDirectory, "/")
			let $xmlUriBase   := fn:concat("/xmlFromCsv/", $batchUuid, $xmlDirectory)
			let $xmlColls     := (("xmlFromCsv", "xml", $batchUuid), xdmp:default-collections())
			let $xmlPerms     := xdmp:default-permissions()

			(: variables for parsing :)
			let $csvHeaderElems    := ()
			let $csvHeaderElemsCnt := 0
			let $parseErrCnt       := 0
			let $parseScsCnt       := 0
			let $createErrCnt      := 0
			let $createScsCnt      := 0
			let $crChar            := "&#13;"
			let $lfChar            := "&#10;"

			(: tokenize on carriage return and/or line feed for rows :)
			let $csvRows := 
				let $crToken := fn:tokenize($csvInput, $crChar)
				return
					for $row in $crToken
					return
						if (fn:contains($row, $lfChar)) then fn:tokenize($row, $lfChar)
						else $row 

			(: loop over the rows and attempt to create an xml document for each :)
			let $_ := (
				for $row at $rowPos in $csvRows
				let $row := fn:normalize-space($row)
				return
					(: empty row, skip it :)
					if (fn:empty($row) or $row eq "") then ()
					else 
						let $csvRowData := csvXqLib:parseRowIntoSequence($row)
						return
							(: havent set headers yet, set them now :)
							if ($csvHeaderElemsCnt eq 0) then (
								xdmp:set($csvHeaderElems, $csvRowData),
								xdmp:set($csvHeaderElemsCnt, fn:count($csvRowData))
							)
							(: headers are populated, this row contains values :)
							else
								(: check to ensure counts for the headers vs. values defined for the row match :)
								if (fn:count($csvRowData) ne $csvHeaderElemsCnt) then (
									xdmp:log(
										"Header count vs value count mismatch; skipping row number: " || $rowPos, 
										"info"
									),
									xdmp:set($parseErrCnt, ($parseErrCnt + 1))
								)
								else
									(: construct the new document and insert it into the database :)
									let $newXmlDoc := csvXqLib:constructXmlDoc($csvHeaderElems, $csvRowData, $xmlTemplate)
									return
										try {
											let $oneUp  := ($createScsCnt + 1)
											let $uri    := fn:concat($xmlUriBase, xs:string($oneUp), ".xml")
											let $insert := xdmp:document-insert($uri, $newXmlDoc, $xmlPerms, $xmlColls)
											return (
												map:put($createdDocsMap, $uri, "true"),
												xdmp:set($createScsCnt, ($createScsCnt + 1)),
												xdmp:set($parseScsCnt,  ($parseScsCnt  + 1))
											)
										} catch ($err) {(
											xdmp:log("Error creating document at row number: " || $rowPos, "info"),
											xdmp:set($createErrCnt, ($createErrCnt + 1)),
											xdmp:set($parseErrCnt,  ($parseErrCnt  + 1))
										)}
			)

			(: store the populated/filled csv file :)
			let $_ := xdmp:document-insert($csvUri, $csvInput, $csvPerms, $csvColls)
			
			(: create response XML :)
			let $respXml  :=
				document { 
					element response {
	    				element batchUuid               { $batchUuid  },
	    				element filledCsvUri            { $csvUri     },
	    				element xmlDocsBaseUriDirectory { $xmlUriBase },
	    				element successAndErrorCounts   { 
	    					csvXqLib:createRespText($parseScsCnt, $parseErrCnt, $createScsCnt, $createErrCnt)
	    				}
	    			}
	    		}
			
			return (
				xdmp:log($respXml, "info"),
				$respXml
			)
};


(: ********************************************************
 : ********************************************************
 : ********************************************************
 : ***                                                  ***
 : *** Supporting functions for creating a CSV from XML ***
 : ***                                                  ***
 : ********************************************************
 : ********************************************************
 : ********************************************************
 :)


(:~
 Adds a value to the $headersMap private global variable as a "key".  If the key 
 already exists, the count (key's value) is increased by 1.

 @param $key - the key to add
 @return     - empty sequence
 :)
declare function csvXqLib:headersMapAdd(
	$key as xs:string?
) as empty-sequence() {
	if (fn:empty($key) or fn:normalize-space($key) eq "") then ()
	else
		(: headersMap already contains this key, increase the count (key's value) :)
		if (map:contains($headersMap, $key)) then
			map:put($headersMap, $key, xs:string(xs:int(map:get($headersMap, $key)) + 1))
		else map:put($headersMap, $key, "1")
};

(:~
 Recursively walk XML node(s) and add the attributes and elements (only elements 
 with no children) to the $headersMap private global variable.

 @param $nodeToTraverse  - the node to walk
 @param $includeFullPath - whether or not to include the full path (dot notation)
                           for each element within the csv header names
 @return                 - recursive descent
 :)
declare function csvXqLib:populateHeadersMap(
	$nodeToTraverse,
	$includeFullPath as xs:boolean
) {
	typeswitch($nodeToTraverse)
		case element() return 
			element { fn:node-name($nodeToTraverse) } {
				let $elemName := 
					if ($includeFullPath) then
						fn:string-join(xs:string($nodeToTraverse/ancestor-or-self::*/name()), ".")
					else xs:string(fn:node-name($nodeToTraverse))
				(: add the attributes to the headers map :)
				let $_ := (
					for $att in $nodeToTraverse/@*/name()
					let $attName := 
						if ($includeFullPath) then fn:concat($elemName, ".@", $att)
						else fn:concat("@", $att)
					return csvXqLib:headersMapAdd($attName)
				)
				return
					(: element has no children - add it to the map :)
					if (fn:count($nodeToTraverse/*) eq 0 ) then
						csvXqLib:headersMapAdd($elemName)
					else $nodeToTraverse/node() ! csvXqLib:populateHeadersMap(., $includeFullPath)
			}
		default return $nodeToTraverse
};


(: ********************************************************
 : ********************************************************
 : ********************************************************
 : ***                                                  ***
 : *** Supporting functions for creating XML from a CSV ***
 : ***                                                  ***
 : ********************************************************
 : ********************************************************
 : ********************************************************
 :)


(:~
 : Returns the generic type of input (xml, json, txt, binary, or unsupported)
 :
 : @param $input - The input to evaluate
 : @return       - The generic input type
 :)
declare function csvXqLib:getInputType(
    $input as document-node()*
) as xs:string {
    typeswitch ($input/node())
        case element()     return "xml"
        case text()        return "text"
        case binary()      return "binary"
        case object-node() return "json"
        case array-node()  return "json"
        default            return "unsupported"
};

(:~
 Creates the response text to send back upon CSV upload and parse

 @param $parseScsCnt  - csv parse success count
 @param $parseErrCnt  - csv parse error count
 @param $createScsCnt - create xml document success count
 @param $createErrCnt - create xml document error count
 @return              - response message text
 :)
declare function csvXqLib:createRespText(
	$parseScsCnt  as xs:int,
	$parseErrCnt  as xs:int, 
	$createScsCnt as xs:int,
	$createErrCnt as xs:int
) as xs:string {
	let $lineBr := "&#13;"

	return fn:concat(
		"CSV parse/upload overall results         ",                $lineBr,
		"---------------------------------------  ",                $lineBr,
		"* CSV parse SUCCESS count................", $parseScsCnt,  $lineBr,
		"* CSV parse ERROR count..................", $parseErrCnt,  $lineBr,
		"---------------------------------------  ",                $lineBr,
		"* New XML document create SUCCESS count..", $createScsCnt, $lineBr,
		"* New XML document create ERROR count....", $createErrCnt, $lineBr
	)
};

(:~
 Takes a single row (string) from a CSV import and parses it into a sequence of fields; this 
 function will handle commas within data points as long as that entire data point value is within 
 double quotes. This function isn't elegant but does seem to handle most edge cases for quoted 
 values containing commas

 @param $row - a single row - a single comma delimited string
 @return     - sequence of values
 :)
declare function csvXqLib:parseRowIntoSequence(
	$row as xs:string
) as xs:string* {
	let $dblQChar       := "&#34;"
	let $commaChar      := "&#44;"
	let $singleComRegEx := fn:concat("(^", $commaChar, "$)")
	let $multiComRegEx  := fn:concat("(^", $commaChar, "+$)")
	(: special case to handle double quotes inside of an already double quoted value :)
	let $escapedDblQ    := "%%22%%"
	let $replRow        := fn:replace($row, "&#34;&#34;", $escapedDblQ)
	let $tokenDQ        := fn:tokenize($replRow, $dblQChar)
	let $tokenDQCnt     := fn:count($tokenDQ)

	return
		(: row doesnt contain double quotes, no special cases required :)
		if ($tokenDQCnt lt 2) then fn:tokenize($row, $commaChar)
		(: row contains double quotes which means there are commas within one or more data 
		 : elements; tokenize on double quotes and then handle all the special cases :)
		else
			for $val at $pos in $tokenDQ
			(: restore the double quote back since we've already tokenized :)
			let $val := fn:replace($val, $escapedDblQ, $dblQChar)
			return
				(: blank value at the first or last position :)
				if ($val eq "" and $pos = (1, $tokenDQCnt)) then ()
				(: value is a single comma (only) :)
				else if (fn:matches($val, $singleComRegEx)) then
					(: at first or last position :)
					if ($pos = (1, $tokenDQCnt)) then ""
					else ()
				(: value is multiple commas (only) :)
				else if (fn:matches($val, $multiComRegEx)) then
					let $newVal := 
						(: preceding value was a quoted value and we are not at the first
						 : sequence value position; remove one preceding comma :)
						if (fn:not(fn:starts-with($tokenDQ[$pos - 1], $commaChar)) and $pos ne 1) then
							fn:substring-after($val, $commaChar)
						else $val
					let $retVal := 
						(: succeeding value is a quoted value and we are not at the last
						 : sequence value position; remove one preceeding comma :)
						if (fn:not(fn:starts-with($tokenDQ[$pos + 1], $commaChar)) and $pos ne $tokenDQCnt) then
							fn:substring-after($newVal, $commaChar) 
						else $newVal
					return
						(: if retVal is empty or blank then return blank, otherwise tokenize it :)
						if (fn:empty($retVal) or $retVal eq "") then ""
						else fn:tokenize($retVal, $commaChar)
				(: value starts with or ends with a comma :)
				else if (fn:starts-with($val, $commaChar) or fn:ends-with($val, $commaChar)) then
					let $newVal := 
						(: value starts with a comma and we are not at the
						 : first position; remove one preceeding comma :)
						if (fn:starts-with($val, ",") and $pos ne 1) then
							fn:substring-after($val, $commaChar)
						else $val
					let $retVal := 
						(: value ends with a comma and we are not at the
						 : last position; remove one succeeding comma :)
						if (fn:ends-with($newVal, $commaChar) and $pos ne $tokenDQCnt) then
							fn:substring($newVal, 1, (fn:string-length($newVal) - 1))
						else $newVal
					(: tokenize the retVal :)
					return fn:tokenize($retVal, $commaChar)
				(: this was a quoted string - so its a single field value :)
				else $val
};

(:~
 Constructs a new XML document with the given data

 @param $csvHeaderElems - headers row sequence (element/attribute names)
 @param $csvRowData     - a single row's data values
 @param $xmlTemplate    - (optional) xml template/structure
 @return                - document node
 :)
declare function csvXqLib:constructXmlDoc(
	$csvHeaderElems as xs:string*,
	$csvRowData     as xs:string*,
	$xmlTemplate    as document-node()?
) as document-node() {
	let $currDateTime := fn:current-dateTime()
	let $stdRootElem  := "xmlFromCsv"
	let $unmappedElem := "unmappedHeaders"

	return
		(: TODO: implement logic for creating XML doc based on an incoming template :)
		(: TODO: implement logic for looking for dot-notation element names/attributes and creating structure :)
		if (fn:not(fn:empty($xmlTemplate))) then 
			document { "" }
		(: no xmlTemplate defined, construct flat XML structure :)
		else
			document {
				element { $stdRootElem } {
					element elements {
						for $header at $pos in $csvHeaderElems
						where fn:not(fn:contains($header, "@"))
						return element { $header } { $csvRowData[$pos] }
					},
					element attributes {
						for $header at $pos in $csvHeaderElems
						where fn:contains($header, "@")
						return element { $header } { $csvRowData[$pos] }
					}
				}
			}
};
