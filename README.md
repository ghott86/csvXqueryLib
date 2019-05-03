# Credit
This project was inspired by Dave Cassell and Matthew Royal's CSV utilities below:
* (Dave Cassell)  - https://github.com/dmcassel/blog-code/blob/master/src/app/models/csv-lib.xqy
* (Matthew Royal) - https://github.com/masyukun/correct-csv-parser-xquery

# Main functions
* createCsvFromXml 
  * Takes an XML structure and parses it to gather all of the element names and attribute names witin. 
  * Creates and stores a CSV file with those element/attribute names as the CSV headers (first row).
  * Supports an optional $fileName parameter which will be included in the stored CSV's document uri.
  * Supports an optional $includePath parameter (default false) which determines if the full path (dot notation) for each element and attribute is included within the CSV header names.
  * Returns the document uri for the created/stored blank (headers only) CSV file.
* getCsvDoc
  * Gets a single CSV document. 
  * Supports an optional $base64Enc boolean query parameter (default false) to determine if the file should be returned as a base64 encoded string.
* createXmlFromCsv
  * Takes a CSV document and creates/stores XML document(s) based on the data within.
  * This function supports an optional $xmlTemplate parameter which allows a user to specify the XML structure for various attributes and/or elements.
    * If no $xmlTemplate is defined, a mostly flat XML structure is created with a root element of <xmlFromCsv> and two sub-elements containing the data <elements> and <attributes> (depending on if the header name is prefixed with an "@").
    * If an $xmlTemplate is defined and this function encounters unmapped CSV headers, those headers and their respective values will be placed as elements within an <unmappedHeaders> element directly under the XML root.
  * Supports an optional $csvFileName parameter which will be included in the stored (filled) CSV's document uri.
  * Supports an optional $xmlDirectory parameter which will be included in the XML document uri(s).