### 13.1.15.0

- Bump tree-sitter version to v0.26.9.

  The following symbols were removed:

  * `InputEncoding`(`InputEncodingUTF16`)
  * `languageVersion`
  * `parserCancellationFlag`
  * `parserSetCancellationFlag`
  * `parserSetTimeoutMicros`
  * `parserTimeoutMicros`
  * `queryCursorSetTimeoutMicros`
  * `queryCursorTimeoutMicros`

  The following symbols were added:

  * `InputEncoding`(`InputEncodingUTF16LE`, `InputEncodingUTF16BE`, `InputEncodingCustom`)
  * `languageAbiVersion`
  * `languageMetadata`
  * `languageName`
  * `languageSubtypes`
  * `languageSupertypes`
  * `parserParseWithOptions`
  * `pointEdit`
  * `queryCursorExecWithOptions`
  * `queryCursorSetContainingByteRange`
  * `queryCursorSetContainingPointRange`
  * `rangeEdit`

### 13.0.14.0

- First version, compiled against tree-sitter v0.24.3.
