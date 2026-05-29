### 13.1.15.0

- The implementation of `Input` is exposed, and `parserParse` takes an
  argument of type `Input` as opposed to accepting its members separately.

- Bump tree-sitter version to v0.26.9.

  The following symbols were removed:

  - `InputEncoding`(`InputEncodingUTF16`)
  - `languageVersion`
  - `parserCancellationFlag`
  - `parserSetCancellationFlag`
  - `parserSetTimeoutMicros`
  - `parserTimeoutMicros`
  - `queryCursorSetTimeoutMicros`
  - `queryCursorTimeoutMicros`

  The following symbols were added:

  - `InputEncoding`(`InputEncodingUTF16LE`, `InputEncodingUTF16BE`, `InputEncodingCustom`)
  - `DecodeFunction`
  - `languageAbiVersion`
  - `languageMetadata` with `LanguageMetadata`
  - `languageName`
  - `languageSubtypes`
  - `languageSupertypes`
  - `pointEdit`
  - `rangeEdit`
  - `ParseOptions` and `ParseOptionsProgressCallback`/`Function`
  - `parserParseWithOptions`
  - `QueryCursorOptions` and `QueryCursorOptionsProgressCallback`/`Function`
  - `queryCursorExecWithOptions`
  - `queryCursorSetContainingByteRange`
  - `queryCursorSetContainingPointRange`

  The following symbols changed:

  - `queryCursorSetByteRange` returns a boolean
  - `queryCursorSetPointRange` returns a boolean

### 13.0.14.0

- First version, compiled against tree-sitter v0.24.3.
