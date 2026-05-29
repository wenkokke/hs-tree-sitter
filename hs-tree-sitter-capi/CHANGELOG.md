### 13.1.15.0

- The implementation of `TSInput` is exposed, and `ts_parser_parse` takes an
  argument of type `TSInput` as opposed to accepting its members separately.

- Bump tree-sitter version to v0.26.9.

  The following symbols were removed:

  - `TSInputEncoding`(`TSInputEncodingUTF16`)
  - `ts_language_version`
  - `ts_parser_cancellation_flag`
  - `ts_parser_set_cancellation_flag`
  - `ts_parser_set_timeout_micros`
  - `ts_parser_timeout_micros`
  - `ts_query_cursor_set_timeout_micros`
  - `ts_query_cursor_timeout_micros`

  The following symbols were added:

  - `TSInputEncoding`(`TSInputEncodingUTF16LE`, `TSInputEncodingUTF16BE`, `TSInputEncodingCustom`)
  - `TSDecodeFunction`
  - `ts_language_abi_version`
  - `ts_language_metadata` with `TSLanguageMetadata`
  - `ts_language_name`
  - `ts_language_subtypes`
  - `ts_language_supertypes`
  - `ts_point_edit`
  - `ts_range_edit`
  - `TSParseOptions` and `TSParseOptionsProgressCallback`/`Function`
  - `ts_parser_parse_with_options`
  - `TSQueryCursorOptions` and `TSQueryCursorOptionsProgressCallback`/`Function`
  - `ts_query_cursor_exec_with_options`
  - `ts_query_cursor_set_containing_byte_range`
  - `ts_query_cursor_set_containing_point_range`

  The following symbols changed:

  - `ts_query_cursor_set_byte_range` returns a boolean
  - `ts_query_cursor_set_point_range` returns a boolean

### 13.0.14.0

- First version, compiled against tree-sitter v0.24.3.
