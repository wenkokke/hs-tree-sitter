### 13.1.15.0

- Bump tree-sitter version to v0.26.9.

  The following symbols were removed:

  * `TSInputEncoding`(`TSInputEncodingUTF16`)
  * `ts_language_version`
  * `ts_parser_cancellation_flag`
  * `ts_parser_set_cancellation_flag`
  * `ts_parser_set_timeout_micros`
  * `ts_parser_timeout_micros`
  * `ts_query_cursor_set_timeout_micros`
  * `ts_query_cursor_timeout_micros`

  The following symbols were added:

  * `TSInputEncoding`(`TSInputEncodingUTF16LE`, `TSInputEncodingUTF16BE`, `TSInputEncodingCustom`)
  * `ts_language_abi_version`
  * `ts_language_metadata`
  * `ts_language_name`
  * `ts_language_subtypes`
  * `ts_language_supertypes`
  * `ts_parser_parse_with_options`
  * `ts_point_edit`
  * `ts_query_cursor_exec_with_options`
  * `ts_query_cursor_set_containing_byte_range`
  * `ts_query_cursor_set_containing_point_range`
  * `ts_range_edit`

### 13.0.14.0

- First version, compiled against tree-sitter v0.24.3.
