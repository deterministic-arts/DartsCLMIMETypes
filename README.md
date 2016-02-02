-*- mode: text; coding: utf-8 -*-

MIME Types
-----------

This is a small library to parse MIME type specifications ("media range") in 
accordance with RFC 2616.

- Condition `mime-parse-error`
- Class `mime-type`
- Function `mime-type` _value_ → _object_
- Generic Function `mime-type-format` _object_ → _format-string_
- Generic Function `mime-type-variant` _object_ → _variant-string_
- Generic Function `mime-type-parameters` _object_ → _parameter-list_
- Generic Function `mime-type-media-type` _object_ → _type-string_
- Generic Function `mime-type-string` _object_ → _representation_
- Function `parse-mime-type-1` _string_ `&key` _start_ _end_ _junk-allowed_ → _format_ _variant_ _parameters_ _condition_
- Function `parse-mime-type` _string_ `&key` _start_ _end_ _junk-allowed_ → _object_ _condition_
- Function `format-mime-type-1` _format_ _variant_ _parameters_ `&key` _stream_ _case-mode_ _dense_ → `nil`
- Function `format-mime-type` _object_ `&key` _stream_ _dense_ → _object_
- Function `mime-type-equal` _type1_ _type2_ `&key` _test_ → _result_
- Function `mime-type-hash` _object_ → _integer_
