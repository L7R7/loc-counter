package com.l7r7.loc

import io.circe.parser.decode
import weaver.FunSuite

object ResultJsonSuite extends FunSuite:
  test("can parse cloc result json"):
    val json =
      """
         {"header" : {
  "cloc_url"           : "github.com/AlDanial/cloc",
  "cloc_version"       : "2.04",
  "elapsed_seconds"    : 0.169693946838379,
  "n_files"            : 181,
  "n_lines"            : 17166,
  "files_per_second"   : 1066.62614296071,
  "lines_per_second"   : 101158.587679909},
"JSON" :{
  "nFiles": 10,
  "blank": 20,
  "comment": 0,
  "code": 11515},
"XML" :{
  "nFiles": 147,
  "blank": 1,
  "comment": 0,
  "code": 4217},
"HTML" :{
  "nFiles": 2,
  "blank": 13,
  "comment": 2,
  "code": 708},
"Scala" :{
  "nFiles": 10,
  "blank": 53,
  "comment": 4,
  "code": 438},
"SUM": {
  "blank": 115,
  "comment": 25,
  "code": 17026,
  "nFiles": 181} }
      """

    val decoded = decode[Result](json)
    expect.eql(
      Right(
        Result(
          total = 17166,
          languages = Map(
            "JSON" -> Result.SingleResult(nFiles = 10, blank = 20, comment = 0, code = 11515),
            "XML" -> Result.SingleResult(nFiles = 147, blank = 1, comment = 0, code = 4217),
            "HTML" -> Result.SingleResult(nFiles = 2, blank = 13, comment = 2, code = 708),
            "Scala" -> Result.SingleResult(nFiles = 10, blank = 53, comment = 4, code = 438)
          )
        )
      ),
      decoded
    )
