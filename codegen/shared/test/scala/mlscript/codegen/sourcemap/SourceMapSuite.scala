package mlscript.codegen.sourcemap

class SourceMapSuite extends munit.FunSuite:
  test("Base64VLQ - intToChar and charToInt") {
    assertEquals(Base64VLQ.intToChar(0), 'A')
    assertEquals(Base64VLQ.charToInt('A'), 0)
  }

  test("Base64VLQ - encode integers in VLQ") {
    assertEquals(Base64VLQ.encode(0), "A")
    assertEquals(Base64VLQ.encode(1), "C")
    assertEquals(Base64VLQ.encode(-1), "D")
    assertEquals(Base64VLQ.encode(123), "2H")
    assertEquals(Base64VLQ.encode(123456789), "qxmvrH")
  }

  private val typicalCases: List[(String, SourceMapMappings)] = {
    import SourceMapSegment as s
    val empty: SourceMapLine = Nil
    (
      "AAAA",
      (s(0, 0, 0, 0) :: Nil) :: Nil
    ) :: (
      ";;;",
      empty :: empty :: empty :: empty :: Nil
    ) :: (
      "A,AAAA;;AACDE;",
      List(s(0), s(0, 0, 0, 0)) :: empty :: List(s(0, 0, 1, -1, 2)) :: empty :: Nil
    ) :: (
      ";;;;EAEEA,EAAE,EAAC,CAAE;ECQY,UACC",
      empty :: empty :: empty :: empty :: List(
        s(2, 0, 2, 2, 0),
        s(4, 0, 2, 4),
        s(6, 0, 2, 5),
        s(7, 0, 2, 7),
      ) :: List(
        s(2, 1, 10, 19),
        s(12, 1, 11, 20),
      ) :: Nil
    ) :: (
      "gw+BAAAA,w+BAAAA,w+BAAAA,w+BAAAA",
      (
        List(
          s(32000, 0, 0, 0, 0),
          s(33000, 0, 0, 0, 0),
          s(34000, 0, 0, 0, 0),
          s(35000, 0, 0, 0, 0),
        )
      ) :: Nil
    ) ::
    (
      "+/////D",
      (s(Int.MaxValue) :: Nil) :: Nil
    ) ::
    (
      "B",
      (s(Int.MinValue) :: Nil) :: Nil
    ) :: (
      Nil
    )
  }

  test("encode - typical cases") {
    typicalCases.foreach { case (text, mappings) =>
      assertEquals(encode(mappings), text)
    }
  }

  private val longerCases: List[(String, SourceMapMappings)] = {
    import SourceMapSegment as s
    (
      "AAAA,aAEA,IAAIA,eAAiB,oCAAoCC,cACzD,SAASC,IACRC,OAAOC,MAAOJ,eAAgB,GAG/B,IAAIK,iBAAmB,sBAAsBJ,cAC7C,SAASK,IACRH,OAAOC,MAAOC,iBAAkB,GAGjCH,IACAI",
      List(
        s(0, 0, 0, 0),
        s(13, 0, 2, 0),
        s(17, 0, 2, 4, 0),
        s(32, 0, 2, 21),
        s(68, 0, 2, 57, 1),
        s(82, 0, 3, 0),
        s(91, 0, 3, 9, 2),
        s(95, 0, 4, 1, 3),
        s(102, 0, 4, 8, 4),
        s(108, 0, 4, 15, 0),
        s(123, 0, 4, 31),
        s(126, 0, 7, 0),
        s(130, 0, 7, 4, 5),
        s(147, 0, 7, 23),
        s(169, 0, 7, 45, 1),
        s(183, 0, 8, 0),
        s(192, 0, 8, 9, 6),
        s(196, 0, 9, 1, 3),
        s(203, 0, 9, 8, 4),
        s(209, 0, 9, 15, 5),
        s(226, 0, 9, 33),
        s(229, 0, 12, 0, 2),
        s(233, 0, 13, 0, 6),
      ) :: Nil
    ) :: (
      "CAAC,SAAUA,EAAQC,GACC,iBAAZC,SAA0C,oBAAXC,OAAyBF,IAC7C,mBAAXG,QAAyBA,OAAOC,IAAMD,OAAOH,GACnDA,IAHF,CAIEK,EAAM,WAAe,aAEtB,IAAIC,EAAiB,oCAAoCC,cAKzD,IAAIC,EAAmB,sBAAsBD,cAH5CE,OAAOC,MAAOJ,EAAgB,GAK9BG,OAAOC,MAAOF,EAAkB",
      List(
        s(1, 0, 0, 1),
        s(10, 0, 0, 11, 0),
        s(12, 0, 0, 19, 1),
        s(15, 0, 1, 20),
        s(32, 0, 1, 8, 2),
        s(41, 0, 1, 50),
        s(61, 0, 1, 39, 3),
        s(68, 0, 1, 64, 1),
        s(72, 0, 2, 19),
        s(91, 0, 2, 8, 4),
        s(99, 0, 2, 33, 4),
        s(106, 0, 2, 40, 5),
        s(110, 0, 2, 46, 4),
        s(117, 0, 2, 53, 1),
        s(120, 0, 3, 2, 1),
        s(124, 0, 0, 0),
        s(125, 0, 4, 2, 6),
        s(127, 0, 4, 8),
        s(138, 0, 4, 23),
        s(151, 0, 6, 1),
        s(155, 0, 6, 5, 7),
        s(157, 0, 6, 22),
        s(193, 0, 6, 58, 8),
        s(207, 0, 11, 1),
        s(211, 0, 11, 5, 9),
        s(213, 0, 11, 24),
        s(235, 0, 11, 46, 8),
        s(249, 0, 8, 2, 10),
        s(256, 0, 8, 9, 11),
        s(262, 0, 8, 16, 7),
        s(264, 0, 8, 32),
        s(267, 0, 13, 2, 10),
        s(274, 0, 13, 9, 11),
        s(280, 0, 13, 16, 9),
        s(282, 0, 13, 34),
      ) :: Nil
    ) :: (
      Nil
    )
  }

  test("encode - longer cases") {
    typicalCases.foreach { case (text, mappings) =>
      assertEquals(encode(mappings), text)
    }
  }
end SourceMapSuite
