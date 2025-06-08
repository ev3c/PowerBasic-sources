'** Graba los Indices del Fichero de crucigramas y de los diccionarios
CALL QSSaveIndex(MCruci$+".NDX",MIndexCruci%())
CALL QSSaveIndex("DICCI000.NDX",MIndexDicciGE%())
IF FNTrim$(MCDic$) <> "" THEN
  CALL QSSaveIndex(MCDic$+".NDX",MIndexDicci%())
END IF
'**
