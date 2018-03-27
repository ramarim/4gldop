Option Compare Text
Sub findRecords()
    On Error Resume Next: Err.Clear
    Dim ra As Range, cell As Range, res, txt$, v, pos&
    Dim ArrAcct(17)
    ArrAcct(0)  = "2011-0189"
    ArrAcct(1)  = "2011-0207"
    ArrAcct(2)  = "2012-0015"
    ArrAcct(3)  = "2012-0043"
    ArrAcct(4)  = "2012-0055"
    ArrAcct(5)  = "2012-0097"
    ArrAcct(6)  = "2013-0203"
    ArrAcct(7)  = "2014-0010"
    ArrAcct(8)  = "2014-0026"
    ArrAcct(9)  = "2014-0031"
    ArrAcct(10) = "2014-0033"
    ArrAcct(11) = "2014-0118"
    ArrAcct(12) = "2014-0124"
    ArrAcct(13) = "2014-0186"
    ArrAcct(14) = "5000-0602"
    ArrAcct(15) = "5000-0608"

    ra.Font.Color = 0: ra.Font.Bold = 0  ' сброс цветового выделения
    For i = UBound(ArrAcct) To LBound(ArrAcct) Step -1
      txt = ArrAcct(i)
      j = 1
      Set ra = Range([A1], Range("A" & Rows.Count).End(xlUp)) ' диапазон для поиска
      Application.ScreenUpdating = False
      For Each cell In ra.Cells    ' перебираем все ячейки
          pos = 1
          If cell.Text Like "*" & txt & "*" Then
              arr = Split(cell.Text, txt, , vbTextCompare)
              If UBound(arr) > 0 Then
                  For Each v In arr
                      pos = pos + Len(v)
                      With cell.Characters(pos, Len(txt))
                          .Font.ColorIndex = 3
                          .Font.Bold = True
                          .Interior.ColorIndex = 6
                      End With
                      pos = pos + Len(txt)
                      Cells(j, 15) = "Found!"
                  Next v
              End If
          End If
          j = j + 1
      Next cell
    Next
End Sub




----
Sub color_nkpo()
    On Error Resume Next: Err.Clear
    Dim ra As Range, cell As Range, res, txt$, v, pos&
    Dim ArrAcct(4)
    ArrAcct(0) = "негосударственных коммерческих организаций"
    ArrAcct(1) = "негосударственной коммерческой организации"
    ArrAcct(2) = "негосударственным коммерческим организациям"
    ArrAcct(3) = "негосударственн* коммерческ* организац*"
    ra.Font.Color = 0: ra.Font.Bold = 0  ' сброс цветового выделения
    For i = UBound(ArrAcct) To LBound(ArrAcct) Step -1
      txt = ArrAcct(i)
      j = 1
      Set ra = Range([E1], Range("E" & Rows.Count).End(xlUp)) ' диапазон для поиска
      Application.ScreenUpdating = False
      For Each cell In ra.Cells    ' перебираем все ячейки
          pos = 1
          If cell.Text Like "*" & txt & "*" Then
              arr = Split(cell.Text, txt, , vbTextCompare)
              If UBound(arr) > 0 Then
                  For Each v In arr
                      pos = pos + Len(v)
                      With cell.Characters(pos, Len(txt))
                          .Font.ColorIndex = 4
                          .Font.Bold = True
                          .Interior.ColorIndex = 6
                      End With
                      pos = pos + Len(txt)
                      Cells(j, 4) = "НКПО=НКО"
                  Next v
              End If
          End If
          j = j + 1
      Next cell
    Next
End Sub
