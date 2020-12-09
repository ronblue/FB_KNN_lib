#INCLUDE "file.bi"
#include once "fb-knn-rev.bi"

Sub printout(p As pt)
    For n As Long=1 To p.count
        Print p.l(n);",";
    Next
    
    Print p.nm
End Sub

Function Splitstring(s_in As String,chars As String,result() As String) As Long
    Dim As Long ctr,ctr2,k,n,LC=Len(chars)
    Dim As boolean tally(Len(s_in))
    #macro check_instring()
    n=0
    While n<Lc
        If chars[n]=s_in[k] Then
            tally(k)=true
            If (ctr2-1) Then ctr+=1
            ctr2=0
            Exit While
        End If
        n+=1
    Wend
    #endmacro
   
    #macro splice()
    If tally(k) Then
        If (ctr2-1) Then ctr+=1:result(ctr)=Mid(s_in,k+2-ctr2,ctr2-1)
        ctr2=0
    End If
    #endmacro
    '==================  LOOP TWICE =======================
    For k  =0 To Len(s_in)-1
        ctr2+=1:check_instring()
    Next k
    If ctr=0 Then
        If Len(s_in) Andalso Instr(chars,Chr(s_in[0])) Then ctr=1':beep
    End If
    If ctr Then Redim result(1 To ctr): ctr=0:ctr2=0 Else  Return 0
    For k  =0 To Len(s_in)-1
        ctr2+=1:splice()
    Next k
    '===================== Last one ========================
    If ctr2>0 Then
      Redim Preserve result(1 To ctr+1)
      result(ctr+1)=Mid(s_in,k+1-ctr2,ctr2)
   End If
   
    Return Ubound(result)
End Function

Function loadfiletostring(file As String) As String
    Var  f=Freefile
    Open file For Binary Access Read As #f
    Dim As String text
    If Lof(1) > 0 Then
        text = String(Lof(f), 0)
        Get #f, , text
    End If
    Close #f
    Return text
End Function

Function vdist(p1 As pt,p2 As pt) As Double
    Dim As Double acc
    For n As Long=Lbound(p1.l) To Ubound(p1.l)
        acc+=(p1.l(n)-p2.l(n))^2
    Next
    Return Sqr(acc)
End Function

Function GetClosest(a() As pt,ans() As Long,v As pt,num As Long) As Long
    Dim As Double d,i
    Dim As Long ctr
    dim as boolean done( lbound( a ) to ubound( a ) )
     
    Do
        d=2e8
        For n As Long=Lbound(a) To Ubound(a)
            Var dst=vdist(a(n),v)
            If d>dst andAlso not done(n) Then
              d=dst : i=n
              done( n ) = true
            end if
        Next n
        ctr+=1
        Redim Preserve ans(1 To ctr)
        ans(ctr)=i
    Loop Until Ubound(ans)>=num
    
    Return Ubound(ans)
End Function

SUB knn_main(f AS STRING)
  dim as long col
  INPUT "how many numeric columons does your csv file has?: "; col
  Dim As String s=loadfiletostring(f)'("knn-dataset-plant.csv")
  Redim As String g()
  splitstring(s,Chr(10),g()) 'load the file into a string array g()
  
  Dim As pt array(Lbound(g) To Ubound(g)) '
  
  for i as integer = lbound( g ) to ubound( g )
    array( i ) = pt( col )
  next
  
  For n As Long=Lbound(array) To Ubound(array)
      Redim As String tmp()
      splitstring(g(n),",",tmp())''load into temp with the , as seperator.
      For m As Long=1 To col
          array(n).l(m)=Val(tmp(m)) 'pick out the values to suit the udt
      Next
      array(n).nm=tmp(col+1)
      print n,
      printout(array(n))
  Next
  
  print
  
  Redim As Long near()
  
  
  GetClosest(array(),near(),array(100),8)'' <<--------  here, get 8 closest to array(100)
  print "using Getclosest()"
  
  Print "index",,"values"
  For n As Long=Lbound(near) To Ubound(near)
      Print near(n),
      printout array(near(n))
  Next
  
  
  '============= new predictions!!!=================
  DIM answer AS STRING
  DO
  'DIM predict AS pt
  var predict = pt( col )
  DIM AS DOUBLE x1
  DIM neighbors AS LONG
  DIM counter AS INTEGER = 1
  
  DO UNTIL counter > col
  
  INPUT "insert NEW value TO prediction: "; x1
  predict.l(counter) = x1
  counter += 1
  LOOP
  
  for i as integer = lbound( predict.l ) to ubound( predict.l )
    ? predict.l( i ),
  next
  ?
  
  INPUT "how many neighbors TO search?: "; neighbors
  
  Redim As Long near()
  GetClosest(array(),near(),predict,neighbors) ' <- new predictions for new values
  print "using Getclosest()"
  
  Print "index",,"values"
  For n As Long=Lbound(near) To Ubound(near)
      Print near(n),
      printout array(near(n))
  NEXT
  
  INPUT "another prediction? y/n: "; answer
  LOOP UNTIL answer = "n"
END SUB

knn_main( ".\..\datasets\iris.csv" )
