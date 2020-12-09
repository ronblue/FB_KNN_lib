#include "file.bi"

'' Don't do this, ever. Using a global to initialize a type is
'' among the worst things you can do to your codebase (and to
'' yourself, if you appreciate your time), and will
'' be the prime cause of a lot of very nasty and hard to track
'' bugs. Use constructors if you need to initialize types.
'DIM SHARED col AS INTEGER

Type pt
    Dim As Double l( any )
    As String nm
    '' This is neither needed, nor it belongs to the state for the
    '' type. Use another var (like 'count' below) to keep track of
    '' the number of elements the array has. Or simply use its
    '' bounds if appropriate.
    'As Long done
    As Long count
    Declare Constructor()
    declare constructor( as long )
    declare destructor()
End Type

Constructor pt : End Constructor

'' Take advantage of constructors for initializing types. That's the
'' whole point of them! You can have as many of them as you like, as
'' long as their signatures are different (they accept different
'' parameters).
constructor pt( c as long )
  count = c
  redim l( 1 to count )
end constructor

'' Also, remember to include a destructor when your type has other
'' composite types inside to properly deinitializing it, even if it's
'' just a stub (ie it's empty). 'Composite' types are types you define
'' yourself, and they include strings and arrays, which internally are
'' treated as composite types by the compiler and need their own
'' deinitialization.
destructor pt() : end destructor

DECLARE Sub printout(p As pt)
DECLARE Function Splitstring(s_in As String,chars As String,result() As String) As Long
DECLARE Function loadfiletostring(file As String) As STRING
DECLARE Function vdist(p1 As pt,p2 As pt) As DOUBLE
DECLARE Function GetClosest(a() As pt,ans() As Long,v As pt,num As Long) As LONG
DECLARE SUB knn_main(f AS STRING)

'#INCLIB "fb_knn"
