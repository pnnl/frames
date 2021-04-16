Attribute VB_Name = "DataTypes"
Option Explicit

Type entry
    time As Double
    valu As Double
    valu2 As Double
End Type

Type sProgeny
    cas As String
    ref As Long
    use As Long
    unit(2) As String
    tunit(2) As String
    dunit(2) As String
    nts As Long
    series() As entry
End Type

Type sParent
    cas As String
    ref As Long
    use As Long
    unit(2) As String
    tunit(2) As String
    dunit(2) As String
    nts As Long
    series() As entry
    numprog As Long
    prog() As sProgeny
End Type

Type Specie
    name As String
    sciname As String
    numcon As Long
    use As Boolean
    con() As sParent
End Type

Type BBFLoc
    lbl As String
    name As String
    typ As String
    use As Boolean
    numspecie As Long
    specs() As Specie
End Type

Type progeny
    name As String
    cas As String
    kind As Long
End Type

Type parent
    name As String
    cas As String
    kind As Long
    numprog As Long
    prog() As progeny
End Type

Type location
    lbl As String
    name As String
    typ As String
    use As Boolean
End Type

Global prevtime As Double
Global bnflag As Boolean
Global numloc As Long
Global loc() As BBFLoc
Global numcon As Long
Global con() As parent
Global specname() As String
Global specsciname() As String
Global numexp As Long
Global numspec As Long

Public Sub AssignSeries(ser1 As entry, ser2 As entry)
    ser1.time = ser2.time
    ser1.valu = ser2.valu
    ser1.valu2 = ser2.valu2
End Sub

Public Sub AssignProgeny(prog1 As sProgeny, prog2 As sProgeny)
    Dim i As Long
    prog1.cas = prog2.cas
    prog1.ref = prog2.ref
    prog1.use = prog2.use
    prog1.unit(1) = prog2.unit(1)
    prog1.unit(2) = prog2.unit(2)
    prog1.tunit(1) = prog2.tunit(1)
    prog1.tunit(2) = prog2.tunit(2)
    prog1.dunit(1) = prog2.dunit(1)
    prog1.dunit(2) = prog2.dunit(2)
    prog1.nts = prog2.nts
    ReDim prog1.series(prog1.nts) As entry
    For i = 1 To prog1.nts
        AssignSeries prog1.series(i), prog2.series(i)
    Next
End Sub

Public Sub AssignParent(par1 As sParent, par2 As sParent)
    Dim i As Long
    par1.cas = par2.cas
    par1.ref = par2.ref
    par1.use = par2.use
    par1.unit(1) = par2.unit(1)
    par1.unit(2) = par2.unit(2)
    par1.tunit(1) = par2.tunit(1)
    par1.tunit(2) = par2.tunit(2)
    par1.dunit(1) = par2.dunit(1)
    par1.dunit(2) = par2.dunit(2)
    par1.nts = par2.nts
    par1.numprog = par2.numprog
    ReDim par1.series(par1.nts) As entry
    ReDim par1.prog(par1.numprog) As sProgeny
    For i = 1 To par1.nts
        AssignSeries par1.series(i), par2.series(i)
    Next
    For i = 1 To par1.numprog
        AssignProgeny par1.prog(i), par2.prog(i)
    Next
End Sub
Public Sub AssignSpecie(spec1 As Specie, spec2 As Specie)
    Dim i As Long
    spec1.name = spec2.name
    spec1.sciname = spec2.sciname
    spec1.numcon = spec2.numcon
    spec1.use = spec2.use
    ReDim spec1.con(spec1.numcon) As sParent
    For i = 1 To spec1.numcon
        AssignParent spec1.con(i), spec2.con(i)
    Next
End Sub
