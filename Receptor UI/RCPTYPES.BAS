Attribute VB_Name = "DataTypes"

Type exprec
  name As String
  typ(7) As Long
End Type

Public refs() As Long
Global UsedCustomized As Boolean   'lets dose know whether or not customization
Global act(4) As String
Global tf(4) As String
Global af(2) As String
Global paths(25) As String


Sub InitConstants()
   
  act(1) = "showering"
  act(2) = "swimming"
  act(3) = "soil contact"
  act(4) = "shoreline sediment contact"

  tf(1) = "leafy vegetables"
  tf(2) = "other vegetables"
  tf(3) = "meat"
  tf(4) = "milk"
  
  af(1) = "finned fish"
  af(2) = "shell fish"

  paths(1) = "ingestion of drinking water"
  paths(2) = "shower dermal contact"
  paths(3) = "ingestion of water while showering"
  paths(4) = "ingestion of leafy vegetables"
  paths(5) = "ingestion of other vegetables"
  paths(6) = "ingestion of meat"
  paths(7) = "ingestion of milk"
  paths(8) = "ingestion of finned fish"
  paths(9) = "ingestion of shell fish"
  paths(10) = "ingestion of water while swimming"
  paths(11) = "swimming dermal contact"
  paths(12) = "shoreline dermal contact"
  paths(13) = "ingestion of shoreline sediment"
  paths(14) = "ingestion of soil"
  paths(15) = "soil dermal contact"
  paths(16) = "ingestion of special foods"
  paths(17) = "shower or indoor air inhalation"
  paths(18) = "air inhalation"
  paths(19) = "soil resuspension inhalation"
  paths(20) = "swimming external exposure"
  paths(21) = "boating external exposure"
  paths(22) = "shoreline external exposure"
  paths(23) = "soil external exposure"
  paths(24) = "air external exposure"
  paths(25) = "shower or indoor air inhalation"

End Sub

