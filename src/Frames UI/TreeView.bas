Attribute VB_Name = "TreeFunc"
Option Explicit
'
' Brad Martinez, http://www.mvps.org/ccrp/
'

'''Declare Function SendMessage Lib "user32" Alias "SendMessageA" _
'''                            (ByVal hwnd As Long, _
'''                            ByVal wMsg As Long, _
'''                            wParam As Any, _
'''                            lParam As Any) As Long   ' <---

' ===========================================================================
' treeview definitions defined in Commctrl.h at:
' http://premium.microsoft.com/msdn/library/sdkdoc/c67_4c8m.htm


'Const used for treeview mnu positioning
'Public Const SM_CXDLGFRAME = 7

Public Enum CBoolean
  CFalse = 0
  CTrue = 1
End Enum

Public Type TVITEM   ' was TV_ITEM
  mask As Long
  hItem As Long
  state As Long
  stateMask As Long
  pszText As String   ' Long   ' pointer
  cchTextMax As Long
  iImage As Long
  iSelectedImage As Long
  cChildren As Long
  lParam As Long
End Type

Public Enum TVITEM_mask
    TVIF_TEXT = &H1
    TVIF_IMAGE = &H2
    TVIF_PARAM = &H4
    TVIF_STATE = &H8
    TVIF_HANDLE = &H10
    TVIF_SELECTEDIMAGE = &H20
    TVIF_CHILDREN = &H40
#If (WIN32_IE >= &H400) Then   ' WIN32_IE = 1024 (>= Comctl32.dll v4.71)
    TVIF_INTEGRAL = &H80
#End If
    TVIF_DI_SETITEM = &H1000   ' Notification
End Enum

' User-defined as the maximum treeview item text length.
' If an items text exceeds this value when calling GetTVItemText
' there could be problems...
Public Const MAX_ITEM = 256

' messages
Public Const TV_FIRST = &H1100
Public Const TVM_GETNEXTITEM = (TV_FIRST + 10)
Public Const TVM_GETITEM = (TV_FIRST + 12)
Public Const TVM_GETITEMRECT = (TV_FIRST + 4)

' TVM_GETNEXTITEM wParam values
Public Enum TVGN_Flags
    TVGN_ROOT = &H0
    TVGN_NEXT = &H1
    TVGN_PREVIOUS = &H2
    TVGN_PARENT = &H3
    TVGN_CHILD = &H4
    TVGN_FIRSTVISIBLE = &H5
    TVGN_NEXTVISIBLE = &H6
    TVGN_PREVIOUSVISIBLE = &H7
    TVGN_DROPHILITE = &H8
    TVGN_CARET = &H9
#If (WIN32_IE >= &H400) Then   ' >= Comctl32.dll v4.71
    TVGN_LASTVISIBLE = &HA
#End If
End Enum


' Returns the text of the specified treeview item if successful,
' returns an empty string otherwise.

'   hwndTV      - treeview's window handle
'   hItem          - item's handle whose text is to be to returned
'   cbItem        - length of the specified item's text.

Public Function GetTVItemText(hwndTV As Long, _
                                                  hItem As Long, _
                                                  Optional cbItem As Long = MAX_ITEM) As String
  Dim tvi As TVITEM
  
  ' Initialize the struct to retrieve the item's text.
  tvi.mask = TVIF_TEXT
  tvi.hItem = hItem
  tvi.pszText = String$(cbItem, 0)
  tvi.cchTextMax = cbItem
  
  If TreeView_GetItem(hwndTV, tvi) Then
    GetTVItemText = GetStrFromBufferA(tvi.pszText)
  End If

End Function

' Returns the string before first null char encountered (if any) from an ANSII string.

Public Function GetStrFromBufferA(sz As String) As String
  If InStr(sz, vbNullChar) Then
    GetStrFromBufferA = Left$(sz, InStr(sz, vbNullChar) - 1)
  Else
    ' If sz had no null char, the Left$ function
    ' above would return a zero length string ("").
    GetStrFromBufferA = sz
  End If
End Function

' If successful, returns the very first node in the specified treeview,
' returns Nothing otherwise.

' Equates to retrieving the hItem of the treeview's first item with
' TVM_GETNEXTITEM/TVGN_ROOT. (Treeview1.Nodes(1)
' or Treeview1.Nodes(1).Root do *not* necessarily return the
' treeview's very first node!)

Public Function GetFirstTVNode(objTV As TreeView) As Node
  Dim nod As Node
  On Error GoTo NoNodes
  
  ' Will err here if there are no treeview nodes
  Set nod = objTV.Nodes(1)
  
  ' Get the first node's highest parent node
  Do While (nod.Parent Is Nothing) = False
    Set nod = nod.Parent
  Loop
  
  ' Return the highest parent node's first sibling
  Set GetFirstTVNode = nod.FirstSibling
  
NoNodes:
End Function

' If successful, returns an object reference to a node object from
' the specified treeview item handle, returns Nothing otherwise.

Public Function GetNodeFromTVItem(objTV As TreeView, _
                                                            ByVal hItem As Long) As Node
  Dim hwndTV As Long
  Dim anSiblingPos() As Integer   ' contains the sibling position of the item and all it's parents
  Dim nLevel As Integer               ' hierarchical level of the item
  Dim nod As Node
  Dim i As Integer
  Dim nPos As Integer
  On Error GoTo Out
  
  hwndTV = objTV.hWnd
  
  ' Continually work backwards from the current item to the current item's
  ' first sibling, caching the current item's sibling position in the one-based
  ' array, Then get the first sibling's parent item and start over. Keep going
  ' until the position of the specified item's top level parent item is obtained.
  Do While hItem
    nLevel = nLevel + 1
    ReDim Preserve anSiblingPos(nLevel)
    anSiblingPos(nLevel) = GetTVItemSiblingPos(hwndTV, hItem)
    hItem = TreeView_GetParent(hwndTV, hItem)
  Loop
  
  ' Get the first treeview node.
  Set nod = GetFirstTVNode(objTV)
  If (nod Is Nothing) = False Then
  
    ' Now work backwards through the cached item positions in the array
    ' (from the first treeview item to the specified item), obtaining the respective
    ' object reference for each item at the cached position. When we get to the
    ' specified item's position (the value of the first element in the array), we
    ' got it's node...
    For i = nLevel To 1 Step -1
      nPos = anSiblingPos(i)
      
      Do While nPos > 1
        Set nod = nod.Next
        nPos = nPos - 1
      Loop
      
      If (i > 1) Then Set nod = nod.Child
    Next
    
    Set GetNodeFromTVItem = nod
  End If
  
Out:
End Function

' Returns the one-base position of the specified item
' with respect to it's sibling order.

Public Function GetTVItemSiblingPos(hwndTV As Long, _
                                                            ByVal hItem As Long) As Integer
  Dim nPos As Integer
  
  ' Keep counting up from one until the item has no more previous siblings
  Do While hItem
    nPos = nPos + 1
    hItem = TreeView_GetPrevSibling(hwndTV, hItem)
  Loop
  
  GetTVItemSiblingPos = nPos
  
End Function

' If successful, returns the treeview item handle represented by
' the specified Node, returns 0 otherwise.

Public Function GetTVItemFromNode(hwndTV As Long, _
                                                            nod As Node) As Long
  Dim nod1 As Node
  Dim anSiblingPos() As Integer  ' contains the sibling position of the node and all it's parents
  Dim nLevel As Integer              ' hierarchical level of the node
  Dim hItem As Long
  Dim i As Integer
  Dim nPos As Integer

  Set nod1 = nod

  ' Continually work backwards from the current node to the current node's
  ' first sibling, caching the current node's sibling position in the one-based
  ' array. Then get the first sibling's parent node and start over. Keep going
  ' until the postion of the specified node's top level parent item is obtained...
  Do While (nod1 Is Nothing) = False
    nLevel = nLevel + 1
    ReDim Preserve anSiblingPos(nLevel)
    anSiblingPos(nLevel) = GetNodeSiblingPos(nod1)
    Set nod1 = nod1.Parent
  Loop

  ' Get the hItem of the first item in the treeview
  hItem = TreeView_GetRoot(hwndTV)
  If hItem Then

    ' Now work backwards through the cached node positions in the array
    ' (from the first treeview node to the specified node), obtaining the respective
    ' item handle for each node at the cached position. When we get to the
    ' specified node's position (the value of the first element in the array), we
    ' got it's hItem...
    For i = nLevel To 1 Step -1
      nPos = anSiblingPos(i)
      
      Do While nPos > 1
        hItem = TreeView_GetNextSibling(hwndTV, hItem)
        nPos = nPos - 1
      Loop
      
      If (i > 1) Then hItem = TreeView_GetChild(hwndTV, hItem)
    Next

    GetTVItemFromNode = hItem

  End If   ' hItem

End Function

' Returns the one-base position of the specified node
' with respect to it's sibling order.

Public Function GetNodeSiblingPos(nod As Node) As Integer
  Dim nod1 As Node
  Dim nPos As Integer
  
  Set nod1 = nod
  
  ' Keep counting up from one until the node has no more previous siblings
  Do While (nod1 Is Nothing) = False
    nPos = nPos + 1
    Set nod1 = nod1.Previous
  Loop
  
  GetNodeSiblingPos = nPos
  
End Function

' ===========================================================================
' Treeview macros defined in Commctrl.h

' Retrieves some or all of a tree-view item's attributes.
' Returns TRUE if successful or FALSE otherwise.

Public Function TreeView_GetItem(hWnd As Long, pitem As TVITEM) As Boolean
  TreeView_GetItem = SendMessage(hWnd, TVM_GETITEM, 0, pitem)
End Function

' TreeView_GetNextItem

' Retrieves the tree-view item that bears the specified relationship to a specified item.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetNextItem(hWnd As Long, hItem As Long, flag As Long) As Long
  TreeView_GetNextItem = SendMessage(hWnd, TVM_GETNEXTITEM, ByVal flag, ByVal hItem)
End Function

' Retrieves the first child item. The hitem parameter must be NULL.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetChild(hWnd As Long, hItem As Long) As Long
  TreeView_GetChild = TreeView_GetNextItem(hWnd, hItem, TVGN_CHILD)
End Function

' Retrieves the next sibling item.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetNextSibling(hWnd As Long, hItem As Long) As Long
  TreeView_GetNextSibling = TreeView_GetNextItem(hWnd, hItem, TVGN_NEXT)
End Function

' Retrieves the previous sibling item.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetPrevSibling(hWnd As Long, hItem As Long) As Long
  TreeView_GetPrevSibling = TreeView_GetNextItem(hWnd, hItem, TVGN_PREVIOUS)
End Function

' Retrieves the parent of the specified item.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetParent(hWnd As Long, hItem As Long) As Long
  TreeView_GetParent = TreeView_GetNextItem(hWnd, hItem, TVGN_PARENT)
End Function

' Retrieves the first visible item.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetFirstVisible(hWnd As Long) As Long
  TreeView_GetFirstVisible = TreeView_GetNextItem(hWnd, 0, TVGN_FIRSTVISIBLE)
End Function

' Retrieves the next visible item that follows the specified item. The specified item must be visible.
' Use the TVM_GETITEMRECT message to determine whether an item is visible.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetNextVisible(hWnd As Long, hItem As Long) As Long
  TreeView_GetNextVisible = TreeView_GetNextItem(hWnd, hItem, TVGN_NEXTVISIBLE)
End Function

' Retrieves the first visible item that precedes the specified item. The specified item must be visible.
' Use the TVM_GETITEMRECT message to determine whether an item is visible.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetPrevVisible(hWnd As Long, hItem As Long) As Long
  TreeView_GetPrevVisible = TreeView_GetNextItem(hWnd, hItem, TVGN_PREVIOUSVISIBLE)
End Function

' Retrieves the currently selected item.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetSelection(hWnd As Long) As Long
  TreeView_GetSelection = TreeView_GetNextItem(hWnd, 0, TVGN_CARET)
End Function

' Retrieves the item that is the target of a drag-and-drop operation.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetDropHilight(hWnd As Long) As Long
  TreeView_GetDropHilight = TreeView_GetNextItem(hWnd, 0, TVGN_DROPHILITE)
End Function

' Retrieves the topmost or very first item of the tree-view control.
' Returns the handle to the item if successful or 0 otherwise.

Public Function TreeView_GetRoot(hWnd As Long) As Long
  TreeView_GetRoot = TreeView_GetNextItem(hWnd, 0, TVGN_ROOT)
End Function

' Retrieves the bounding rectangle for a tree-view item and indicates whether the item is visible.
' If the item is visible and retrieves the bounding rectangle, the return value is TRUE.
' Otherwise, the TVM_GETITEMRECT message returns FALSE and does not retrieve
' the bounding rectangle.

Public Function TreeView_GetItemRect(hWnd As Long, hItem As Long, prc As RECT, fItemRect As CBoolean) As Boolean
  prc.Left = hItem
  TreeView_GetItemRect = SendMessage(hWnd, TVM_GETITEMRECT, ByVal fItemRect, prc)
End Function



