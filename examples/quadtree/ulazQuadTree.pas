{ a general Quad-Tree for Objects
  Copyright (C) 2023,2024 Ekkehard Domning (www.domis.de)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Version Date        Change
  0.0.1   2023-04-27  First release
  0.0.2   2024-05-25  Bugfixes, changes in visibility of virtual methods
  0.0.3   2024-05-29  Caching for NodeItem-Positions and Area
}
unit ulazQuadTree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

const
  { DefaultMaxQuadTreeLevel: Each level quaters the area of the level above.
    Subsequently the one Node in Level 25 contains, if used as an earth map, roughly one squaremeter (at the equator).
    Keep in mind that the amount of Items that *could* be stored increases very fast.
    The default is about 1280 TerraItems, far beyond what can be actually processed.
    Used as initial value of the MaxQuadTreeLevel property of the QuadTree. }
  DefaultMaxQuadTreeLevel = 25;
  { DefaultMaxQuadNodeItemCount: If a node contrains more items it will be split in subnodes.
    Used as initial value of the MaxQuadNodeItemCount property of the QuadTree.
    if the items are close together, this will be continued until MaxLazQuadTreeLevels is reached}
  DefaultMaxQuadNodeItemCount = 4;

type
  { TLazQuadTreeYAxisDirection distinguish between the cartesian and the windows coordinate system
    qxdUpPlus: Indicates that the Y-Axis-Values increases when moving upwards. This is used in maps or mathematical
      functions.
    qxdUpMinus: Inicates that the Y-Axis-Values decreases when moving upwards. This is used in Editor (LineNumbers!) or
      Rastergraphics like Bitmaps or Window positions in GUIs}
  TLazQuadTreeYAxisDirection = (qydUpPlus, qydUpMinus);

  { TLazQuadNodePoint describes the a single point in the QuadTree-World }
  TLazQuadNodePoint = record
    X : Double;
    Y : Double;
  end;

  { TLazQuadNodeArea describes an area in the QuadTree-World
    The Terms of Left, Top, Right, Bottom are used as seen on a sheet of paper.
    So "Left" must be left of "Right" and "Top" has to be upper than "Bottom".
    If "Right" and "Left" are equal or "Top" and "Bottom" are equal, than the
    covered Area is zero.
    To allow a seamless und unique tessellation of a greater area, the
    fields "Right" and "Bottom" are not included in the area, while "Left" and "Top" does!
    Example:

       0 10 20
     0 +--*--+      Left, Top, Right, Bottom
       |A |B |   A:    0,   0,    10,     10
    10 +--+--+   B:   10,   0,    20,     10

    The two areas are not overlapping, even they share the common X-coordinate of "10"!
    Testing the Point (*) X: 10, Y: 0 on both areas will be false on A, but true on B.

    The surface of the area is assumed to be flat, or more precise,
    could be flattened without distortion of the surface.
    The values of the fields are interpreted in the context of two assumptions:
    1.) The orientation of the Y-Axis and
    2.) The X-Axis forming a flat or a cylindrical world.
    This two parameters could be combined.
    Details for 1.): The orientation of the Y-Axis could be either
     - mathematical (The values of X increases while moving in the direction "Top",
                     saying that the value of "Top" is larger than "Bottom".
                     This is used by cartesian systems, like maps or mathematical
                     function) or
     - computer-coordinate-system (The values of X increases while moving in the
                     direction "Bottom", saying that the value of "Top" is smaller
                     than "Bottom".
                     This is used for Bitmaps or Editors).
    Details for 2.):
     - In the "flat world" the value of "Right" must be always larger than "Left"
    (if this rule is violated, some functions simply swap the values or using the
    Abs(...)-function to normalize the values and continue without notification).
     - The cylindrical world is used for mapping world coordinates to the NodeArea,
    allowing rectangular areas to cross the east-west-border. Those rectangles are
    defined by the "Left" value are larger than the "Right" value. To calculate
    the with of the area, an additional parameter is needed: The width of the world.
    This value has to be passed to the related functions or are derived from the
    setting in the RootNode of the tree, which contains the size of the world.
    It is mandatory for a cylindrical world to be symetrical around zero!
  }
  TLazQuadNodeArea = record
    Left   : Double; {or West border}
    Top    : Double; {or North border}
    Right  : Double; {or East border}
    Bottom : Double; {or South border}
  end;

  { TLazQuadNodeAreaOverlap kind of hit between the node-area with a test-area }
  TLazQuadNodeAreaOverlap = (
    qaoNone   {no hit},
    qaoPartly {the node-area is partly covered by the test-area},
    qaoInside {the test-area is located inside the node-area},
    qaoEqual  {the test-area and node-area are equal},
    qaoFull   {the test-area covers the node-area fully}
  );

  { TLazQuadTreeChildLocation the Index of the four ChildNodes}
  TLazQuadTreeChildLocation = (qclNE,qclSE,qclSW,qclNW); //Clockwise

  TLazQuadTree = class;

  { TLazQuadTreeNodeItemsRec is used by the Node to store the Items and chache there positions }
  TLazQuadTreeNodeItemsRec = record
    Item : TObject;
    PointValid : Boolean;
    AreaValid : Boolean;
    Point : TLazQuadNodePoint;
    Area : TLazQuadNodeArea;
  end;

  { TLazQuadNode
    A node in the Tree.
    The node is assumed to be the root node, if the ParentNode is not assigned.
    The node can contain data items. These data items are generic TObjects.
    There actual Coordinates are fetched via an Event in the Tree
  }
  TLazQuadNode = class(TObject)
  private
    FNodeLevel : Integer;
    FNodeArea : TLazQuadNodeArea;
    FOwner : TLazQuadTree;
    FParentNode : TLazQuadNode;
    FChildNodes : array[TLazQuadTreeChildLocation] of TLazQuadNode;
    FNodeItems : array of TLazQuadTreeNodeItemsRec;
    FTotalNodeItemsCount : Integer;
    FNodeTouched : Boolean;
    { CoordIsInArea tests whether the point AX, AY is within the area.
      If no Owner is assigned, the World is assumed as not cylindrical and
      the YAxis is increasing upwards }
    procedure SetTotalNodeItemsCount(Value : Integer);
    { DoFetchItemCoords performs the Notification to get the Item Coords, if not already cached
      The result ist true if at least on of the two coords (point or area) the valid }
    function DoFetchItemCoords(const AItemIndex : Integer) : Boolean;
  protected
    function GetNodeItemsLength : Integer;virtual;
    procedure SetNodeItemsLength(Value : Integer);virtual;
    function GetNodeItems(Index : Integer) : TObject;virtual;
    procedure SetNodeItems(Index : Integer; Value : TObject);virtual;
    function GetAssignedNodeItemsCount : Integer;virtual;
    function GetChildNodes(Index : TLazQuadTreeChildLocation) : TLazQuadNode;virtual;
    function GetChildAreas(Index : TLazQuadTreeChildLocation) : TLazQuadNodeArea;virtual;
  public
    { Owner: The Tree this node belongs to }
    property Owner : TLazQuadTree read FOwner;
    { ParentNode: The node, where this node is one of the four ChildNodes.
      If Nil, this node is the RootNode of the Tree }
    property ParentNode : TLazQuadNode read FParentNode;
    { ChildNodes: Indexed array of the four ChildNode. Each Node covers one quadrant of the
      area of this node }
    property ChildNodes[Index : TLazQuadTreeChildLocation] : TLazQuadNode read GetChildNodes;
    property ChildNodeNE : TLazQuadNode index qclNE read GetChildNodes;
    property ChildNodeSE : TLazQuadNode index qclSE read GetChildNodes;
    property ChildNodeSW : TLazQuadNode index qclSW read GetChildNodes;
    property ChildNodeNW : TLazQuadNode index qclNW read GetChildNodes;
    { ChildAreas: Contains the definition of the for quadrants (=areas) of the ChildNodes.
      The virtual getter function GetChildAreas might be overwritten in derived classes to
      implement a different distribution to adapt to different projections.
    }
    property ChildAreas[Index : TLazQuadTreeChildLocation] : TLazQuadNodeArea read GetChildAreas;
    { NodeLevel: The depth of this node. 0 for the RootNode }
    property NodeLevel : Integer read FNodeLevel;
    { NodeArea: The area covered by this node.
      Care mus be taken, by interpreting the values of the area.
      Depending of the configuration of the YAxisDirection-property of the related tree, the value
      Top is greater or lesser than Bottom.
      Right is per definition always greater than Left.
      Even if the XAxisIsCylindrical-property of the related tree is set,
      the NodeArea is guaranteed never passing the East-West-Border.
    }
    property NodeArea : TLazQuadNodeArea read FNodeArea;
    property Left : Double read FNodeArea.Left;
    property Top : Double read FNodeArea.Top;
    property Right : Double read FNodeArea.Right;
    property Bottom : Double read FNodeArea.Bottom;
    { TotalNodeItemsCount: Contains the maintained Number of assigned Items of
      this Node and all Child-nodes to the bottom }
    property TotalNodeItemsCount : Integer read FTotalNodeItemsCount write SetTotalNodeItemsCount;
    { NodeItemsLength contains the Length of the NodeItems array.
      Caution: When writing a smaller value than the current length, the shortened Items are freed,
      if the related Tree owns the objects! }
    property NodeItemsLength : Integer read GetNodeItemsLength write SetNodeItemsLength;
    { NodeItems gives a raw access to the NodeItems array.
      Caution: overwriting existing Items will not free them, this has to be done manually if needed! }
    property NodeItems[Index : Integer] : TObject read GetNodeItems write SetNodeItems;
    { AssignedNodeItemsCount gives the number of assigned items in this node. }
    property AssignedNodeItemsCount : Integer read GetAssignedNodeItemsCount;
    { CreateOrGetChildNode creates a childnode unless one exists in this location.
       The result is the new created or the existing ChildNode from this position. }
    function CreateOrGetChildNode(const AChildLocation : TLazQuadTreeChildLocation) : TLazQuadNode;
    { DeleteChildNode deletes the childnode (if existing) and all subchildnodes.
      The function moves all NodeItems from the deleted ChildNodes to this Node. }
    procedure DeleteChildNode(const AChildLocation : TLazQuadTreeChildLocation);
    { CoordIsInNodeArea returns true if the Point AX, AY is located within the NodeArea. }
    function CoordIsInNodeArea(const AX, AY : Double) : Boolean;virtual;
    { NodeAreaOverlapTest returns the Test result of the comparism of AArea with the NodeArea. }
    function NodeAreaOverlapTest(const AArea : TLazQuadNodeArea) : TLazQuadNodeAreaOverlap;virtual;
    { NodeAreaOverlap returns true, if AArea overlap at least a part a same area. }
    function NodeAreaOverlap(const AArea : TLazQuadNodeArea) : Boolean;virtual;
    { AddNodeItem insert the AItem into the a node items array.
      The optional parameters could be used to speed up the processing
        APointValid, AAreaValid, APointX, APointY
        AAreaLeft, AAreaTop, AAreaRight, AAreaBottom
    }
    function AddNodeItem(const AItem : TObject;
                         const APointValid : Boolean = False;
                         const AAreaValid : Boolean = False;
                         const APointX : Double = 0.0;
                         const APointY : Double = 0.0;
                         const AAreaLeft : Double = 0.0;
                         const AAreaTop : Double = 0.0;
                         const AAreaRight : Double = 0.0;
                         const AAreaBottom : Double = 0.0
      ) : Integer;virtual;
    { ExtractNodeItem removes an Item from the internal storage, the current field in the Items-array ae set to Nil }
    function ExtractNodeItem(const AItemIndex : Integer) : TObject;
    { SplitNode split the node into child nodes if necesarry. Recurses down with splitting, if necesarry.
      Returns true, if the node is splitted. }
    function SplitNode : Boolean;virtual;
    { PackNode tries to remove unused ChildNodes. Recurses down with packing, if useful.
      Return true if at least one node was combined }
    function PackNode : Boolean;virtual;
    { NodeIsEmpty recurses through all subnodes and counts all populated NodeItems.
      If there are none, the result is true. }
    function NodeIsEmpty : Boolean;virtual;
    {HasChildNodes returns true if at least one of the four possible ChildNodes exists }
    function HasChildNodes : Boolean;
    { NodeItemIndex returns the Index of the passed Item, -1 if not found}
    function NodeItemIndex(const AItem : TObject) : Integer;
    { PackNodeItems removes unused and duplicate items. }
    procedure PackNodeItems;virtual;
    { NodeAsString returns some information of the current node in a string }
    function NodeAsString : String;virtual;
    { NodeAsStrings returns the content of the Node and its children in
      readable form for debugging purpose. }
    procedure NodeAsStrings(const AStrings : TStrings);virtual;
    { Clear recursivly delete all Childnodes. If the related Tree OwnsObjects,
      all containing Items are freed }
    procedure Clear;
    { Create the Node within the owning Tree, a Parent and an covered area.
      Caution: It is not allowed that Parent- and Child-node have different Owners.
      Caution: It is assumed that the NodeArea contains a part of the Area of the Parent,
      in a way, that the areas of the four children covers exactly the area of the parent node.
      Saying this, Create will be used in very rare cases outside this unit. }
    constructor Create(const AOwner : TLazQuadTree; const AParentNode : TLazQuadNode; const ANodeArea : TLazQuadNodeArea);
    { Destroy frees the Node. Clear is called prior inherited destroy, causing all
      Items are freed, if the owning tree owns the items, and all child nodes are freed. }
    destructor Destroy;override;
    // Debugging only
    // procedure SetNodeArea(const ANodeArea : TLazQuadNodeArea);
  end;

  { TLazQuadTreeGetItemCoordEvent
    An event which has to return an X and a Y Coordinate for the given ItemIndex.
    Return True if the return values AX and AY are valid, False if not. }
  TLazQuadTreeGetItemCoordEvent = function (Sender : TObject; AItem : TObject; var AX, AY : Double) : Boolean of object;
  { TLazQuadTreeGetItemAreaEvent
    An event which may return an area which contains the entire item. If the item has no
    area, the Event should return false.
    Return True if the return value AItemArea is valid, False if not. }
  TLazQuadTreeGetItemAreaEvent = function (Sender : TObject; AItem : TObject; var AItemArea : TLazQuadNodeArea) : Boolean of object;
  { TLazQuadTreeGetItemCaptionEvent
    An event which can return a Caption (title, descriptive string) of the item.
    If the item has not such a Caption than the result shlould return false
    Return True if the Caption is valid, False if not. }
  TLazQuadTreeGetItemCaptionEvent = function (Sender : TObject; AItem : TObject; var ACaption : String) : Boolean of object;
  { TLazQuadTreeCreateQuadNodeEvent
    An Event which has to return a new created QuadNode. This event could be used to insert derived
    nodes into the QuadTree. Probably rarely used. }
  TLazQuadTreeCreateQuadNodeEvent = function (AOwner : TLazQuadTree;
                                               AParentNode : TLazQuadNode;
                                               ANodeArea : TLazQuadNodeArea) : TLazQuadNode of object;
  { TLazQuadTreeGetNodeChildAreaEvent
    This event is called whenever a queried ChildArea Size is variable, e.g. not fixed
    by other childrens positions. It allows to modify the suggested ChildArea.
    The purpose is to shift the borders between the newly created children so that a
    uniform distribution of the items between the nodes are possible. If using non point
    items (e.g. Shapes of countries, tracks) a good selection of borders, help to avoid
    inefficent long list of Items at a high level. On a globe, the 0°-Longitude crosses a
    long list of countries, which by default all will be placed on the root node.
    By moving the division to -25° this could be reduced to one country.
    Caution: Asking the AParentNode for other ChildAreas may cause a recursive call of this event!
    Asking the ParentNode for its NodeArea is always possible since this Area is fixed.
    The parameters AChangeAllowedTop, ... , ...Left indicates, which of the field of the
    AChildArea could be modified. Modification of the not allowed fields are ignored.
  }
  TLazQuadTreeGetNodeChildAreaEvent = procedure (AOwner : TLazQuadTree;
                                               AParentNode : TLazQuadNode;
                                               AChildLocation : TLazQuadTreeChildLocation;
                                               AChangeAllowedTop, AChangeAllowedRight, AChangeAllowedBottom, AChangeAllowedLeft : Boolean;
                                               var AChildArea : TLazQuadNodeArea) of object;
  { TLazQuadTreeNodeEnumerationCallBack
    This event is used in the enumeration function. It will be called for each found
    Node.
    AQuadNode contains the found node.
    The parameter AUserData is passed unchanged from the function call and
    may be used by the callback-function to identify data, avoiding the usage of
    global variable.}
  TLazQuadTreeNodeEnumerationCallBack = function (Sender : TObject;
                                                    AQuadNode : TLazQuadNode;
                                                    AUserData : Pointer) : Boolean of object;
  { TLazQuadTreeItemEnumerationCallBack
    This event is used in the enumeration function. It will be called for each found
    Item.
    AQuadNode contains the node of the Item.
    ANodeItem contains the Item.
    ANodeItemIndex contains the index of the ANodeItem in the NodeItems-Array.
    The parameter AUserData is passed unchanged from the function call and
    may be used by the callback-function to identify data, avoiding the usage of
    global variable.}
  TLazQuadTreeItemEnumerationCallBack = function (Sender : TObject;
                                                    AQuadNode : TLazQuadNode;
                                                    ANodeItem : TObject;
                                                    ANodeItemIndex : Integer;
                                                    AUserData : Pointer) : Boolean of object;

  { TLazQuadTree
    A QuadTree for Lazarus.
    Organizes arbitrary Items who are ordered on a plane by X and Y coordinates for
    a fast access for a given area.
    The TLazQuadTree could be used to store tenthousands of items and access them very fast,
    because items that are not within the desired area are sorted out quickly.
    To allow the processing of any kind of objects or data, an event is used
    where the user has to provide the location or area of a specific item in the tree.
    The tree could be used for a flat world with borders on all four edges or
    a cylindric world, where the right and left edge are connected. Additionally the
    orientation of the Y-Axis could be adjusted, to cover cartesian-coordinate-systems
    (like Maps or mathematical function diagrams, where the Y value increases when going upwards) or
    computer-coordinate-system (like Bitmaps or Editors, where the Y increases when going downwards).
    To aggregate data on smaller magnification (e.g. showing bigger parts of the plane)
    a sum of TotalItems in the (Sub-)Nodes is maintained. This allowes, instead of a
    search of a bunch of unusefull Items, the display of a Placeholder/Sum of the
    items which are hidden.
  }
  TLazQuadTree = class(TObject)
  private
    FRootNode : TLazQuadNode;
    FOwnsObjects: Boolean;
    FUpdateCount : Integer;
    FYAxisDirection : TLazQuadTreeYAxisDirection;
    FXAxisIsCylindrical : Boolean;
    FMaxQuadTreeLevel : Integer;
    FMaxQuadNodeItemCount : Integer;
    FGetItemCoordEvent : TLazQuadTreeGetItemCoordEvent;
    FGetItemAreaEvent : TLazQuadTreeGetItemAreaEvent;
    FGetItemCaptionEvent : TLazQuadTreeGetItemCaptionEvent;

    FQuadTreeCreateQuadNodeEvent : TLazQuadTreeCreateQuadNodeEvent;
  protected
    {InternalCreateQuadNode creates a new node.
       If FQuadTreeCreateQuadNodeEvent is assigned, it is called to allow
       Users to create Nodes of a derived class.
       If this failed, a Node of the type TLazQuadNode is created and returned.
    }
    function InternalCreateQuadNode(const AParentNode : TLazQuadNode;
                                    const ANodeArea : TLazQuadNodeArea) : TLazQuadNode;virtual;
    // The following "Internal" methods, implements the recursive variants of
    // the non-"Internal" methods in the public section.
    // E.g. the public ListNodesInArea will call the private InternalListNodesInArea
    // with the RootNode as the parameter
    function InternalEnumerateNodesInArea(
                                    const ANode : TLazQuadNode;
                                    const ANodeArea : TLazQuadNodeArea;
                                    const AEnumCallBackFunc : TLazQuadTreeNodeEnumerationCallBack;
                                    const AUserData : Pointer) : Boolean;virtual;

    procedure InternalListNodesInArea(
                                    const ANode : TLazQuadNode;
                                    const ANodeArea : TLazQuadNodeArea;
                                    const AList : TList);virtual;
    function InternalEnumerateItemsInArea(
                                    const ANode : TLazQuadNode;
                                    const ANodeArea : TLazQuadNodeArea;
                                    const AEnumCallBackFunc : TLazQuadTreeItemEnumerationCallBack;
                                    const AUserData : Pointer) : Boolean;virtual;

    procedure InternalListItemsInArea(
                                    const ANode : TLazQuadNode;
                                    const ANodeArea : TLazQuadNodeArea;
                                    const AList : TList);virtual;

    function InternalCountNodesInArea(const ANode : TLazQuadNode;
                                                 const ANodeArea : TLazQuadNodeArea) : Integer;virtual;
    function InternalCountAssignedNodeItemsInArea(const ANode : TLazQuadNode;
                                                 const ANodeArea : TLazQuadNodeArea) : Integer;virtual;
    function InternalEstimatedCountOfAssignedItemsInArea(const ANode : TLazQuadNode;
                                                 const ATestArea : TLazQuadNodeArea) : Double;virtual;
    { DoGetItemCoord: Fetches the coordinates for the given AItem.
      The coordinates (AX, AY) are valid if the return value is true.
      The function calls the assigned GetItemCoordEvent.
      Derived class may overwrite the method and implement a different approach to get the coordinates. }
    function DoGetItemCoord(AItem : TObject; var AX, AY : Double) : Boolean;virtual;

    { DoGetItemArea: Fetches the covered area for the given AItem.
      The AItemArea are valid if the return value is true.
      The function calls the assigned GetItemAreaEvent.
      Derived class may overwrite the method and implement a different approach to get the area. }
    function DoGetItemArea(AItem : TObject; var AItemArea : TLazQuadNodeArea) : Boolean;virtual;

    { DoGetItemCaption: Fetches the Caption of the given AItem.
      If the AItemCaption is valid, the result is True.
      The function call the assigned GetItemCaptionEvent.}
    function DoGetItemCaption(AItem : TObject; var AItemCaption : String) : Boolean;virtual;
  public
    { YAxisDirection: distinguish between the to possibilities, Top is greater than Bottom (cartesian)
      or Bottom is greater than Top (Window, Editor) }
    property YAxisDirection : TLazQuadTreeYAxisDirection read FYAxisDirection;
    { XAxisIsCylindrical: If True the right end of the world is connected to the left end }
    property XAxisIsCylindrical : Boolean read FXAxisIsCylindrical;
    { OwnsObjects: True if the Tree is the owner of the added NodeItems, so that they will be freed
        on destroying the tree. }
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    { RootNode: The RootNode where the tree is build below }
    property RootNode : TLazQuadNode read FRootNode;
    { MaxQuadNodeItemCount: This is the amount of NodeItems one Node could hold. If the amount of NodeItems
        increases over this amount, the Node is split and the items are distributed to the new children.
        This value is ignored for Nodes at the deepest level, since this nodes are not allowed to split.
        A too small value will create a lot of nodes, with no Items in.
        A too big value will reduce the performance, since the items are processed like
        an unordered list. The value has a big performance impact depending of the desired
        functionality. The default value is 4}
    property MaxQuadNodeItemCount : Integer read FMaxQuadNodeItemCount write FMaxQuadNodeItemCount default DefaultMaxQuadNodeItemCount;
    { MaxQuadTreeLevel: This is the maximum depth the tree could have. By default the value is 25.
      Keep in mind that the amount of Items that *could* be stored increases very fast.
      The default is about 1280 TerraItems, and a value of 32 exceeds the range of the used Integer variable.
      The value should be choosen, so that the median distance of the used items,
      will distribute them into different nodes.
      A too small value may lead into the situation, where to many items are collected
      in one node at the maximum depth, reducing the performance back to an unordered list.
      A too big value instead, creates a tree with only few, but very long branches,
      causing a lot of running around in empty nodes. }
    property MaxQuadTreeLevel : Integer read FMaxQuadTreeLevel write FMaxQuadTreeLevel default DefaultMaxQuadTreeLevel;
    { OnGetItemCoord: The Event that is called to get the location of a specific NodeItem in the tree.
        If instantiate this class this Event *must* be assigned, since the Tree
        needs the get the coordinates of the Items to get the tree organized.
        A derived class of the tree may overwrite the DoGetItemCoord method to
        collect the coordinates of the items direct. }
    property OnGetItemCoord : TLazQuadTreeGetItemCoordEvent read FGetItemCoordEvent write FGetItemCoordEvent;
    { OnGetItemArea: The Event that is called to get the covered area of a specific NodeItem in the tree.
        This Event may be not assigned, if only point-like NodeItems are used.
        If NodeItems are covering an area, this event must be assigned. }
    property OnGetItemArea : TLazQuadTreeGetItemAreaEvent read  FGetItemAreaEvent write FGetItemAreaEvent;
    { OnGetItemCaption: The Event is called to get a descriptive caption for a specific item in the tree.
        This event may be not assigned if items have no caption or the caption is not used.}
    property OnGetItemCaption : TLazQuadTreeGetItemCaptionEvent read FGetItemCaptionEvent write FGetItemCaptionEvent;
    { OnQuadTreeCreateQuadNode: This Event could be assigned to create descendants
        classes of the TLazQuadNode to be used by the tree.
        Under nomal circumstances it remains unassigned. }
    property OnQuadTreeCreateQuadNode : TLazQuadTreeCreateQuadNodeEvent read FQuadTreeCreateQuadNodeEvent write FQuadTreeCreateQuadNodeEvent;
    { InsertItem insert the Item into the tree.
      The optional parameters could be used to speed up the processing
        APointValid, AAreaValid, APointX, APointY
        AAreaLeft, AAreaTop, AAreaRight, AAreaBottom
      The function returns the node where the item was inserted.
      If the item is already in the tree, the containing node is returned.
      If the node could not be inserted, the result is Nil!
      The tree might be reorganized if necesarry and possible, thus some or all
      Items in the tree may be moved in different nodes or indices than before this call. }
    function InsertItem(const AItem : TObject;
                        const APointValid : Boolean = False;
                        const AAreaValid : Boolean = False;
                        const APointX : Double = 0.0;
                        const APointY : Double = 0.0;
                        const AAreaLeft : Double = 0.0;
                        const AAreaTop : Double = 0.0;
                        const AAreaRight : Double = 0.0;
                        const AAreaBottom : Double = 0.0
                       ) : TLazQuadNode;virtual;
    { ExtractItem removes an Item from the Tree.
      The function returns the removed Item.
      On any other case the result is Nil!
      The tree might be reorganized if necesarry and possible, thus Items may be moved
      to different nodes than they located before this call. }
    function ExtractItem(const AItem : TObject) : TObject;virtual;
    { MoveItemAfterCoordChange moves the given Item to a new Node, reflecting an
      updated internal position.
      This may cause the relocation of the Item and a restructure of the tree. }
    function MoveItemAfterCoordChange(const AItem : TObject) : TLazQuadNode;virtual;
    { FindAreaNode returns the deepest (leaf)node which contains (or is equal to) the ANodeArea. }
    function FindAreaNode(const ANodeArea : TLazQuadNodeArea) : TLazQuadNode;virtual;
    { FindCoordNode returns the deepest (leaf)node which contains the coordinate. }
    function FindCoordNode(const AX, AY : Double) : TLazQuadNode;virtual;
    { FindItemNode returns the Node where the Item is located.
      The Tree tries to locate the Item by its coords, if this fails, the full tree
      is searched. So it is very time consuming looking for Items that are not in the tree.
      If the item is not found the result is Nil. }
    function FindItemNode(const AItem : TObject) : TLazQuadNode;virtual;
    { EnumerateNodesInNodeArea enumerates all Nodes in the Tree who are at least partly covered by
      the passed ANodeArea. For each found node, the passed callback function is called.
      Since the RootNode contains the whole world, the callback is called always at least once.
      See the TLazQuadTreeNodeEnumerationCallBack defintion for more details.
      AUserData is a generic Pointer, containing any data what the caler likes.
      The value is unchanged passed to the callback. This allows the usage of local
      variables as a context to be used in the callback, avoiding the usage ob global
      variables. }
    function EnumerateNodesInArea(const ANodeArea : TLazQuadNodeArea;
                                  const AEnumCallBackFunc : TLazQuadTreeNodeEnumerationCallBack;
                                  const AUserData : Pointer = Nil) : Boolean;virtual;
    { ListNodesInNodeArea lists all Nodes in the Tree who are at least partly covered by
      the passed ANodeArea.
      Since the RootNode contains the whole world, the List will contain always
      at least one Node.
      The AList must be created prior and freed after the call by the using code. }
    procedure ListNodesInArea(const ANodeArea : TLazQuadNodeArea;
                                  const AList : TList);virtual;
    { CountNodesInArea returns the number of Nodes in the passed area.
      The function will return the precise count of nodes in the given area.
      This function has to traverse the covered parts of the tree, while nodes
      outside the ANodeArea are completely ignored. }
    function CountNodesInArea(const ANodeArea : TLazQuadNodeArea) : Integer;virtual;
    { EnumerateItemsInArea enumerates all Nodes in the Tree, who are at least partly covered by
      the passed ANodeArea. For each found node, the passed callback function is called.
      Since the RootNode contains the whole world, the callback is called always at least once.
      See the definition of TLazQuadTreeItemEnumerationCallBack for more details.
      AUserData is a generic Pointer, containing any data what the caler likes.
      The value is unchanged passed to the callback. This allows the usage of local
      variables as a context to be used in the callback, avoiding the usage ob global
      variables. }
    function EnumerateItemsInArea(const ANodeArea : TLazQuadNodeArea;
                                    const AEnumCallBackFunc : TLazQuadTreeItemEnumerationCallBack;
                                    const AUserData : Pointer = Nil) : Boolean;virtual;
    { ListItemsInArea lists all Items in the Tree who are inside the passed ANodeArea.
      The AList must be created prior and freed after the call by the using code.}
    procedure ListItemsInArea(const ANodeArea : TLazQuadNodeArea;
                                  const AList : TList);virtual;
    { CountAssignedItemsInNodeArea returns the number of items in the passed area.
        This method returns the exact number of items in the area.
        If the area is large, a lot of items are queried for there position
        and the method may takes her time. If only an estimation
        is wanted use the faster EstimatedCountOfAssignedItemsInArea instead. }
    function CountAssignedItemsInArea(const ANodeArea : TLazQuadNodeArea) : Integer;virtual;
    { EstimatedCountOfAssignedItemsInArea returns the estimated amount of Items in the passed area.
        This estimation is very fast, since no items are queried for there positions.
        Only the TotalNodeItemsCount is used for the estimation and the fraction of the passed area.
        The use of this method is to decide quickly, wether to many items in an area exists, so
        that only a placeholder should be displayed, instead of an bunch of overlapping items.
        A result greater than 0 will not guaranty that items are really exists in the
        passed area. If a node is covering a large area, but all items sit in a small region,
        than a mismatch between the reality and the estimation occour.
        A threshold of approx. 4 times MaxQuadNodeItemCount will ensure that more than one node is taken
        into account. A smaller value should counted by using CountAssignedItemsInArea, to
        get an exact number}
    function EstimatedCountOfAssignedItemsInArea(const ANodeArea : TLazQuadNodeArea) : Double;virtual;
    { TreeAsStrings returns the content of the Tree in a readable form for debugging purpose. }
    procedure TreeAsStrings(const AStrings : TStrings);virtual;
    { ListSubNodes recurses through the ANode and all SubNodes and stores them in the passed list.
        The AList must be created prior and freed after the call by the using code.}
    procedure ListSubNodes(const ANode : TLazQuadNode;
                           const AList : TList);virtual;
    { CountSubNodes returns the number of SubNodes of the passed ANode (inclusive).
        If ANode is Nil, the result is 0, if ANode has no children, the result is 1}
    function CountSubNodes(const ANode : TLazQuadNode) : Integer; virtual;

    { EnumerateItemsInSubNodes recurses through the ANode and all SubNodes and
        calls the callback function once for every found Item.
        See the definition of TLazQuadTreeItemEnumerationCallBack for details.
        AUserData is a generic Pointer, containing any data what the caller likes. }
    function EnumerateItemsInSubNodes(const ANode : TLazQuadNode;
                           const AEnumCallBackFunc : TLazQuadTreeItemEnumerationCallBack;
                           const AUserData : Pointer = Nil) : Boolean;virtual;
    { ListItemsInSubNodes recurses through the ANode and all SubNodes and
        stores the found Items in the passed list.
        The AList must be created prior and freed after the call by the using code.}
    procedure ListItemsInSubNodes(const ANode : TLazQuadNode;
                           const AList : TList);virtual;
    { NextNode will return the next node in the tree, behind the given node.
        If the passed node is empty, the RootNode is returned. If StartNode is the last node,
        than Nil is returned. }
    function NextNode(const AStartNode : TLazQuadNode = Nil) : TLazQuadNode;
    { RecountTotalNodeItems iterate through all Nodes and Items and refreshes the Nodes TotalNodeItemsCount.
        This method might be useful after a manually reconfiguration of the Tree. }
    procedure RecountTotalNodeItems; virtual;
    { Clear, empties the QuadTree, frees all Nodes and if OwnsObjects is true Items are freed too }
    procedure Clear; virtual;
    { BeginUpdate initiate a BeginUpdate..EndUpdate sequences. If inside this sequence splitting and
        packing nodes and packing node items are suppressed. BeginUpdate..EndUpdate calls could be nested.
        Caution: BeginUpdate..EndUpdate slows down the adding of many items to the tree, because all new
                 Items are stored in the root node!}
    procedure BeginUpdate;
    { EndUpdate finalize a BeginUpdate..EndUpdate sequences. If the last EndUpdate call from a BeginUpdate
        sequence is reached, the RootNode is "Splitted" and "Packed" (including cleaning up the Items) and
        this is recursed through the whole tree.}
    procedure EndUpdate;
    { Create: Creates the Tree.
        AWorldSize defines the used world. if AXAxisIsCylindrical is True (this is the default),
         the Right-value should be the negative of the Left-value, to allow a seamless wrap around of
         the NodeItems.
        AFreeObjects defines whether the Tree owns the objects (and frees them, when needed) or
         if the NodeItems are held somewhere else and the NodeItems are only references.
        AXAxisIsCylindrical defines whether the World is flat, like a piece of paper (=False) or
         wraping around like a cylinder (=True, the default).
        AYAxisDirection defines whether Top is larger than Bottom (= qydUpPlus, the default) or
         Top is smaller than Bottom (=qydUpMinus). The first case is used for maps or cartesian
         projections, the second for Bitmaps or Editors
        AQuadTreeCreateQuadNodeEvent could be passed to allow the creation of derived classes of
         Nodes, which than extend or replace certain functionallities than the default Node-class.
         Usually this parameter is not used (=Nil)
    }
    constructor Create(const AWorldSize : TLazQuadNodeArea;
                       const AFreeObjects: Boolean = False;
                       const AXAxisIsCylindrical : Boolean = True;
                       const AYAxisDirection : TLazQuadTreeYAxisDirection = qydUpPlus;
                       const AQuadTreeCreateQuadNodeEvent : TLazQuadTreeCreateQuadNodeEvent = Nil);
    { Destroy frees the Tree. It frees the RootNode and subsequently all data in the tree.}
    destructor Destroy;override;
  end;

{ NodeAreaWidth calculates the width of the passed area, keeping in that the world might be
    cylindrical }
function NodeAreaWidth(const AArea: TLazQuadNodeArea;
                       const AAreaWorld : TLazQuadNodeArea;
                       const AXAxisIsCylindrical : Boolean = False) : Double;

{ CoordIsInArea returns True if the Coordinate AX,AY is located within the AArea}
function CoordIsInArea(const AX, AY: Double;
    const AArea: TLazQuadNodeArea;
    const AYAxisDirection : TLazQuadTreeYAxisDirection;
    const AXAxisIsCylindrical : Boolean;
    const AAreaWorld : TLazQuadNodeArea): Boolean;

{ NodeAreasAreEqual returns true if the two passed areas are equal }
function NodeAreasAreEqual(const AAreaA, AAreaB : TLazQuadNodeArea) : Boolean;

{ NormalizeAndUnfoldArea limits and correct the passed Area to the criteria given by the parameters.
  If the X-Axis is cylindrical, then
   - the leftside is increased by the width of the earth, if the left value is smaller than the right,
   - the Area is moved to the right, if the Area is crossing the East/West-Border.
  The top and bottom Values are limited to the maximum extension of the "World", taking care to
  the direction of thy Y-Axis }
procedure NormalizeAndUnfoldArea(var AUnfoldArea : TLazQuadNodeArea;
                                 const AYAxisDirection : TLazQuadTreeYAxisDirection;
                                 const AXAxisIsCylindrical : Boolean;
                                 const AAreaWorld : TLazQuadNodeArea);


{ NodeAreaHit returns true, if the both areas overlapping }
function NodeAreaOverlap(const AAreaBase, AAreaTest : TLazQuadNodeArea;
                             const AYAxisDirection : TLazQuadTreeYAxisDirection;
                             const AXAxisIsCylindrical : Boolean;
                             const AAreaWorld : TLazQuadNodeArea) : Boolean;
{ NodeAreaHitTest returns the Test result of the comparism of AAreaBase with the AAreaTest }
function NodeAreaOverlapTest(const AAreaBase, AAreaTest : TLazQuadNodeArea;
                             const AYAxisDirection : TLazQuadTreeYAxisDirection;
                             const AXAxisIsCylindrical : Boolean;
                             const AAreaWorld : TLazQuadNodeArea) : TLazQuadNodeAreaOverlap;

{ NodeAreaOverlapTestEx returns the Test result of the comparism of AAreaBase with the AAreaTest.
  The additional input parameters are
  AYAxisDirection : If Top values are greater or smaller than the Bottom values.
  AXAxisIsCylindrical : If the world is flat or cylindrical closed.
  AAreaWorld : The world.
  The additional output parameters are
  AOverlappingArea : The overlapping area, valid if the return code is <> qaoNone.
  ASecondaryOverlappingAreaValid : Is true if a secondary overlapping area exists.
    This parameter can only become true if AXAxisIsCylindrical is true.
  ASecondaryOverlappingArea : If ASecondaryOverlappingAreaValid, the secondary overlapping area.

  Under which circumstances a secondary overlapping area exists?
  Example:
    The Y-Axis is assumed to reach from the very north to the very south,
    and could be ignored in the explanation.
    The AAreaBase covers: Dublin    (X: -7) to Hamburg   (X: +10).
    The AreaTest covers:  Amsterdam (X: +5) to Liverpool (X:  -3).
    Be aware that the AreaTest is very long and reaches around the east-west-border.
    So there exists one overlapping area from Dublin (X: -7) to Liverpool (X: -3) and
    a second one from
                            Dublin  Livpl.   Amstd.  Hamburg
              -180 =========== -7 == -3 ===== +5 ==== +10 ================ 180
    AAreaBase                  BBBBBBBBBBBBBBBBBBBBBBBBBB
    AAreaTest TTTTTTTTTTTTTTTTTTTTTTTTT       TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    AOverlappingArea                          OOOOOOOOOOO
    ASecondaryOverlappingArea  22222222
}
function NodeAreaOverlapTestEx(const AAreaBase, AAreaTest : TLazQuadNodeArea;
                               const AYAxisDirection : TLazQuadTreeYAxisDirection;
                               const AXAxisIsCylindrical : Boolean;
                               const AAreaWorld : TLazQuadNodeArea;
                               var AOverlappingArea : TLazQuadNodeArea;
                               var ASecondaryOverlappingAreaValid : Boolean;
                               var ASecondaryOverlappingArea : TLazQuadNodeArea) : TLazQuadNodeAreaOverlap;



const
  LazQuadChildLocationStrings : array[TLazQuadTreeChildLocation] of String = (
    'NorthEast', 'SouthEast', 'SouthWest', 'NorthWest');

implementation

function NodeAreaWidth(const AArea: TLazQuadNodeArea;
  const AAreaWorld: TLazQuadNodeArea; const AXAxisIsCylindrical: Boolean
  ): Double;
var
  ww : Double;
begin
  ww := AAreaWorld.Right - AAreaWorld.Left;
  Result := FMod(AArea.Right - AArea.Left, ww);
  if AXAxisIsCylindrical then
  begin
    if Result < 0 then
      Result := Result + ww;
  end;
  if Result > ww then
    Result := ww;
end;

function CoordIsInArea(const AX, AY: Double;
  const AArea: TLazQuadNodeArea;
  const AYAxisDirection : TLazQuadTreeYAxisDirection;
  const AXAxisIsCylindrical : Boolean;
  const AAreaWorld : TLazQuadNodeArea): Boolean;
var
  ww : Double;
  a : TLazQuadNodeArea;
  lx : Double;
begin
  Result := (AX >= AArea.Left) and (AX < AArea.Right);
  if (not Result) and
     AXAxisIsCylindrical then
  begin // The world is round
    ww := Abs(AAreaWorld.Left - AAreaWorld.Right);  // Width of the world
    a := AArea;
    // If left ist larger then right...
    if (AArea.Left > AArea.Right) then
      // ...crossing the east west border
      a.Right := a.Right+ww; //Move the right corner to the right (=greater) side
    // Iterate with the test value around the world until
    // it is beyond the right border of test area
    lx := AX;
    repeat
      Result := (lx >= a.Left) and (lx < a.Right);
      if Result then Break;
      lx := lx + ww;
    until lx > (a.Right + ww);
  end;
  if Result then
  begin
    if AYAxisDirection = qydUpMinus then
      Result := (AY >= AArea.Top) and (AY < AArea.Bottom)
    else
      Result := (AY <= AArea.Top) and (AY > AArea.Bottom);
  end;
end;

function NodeAreasAreEqual(const AAreaA, AAreaB : TLazQuadNodeArea) : Boolean;
const
  prec = 1.0E-15;
var
  epsl, epsr, epst, epsb : Double;
begin
  epsl := Math.Max(Abs(AAreaA.Left), Abs(AAreaB.Left)) * prec;
  epsr := Math.Max(Abs(AAreaA.Right), Abs(AAreaB.Right)) * prec;
  epst := Math.Max(Abs(AAreaA.Top), Abs(AAreaB.Top)) * prec;
  epsb := Math.Max(Abs(AAreaA.Bottom), Abs(AAreaB.Bottom)) * prec;
  Result :=
    (Abs(AAreaA.Left - AAreaB.Left) <= epsl) and
    (Abs(AAreaA.Right - AAreaB.Right) <= epsr) and
    (Abs(AAreaA.Top - AAreaB.Top) <= epst) and
    (Abs(AAreaA.Bottom - AAreaB.Bottom) <= epsb);
end;

procedure NormalizeAndUnfoldArea(var AUnfoldArea : TLazQuadNodeArea;
                                 const AYAxisDirection : TLazQuadTreeYAxisDirection;
                                 const AXAxisIsCylindrical : Boolean;
                                 const AAreaWorld : TLazQuadNodeArea);
var
  ww : Double;
begin
  ww := Abs(AAreaWorld.Left - AAreaWorld.Right);
  if AXAxisIsCylindrical then
  begin
    AUnfoldArea.Left:= Math.FMod(AUnfoldArea.Left,ww);
    AUnfoldArea.Right:= Math.FMod(AUnfoldArea.Right,ww);
    if Abs(AUnfoldArea.Left - AUnfoldArea.Right) >= ww then
    begin
      AUnfoldArea.Left:= AAreaWorld.Left;
      AUnfoldArea.Right:= AAreaWorld.Right;
    end;
    if AUnfoldArea.Right <= AUnfoldArea.Left then
      AUnfoldArea.Right := AUnfoldArea.Right + ww;
    // Normalize the bounding rectangle, back to WorldSize
    while AUnfoldArea.Right > AAreaWorld.Right do
    begin
      AUnfoldArea.Left := AUnfoldArea.Left-ww;
      AUnfoldArea.Right := AUnfoldArea.Right-ww;
    end;
    // But ensure that Left is always within the world base coordinate system
    while AUnfoldArea.Left < AAreaWorld.Left do
    begin
      AUnfoldArea.Left := AUnfoldArea.Left+ww;
      AUnfoldArea.Right := AUnfoldArea.Right+ww;
    end;
  end
  else
  begin
    AUnfoldArea.Left := Math.EnsureRange(AUnfoldArea.Left,AAreaWorld.Left, AAreaWorld.Right);
    AUnfoldArea.Right := Math.EnsureRange(AUnfoldArea.Right,AAreaWorld.Left, AAreaWorld.Right);
  end;
  if AYAxisDirection = qydUpPlus then
  begin
    AUnfoldArea.Top := Math.EnsureRange(AUnfoldArea.Top,AAreaWorld.Bottom, AAreaWorld.Top);
    AUnfoldArea.Bottom := Math.EnsureRange(AUnfoldArea.Bottom, AAreaWorld.Bottom, AAreaWorld.Top);
  end
  else
  begin
    AUnfoldArea.Top := Math.EnsureRange(AUnfoldArea.Top,AAreaWorld.Top, AAreaWorld.Bottom);
    AUnfoldArea.Bottom := Math.EnsureRange(AUnfoldArea.Bottom,AAreaWorld.Top, AAreaWorld.Bottom);
  end;
end;


function NodeAreaOverlap(const AAreaBase, AAreaTest: TLazQuadNodeArea;
  const AYAxisDirection: TLazQuadTreeYAxisDirection;
  const AXAxisIsCylindrical: Boolean; const AAreaWorld: TLazQuadNodeArea
  ): Boolean;
begin
  Result := NodeAreaOverlapTest(AAreaBase, AAreaTest,
                                  AYAxisDirection, AXAxisIsCylindrical,
                                  AAreaWorld) <> qaoNone;
end;

function NodeAreaOverlapTest(const AAreaBase, AAreaTest: TLazQuadNodeArea;
  const AYAxisDirection: TLazQuadTreeYAxisDirection;
  const AXAxisIsCylindrical: Boolean; const AAreaWorld: TLazQuadNodeArea
  ): TLazQuadNodeAreaOverlap;
var
  lOA1, lOA2 : TLazQuadNodeArea;
  b : Boolean;
begin
  b := False;
  lOA1.Left := 0;
  lOA1.Right := 0;
  lOA1.Top := 0;
  lOA1.Bottom := 0;
  lOA2.Left := 0;
  lOA2.Right := 0;
  lOA2.Top := 0;
  lOA2.Bottom := 0;
  Result := NodeAreaOverlapTestEx(AAreaBase, AAreaTest,
                                  AYAxisDirection, AXAxisIsCylindrical,
                                  AAreaWorld,
                                  lOA1, b, lOA2);
end;

function NodeAreaOverlapTestEx(const AAreaBase, AAreaTest: TLazQuadNodeArea;
  const AYAxisDirection: TLazQuadTreeYAxisDirection;
  const AXAxisIsCylindrical: Boolean; const AAreaWorld: TLazQuadNodeArea;
  var AOverlappingArea: TLazQuadNodeArea;
  var ASecondaryOverlappingAreaValid: Boolean;
  var ASecondaryOverlappingArea: TLazQuadNodeArea): TLazQuadNodeAreaOverlap;
{ After a long work with dealing with a lot of strange cases,
  math does the work - at least easier.
  But the function is still much complexer than I had expected
}

  function BoundingArea(const AAreaA, AAreaB : TLazQuadNodeArea) : TLazQuadNodeArea;
  var
    laa, lab : TLazQuadNodeArea;
    ww : Double;
  begin
    if AXAxisIsCylindrical then
    begin
      ww := Abs(AAreaWorld.Left - AAreaWorld.Right);
      laa := AAreaA;
      lab := AAreaB;
      // Now the extension could be calculated
      Result.Left := Min(laa.Left, lab.Left);
      Result.Right := Max(laa.Right, lab.Right);
      Result.Left := Min(laa.Left,lab.Left);
      Result.Right := Max(laa.Right,lab.Right);
      // If the right corner is outside the base word rotate in
      while Result.Right > AAreaWorld.Right do
      begin
        Result.Left := Result.Left-ww;
        Result.Right := Result.Right-ww;
      end;
      // But ensure, that the left side is always within the base world system
      while Result.Left < AAreaWorld.Left do
      begin
        Result.Left := Result.Left+ww;
        Result.Right := Result.Right+ww;
      end;
      // If the width of the bounding rectangle is larger than the world
      if (Result.Right - Result.Left) >= ww then
      begin
        Result.Right := AAreaWorld.Right;
        Result.Left :=  AAreaWorld.Left;
      end;
    end
    else
    begin
      Result.Left := Min(AAreaA.Left, AAreaB.Left);
      Result.Right := Max(AAreaA.Right, AAreaB.Right);
    end;
    if AYAxisDirection = qydUpPlus then
    begin
      Result.Top := Math.Max(AAreaA.Top, AAreaB.Top);
      Result.Bottom := Math.Min(AAreaA.Bottom, AAreaB.Bottom);
    end
    else
    begin
      Result.Top := Math.Min(AAreaA.Top, AAreaB.Top);
      Result.Bottom := Math.Max(AAreaA.Bottom, AAreaB.Bottom);
    end;
  end;

  procedure NormalizeOverlappingArea(var AOA : TLazQuadNodeArea);
  var
    ww : Double;
  begin
    if AXAxisIsCylindrical then
    begin
      ww := Abs(AAreaWorld.Left - AAreaWorld.Right);
      // Normalize the very long around the word areas
      while (AOA.Right - AOA.Left) > ww do
        AOA.Left := AOA.Left+ww;
      // Normalize the bounding rectangle, back to WorldSize
      // If the right corner is outside the base word rotate in
      while AOA.Right > AAreaWorld.Right do
      begin
        AOA.Left := AOA.Left-ww;
        AOA.Right := AOA.Right-ww;
      end;
      // But ensure, that the left side is always within the base world system
      while AOA.Left < AAreaWorld.Left do
      begin
        AOA.Left := AOA.Left+ww;
        AOA.Right := AOA.Right+ww;
      end;
    end;
  end;


var
  lab, lat, labounds : TLazQuadNodeArea;
  ww, wb, hb, wt, ht, wbounds, hbounds : Double;
  // areab, areat, areabounds : Double;
  wo, ho, wox : Double;
begin
  ASecondaryOverlappingAreaValid := False;
  ASecondaryOverlappingArea.Left := 0;
  ASecondaryOverlappingArea.Right := 0;
  ASecondaryOverlappingArea.Top := 0;
  ASecondaryOverlappingArea.Bottom := 0;

  // Prepare the value, normalize, unfold and correct if needed
  lab := AAreaBase;
  lat := AAreaTest;
  NormalizeAndUnfoldArea(lab, AYAxisDirection, AXAxisIsCylindrical, AAreaWorld);
  NormalizeAndUnfoldArea(lat, AYAxisDirection, AXAxisIsCylindrical, AAreaWorld);
  if AXAxisIsCylindrical then
  begin
    ww := Abs(AAreaWorld.Left - AAreaWorld.Right);
    // If the distance between the west an east border of the
    // both areas are greater than the worldsize, than
    // a loop around is not resolved
    // Move the more "western" area by a worldsize to the right
    while (lab.Right-lat.Left) > ww do
    begin
      lat.Left := lat.Left + ww;
      lat.Right := lat.Right + ww;
    end;
    // Move the more "eastern" area by a worldsize to the left
    while (lat.Right-lab.Left) > ww do
    begin
      lab.Left := lab.Left + ww;
      lab.Right := lab.Right + ww;
    end;
    // Now proof the width of both areas, and correct if needed
    wt := lat.Right - lat.Left;
    wb := lab.Right - lab.Left;
    if wt >= ww then
    begin
      lat.Right := AAreaWorld.Right;
      lat.Left :=  AAreaWorld.Left;
    end;
    if wb >= ww then
    begin
      lab.Right := AAreaWorld.Right;
      lab.Left :=  AAreaWorld.Left;
    end;
  end;
  // The equal case is simple, sort it out first
  if NodeAreasAreEqual(lab,lat) then
  begin
    AOverlappingArea := lab;
    NormalizeOverlappingArea(AOverlappingArea);
    Result := qaoEqual;
    Exit;
  end;
  // If not equal, but the BaseArea is equal to the World then, per definition, Test-Area must be fully included
  if NodeAreasAreEqual(lab,AAreaWorld) then
  begin
    AOverlappingArea := lat;
    NormalizeOverlappingArea(AOverlappingArea);
    Result := qaoInside;
    Exit;
  end;
  // If not equal, but the Test-Area is the equal to the world, the BaseArea must be fully included
  if NodeAreasAreEqual(lat,AAreaWorld) then
  begin
    AOverlappingArea := lab;
    NormalizeOverlappingArea(AOverlappingArea);
    Result := qaoFull;
    Exit;
  end;

  // So from here
  // - the Rectangles are different
  // - we are not talking about covering the full sphere

  // First we calculate the heights and width of the two rect
  // additionally the bounding rectangle and its height and width.
  // Having this we could sort out all annoing cases.
  // If the bounding is eqal to one of the rect the other must be part of it
  wb := lab.Right - lab.Left;
  hb := Abs(lab.Top - lab.Bottom);
  wt := lat.Right - lat.Left;
  ht := Abs(lat.Top - lat.Bottom);
  // Init labounds only to keep the compiler calm
  labounds.Top:=0;
  labounds.Bottom:=0;
  labounds.Right:=0;
  labounds.Left:=0;
  // Get the bounding rectangle, including the wrap around stuff
  labounds := BoundingArea(lab, lat);
  // if the bounding area is equal to the Base-rect, then the test-rect must be inside
  if NodeAreasAreEqual(labounds,lab) then
  begin
    AOverlappingArea := lat;
    NormalizeOverlappingArea(AOverlappingArea);
    Result := qaoInside;
    Exit;
  end;
  // if the bounding area is equal to the test-rect, then the base-rect must be inside
  if NodeAreasAreEqual(labounds, lat) then
  begin
    AOverlappingArea := lab;
    NormalizeOverlappingArea(AOverlappingArea);
    Result := qaoFull;
    Exit;
  end;
  // calculate the width and height and area of the bounding box
  wbounds := Abs(labounds.Right - labounds.Left);
  hbounds := Abs(labounds.Top - labounds.Bottom);
  // Calculate the width and height of the overlapping area.
  // zero or a negative value indicates that there is no overlapping
  wo := wb + wt - wbounds;
  ho := hb + ht - hbounds;

  if (wo <= 0) or
     (ho <= 0) then
  begin
    // Obviously the length of the boundingbox sides are longer than the sum of the sides
    // so they could not overlap
    AOverlappingArea.Top := 0;
    AOverlappingArea.Right := 0;
    AOverlappingArea.Bottom := 0;
    AOverlappingArea.Left := 0;
    Result := qaoNone;
    Exit;
  end;

  Result := qaoPartly;
  // If the width of the bounding box is equal to the width of one of the rectangles
  // obviously the rectangle is located neither left or right.
  // The same case is if the height of the bounding box is equal to the height of one of the rectangles
  // Those cases could be sorted out
  if (hb = hbounds) then
  begin // height of BaseRect is equal to the bounds, so TestRect is fully inside
    AOverlappingArea.Top := lat.Top;
    AOverlappingArea.Bottom := lat.Bottom;
  end
  else if (ht = hbounds) then
  begin // height of TestRect is equal to the bounds, so BaseRect is fully inside
    AOverlappingArea.Top := lab.Top;
    AOverlappingArea.Bottom := lab.Bottom;
  end
  // The rectangles overlapping, so that one is the upper, and one is the lower one
  else if AYAxisDirection = qydUpPlus then
  begin
    if lab.Top < lat.Top then
      // Test area is upper of base area
      // So Top of overlapping must be the top-side of the base-area
      AOverlappingArea.Top := lab.Top
    else
      // Test area is lower of base area
      // So Top of overlapping must be the top-side of the test-area
      AOverlappingArea.Top := lat.Top;
    // Since there is no secondary area possible in top-down direction, we can use
    // the calculated height of the overlapping area
    AOverlappingArea.Bottom := AOverlappingArea.Top - ho;
  end
  else
  begin
    if lab.Top > lat.Top then
      // Test area is upper of base area
      // So Top of overlapping must be the top-side of the base-area
      AOverlappingArea.Top := lab.Top
    else
      // Test area is lower of base area
      // So Top of overlapping must be the top-side of the test-area
      AOverlappingArea.Top := lat.Top;
    // Since there is no secondary area possible in top-down direction, we can use
    // the calculated height of the overlapping area
    AOverlappingArea.Bottom := AOverlappingArea.Top + ho;
  end;

  // Same for right and left
  if (wb = wbounds) then
  begin // width of BaseRect is equal to the bounds, so TestRect is fully inside
    AOverlappingArea.Left := lat.Left;
    AOverlappingArea.Right:= lat.Right;
  end
  else if (wt = wbounds) then
  begin // width of TestRect is equal to the bounds, so BaseRect is fully inside
    AOverlappingArea.Left := lab.Left;
    AOverlappingArea.Right := lab.Right;
  end
  // Distinguish which Area is left and which is right
  else
  begin
    if lab.Left < lat.Left then
      // Test area is right of base area
      // So Left of overlapping must be the left-side of the test-area
      AOverlappingArea.Left := lat.Left
    else
      // Test area is left of base area
      // So Left of overlapping must be the left-side of the base-area
      AOverlappingArea.Left := lab.Left;
    if lab.Right < lat.Right then
      // Test area is left of base area
      // So Right of overlapping must be the right-side of the base-area
      AOverlappingArea.Right := lab.Right
    else
      // Test area is right of base area
      // So Right of overlapping must be the right-side of the tes-area
      AOverlappingArea.Right := lat.Right;
  end;

  // Now catch the case that there might be a secondary overlapping zone
  wox := AOverlappingArea.Right-AOverlappingArea.Left;
  // If the width of the first overlapping zone is smaller than the total width of the calculated overlapping zone
  if wox < wo then
  begin
    ASecondaryOverlappingAreaValid := True;
    ASecondaryOverlappingArea.Top := AOverlappingArea.Top;
    ASecondaryOverlappingArea.Bottom := AOverlappingArea.Bottom;
    // Distinguish which Area is left and which is right
    if lab.Left < lat.Left then
      // Test area is right of base area
      // So Left of overlapping must be the left-side of the base-area
      ASecondaryOverlappingArea.Left := lab.Left
    else
      // Test area is left of base area
      // So Left of overlapping must be the left-side of the test-area
      ASecondaryOverlappingArea.Left := lat.Left;
    // Right must be the left plus the remaining width of the calculated overlapping zone
    ASecondaryOverlappingArea.Right := ASecondaryOverlappingArea.Left + (wo-wox);

    NormalizeOverlappingArea(ASecondaryOverlappingArea);
  end;
  NormalizeOverlappingArea(AOverlappingArea);
end;

{ TLazQuadNode }

function TLazQuadNode.GetNodeItemsLength: Integer;
begin
  Result := Length(FNodeItems);
end;

procedure TLazQuadNode.SetNodeItemsLength(Value: Integer);
var
  len, i : Integer;
  o : TObject;
begin
  // If the length of the Item-array should be shortended
  // extract the Items in this part of the array and free them if the Tree own them.
  len := Length(FNodeItems);
  if Assigned(FOwner) and FOwner.OwnsObjects and
    (Value < len) then
  begin
    for i := Value to len-1 do
    begin
      o := ExtractNodeItem(i);
      if Assigned(o) then
        o.Free;
    end;
  end;
  SetLength(FNodeItems,Value);
end;

function TLazQuadNode.GetNodeItems(Index: Integer): TObject;
begin
  Result := FNodeItems[Index].Item;
end;

procedure TLazQuadNode.SetNodeItems(Index: Integer; Value: TObject);
begin
  FNodeItems[Index].Item := Value;
end;

function TLazQuadNode.GetAssignedNodeItemsCount: Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to High(FNodeItems) do
    if Assigned(FNodeItems[i].Item) then
      Inc(Result);
end;

function TLazQuadNode.GetChildNodes(Index: TLazQuadTreeChildLocation
  ): TLazQuadNode;
begin
  Result := FChildNodes[Index];
end;

function TLazQuadNode.GetChildAreas(Index: TLazQuadTreeChildLocation
  ): TLazQuadNodeArea;
var
  lLLA : TLazQuadNodeArea;
  w2, h2 : Double;
  lw2, th2 : Double;
  ydir : Double;
begin
  if Assigned(FChildNodes[Index]) then
    Result := FChildNodes[Index].NodeArea
  else
  begin
    if Fowner.YAxisDirection = qydUpPlus then
      ydir := 1.0
    else
      ydir := -1.0;
    lLLA := FNodeArea;
    w2 := Abs(lLLA.Left - lLLA.Right) / 2.0;
    h2 := Abs(lLLA.Top - lLLA.Bottom) / 2.0;
    lw2 := lLLA.Left + w2;
    th2 := lLLA.Top - (h2*ydir);
    case Index of
      qclNE :
        begin
          lLLA.Left   := lw2;
          lLLA.Bottom := th2;
        end;
      qclSE :
        begin
          lLLA.Left := lw2;
          lLLA.Top  := th2;
        end;
      qclSW :
        begin
          lLLA.Right := lw2;
          lLLA.Top   := th2;
        end;
      qclNW :
        begin
          lLLA.Right  := lw2;
          lLLA.Bottom := th2;
        end;
    end;
    Result := lLLA;
  end;
end;

procedure TLazQuadNode.SetTotalNodeItemsCount(Value: Integer);
var
  delta : Integer;
begin
  if Value = FTotalNodeItemsCount then Exit;
  delta := Value - FTotalNodeItemsCount;
  FTotalNodeItemsCount := Value;
  if Assigned(FParentNode) then
    FParentNode.TotalNodeItemsCount := FParentNode.TotalNodeItemsCount+delta;
end;

function TLazQuadNode.DoFetchItemCoords(const AItemIndex : Integer) : Boolean;
begin
  if (not FNodeItems[AItemIndex].PointValid) and (not FNodeItems[AItemIndex].AreaValid) then
  begin
    FNodeItems[AItemIndex].AreaValid := FOwner.DoGetItemArea(FNodeItems[AItemIndex].Item,FNodeItems[AItemIndex].Area); // Try to get the items Area
    FNodeItems[AItemIndex].PointValid := FOwner.DoGetItemCoord(FNodeItems[AItemIndex].Item,FNodeItems[AItemIndex].Point.X,FNodeItems[AItemIndex].Point.Y); // try to get the coords from the item
  end;
  Result := FNodeItems[AItemIndex].PointValid or FNodeItems[AItemIndex].AreaValid;
end;

function TLazQuadNode.CreateOrGetChildNode(
  const AChildLocation: TLazQuadTreeChildLocation): TLazQuadNode;
var
  lLLA : TLazQuadNodeArea;
begin
  Result := FChildNodes[AChildLocation];
  if not Assigned(Result) then
  begin
    lLLA := ChildAreas[AChildLocation];
    Result := FOwner.InternalCreateQuadNode(Self,lLLA);
    FChildNodes[AChildLocation] := Result;
  end;
end;

procedure TLazQuadNode.DeleteChildNode(
  const AChildLocation: TLazQuadTreeChildLocation);
var
  cn : TLazQuadNode;
  li : TLazQuadTreeChildLocation;
  assignedcnt, len, ndx : Integer;
  i : Integer;
  ir : TLazQuadTreeNodeItemsRec;
begin
  cn := FChildNodes[AChildLocation];
  if not Assigned(cn) then Exit;
  // Compress the current NodeItems array, to avoid gaps
  PackNodeItems;
  // recurse and delete all four grandchild nodes. They will accumulate all the user data
  for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
    cn.DeleteChildNode(li);
  // Append the accumulated child data to our data
  assignedcnt := cn.AssignedNodeItemsCount;
  if assignedcnt > 0 then
  begin // There are Child data to append
    len := cn.NodeItemsLength; //The length of the child data
    ndx := NodeItemsLength; // The index where to write the next DataItem (= the length of NodeItems-array)
    SetLength(FNodeItems,Length(FNodeItems)+assignedcnt); //Increase the array
    for i := 0 to len-1 do // run through all fields in the NodeItems array
    begin
      // To prevent a lot of calculations in the TotalNodeItemsCount value,
      // which remains constant by this manipulation,
      // the extract and add functions are not used.
      // Instead the Items are copyied by direct acces to the arrays
      ir := cn.FNodeItems[i]; //Copy the data and set the source field to Nil
      if Assigned(ir.Item) then //If an Item was moved, increase the Index
      begin
        FNodeItems[ndx] := ir;
        cn.FNodeItems[i].Item := Nil;
        Inc(ndx);
      end;
    end;
    // TotalNodeItemsCount remains on the same value!
  end;
  FChildNodes[AChildLocation] := Nil; //Nil the relation
  cn.Free; //Free the child object
end;

function TLazQuadNode.CoordIsInNodeArea(const AX, AY: Double): Boolean;
var
  ydir : TLazQuadTreeYAxisDirection;
  xcyl : Boolean;
  aw : TLazQuadNodeArea;
begin
  if Assigned(FOwner) then
  begin
    ydir := FOwner.YAxisDirection;
    xcyl := FOwner.XAxisIsCylindrical;
    aw := FOwner.RootNode.NodeArea;
  end
  else
  begin
    ydir := qydUpPlus;
    xcyl := False;
    aw.Left := 0;
    aw.Right := 0;
    aw.Top := 0;
    aw.Bottom := 0;
  end;
  Result := CoordIsInArea(AX, AY, FNodeArea,
                          ydir,xcyl,aw);
end;

function TLazQuadNode.NodeAreaOverlapTest(const AArea: TLazQuadNodeArea
  ): TLazQuadNodeAreaOverlap;
{ After a long work with dealing with a lot of strange cases,
  math does the work - at least easier.
  The most work is to get the two rectangles to a compareable format.
  Then the rest ist multiplication and addition.
}

  procedure NormalizeAndUnfoldArea(var AUnfoldArea : TLazQuadNodeArea);
  { NormalizeAndUnfoldArea limits and correct the passed Area to the criteria given by the Tree.
    If the X-Axis is cylindrical, then the Area is moved to the right, if the Area is crossing the
    East/West-Border.
    The top and bottom Values are limited to the maximum extension of the "World", taking care to
    the direction of thy Y-Axis
  }
  var
    ww : Double;
  begin
    ww := Abs(FOwner.RootNode.Left - FOwner.RootNode.Right);
    if FOwner.XAxisIsCylindrical then
    begin
      AUnfoldArea.Left:= Math.FMod(AUnfoldArea.Left,ww);
      AUnfoldArea.Right:= Math.FMod(AUnfoldArea.Right,ww);
      if AUnfoldArea.Right <= AUnfoldArea.Left then
        AUnfoldArea.Right := AUnfoldArea.Right + ww;
    end
    else
    begin
      AUnfoldArea.Left := Math.EnsureRange(AUnfoldArea.Left,FOwner.RootNode.Left, FOwner.RootNode.Right);
      AUnfoldArea.Right := Math.EnsureRange(AUnfoldArea.Right,FOwner.RootNode.Left, FOwner.RootNode.Right);
    end;
    if FOwner.YAxisDirection = qydUpPlus then
    begin
      AUnfoldArea.Top := Math.EnsureRange(AUnfoldArea.Top,FOwner.RootNode.Bottom, FOwner.RootNode.Top);
      AUnfoldArea.Bottom := Math.EnsureRange(AUnfoldArea.Bottom, FOwner.RootNode.Bottom, FOwner.RootNode.Top);
    end
    else
    begin
      AUnfoldArea.Top := Math.EnsureRange(AUnfoldArea.Top,FOwner.RootNode.Top, FOwner.RootNode.Bottom);
      AUnfoldArea.Bottom := Math.EnsureRange(AUnfoldArea.Bottom,FOwner.RootNode.Top, FOwner.RootNode.Bottom);
    end;
  end;

  function AreasAreEqual(const AAreaA, AAreaB : TLazQuadNodeArea) : Boolean;
  const
    prec = 1.0E-15;
  var
    epsl, epsr, epst, epsb : Double;
  begin
    epsl := Math.Max(Abs(AAreaA.Left), Abs(AAreaB.Left)) * prec;
    epsr := Math.Max(Abs(AAreaA.Right), Abs(AAreaB.Right)) * prec;
    epst := Math.Max(Abs(AAreaA.Top), Abs(AAreaB.Top)) * prec;
    epsb := Math.Max(Abs(AAreaA.Bottom), Abs(AAreaB.Bottom)) * prec;
    Result :=
      (Abs(AAreaA.Left - AAreaB.Left) <= epsl) and
      (Abs(AAreaA.Right - AAreaB.Right) <= epsr) and
      (Abs(AAreaA.Top - AAreaB.Top) <= epst) and
      (Abs(AAreaA.Bottom - AAreaB.Bottom) <= epsb);
  end;

  function BoundingArea(const AAreaA, AAreaB : TLazQuadNodeArea) : TLazQuadNodeArea;
  var
    laa, lab : TLazQuadNodeArea;
    ww, wa, wb : Double;
  begin
    if FOwner.XAxisIsCylindrical then
    begin
      ww := Abs(FOwner.RootNode.Left - FOwner.RootNode.Right);
      laa := AAreaA;
      lab := AAreaB;
      // If the distance between the west an east border of the
      // both areas are greater than the worldsize, than
      // a loop around is not resolved
      // Move the more "western" area by a worldsize to the right
      while (lab.Right-laa.Left) > ww do
      begin
        laa.Left := laa.Left + ww;
        laa.Right := laa.Right + ww;
      end;
      // Move the more "eastern" area by a worldsize to the left
      while (laa.Right-lab.Left) > ww do
      begin
        lab.Left := lab.Left + ww;
        lab.Right := lab.Right + ww;
      end;
      // Now proof the width of both areas, and correct if needed
      wa := laa.Right - laa.Left;
      wb := lab.Right - lab.Left;
      if wa >= ww then
      begin
        laa.Right := FOwner.RootNode.Right;
        laa.Left :=  FOwner.RootNode.Left;
      end;
      if wb >= ww then
      begin
        lab.Right := FOwner.RootNode.Right;
        lab.Left :=  FOwner.RootNode.Left;
      end;
      // Now the extension could be calculated
      Result.Left := Min(laa.Left, lab.Left);
      Result.Right := Max(laa.Right, lab.Right);
      Result.Left := Min(laa.Left,lab.Left);
      Result.Right := Max(laa.Right,lab.Right);
      // Normalize the bounding rectangle, back to WorldSize
      while Result.Left > (FOwner.RootNode.Left + (ww / 2.0)) do
      begin
        Result.Left := Result.Left-ww;
        Result.Right := Result.Right-ww;
      end;
      // If the width of the bounding rectangle is larger than the world
      if (Result.Right - Result.Left) >= ww then
      begin
        Result.Right := FOwner.RootNode.Right;
        Result.Left :=  FOwner.RootNode.Left;
      end;
    end
    else
    begin
      Result.Left := Math.Min(AAreaA.Left, AAreaB.Left);
      Result.Right := Math.Max(AAreaA.Right, AAreaB.Right);
    end;
    if FOwner.YAxisDirection = qydUpPlus then
    begin
      Result.Top := Math.Max(AAreaA.Top, AAreaB.Top);
      Result.Bottom := Math.Min(AAreaA.Bottom, AAreaB.Bottom);
    end
    else
    begin
      Result.Top := Math.Min(AAreaA.Top, AAreaB.Top);
      Result.Bottom := Math.Max(AAreaA.Bottom, AAreaB.Bottom);
    end;
  end;


var
  lan, lat, labounds : TLazQuadNodeArea;
  wn, hn, wt, ht, wbounds, hbounds : Double;
  // areab, areat, areabounds : Double;
begin
  // Prepare the value, normalize, unfold and correct if needed
  lan := FNodeArea;
  lat := AArea;
  NormalizeAndUnfoldArea(lan);
  NormalizeAndUnfoldArea(lat);
  // The equal case is simple, sort it out first
  if AreasAreEqual(lan,lat) then
  begin
    Result := qaoEqual;
    // ADetectionState := 1;
    Exit;
  end;
  // If not equal, but the current Node is the RootNode then, per definition, Test-Area must be fully included
  if Self = FOwner.RootNode then
  begin
    Result := qaoInside;
    // ADetectionState := 2;
    Exit;
  end;
  // If not equal, but the Test-Area is the equal to the world, the NodeArea must be fully included
  if AreasAreEqual(lat,FOwner.RootNode.NodeArea) then
  begin
    Result := qaoFull;
    // ADetectionState := 3;
    Exit;
  end;

  // So from here
  // - the Rectangles are different
  // - we are not talking about covering the full sphere

  // First we calculate the heights and width of the two rect
  // additionally the bounding rectangle and its height and width.
  // Having this we could sort out all annoing cases.
  // If the bounding is eqal to one of the rect the other must be part of it
  wn := lan.Right - lan.Left;
  hn := Abs(lan.Top - lan.Bottom);
  wt := lat.Right - lat.Left;
  ht := Abs(lat.Top - lat.Bottom);
  // Init labounds only to keep the compiler calm
  labounds.Top:=0;
  labounds.Bottom:=0;
  labounds.Right:=0;
  labounds.Left:=0;
  // Get the bounding rectangle, including the wrap around stuff
  labounds := BoundingArea(lan, lat);
  // if the bounding area is equal to the Node-rect, then the test-rect must be inside
  if AreasAreEqual(labounds,lan) then
  begin
    Result := qaoInside;
    // ADetectionState := 4;
    Exit;
  end;
  // if the bounding area is equal to the test-rect, then the base-rect must be inside
  if AreasAreEqual(labounds, lat) then
  begin
    Result := qaoFull;
    //ADetectionState := 5;
    Exit;
  end;
  // calculate the width and height and area of the bounding box
  wbounds := Abs(labounds.Right - labounds.Left);
  hbounds := Abs(labounds.Top - labounds.Bottom);
//  areab := wn*hn;
//  areat := wt*ht;
//  areabounds := wbounds*hbounds;
  // If the length of the sides of the bounding rectangle
  // are smaller than the sum of the length of the sides of the
  // both rectangles, they must overlap partly
  if (wbounds < (wn+wt)) and
     (hbounds < (hn+ht)) then
  begin
    Result := qaoPartly;
    // ADetectionState := 6;
    Exit;
  end;
  // Obviously the length of the boundingbox sides are longer
  // so the could not overlap
  Result := qaoNone;
//  ADetectionState := 7;
end;

function TLazQuadNode.NodeAreaOverlap(const AArea: TLazQuadNodeArea): Boolean;
begin
  Result := NodeAreaOverlapTest(AArea) <> qaoNone;
end;

function TLazQuadNode.AddNodeItem(const AItem: TObject;
                                  const APointValid : Boolean = False;
                                  const AAreaValid : Boolean = False;
                                  const APointX : Double = 0.0;
                                  const APointY : Double = 0.0;
                                  const AAreaLeft : Double = 0.0;
                                  const AAreaTop : Double = 0.0;
                                  const AAreaRight : Double = 0.0;
                                  const AAreaBottom : Double = 0.0
                                  ) : Integer;
var
  i : Integer;
  len : Integer;
begin
  Result := -1;
  if not Assigned(AItem) then Exit;
  len := Length(FNodeItems);
  if len > 0 then
  begin
    // In the case that the Owner is in the BeginUpdate...EndUpdate mode
    // or not the Owner of the items
    // ignore the duplicate Items, they will be sorted out later
    if (not Assigned(FOwner)) or
       (not FOwner.OwnsObjects) or
       (FOwner.FUpdateCount <= 0) then
    begin
      // Avoid duplicate entries
      for i := 0 to len-1 do
      begin
        if FNodeItems[i].Item = AItem then
        begin
        // Entry is in, exit
          Result := i;
          Exit;
        end;
      end;
    end;
    // Try to find an unused space
    for i := len-1 downto 0 do
    begin
      if not Assigned(FNodeItems[i].Item) then
      begin // Found a free space
        FNodeItems[i].Item := AItem;  // use it and exit
        FNodeItems[i].PointValid := APointValid;
        FNodeItems[i].AreaValid := AAreaValid;
        FNodeItems[i].Point.X := APointX;
        FNodeItems[i].Point.Y := APointY;
        FNodeItems[i].Area.Left := AAreaLeft;
        FNodeItems[i].Area.Top := AAreaTop;
        FNodeItems[i].Area.Right := AAreaRight;
        FNodeItems[i].Area.Bottom := AAreaBottom;
        TotalNodeItemsCount := FTotalNodeItemsCount+1; //Update the number of items in this and all parent nodes
        Result := i;
        Exit;
      end;
    end;
  end;
  // Append the data at the end of the array
  // If the Owner is in the  BeginUpdate...EndUpdate mode reserve more spce at the end to speedup the insertion
  // of many Items
  if (Assigned(FOwner) and (FOwner.FUpdateCount > 0)) then
    SetLength(FNodeItems,len+(FOwner.MaxQuadNodeItemCount*10))
  else
    SetLength(FNodeItems,len+DefaultMaxQuadNodeItemCount);
  FNodeItems[len].Item := AItem;
  FNodeItems[len].PointValid := APointValid;
  FNodeItems[len].AreaValid := AAreaValid;
  FNodeItems[len].Point.X := APointX;
  FNodeItems[len].Point.Y := APointY;
  FNodeItems[len].Area.Left := AAreaLeft;
  FNodeItems[len].Area.Top := AAreaTop;
  FNodeItems[len].Area.Right := AAreaRight;
  FNodeItems[len].Area.Bottom := AAreaBottom;
  TotalNodeItemsCount := TotalNodeItemsCount+1; //Update the number of items in this and all parent nodes
  Result := len; // High(FNodeItems);
end;

function TLazQuadNode.ExtractNodeItem(const AItemIndex: Integer): TObject;
begin
  Result := FNodeItems[AItemIndex].Item;
  FNodeItems[AItemIndex].Item := Nil;
  if Assigned(Result) then
    TotalNodeItemsCount := TotalNodeItemsCount-1; //Update the number of items in this and all parent nodes
end;

function TLazQuadNode.SplitNode: Boolean;
var
  li : TLazQuadTreeChildLocation;
  i : Integer;
  lLLA : TLazQuadNodeArea;
  datalen, cnt : Integer;
  cn : TLazQuadNode;
  o : TObject;
  childplitted :  array[TLazQuadTreeChildLocation] of Boolean;
  hit : TLazQuadNodeAreaOverlap;
  moveitem : Boolean;
  itmrec : TLazQuadTreeNodeItemsRec;
begin
  Result := False;
  if (not Assigned(FOwner)) then Exit; // Without Owner we have to quit
  if FOwner.FUpdateCount > 0 then
  begin
    FNodeTouched := True;
    Exit; // SplittingNodes are not allowed during BeginUpdate ... EndUpdate
  end;
  for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
    childplitted[li] := False;

  if FNodeTouched then
  begin
    // Pack DataItems, they may contain unwanted duplicates or empty spaces
    PackNodeItems;
    FNodeTouched := False;
  end;
  datalen := NodeItemsLength;
  cnt := AssignedNodeItemsCount;
  // The node must be split, if the array is overpopulated
  if (cnt > FOwner.MaxQuadNodeItemCount) and
     (FNodeLevel < FOwner.MaxQuadTreeLevel) then
  begin
    // Loop through all NodeItems, since all may have to be distributed to the new children
    for i := 0 to datalen-1 do
    begin
      if not Assigned(FNodeItems[i].Item) then Continue;
      if not DoFetchItemCoords(i) then
        Continue; //if this fail, skip this item and try the next
      for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
      begin  // loop through all four sub quadrants
        lLLA := ChildAreas[li];  // Get the rectangle
        moveitem := False;
        if FNodeItems[i].AreaValid then
        begin
          hit := ulazQuadTree.NodeAreaOverlapTest(lLLA, FNodeItems[i].Area,FOwner.YAxisDirection,FOwner.XAxisIsCylindrical,FOwner.RootNode.NodeArea);
          moveitem := (hit = qaoInside) or {the test-area is located inside the node-area}
                      (hit = qaoEqual);  {the test-area and node-area are equal}
        end
        else
          moveitem := CoordIsInArea(FNodeItems[i].Point.X,FNodeItems[i].Point.Y, lLLA,FOwner.YAxisDirection,FOwner.XAxisIsCylindrical,FOwner.RootNode.NodeArea);  // check if the coordinates are in
        if moveitem then
        begin
          itmrec := FNodeItems[i];
          o := ExtractNodeItem(i); // Remove the Item from this node (keeps track of the TotalCounter)
          cn := CreateOrGetChildNode(li); // Create a child node (if one exists, is is not created twice)
          cn.AddNodeItem(o, itmrec.PointValid, itmrec.AreaValid,
                         itmrec.Point.X, itmrec.Point.Y,
                         itmrec.Area.Left,itmrec.Area.Top,itmrec.Area.Right,itmrec.Area.Bottom
                        );  // Add the DataIndex in the ChildNode (keeps track of the TotalCounter)
          FNodeItems[i].Item := Nil; //Remove the DataNdx from this node
          childplitted[li] := True; //Memorize, that this child has been splitted
          cn.SplitNode; //Recurse downwards
          Result := True; //Return the "something changed" Result
          Break;
        end;
      end;
    end;
    // Pack NodeItems
    // Since some/all of the indexes are moved, they contains now Nil
    // we clear the unused array space
    PackNodeItems;
  end;
  for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin  // loop through all four children
    if childplitted[li] then Continue; //is allready processed
    cn := ChildNodes[li];
    if Assigned(cn) then
      if cn.SplitNode then //Recurse down, sum the "something changed flag"
        Result := True;
  end;
end;

function TLazQuadNode.PackNode: Boolean;
var
  cnt : Integer;
  li : TLazQuadTreeChildLocation;
  cn : TLazQuadNode;
  childitemscnt : Integer;
begin
  Result := False;
  if (not Assigned(FOwner)) then Exit; // Without an Owner packing is not possible
  if FOwner.FUpdateCount > 0 then
  begin
    FNodeTouched := True;
    Exit; // Packing Nodes are not allowed in this moment
  end;
  if FNodeTouched then
  begin
    // Pack DataItems, they may contain unwanted duplicates or empty spaces
    PackNodeItems;
    FNodeTouched := False;
  end;

  cnt := AssignedNodeItemsCount;
  if cnt >= FOwner.MaxQuadNodeItemCount then Exit; //There is nothing to combine, since this node is full
  childitemscnt := 0;
  for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ChildNodes[li];
    if Assigned(cn) then
    begin // The childnode exists
      Result := Result or cn.PackNode; // Recurse down with combining
      if (not cn.HasChildNodes) then // combining could be done only if the child node has no
                                     // grandchildren, so only in this case we count the NodeItems
        childitemscnt := childitemscnt + cn.AssignedNodeItemsCount;
    end;
  end;
  if ((cnt + childitemscnt) <= FOwner.MaxQuadNodeItemCount) then
  begin // The sum of the Items in the current node and the sub nodes are
        // equal or less than the node can take alone.
        // So we can try to get rid of some of the child nodes
    PackNodeItems;
    for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
    begin
      cn := ChildNodes[li];
      // Again, combining is only possible, if the childnode has no
      // Grandchildren (since this means, that somewhere down a combining was not possible!!)
      if Assigned(cn) and (not cn.HasChildNodes) then
        DeleteChildNode(li); // This deletion, will move the contained Items
    end;
  end;
end;

function TLazQuadNode.NodeIsEmpty: Boolean;
var
  li : TLazQuadTreeChildLocation;
  i : Integer;
  cn : TLazQuadNode;
begin
  Result := True; //Assume emptiness
  for i := 0 to NodeItemsLength-1 do
  begin
    if Assigned(FNodeItems[i].Item) then
    begin // It is not empty, further search will not find anything else
      Result := False;
      Exit;
    end;
  end;
  // here are no data stored, but maybe in the children
  for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ChildNodes[li];
    if Assigned(cn) then
      Result := cn.NodeIsEmpty; // check the child
    if not Result then Exit; // is not empty, then finish the search
  end;
end;

function TLazQuadNode.HasChildNodes: Boolean;
begin
  Result := Assigned(ChildNodeNE) or
            Assigned(ChildNodeSE) or
            Assigned(ChildNodeSW) or
            Assigned(ChildNodeNW);
end;

function TLazQuadNode.NodeItemIndex(const AItem: TObject): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to High(FNodeItems) do
    if FNodeItems[i].Item = AItem then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TLazQuadNode.PackNodeItems;
var
  i,j : Integer;
  cnt : Integer;
  len : Integer;
  newlen : Integer;
  nir : TLazQuadTreeNodeItemsRec;
  tmpitems : array of TLazQuadTreeNodeItemsRec = Nil;
  ndx : Integer;
begin
  if (Assigned(FOwner)) and (FOwner.FUpdateCount > 0) then Exit; // Packing Nodes not allowed here
  if FOwner.OwnsObjects then
  begin
    // Remove duplicate Entries
    len := Length(FNodeItems);
    if len > 1 then
    begin
      // For all items, but not the last
      for i := 0 to High(FNodeItems)-1 do
      begin
        nir := FNodeItems[i]; // get the item
        if not Assigned(nir.Item) then Continue; //if not assigned, take the next
        for j := i+1 to High(FNodeItems) do // for all items behind the current one, including the last
        begin
          if nir.Item = FNodeItems[j].Item then // if equal then Nil the item
            FNodeItems[j].Item := Nil;
        end;
      end;
    end;
  end;

  // Count the unused data ndxs
  cnt := AssignedNodeItemsCount;
  // The easier case
  if cnt = 0 then
  begin  // All places are unused
    SetLength(FNodeItems,FOwner.MaxQuadNodeItemCount);
    Exit;
  end;
  len := Length(FNodeItems);
  if len = cnt then Exit; // No free places in the list
  // there are unused fields
  // Copy the assigned items into an temporary field;
  SetLength(tmpitems,cnt);
  ndx := 0;
  for i := 0 to len-1 do
  begin
    // Run through all places
    if Assigned(FNodeItems[i].Item) then
    begin
      tmpitems[ndx] := FNodeItems[i];
      FNodeItems[i].Item := Nil; // Clear the field!
      Inc(ndx);
    end;
    if ndx >= cnt then Break;
  end;
  // Move the items from the temporary field back
  for i := 0 to cnt-1 do
    FNodeItems[i] := tmpitems[i];
  // Set the array to the count of the used places length, keep the minimum length
  newlen := cnt;
  if newlen < FOwner.MaxQuadNodeItemCount then
    newlen := FOwner.MaxQuadNodeItemCount;
  SetLength(FNodeItems,newlen);
end;

function TLazQuadNode.NodeAsString: String;
var
  i : Integer;
  s, orient : String;
  inds : String;
  cn : TLazQuadNode;
  ic : TLazQuadTreeChildLocation;
  cnt : Integer;
  childrencnt : Integer;
begin
  inds := '';
  for i := 0 to FNodeLevel do
    inds := inds + ' ';
  orient := '';
  if Assigned(FParentNode) then
  begin
    for ic := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
    begin
      if FParentNode.ChildNodes[ic] = Self then
      begin
        orient := LazQuadChildLocationStrings[ic];
        Break;
      end;
    end;
  end;
  s := Format('Level: %d, TotalItems %d, Area("%s") (Left/X: %1.6f, Top/Y %1.6f), (Right/X: %1.6f, Bottom/Y %1.6f)',
               [FNodeLevel,
               FTotalNodeItemsCount,
               orient,
               FNodeArea.Left,
               FNodeArea.Top,
               FNodeArea.Right,
               FNodeArea.Bottom]);
  cnt := NodeItemsLength;
  s := s + ', DataItems '+IntToStr(cnt);
  childrencnt := 0;
  for ic := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := FChildNodes[ic];
    if Assigned(cn) then
      Inc(childrencnt);
  end;
  s := s + ', Children '+IntToStr(childrencnt);
  Result := s;
end;

procedure TLazQuadNode.NodeAsStrings(const AStrings: TStrings);
var
  i : Integer;
  s : String;
  inds : String;
  cn : TLazQuadNode;
  ic : TLazQuadTreeChildLocation;
  cnt, cnt1 : Integer;
  childrencnt : Integer;
  lItemCaptionValid : Boolean;
  lItemCaption : String = '';
begin
  inds := '';
  for i := 0 to FNodeLevel do
    inds := inds + ' ';
  s := Format('Level: %d, TotalItems %d, Area (Left/X: %1.6f, Top/Y %1.6f), (Right/X: %1.6f, Bottom/Y %1.6f)',
               [FNodeLevel,
               FTotalNodeItemsCount,
               FNodeArea.Left,
               FNodeArea.Top,
               FNodeArea.Right,
               FNodeArea.Bottom]);

  AStrings.Add(inds+s);
  cnt1 := AssignedNodeItemsCount;
  cnt := NodeItemsLength;
  AStrings.Add(inds+Format('+ DataItems Assigned %d, Total %d',[cnt1, cnt]));
  for i := 0 to cnt-1 do
  begin
    s := Format('[%d] = ',[i]);
    if Assigned(FNodeItems[i].Item) then
    begin
      DoFetchItemCoords(i);
      if FNodeItems[i].AreaValid then
        s := s + Format(' L: %1.6f, T: %1.6f, R: %1.6f, B: %1.6f',
                        [FNodeItems[i].Area.Left, FNodeItems[i].Area.Top, FNodeItems[i].Area.Right, FNodeItems[i].Area.Bottom])
      else if FNodeItems[i].PointValid then
        s := s + Format('X: %1.6f, Y: %1.6f',[FNodeItems[i].Point.X,FNodeItems[i].Point.Y])
      else
        s := s + 'Coords inaccessable!';
      lItemCaptionValid := FOwner.DoGetItemCaption(FNodeItems[i].Item,lItemCaption);
      if lItemCaptionValid then
        s := s + ' "'+lItemCaption+'"';
    end
    else
      s := s + '<unused>';
    AStrings.Add(inds+' '+s);
  end;
  childrencnt := 0;
  for ic := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := FChildNodes[ic];
    if Assigned(cn) then
      Inc(childrencnt);
  end;
  AStrings.Add(inds+'+ Children '+IntToStr(childrencnt));
  for ic := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := FChildNodes[ic];
    if Assigned(cn) then
    begin
      s := inds+'ChildNode: '+LazQuadChildLocationStrings[ic];
      AStrings.Add(s);
      cn.NodeAsStrings(AStrings);
    end;
  end;
end;

procedure TLazQuadNode.Clear;
var
  cn : TLazQuadNode;
  li : TLazQuadTreeChildLocation;
  i : Integer;
  o : TObject;
begin
  if FOwner.OwnsObjects then
  begin
    for i := 0 to High(FNodeItems) do
    begin
      o := ExtractNodeItem(i);
      if Assigned(o) then
      begin
        o.Free;
        o := Nil;
      end;
    end;
  end;
  SetLength(FNodeItems,FOwner.MaxQuadNodeItemCount);
  // recurse and free all four child nodes. They will free there childs too
  for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := FChildNodes[li];
    if Assigned(cn) then
    begin
      cn.Clear;
      cn.Free;
      FChildNodes[li] := Nil;
    end;
  end;
  // Update the TotalNodeItemsCount
  TotalNodeItemsCount := 0;
end;

constructor TLazQuadNode.Create(const AOwner: TLazQuadTree;
  const AParentNode: TLazQuadNode; const ANodeArea: TLazQuadNodeArea);
begin
  inherited Create;
  FOwner := AOwner;
  FParentNode := AParentNode;
  FNodeArea := ANodeArea;
  // Calculate the NodeLevel. Store the result so that it is readable during lifetime
  FNodeLevel := 0;
  if Assigned(FParentNode) then
    FNodeLevel := FParentNode.NodeLevel+1;
end;

destructor TLazQuadNode.Destroy;
begin
  Clear;
  inherited;
end;

{ TLazQuadTree }
function TLazQuadTree.InternalCreateQuadNode(const AParentNode: TLazQuadNode; const ANodeArea: TLazQuadNodeArea): TLazQuadNode;
begin
  Result := Nil;
  if Assigned(FQuadTreeCreateQuadNodeEvent) then
    Result := FQuadTreeCreateQuadNodeEvent(Self, AParentNode, ANodeArea);
  if not Assigned(Result) then
    Result := TLazQuadNode.Create(Self, AParentNode, ANodeArea);
end;

function TLazQuadTree.InternalEnumerateNodesInArea(
  const ANode: TLazQuadNode; const ANodeArea: TLazQuadNodeArea;
  const AEnumCallBackFunc: TLazQuadTreeNodeEnumerationCallBack;
  const AUserData: Pointer): Boolean;
var
  cn : TLazQuadNode;
  hit : TLazQuadNodeAreaOverlap;
  i : TLazQuadTreeChildLocation;
begin
  Result := True;
  if not Assigned(AEnumCallBackFunc) then Exit;
  if not Assigned(ANode) then Exit;
  hit := ANode.NodeAreaOverlapTest(ANodeArea);
  if hit = qaoNone then Exit; //This branch contains no relevant data
  if ANode.NodeItemsLength > 0 then
    Result := AEnumCallBackFunc(Self,ANode,AUserData); //Call the users callback function with the found node
  if not Result then Exit; // Abortion as the user requests
  // Iterate through the four childs (if existing)
  for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[i]; // Get the child
    if Assigned(cn) then // Recurse throught the node
    begin
      Result := InternalEnumerateNodesInArea(cn,ANodeArea,AEnumCallBackFunc,AUserData);
      if not Result then Exit; // Abortion as the user requests
    end;
  end;
end;

procedure TLazQuadTree.ListSubNodes(const ANode: TLazQuadNode;
  const AList: TList);
var
  cn : TLazQuadNode;
  li : TLazQuadTreeChildLocation;
begin
  if not Assigned(AList) then Exit;
  if not Assigned(ANode) then Exit;
  AList.Add(ANode);
  // Iterate through the four childs (if existing)
  for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[li]; // Get the child
    if Assigned(cn) then // Recurse throught the node
      ListSubNodes(cn,AList);
  end;
end;

procedure TLazQuadTree.InternalListNodesInArea(
  const ANode: TLazQuadNode; const ANodeArea: TLazQuadNodeArea;
  const AList: TList);
var
  cn : TLazQuadNode;
  hit : TLazQuadNodeAreaOverlap;
  li : TLazQuadTreeChildLocation;
begin
  if not Assigned(AList) then Exit;
  if not Assigned(ANode) then Exit;
  hit := ANode.NodeAreaOverlapTest(ANodeArea);
  if hit = qaoNone then Exit; //This branch contains no relevant data
  // If the Nodes Area covers the node or the node is fully inside the area...
  if (hit = qaoEqual) or
     (hit = qaoFull) then
  begin
    // ... than all subnodes could be inserted, without any further investigation
    // Remark ListSubNodes inserts the passed ANode into the list
    ListSubNodes(ANode,AList);
    Exit;
  end;
  // The node is covered partly, so it is up to the children to decide
  AList.Add(ANode);
  // Iterate through the four childs (if existing)
  for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[li]; // Get the child
    if Assigned(cn) then // Recurse throught the node
      InternalListNodesInArea(cn,ANodeArea,AList);
  end;
end;

function TLazQuadTree.InternalEnumerateItemsInArea(const ANode: TLazQuadNode;
  const ANodeArea: TLazQuadNodeArea;
  const AEnumCallBackFunc: TLazQuadTreeItemEnumerationCallBack;
  const AUserData: Pointer): Boolean;
var
  cn : TLazQuadNode;
  hit : TLazQuadNodeAreaOverlap;
  i : TLazQuadTreeChildLocation;
  ii : Integer;
  ni : TObject;
begin
  Result := True;
  if not Assigned(FGetItemCoordEvent) then Exit;
  if not Assigned(AEnumCallBackFunc) then Exit;
  if not Assigned(ANode) then Exit;
  hit := ANode.NodeAreaOverlapTest(ANodeArea);
  if hit = qaoNone then Exit; //This branch contains no relevant data
  // If the Nodes Area covers the node or the node is fully inside the area...
  if (hit = qaoEqual) or
     (hit = qaoFull) then
  begin
    // ... than all Items in the SubNodes could be processed, without any further investigation
    // Remark: CountSubNodes returns 1 for the passed ANode
    Result := EnumerateItemsInSubNodes(ANode,AEnumCallBackFunc,AUserData);
    Exit;
  end;
  // The node is covered partly,
  // So some Items may be inside, some not
  // and it is up to the children to process further
  for ii := 0 to High(ANode.FNodeItems) do
  begin
    ni := ANode.FNodeItems[ii].Item;
    if Assigned(ni) then
    begin
      if not ANode.DoFetchItemCoords(ii) then
        Continue; //if this fail, skip this item and try the next

      if ANode.FNodeItems[ii].PointValid and
         CoordIsInArea(ANode.FNodeItems[ii].Point.X, ANode.FNodeItems[ii].Point.Y,
                       ANodeArea,FYAxisDirection,FXAxisIsCylindrical,FRootNode.FNodeArea) then
      begin
        Result := AEnumCallBackFunc(Self,ANode,ni,ii,AUserData);
        if not Result then Exit; // Abortion as the user requests
      end;
    end;
  end;
  // Iterate through the four childs (if existing)
  for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[i]; // Get the child
    if Assigned(cn) then // Recurse throught the node
    begin
      Result := InternalEnumerateItemsInArea(cn,ANodeArea,AEnumCallBackFunc,AUserData);
      if not Result then Exit; // Abortion as the user requests
    end;
  end;
end;

procedure TLazQuadTree.InternalListItemsInArea(const ANode: TLazQuadNode;
  const ANodeArea: TLazQuadNodeArea; const AList: TList);
var
  cn : TLazQuadNode;
  hit : TLazQuadNodeAreaOverlap;
  i : TLazQuadTreeChildLocation;
  ii : Integer;
  ni : TObject;
begin
  if not Assigned(FGetItemCoordEvent) then Exit;
  if not Assigned(ANode) then Exit;
  hit := ANode.NodeAreaOverlapTest(ANodeArea);
  if hit = qaoNone then Exit; //This branch contains no relevant data
  // If the Nodes Area covers the node or the node is fully inside the area...
  if (hit = qaoEqual) or
     (hit = qaoFull) then
  begin
    // ... than all Items in the SubNodes could be processed, without any further investigation
    ListItemsInSubNodes(ANode,AList);
    Exit;
  end;
  // The node is covered partly,
  // So some Items may be inside, some not
  // and it is up to the children to process further
  for ii := 0 to High(ANode.FNodeItems) do
  begin
    ni := ANode.FNodeItems[ii].Item;
    if Assigned(ni) then
    begin
      if not ANode.DoFetchItemCoords(ii) then
        Continue; //if this fail, skip this item and try the next
      if ANode.FNodeItems[ii].PointValid and
         CoordIsInArea(ANode.FNodeItems[ii].Point.X,ANode.FNodeItems[ii].Point.Y,
                       ANodeArea,FYAxisDirection,FXAxisIsCylindrical,FRootNode.FNodeArea) then
        AList.Add(ni);
    end;
  end;
  // Iterate through the four childs (if existing)
  for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[i]; // Get the child
    if Assigned(cn) then // Recurse throught the node
      InternalListItemsInArea(cn,ANodeArea,AList);
  end;
end;

function TLazQuadTree.CountSubNodes(const ANode: TLazQuadNode): Integer;
var
  cn : TLazQuadNode;
  li : TLazQuadTreeChildLocation;
begin
  Result := 0;
  if not Assigned(ANode) then Exit;
  Result := 1; // Count the passed Node
  // Iterate through the four childs (if existing)
  for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[li]; // Get the child
    if Assigned(cn) then // Recurse throught the node
      Result := Result + CountSubNodes(cn); //add the count of the child
  end;
end;

function TLazQuadTree.EnumerateItemsInSubNodes(const ANode: TLazQuadNode;
  const AEnumCallBackFunc: TLazQuadTreeItemEnumerationCallBack;
  const AUserData: Pointer): Boolean;
var
  cn : TLazQuadNode;
  i : TLazQuadTreeChildLocation;
  ii : Integer;
  ni : TObject;
begin
  Result := True;
  if not Assigned(AEnumCallBackFunc) then Exit;
  if not Assigned(ANode) then Exit;
  for ii := 0 to High(ANode.FNodeItems) do
  begin
    ni := ANode.FNodeItems[ii].Item;
    if Assigned(ni) then
    begin
      Result := AEnumCallBackFunc(Self,ANode,ni,ii,AUserData);
      if not Result then Exit;
    end;
  end;
  // Iterate through the four childs (if existing)
  for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[i]; // Get the child
    if Assigned(cn) then // Recurse throught the node
    begin
      Result := EnumerateItemsInSubNodes(cn,AEnumCallBackFunc,AUserData);
      if not Result then Exit; // Abortion as the user requests
    end;
  end;
end;

procedure TLazQuadTree.ListItemsInSubNodes(const ANode: TLazQuadNode;
  const AList: TList);
var
  cn : TLazQuadNode;
  i : TLazQuadTreeChildLocation;
  ii : Integer;
  ni : TObject;
begin
  if not Assigned(ANode) then Exit;
  for ii := 0 to High(ANode.FNodeItems) do
  begin
    ni := ANode.FNodeItems[ii].Item;
    if Assigned(ni) then
      AList.Add(ni);
  end;
  // Iterate through the four childs (if existing)
  for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[i]; // Get the child
    if Assigned(cn) then // Recurse throught the node
      ListItemsInSubNodes(cn,AList);
  end;
end;

function TLazQuadTree.InternalCountNodesInArea(
  const ANode: TLazQuadNode; const ANodeArea: TLazQuadNodeArea): Integer;
var
  cn : TLazQuadNode;
  hit : TLazQuadNodeAreaOverlap;
  i : TLazQuadTreeChildLocation;
begin
  Result := 0;
  if not Assigned(ANode) then Exit;
  hit := ANode.NodeAreaOverlapTest(ANodeArea);
  if hit = qaoNone then Exit; //This branch contains no relevant data
  // If the Nodes Area covers the node or the node is fully inside the area...
  if (hit = qaoEqual) or
     (hit = qaoFull) then
  begin
    // ... than all subnodes could be inserted, without any further investigation
    // Remark: CountSubNodes returns 1 for the passed ANode
    Result := CountSubNodes(ANode);
    Exit;
  end;
  // The node is covered partly, so it is up to the children to decide
  Result := 1;
  // Iterate through the four childs (if existing)
  for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[i]; // Get the child
    if Assigned(cn) then // Recurse throught the node
      Result := Result + InternalCountNodesInArea(cn,ANodeArea);
  end;
end;

function TLazQuadTree.InternalCountAssignedNodeItemsInArea(
  const ANode: TLazQuadNode; const ANodeArea: TLazQuadNodeArea): Integer;
var
  cn : TLazQuadNode;
  hit : TLazQuadNodeAreaOverlap;
  i : TLazQuadTreeChildLocation;
  ni : TObject;
  ii : Integer;
begin
  Result := 0;
  if not Assigned(ANode) then Exit;
  hit := ANode.NodeAreaOverlapTest(ANodeArea);
  if hit = qaoNone then Exit; //This branch contains no relevant data
  // If the Nodes Area covers the node or the node is fully inside the area...
  if (hit = qaoEqual) or
     (hit = qaoFull) then
  begin
    // ... than the sum of all items of the subnodes could be inserted, without any further investigation
    Result := ANode.TotalNodeItemsCount;
    Exit;
  end;
  // The node is covered partly, so it is up to the children (if any) to decide.
  // But we may have items here too, because of no children, or big item extent
  for ii := 0 to High(ANode.FNodeItems) do // run through all item
  begin
    ni := ANode.FNodeItems[ii].Item;
    if Assigned(ni) then
    begin
      if not ANode.DoFetchItemCoords(ii) then
        Continue; //if this fail, skip this item and try the next
      if ANode.FNodeItems[ii].PointValid and
         CoordIsInArea(ANode.FNodeItems[ii].Point.X,ANode.FNodeItems[ii].Point.Y,
                       ANodeArea,FYAxisDirection,FXAxisIsCylindrical,FRootNode.FNodeArea) then
        Inc(Result); // Add to result if the item lays in the processed area
    end;
  end;

  // Iterate through the four childs (if existing)
  for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := ANode.ChildNodes[i]; // Get the child
    if Assigned(cn) then // Recurse throught the node
      Result := Result + InternalCountAssignedNodeItemsInArea(cn,ANodeArea);
  end;
end;

function TLazQuadTree.InternalEstimatedCountOfAssignedItemsInArea(
  const ANode: TLazQuadNode; const ATestArea: TLazQuadNodeArea): Double;
{ Generally the estimation is performed by calculation the
  fraction of the overlapping area of the node's area and
  multiplying this value with the TotalNodeItemsCount of the
  node, which is the accumulated count of all nodes in this part of the tree.
  So if a node is overlapped by the test area by 1/3 and has 15 items,
  then 5.0 is returned.
  The details are a bit more complex, because it is necesarry to dive deeper in the
  tree, to avoid a big quality range in the estimation.
  You may think about a small area in London around the Longitude "0°".
  It is not wise, to take all items in the NorthEast and NorthWest quadrants
  of the whole world, to estimate the items located in London.
  So recursing through the tree is done as deep as necessarry:
  - If a node is fully outside of the TestArea, this part of the Tree is ignored.
  - If a node is fully inside the TestArea, than all items below are counted, but
    no further investigation is necesarry.
  - For all parts of a node, where no children exists, the four quadrants are
    examined and if they fully in, the calculation is performed as described
    above (fraction of the area, multiplyed by TotalItemsCount).
  - To avoid double counting the calculation ist divided in parts where children exists and where not.
  And all Nodes, who are not covered by the test area are ignored as soon as possible.
  Even if the code is a bit complex, the speed is good, since wide areas of the
  tree are excluded from searching very soon.
}

  {FractionOfNodeAreaPartly: Calculates the fraction of a partly covered area.
  }
  function FractionOfNodeAreaPartly (
             const ABaseArea : TLazQuadNodeArea; // The Area to proof (NodeArea or ChildArea)
             const AOverlappingArea : TLazQuadNodeArea;
             const ASecondaryOverlappingAreaValid : Boolean;
             const ASecondaryOverlappingArea : TLazQuadNodeArea) : Double;
  var
    arn, ar : Double;
    w,h : Double;
  begin
    w := NodeAreaWidth(AOverlappingArea,FRootNode.NodeArea,FXAxisIsCylindrical);
    if ASecondaryOverlappingAreaValid then
      w := w + NodeAreaWidth(ASecondaryOverlappingArea,FRootNode.NodeArea,FXAxisIsCylindrical);
    h := Abs(AOverlappingArea.Bottom-AOverlappingArea.Top);
    ar := h*w;
    w := NodeAreaWidth(ABaseArea,FRootNode.NodeArea,FXAxisIsCylindrical);
    h := Abs(ABaseArea.Bottom-ABaseArea.Top);
    arn := h*w;
    Result := ar / arn;
  end;

var
  cn : TLazQuadNode;
  li : TLazQuadTreeChildLocation;
  qarterscnt : Integer;
  directitemsum, directexcludedsum : Double;
  remainingitems : Double;
  lhit : TLazQuadNodeAreaOverlap;
  d : Double;
  lOverlay, lOverlay2 : TLazQuadNodeArea;
  lOverlay2Valid : Boolean;
  ncar : TLazQuadNodeArea;
begin
  Result := 0.0;
  if not Assigned(ANode) then Exit;
  if ANode.TotalNodeItemsCount <= 0 then Exit; // Nothing to do here, since no items here or below

  // Keep the compiler calm
  lOverlay.Left := 0; lOverlay.Right := 0; lOverlay.Top := 0; lOverlay.Bottom := 0;
  lOverlay2.Left := 0; lOverlay2.Right := 0; lOverlay2.Top := 0; lOverlay2.Bottom := 0;
  lOverlay2Valid := False;

  // Get the kind of hit and the overlapping area(s) if existing
  lhit := NodeAreaOverlapTestEx(ANode.NodeArea, ATestArea,
                                  YAxisDirection, XAxisIsCylindrical,
                                  FRootNode.NodeArea,
                                  lOverlay,
                                  lOverlay2Valid, lOverlay2);
  case lhit of
    qaoNone :; // This node is not hit, so no items will be found here or below, so good bye
    qaoPartly, qaoInside :
      begin
        // Partly and Inside needs almost the same action.
        // For each existing child, pass the work to it, for each "non existing" child
        // do the work here.
        // If no children exists, than the calculation could be shortened
        if not ANode.HasChildNodes then
        begin
          // No children exists, take the fraction of the items
          lhit := NodeAreaOverlapTestEx(ANode.NodeArea, ATestArea,
                                          YAxisDirection, XAxisIsCylindrical,
                                          FRootNode.NodeArea,
                                          lOverlay,
                                          lOverlay2Valid, lOverlay2);

          d := FractionOfNodeAreaPartly(ANode.NodeArea, lOverlay, lOverlay2Valid, lOverlay2);
          if d > 1.0 then
          begin
            lhit := NodeAreaOverlapTestEx(ANode.NodeArea, ATestArea,
                                            YAxisDirection, XAxisIsCylindrical,
                                            FRootNode.NodeArea,
                                            lOverlay,
                                            lOverlay2Valid, lOverlay2);
            d := FractionOfNodeAreaPartly(ANode.NodeArea, lOverlay, lOverlay2Valid, lOverlay2);
          end;
          Result := ANode.TotalNodeItemsCount * d;
        end
        else
        begin
          // At least one child exists, the work has to be devided into the four quadrants.
          directitemsum := 0.0; // Summation of the direct assigned children of the child node
          directexcludedsum := 0.0; // Summation of all not assigned children of the child node
          qarterscnt := 0;
          // Iterate through the four child positions
          for li := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
          begin
            cn := ANode.ChildNodes[li]; // Get the child
            // If the child exists ...
            if Assigned(cn) then
            begin
              // let do it the work
              d := InternalEstimatedCountOfAssignedItemsInArea(cn,ATestArea);
              // We will count two sums
              // The sum of the hit item of the child.
              directitemsum := directitemsum + d;
              // and the remaining items of the child, since these are also out of the race
              directexcludedsum := directexcludedsum + (cn.TotalNodeItemsCount - d);
            end
            else
            begin
              // If no child at this position exists, do the calculation for it here.
              // The calculation is fairly simple.
              // If the child's area is none, party or inside (means the test area is fully inside the non existing child)
              // then the items in this parts are simple zero.
              // If the opposite is true, the child's area is fully part of the test area
              // than this quarter of the area has to be taken fully into account
              // this means, we have not to deal with fractions of covered child ares here.
              // we can just count the touched quarters.
              ncar := ANode.ChildAreas[li];
              lhit := NodeAreaOverlapTest(ncar,ATestArea,
                                            YAxisDirection, XAxisIsCylindrical,
                                            FRootNode.NodeArea);
              case lhit of
                qaoEqual, qaoFull :
                  // Equal should never occour, but will catched for sanity.
                  // Full means that the area of the non existing child lies fully inside the
                  // test area. Thus 1/4 of the not directly assigned items of this node has to be counted
                  Inc(qarterscnt);
              else
                // qaoPartly :
                //   a part of the child node area is covered by the test area (which may in total be much bigger)
                //   but since the child does not exists, zero items are located here
                // qaoInside :
                //   the test area is fully inside the child area,
                //   but the child does not exists.
                //   So the sum of the estimated Items must be zero
                // qaoNone :
                //   Part of child not hit
                // In all cases the parts area sum remains 0.0
              end;
            end;
          end;
          // Take the sum collected from the existing children
          Result := directitemsum;
          // Now we have to calculate the remaining items. These are items who are
          // either are allready "in" or definitly "out".
          remainingitems := ANode.TotalNodeItemsCount-(directitemsum+directexcludedsum);
          // Process the rest.
          // if at least one quarter has to be counted and at least one item is remaining.
          if (qarterscnt > 0) and (remainingitems > 0) then
          begin
            d := (qarterscnt/4.0)*remainingitems; // Adding this remaining fraction of items
            Result := Result + d;
          end;
        end;
      end;
    qaoEqual, qaoFull :
      begin
        // Equal means, this node is the only node that contains relevant information
        // Full means, the test area is larger than this nodes area, so all items of this node needs to be counted.
        // Other nodes, but not child node here, has to be queried too.
        Result := ANode.TotalNodeItemsCount;
      end;
  else
  end;
end;

function TLazQuadTree.DoGetItemCoord(AItem: TObject; var AX, AY: Double
  ): Boolean;
begin
  Result := False;
  if not Assigned(FGetItemCoordEvent) then Exit; // The callback is not assigned
  Result := FGetItemCoordEvent(Self,AItem,AX,AY);
end;

function TLazQuadTree.DoGetItemArea(AItem: TObject;
  var AItemArea: TLazQuadNodeArea): Boolean;
begin
  Result := False;
  if not Assigned(FGetItemAreaEvent) then Exit; // The callback is not assigned
  Result := FGetItemAreaEvent(Self,AItem,AItemArea);
end;

function TLazQuadTree.DoGetItemCaption(AItem: TObject; var AItemCaption: String
  ): Boolean;
begin
  Result := False;
  if not Assigned(FGetItemCaptionEvent) then Exit; // The callback is not assigned
  Result := FGetItemCaptionEvent(Self,AItem,AItemCaption);
end;


function TLazQuadTree.InsertItem(const AItem: TObject;
  const APointValid: Boolean; const AAreaValid: Boolean; const APointX: Double;
  const APointY: Double; const AAreaLeft: Double; const AAreaTop: Double;
  const AAreaRight: Double; const AAreaBottom: Double): TLazQuadNode;
var
  lpntvalid, lareavalid : Boolean;
  lX, lY : Double;
  lItemArea : TLazQuadNodeArea;
begin
  Result := Nil;
  if not Assigned(AItem) then Exit; // Data item is not valid
  lpntvalid := APointValid;
  lareavalid := AAreaValid;;
  lX := APointX;
  lY := APointY;
  lItemArea.Left := AAreaLeft;
  lItemArea.Top := AAreaTop;
  lItemArea.Right := AAreaRight;
  lItemArea.Bottom := AAreaBottom;
  if (not APointValid) and (not AAreaValid) then
  begin
    lareavalid := DoGetItemArea(AItem,lItemArea);  // Get the items area, if defined
    lpntvalid := DoGetItemCoord(AItem,lX,lY); // Get items coord
  end;
  if (not lpntvalid) and (not lareavalid) then Exit;
  if lareavalid then
    Result := FindAreaNode(lItemArea); // Find the node who contains the area
  if not Assigned(Result) and lpntvalid then
    Result:= FindCoordNode(lX,lY); // Find the node who contains the Coord
  if not Assigned(Result) then Exit; // Should never occour, since the Root-Node covers the whole world
  Result.AddNodeItem(AItem,
                     lpntvalid,lareavalid,lX,lY,
                     lItemArea.Left,lItemArea.Top,lItemArea.Right,lItemArea.Bottom
                     ); // Add the data
  if Result.SplitNode then // Split the node (if needed, this is decided there)
    Result := FindItemNode(AItem); // If the node was splitted, return the insertion node
end;

function TLazQuadTree.ExtractItem(const AItem: TObject): TObject;
var
  qn : TLazQuadNode;
  ndx : Integer;
begin
  Result := Nil;
  qn := FindItemNode(AItem);
  if not Assigned(qn) then Exit;
  ndx := qn.NodeItemIndex(AItem);
  if ndx < 0 then Exit; // should/could never occour, since FindItemNode was successful
  Result := qn.ExtractNodeItem(ndx);
end;

function TLazQuadTree.MoveItemAfterCoordChange(const AItem: TObject): TLazQuadNode;
var
  qn : TLazQuadNode;
  lpntvalid, lareavalid : Boolean;
  lX,lY : Double;
  lItemArea : TLazQuadNodeArea;
  ndx : Integer;
begin
  Result := Nil;
  lX := 0.0; // Keep the compiler calm
  lY := 0.0;
  lItemArea.Left := 0.0;
  lItemArea.Right := 0.0;
  lItemArea.Top := 0.0;
  lItemArea.Bottom := 0.0;

  qn := FindItemNode(AItem);
  if not Assigned(qn) then Exit;

  // Find the new node, where the new position fits
  lpntvalid := False;
  lareavalid := DoGetItemArea(AItem,lItemArea);  // Get the items area, if defined
  if lareavalid then
    Result := FindAreaNode(lItemArea) // Find the node who contains the area
  else
  begin
    lpntvalid := DoGetItemCoord(AItem,lX,lY);
    if not lpntvalid then Exit; // No area and no coord of the item could be retrieved, exit
    Result := FindCoordNode(lX,lY); // Find the node who contains the Coord
  end;
  if not Assigned(Result) then Exit; // Should never occour, since the Root-Node covers the whole world
  // if the target node is the same as the current node,
  // no further action is required
  if Result <> qn then
  begin
    // since we know the node already, we can shortcut a bit, rather than calling ExtractItem(AItem);
    ndx := qn.NodeItemIndex(AItem);
    if ndx < 0 then Exit; // should/could never occour, since FindItemNode was successful
    qn.ExtractNodeItem(ndx);
    // since we know the node already, we can shortcut a bit, rather than calling InsertItem(AItem);
    Result.AddNodeItem(AItem,
                       lpntvalid,lareavalid,lX,lY,
                       lItemArea.Left,lItemArea.Top,lItemArea.Right,lItemArea.Bottom); // Add the data
    if Result.SplitNode then // Split the node (if needed, this is decided there)
      Result := FindItemNode(AItem); // The the node was splitted, return the insertion node
  end;
end;

function TLazQuadTree.FindAreaNode(const ANodeArea: TLazQuadNodeArea
  ): TLazQuadNode;
{
  There is always an ordered List of Nodes, beginning from the RootNode,
  down to the smallest rectangle, that contains the passed coordinate.
  If the Tree is empty, than the result will be the RootNode.
}
var
  node : TLazQuadNode;
  i : TLazQuadTreeChildLocation;
  found : Boolean;
  hit : TLazQuadNodeAreaOverlap;
begin
  // the RootNode contains the world
  Result := FRootNode;
  hit := FRootNode.NodeAreaOverlapTest(ANodeArea);
  if (hit = qaoEqual) or // node's area is equal
     (hit = qaoFull) or // test area covers the node fully
     (hit = qaoPartly) then // test area covers the node partly
    Exit; // No chance to find something better

  while True do
  begin
    // Assume no further finding
    found := False;
    // Iterate through the four children (if existing)
    for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
    begin
      node := Result.ChildNodes[i]; // Get the child
      if Assigned(node) then
      begin
        hit := node.NodeAreaOverlapTest(ANodeArea);
        if (hit = qaoEqual) then // node's area is equal
        begin
          Result := node; // take this child as the (better = smaller rectangle) result
          Exit; // No chance to find something better
        end
        else if (hit = qaoInside) then
        begin
          Result := node; // take this child as the (better = smaller rectangle) result
          found := True; // remember we have found something
          Break; // stop searching in the other children, there is no chance to find something
        end
        else if (hit = qaoPartly) or // test area covers the node partly
                (hit = qaoFull) then // test area covers the node fully
        begin
          Exit; // stop searching, the Result (parents node) is the best we can find
        end;
      end;
    end;
    if not found then Break; //if nothing further found, than keep the last result
  end;
end;

function TLazQuadTree.FindCoordNode(const AX, AY: Double): TLazQuadNode;
{
  There is always an ordered List of Nodes, beginning from the RootNode,
  down to the smallest rectangle, that contains the passed coordinate.
  If the Tree is empty, than the result will be the RootNode.
}
var
  node : TLazQuadNode;
  i : TLazQuadTreeChildLocation;
  found : Boolean;
begin
  // the RootNode contains the world
  Result := FRootNode;
  while True do
  begin
    // Assume no further finding
    found := False;
    // Iterate through the four children (if existing)
    for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
    begin
      node := Result.ChildNodes[i]; // Get the child
      if Assigned(node) and node.CoordIsInNodeArea(AX,AY) then
      begin // child exists and the coordinate is in the child
        Result := node; // take this child as the (better = smaller rectangle) result
        found := True; // remember we have found something
        Break; // stop searching in the other children, there is no chance to find something
      end;
    end;
    if not found then Break; //if nothing further found, than keep the last result
  end;
end;

function TLazQuadTree.FindItemNode(const AItem: TObject): TLazQuadNode;
var
  lX, lY : Double;
  qn : TLazQuadNode;
  ndx : Integer;
begin
  Result := Nil;
  if not Assigned(AItem) then Exit; // Data item is not valid
  qn := Nil; // The Node where the Item is located (Nil = not found)
  ndx := -1; // The Index of the Item within the node
  lX := 0.0; // Keep the compiler calm
  lY := 0.0;
  if DoGetItemCoord(AItem,lX,lY) then
  begin // the function delivered useful data
    qn := FindCoordNode(lX,lY); // Find the node who contains the Coord
    // Now ask the node if it contains the Item, if not climb up to the to the rootnode
    while Assigned(qn) do
    begin
      ndx := qn.NodeItemIndex(AItem);
      if ndx >= 0 then Break;
      qn := qn.ParentNode;
    end;
  end;
  if ndx < 0 then // We have not found the item's node in the smart way
  begin // we have to do it slowly, by iteration through the Tree
    qn := NextNode(Nil);
    while Assigned(qn) do
    begin
      ndx := qn.NodeItemIndex(AItem);
      if ndx >= 0 then Break; //Item and Item's node found!
      qn := NextNode(qn);
    end;
  end;
  if ndx < 0 then Exit; //The Item was not found, neither smart, nor the long way
  Result := qn;
end;

function TLazQuadTree.EnumerateNodesInArea(
  const ANodeArea: TLazQuadNodeArea;
  const AEnumCallBackFunc: TLazQuadTreeNodeEnumerationCallBack;
  const AUserData: Pointer): Boolean;
begin
  Result := InternalEnumerateNodesInArea(FRootNode,ANodeArea,AEnumCallBackFunc, AUserData);
end;

procedure TLazQuadTree.ListNodesInArea(
  const ANodeArea: TLazQuadNodeArea; const AList: TList);
begin
  InternalListNodesInArea(FRootNode,ANodeArea,AList);
end;

function TLazQuadTree.CountNodesInArea(
  const ANodeArea: TLazQuadNodeArea): Integer;
begin
  Result := InternalCountNodesInArea(FRootNode,ANodeArea);
end;

function TLazQuadTree.EnumerateItemsInArea(const ANodeArea: TLazQuadNodeArea;
  const AEnumCallBackFunc: TLazQuadTreeItemEnumerationCallBack;
  const AUserData: Pointer): Boolean;
begin
   Result := InternalEnumerateItemsInArea(FRootNode,ANodeArea,AEnumCallBackFunc, AUserData);
end;

procedure TLazQuadTree.ListItemsInArea(const ANodeArea: TLazQuadNodeArea;
  const AList: TList);
begin
  InternalListItemsInArea(FRootNode,ANodeArea,AList);
end;

function TLazQuadTree.CountAssignedItemsInArea(
  const ANodeArea: TLazQuadNodeArea): Integer;
begin
  Result := InternalCountAssignedNodeItemsInArea(FRootNode,ANodeArea);
end;

function TLazQuadTree.EstimatedCountOfAssignedItemsInArea(
  const ANodeArea: TLazQuadNodeArea): Double;
begin
  Result := InternalEstimatedCountOfAssignedItemsInArea(FRootNode,ANodeArea);
end;

procedure TLazQuadTree.TreeAsStrings(const AStrings: TStrings);
begin
  AStrings.Clear;
  AStrings.Add('QuadTree');
  AStrings.Add('+ RootNode');
  FRootNode.NodeAsStrings(AStrings);
end;

function TLazQuadTree.NextNode(const AStartNode: TLazQuadNode): TLazQuadNode;
var
  pn, cn, sibling : TLazQuadNode;
  i : TLazQuadTreeChildLocation;
  found : Boolean;
begin
  if not Assigned(AStartNode) then
  begin
    Result := FRootNode;
    Exit;
  end;
  if AStartNode.Owner <> Self then
  begin // Not the right tree
    Result := Nil;
    Exit;
  end;
  // Find the first assigned child and return this
  for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
  begin
    cn := AStartNode.ChildNodes[i]; // Get the child
    if Assigned(cn) then
    begin // child exists than return the child
      Result := cn;
      Exit;
    end;
  end;
  Result := Nil; //Assume end of the journey
  // This node has no children, so we have to look in the parents child list to find siblings
  // So we are looking now from this node beeing a child
  cn := AStartNode;
  pn := AStartNode.ParentNode;
  while Assigned(pn) do
  begin
    found := False;
    for i := Low(TLazQuadTreeChildLocation) to High(TLazQuadTreeChildLocation) do
    begin
      sibling := pn.ChildNodes[i]; // Get the child
      if Assigned(sibling) and found then // If we found ourself in one of the loops before, this is the next sibling
      begin
        Result := sibling; // Return this sibling and exit
        Exit;
      end;
      if sibling = cn then // this sibling is this node, so we found our self, the next sibling is our result!
        found := True;
    end;
    if not found then Exit; // Should never happens
    // We found no other sibling than ourself (= we are the last sibling)
    // Than move one level up. Make the new child the current parent, and the parent the parent of the new child
    cn := pn;
    pn := cn.ParentNode;
  end;
end;

procedure TLazQuadTree.RecountTotalNodeItems;
var
  qn : TLazQuadNode;
  cnt, sum : Integer;

begin
  // First step: Iterate to all Nodes and set the
  // FTotalNodeItemsCount field to 0.
  qn := FRootNode;
  repeat
    qn.FTotalNodeItemsCount := 0; // Set the field variable direct instead of the property
    qn := NextNode(qn);
  until not Assigned(qn);
  // Second step: Iterate to all Nodes and increment the
  // TotalNodeItemsCount property by the UsedNodeItemsCount
  // of each node.
  // Since modifiying the TotalNodeItemsCount updates all the
  // parent nodes up to the RootNode, the summation is done.
  sum := 0;
  qn := FRootNode;
  repeat
    cnt := qn.AssignedNodeItemsCount;
    qn.TotalNodeItemsCount := qn.FTotalNodeItemsCount + cnt; // Write to the property, to force the action
    sum := sum + cnt;
    qn := NextNode(qn);
  until not Assigned(qn);
  if FRootNode.FTotalNodeItemsCount <> sum then
    raise Exception.Create('Something went terrible wrong in RecountTotalNodeItems');
end;

procedure TLazQuadTree.Clear;
begin
  FRootNode.Clear;
end;

procedure TLazQuadTree.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLazQuadTree.EndUpdate;
begin
  if FUpdateCount > 0 then
     Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FRootNode.SplitNode;
    FRootNode.PackNode;
  end;
end;

constructor TLazQuadTree.Create(const AWorldSize: TLazQuadNodeArea;
  const AFreeObjects: Boolean; const AXAxisIsCylindrical: Boolean;
  const AYAxisDirection: TLazQuadTreeYAxisDirection;
  const AQuadTreeCreateQuadNodeEvent: TLazQuadTreeCreateQuadNodeEvent);
begin
  inherited Create;
  FOwnsObjects := AFreeObjects;
  FXAxisIsCylindrical := AXAxisIsCylindrical;
  FYAxisDirection := AYAxisDirection;
  FQuadTreeCreateQuadNodeEvent := AQuadTreeCreateQuadNodeEvent;
  FMaxQuadNodeItemCount := DefaultMaxQuadNodeItemCount;
  FMaxQuadTreeLevel := DefaultMaxQuadTreeLevel;

  FRootNode := InternalCreateQuadNode(Nil,AWorldSize);
end;

destructor TLazQuadTree.Destroy;
begin
  FreeAndNil(FRootNode);
  inherited Destroy;
end;

end.

