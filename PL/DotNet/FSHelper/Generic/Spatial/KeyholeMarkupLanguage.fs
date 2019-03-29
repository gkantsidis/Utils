namespace CGFSHelper.Spatial

(*
 * Requirements:
 * - Collections.fs
 * - GeoLocation.fs
 *)
module KeyholeMarkupLanguage =
    open System
    open System.Xml
    open CGFSHelper.Spatial.GeoLocation

    (** References:
    * - http://www.opengeospatial.org/standards/kml/
    * - https://developers.google.com/kml/documentation/kmlreference
    *)

    /// Captures an element which cannot be processed
    type UnknownItem =
    | XmlAttribute of XmlAttribute
    | XmlElement of XmlElement

    /// Captures a collection of elemenets that cannot be processed
    type Unknown = UnknownItem list option

    /// The unique Id of every element
    type Id = string option

    (** Time Primitives
    *)

    type DateTimePrecision =
    | Year
    | YearMonth
    | Date
    | DateTime
    | DateTimeZone

    type TimestampWithPrecision = System.DateTime * DateTimePrecision

    /// See https://developers.google.com/kml/documentation/kmlreference#timespan
    type TimeSpan = TimeSpan of Id:Id * Begin:TimestampWithPrecision * End:TimestampWithPrecision * Unknown:Unknown

    /// See https://developers.google.com/kml/documentation/kmlreference#timestamp
    type TimeStamp = TimeStamp of Id:Id * When:TimestampWithPrecision * Unknown:Unknown

    type TimePrimitive =
    | TimeSpan of TimeSpan
    | TimeStamp of TimeStamp

    (** Style Definitions
    *)

    type Color = int

    type DisplayMode =
    | Default
    | Hide

    type ColorMode =
    | Normal
    | Random

    type UnitType =
    | Fraction
    | Pixels
    | InsetPixels

    type Unit = Unit of Value:int * Type:UnitType
    type UnitF = UnitF of Value:float * Type:UnitType
    type UnitD = UnitD of Value:decimal * Type:UnitType
    type UnitG = UnitG of Value:GeoLocation.BaseUnit * Type:GeoLocation.BaseUnit
    type HotSpot = HotSpot of X:Unit * Y:Unit

    /// See https://developers.google.com/kml/documentation/kmlreference#balloonstyle
    type BalloonStyle = {
        Id              : Id
        BackgroundColor : Color
        TextColor       : Color
        Text            : string
        DisplayMode     : DisplayMode
        Unknown         : Unknown
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#colorstyle
    type ColorStyleInfo = {
        Id              : Id
        Color           : Color
        ColorMode       : ColorMode
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#iconstyle
    type IconStyle = {
        Info            : ColorStyleInfo
        Scale           : float
        Heading         : float
        Icon            : Uri
        HotSpot         : HotSpot
        Unknown         : Unknown
    }
    with
        member this.Id = this.Info.Id
        member this.Color = this.Info.Color
        member this.ColorMode = this.Info.ColorMode

    /// See https://developers.google.com/kml/documentation/kmlreference#linestyle
    type LineStyle = {
        Info            : ColorStyleInfo
        Width           : float
        OuterColor      : Color
        OuterWidth      : float
        PhysicalWidth   : float
        LabelVisibility : bool
        Unknown         : Unknown
    }
    with
        member this.Id = this.Info.Id
        member this.Color = this.Info.Id
        member this.ColorMode = this.Info.ColorMode

    /// See https://developers.google.com/kml/documentation/kmlreference#polystyle
    type PolyStyle = {
        Info            : ColorStyleInfo
        Fill            : bool
        Outline         : bool
        Unknown         : Unknown
    }
    with
        member this.Id = this.Info.Id
        member this.Color = this.Info.Id
        member this.ColorMode = this.Info.ColorMode

    /// https://developers.google.com/kml/documentation/kmlreference#labelstyle
    type LabelStyle = {
        Info            : ColorStyleInfo
        Scale           : float
        Unknown         : Unknown
    }
    with
        member this.Id = this.Info.Id
        member this.Color = this.Info.Id
        member this.ColorMode = this.Info.ColorMode

    /// See https://developers.google.com/kml/documentation/kmlreference#colorstyle
    type ColorStyle =
    | Line of LineStyle
    | Poly of PolyStyle
    | Icon of IconStyle
    | Label of LabelStyle

    type ListItemType =
    | Check
    | CheckOffOnly
    | CheckHideChildren
    | RadioFolder

    type ItemIconMode =
    | Open
    | Closed
    | Error
    | Fetching0
    | Fetching1
    | Fetching2

    type ItemIcon = {
        State           : ItemIconMode
        Href            : Uri
        Unknown         : Unknown
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#liststyle
    type ListStyle = {
        Id              : Id
        ListItemType    : ListItemType
        BackgroundColor : Color
        ItemIcon        : ItemIcon
    }

    (** Style Selector
    *)

    /// See https://developers.google.com/kml/documentation/kmlreference#style
    type Style = {
        Id              : Id
        IconStyle       : IconStyle
        LabelStyle      : LabelStyle
        LineStyle       : LineStyle
        PolyStyle       : PolyStyle
        BalloonStyle    : BalloonStyle
        ListStyle       : ListStyle
        Unknown         : Unknown
    }

    type StyleState =
    | Normal
    | Highlight

    type StyleMapPairItemTarget =
    | Url of Uri
    | Style of Style

    type StyleMapPairItem = {
        Id              : Id
        Key             : StyleState
        Target          : StyleMapPairItemTarget
        Unknown         : Unknown
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#stylemap
    type StyleMap = {
        Id              : Id
        Pairs           : StyleMapPairItem list
        Unknown         : Unknown
    }

    (*
     * Basic types that are used in multiple places
    *)

    /// See https://developers.google.com/kml/documentation/kmlreference#orientation
    type Orientation = Orientation of Heading:Degree * Tilt:Degree * Roll:Degree * Unknown:Unknown

    /// See https://developers.google.com/kml/documentation/kmlreference#location
    type Location = Coordinates

    /// See https://developers.google.com/kml/documentation/kmlreference#scale
    type Scale = Scale of X:float * Y:float * Z:float * Unknown:Unknown

    type AltitudeMode =
    | ClampToGround
    | RelativeToGround
    | Absolute
    | RelativeToSeaFloor
    | ClampToSeaFloor

    type RefreshMode =
    | OnChange
    | OnInterval
    | OnExpire

    type ViewRefreshMode =
    | Never
    | OnStop
    | OnRequest
    | OnRegion

    /// See https://developers.google.com/kml/documentation/kmlreference#latlonbox
    type LatLonBox = {
        North           : Degree
        South           : Degree
        East            : Degree
        West            : Degree
        Rotation        : Degree
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#latlonaltbox
    type LatLonAltBox = {
        AltitudeMode    : AltitudeMode
        MinAltitude     : Distance
        MaxAltitude     : Distance
        North           : Degree
        South           : Degree
        East            : Degree
        West            : Degree
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#lod
    type LevelOfDetail = {
        MinLodPixels    : int
        MaxLodPixels    : int
        MinFadeExtent   : int
        MaxFadeExtent   : int
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#region
    type Region = {
        Id              : Id
        LatLonAltBox    : LatLonAltBox
        Lod             : LevelOfDetail
        Unknown         : Unknown
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#gxlatlonquad
    type LatLonQuad = {
        Point1  : Coordinates
        Point2  : Coordinates
        Point3  : Coordinates
        Point4  : Coordinates
    }
    with
        member this.Points = [ this.Point1; this.Point2; this.Point3; this.Point4 ]

    /// See https://developers.google.com/kml/documentation/kmlreference#alias
    type Alias = Alias of Target:string * Source:string

    // TODO: Add proper implementation of the Web Map Service (WMS) bounding box specification
    type BoundingBox = BoundingBox of string

    /// See https://developers.google.com/kml/documentation/kmlreference#Link
    type Link = Link of Id:Id * Href:Uri * RefreshMode:RefreshMode * RefreshInterval:float * ViewRefreshMode:ViewRefreshMode * ViewRefreshTime:float * ViewBoundScale:float * ViewFormat:BoundingBox * HttpQuery:string * Unknown:Unknown

    type IconPalette = IconPalette of X:int * Y:int * W:int * H:int

    /// See https://developers.google.com/kml/documentation/kmlreference#icon
    type Icon = Icon of Id:Id * IconPalette:IconPalette * Href:Uri * RefreshMode:RefreshMode * RefreshInterval:float * ViewRefreshMode:ViewRefreshMode * ViewRefreshTime:float * ViewBoundScale:float * ViewFormat:BoundingBox * HttpQuery:string * Unknown:Unknown

    type ResourceMap = Alias list

    /// See https://developers.google.com/kml/documentation/kmlreference#point
    type Point = Point of Id:Id * Extrude:bool * AltitudeMode:AltitudeMode * Coordinates:Coordinates * Unknown:Unknown

    /// See https://developers.google.com/kml/documentation/kmlreference#linestring
    type LineString = LineString of Id:Id * Extrude:bool * Tesselate:bool * AltitudeMode:AltitudeMode * DrawOrder:int * Coordinates:(Coordinates list) * Unknown:Unknown

    /// See https://developers.google.com/kml/documentation/kmlreference#linearring
    type LinearRing = LinearRing of Id:Id * Extrude:bool * Tesselate:bool * AltitudeMode:AltitudeMode * Coordinates:(Coordinates list) * Unknown:Unknown

    /// See https://developers.google.com/kml/documentation/kmlreference#polygon
    type Polygon = Polygon of Id:Id * Extrude:bool * Tesselate:bool * AltitudeMode:AltitudeMode * OuterBoundaryIs:LinearRing * InnerBoundaryIs:(LinearRing list) * Unknown:Unknown

    /// See https://developers.google.com/kml/documentation/kmlreference#model
    type Model = Model of Id:Id * AltitudeMode:AltitudeMode * Location:Coordinates * Orientation:Orientation * Scale:Scale * Link:Link * ResourceMap:ResourceMap * Unknown:Unknown

    type SimpleData = SimpleData of Name:string * Value:string

    /// See https://developers.google.com/kml/documentation/kmlreference#extendeddata
    type ExtendedDataItem =
    | UntypedData of Name:string * DisplayName:(string option) * Value:string
    | SchemaData of Url:string * Data:(SimpleData list)
    | NamespaceData of Prefix:string * Data:XmlElement

    type ExtendedData = ExtendedDataItem list

    /// See https://developers.google.com/kml/documentation/kmlreference#gxtrack
    type Track = Track of Id:Id * AltitudeMode:AltitudeMode * When:TimestampWithPrecision * Coord:Coordinates * Angles:Orientation * Model:Model * ExtendedData:ExtendedData * Unknown:Unknown

    /// See https://developers.google.com/kml/documentation/kmlreference#gxmultitrack
    type MultiTrack = MultiTrack of Id:Id * AltitudeMode:AltitudeMode * Interpolate:bool * Track:(Track list) * Unknown:Unknown

    /// See https://developers.google.com/kml/documentation/kmlreference#geometry
    type Geometry =
    | Point of Point
    | LineString of LineString
    | LinearRing of LinearRing
    | Polygon of Polygon
    | Track of Track
    | MultiTrack of MultiTrack
    | MultiGeometry of MultiGeometry
    and MultiGeometry = MultiGeometry of Geometry list

    type XmlnsAtom = string
    type XmlnsXal = string
    type NCName = string

    /// See https://developers.google.com/kml/documentation/kmlreference#object
    type ObjectInfo = {
        Id : Id
        TargetId : NCName
    }

    type ViewerOptionsName =
    | StreetView
    | HistoricalImagery
    | Sunlight

    type ViewerOptions = ViewerOptions of Name:ViewerOptionsName * Enabled:bool

    /// See https://developers.google.com/kml/documentation/kmlreference#abstractview
    type AbstractViewInfo = {
        Info            : ObjectInfo
        ViewerOptions   : ViewerOptions
        TimePrimitive   : TimePrimitive
    }
    with
        member this.Id = this.Info.Id

    /// See https://developers.google.com/kml/documentation/kmlreference#camera
    type Camera = {
        Info            : AbstractViewInfo
        Coordinates     : Coordinates
        Orientation     : Orientation
        AltitudeMode    : AltitudeMode
        Unknown         : Unknown
    }
    with
        member this.Id = this.Info.Id
        member this.TimePrimitive = this.Info.TimePrimitive
        member this.ViewerOptions = this.Info.ViewerOptions

    /// See https://developers.google.com/kml/documentation/kmlreference#lookat
    type LookAt = {
        Info            : AbstractViewInfo
        Coordinates     : Coordinates
        Orientation     : Orientation
        AltitudeMode    : AltitudeMode
        Unknown         : Unknown
    }
    with
        member this.Id = this.Info.Id
        member this.TimePrimitive = this.Info.TimePrimitive
        member this.ViewerOptions = this.Info.ViewerOptions

    type AbstractView =
    | Camera of Camera
    | LookAt of LookAt

    (** Features
    *)

    type StyleSelectorItem =
    | Style of Style
    | StyleMap of StyleMap

    type StyleSelector = StyleSelectorItem list

    type Metadata = string

    /// See https://developers.google.com/kml/documentation/kmlreference#feature
    type FeatureInfo = {
        Id              : Id
        Name            : string
        Visibility      : bool
        Open            : bool
        Author          : XmlnsAtom
        Link            : Uri
        Address         : string
        AddressDetails  : XmlnsXal
        PhoneNumber     : string
        Snippet         : string
        Description     : string
        AbstractView    : AbstractView
        TimePrimitive   : TimePrimitive
        StyleUrl        : Uri
        StyleSelector   : StyleSelector
        Region          : Region
        Metadata        : Metadata
        ExtendedData    : ExtendedData
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#overlay
    type OverlayInfo = {
        Info            : FeatureInfo
        Color           : Color
        DrawOrder       : int
        Icon            : Uri
    }
    with
        member this.Id = this.Info.Id

    type SimpleFieldType =
    | String
    | Int
    | UInt
    | Short
    | UShort
    | Float
    | Double
    | Bool

    type SimpleField = {
        Id              : Id
        Name            : string
        Type            : SimpleFieldType
        DisplayName     : string
    }

    type Schema = {
        Id              : Id
        Name            : string
        Elements        : SimpleField list
    }

    type ViewVolume = ViewVolume of Left:Degree * Right:Degree * Bottom:Degree * Top:Degree * Near:GeoLocation.BaseUnit

    type GridOrigin =
    | LowerLeft
    | UpperLeft

    type ImagePyramid = ImagePyramid of TileSize:int * MaxWidth:int * MaxHeight:int * GridOrigin:GridOrigin

    type Shape =
    | Rectangle
    | Cylinder
    | Sphere

    /// See https://developers.google.com/kml/documentation/kmlreference#photooverlay
    type PhotoOverlay = {
        Info            : OverlayInfo
        Rotation        : Degree
        ViewVolume      : ViewVolume
        ImagePyramid    : ImagePyramid
        Point           : Coordinates
        Shape           : Shape
        Unknown         : Unknown
    }
    with
        member this.Id = this.Info.Id

    type XY = XY of X:UnitG * Y:UnitG

    /// See https://developers.google.com/kml/documentation/kmlreference#screenoverlay
    type ScreenOverlay = {
        Info            : OverlayInfo
        OverlayXY       : XY
        ScreenXY        : XY
        RotationXY      : XY
        SizeXY          : XY
        Rotation        : Degree
        Unknown         : Unknown
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#groundoverlay
    type GroundOverlay = {
        Info            : OverlayInfo
        Altitude        : Distance
        AltitudeMode    : AltitudeMode
        LatLonBox       : LatLonBox
        LatLonQuad      : LatLonQuad
        Unknown         : Unknown
    }
    with
        member this.Id = this.Info.Id

    /// See https://developers.google.com/kml/documentation/kmlreference#networklink
    type NetworkLink = {
        Info                : FeatureInfo
        RefreshVisibility   : bool
        FlyToView           : bool
        Link                : Id
        Unknown             : Unknown
    }
    with
        member this.Id = this.Info.Id

    /// See https://developers.google.com/kml/documentation/kmlreference#placemark
    type Placemark = {
        Info                : FeatureInfo
        Geometry            : Geometry
        Unknown             : Unknown
    }
    with
        member this.Id = this.Info.Id

    type FlyToMode =
    | Smooth
    | Bounce

    /// See https://developers.google.com/kml/documentation/kmlreference#gxflyto
    type FlyToPrimitive = {
        Id              : Id
        Duration        : float
        FlyToMode       : FlyToMode
        View            : AbstractView
        Unknown         : Unknown
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#gxsoundcue
    type SoundCuePrimitive = {
        Id              : Id
        Target          : Uri
        DelayedStart    : float
        Unknown         : Unknown
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#gxwait
    type WaitPrimitive = {
        Id              : Id
        Duration        : float
        Unknown         : Unknown
    }

    type PlayMode =
    | Pause

    /// See https://developers.google.com/kml/documentation/kmlreference#gxtourcontrol
    type TourControlPrimitive = {
        Id              : Id
        PlayMode        : PlayMode
        Unknown         : Unknown
    }

    /// See https://developers.google.com/kml/documentation/kmlreference#feature
    type Feature =
    | Container     of ContainerItem
    | Overlay       of OverlayItem
    | NetworkLink   of NetworkLink
    | Placemark     of Placemark
    | Tour          of Tour
    /// See https://developers.google.com/kml/documentation/kmlreference#container
    and ContainerItem =
    | Folder        of Folder
    | Document      of Document
    /// See https://developers.google.com/kml/documentation/kmlreference#folder
    and Folder      = Folder of Info:FeatureInfo * Elements:(Feature list) * Unknown:Unknown
    and Document    = Document of Info:FeatureInfo * Elements:(Feature list) * Schemas:(Schema list) * Unknown:Unknown
    /// See https://developers.google.com/kml/documentation/kmlreference#overlay
    and OverlayItem =
    | PhotoOverlay  of PhotoOverlay
    | ScreenOverlay of ScreenOverlay
    | GroundOverlay of GroundOverlay
    /// See https://developers.google.com/kml/documentation/kmlreference#gxtour
    and Tour = {
        Id                  : Id
        Description         : string
        Playlist            : TourPrimitive list
        Unknown             : Unknown
    }
    /// See https://developers.google.com/kml/documentation/kmlreference#gxtourprimitive
    and TourPrimitive =
    | FlyTo             of FlyToPrimitive
    | AnimatedUpdate    of AnimatedUpdatePrimitive
    | TourControl       of TourControlPrimitive
    | Wait              of WaitPrimitive
    | SoundCue          of SoundCuePrimitive
    /// See https://developers.google.com/kml/documentation/kmlreference#gxanimatedupdate
    and AnimatedUpdatePrimitive = {
        Id              : Id
        Duration        : float
        DelayedStart    : float
        Update          : Update
        Unknown         : Unknown
    }
    /// See https://developers.google.com/kml/documentation/kmlreference#syntax_221
    and Update = {
        AttributeTargets    : Uri
        Items               : UpdateItem list
    }
    and UpdateItem =
    | Change of Feature
    | Create of Feature
    | Delete of Feature

    (** TODO: Parsing of tours, replace TargetId with Id
    * 
    *)

    /// See https://developers.google.com/kml/documentation/kmlreference#gxplaylist
    type PlayList = TourPrimitive list

    type UpdateType =
    | Change
    | Create
    | Delete

    /// See https://developers.google.com/kml/documentation/kmlreference#networklinkcontrol
    type NetworkLinkControl = {
        MinRefreshPeriod        : float
        MaxSessionLength        : float
        Cookie                  : string
        Message                 : string
        LinkName                : string
        LinkDescription         : string
        LinkSnippet             : string
        Expires                 : DateTime
        Update                  : UpdateType
        AbstractView            : AbstractView
    }
