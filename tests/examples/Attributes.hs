{- |
   Module      : Data.GraphViz.Attributes
   Description : Definition of the Graphviz attributes.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the various attributes that different parts of
   a Graphviz graph can have.  These attributes are based on the
   documentation found at:
     <http://graphviz.org/doc/info/attrs.html>

   For more information on usage, etc. please see that document.

   A summary of known current constraints\/limitations\/differences:

   * There might still be a few cases where quotes are still not
     escaped/parsed correctly; if you find such a situation, please
     let me know; however, you should be able to use 'String' values
     directly without having to worry about when quotes are required
     or extra escaping of quote characters as 'PrintDot' and
     'ParseDot' instances for 'String' should take care of that
     for you.

   * Note that for an edge, in /Dot/ parlance if the edge goes from
     /A/ to /B/, then /A/ is the tail node and /B/ is the head node
     (since /A/ is at the tail end of the arrow).

   * ColorList and PointfList are defined as actual lists (but
     'LayerList' is not).  Note that for the Color 'Attribute' for
     node values, only a single Color is valid; edges are allowed
     multiple colors with one spline/arrow per color in the list (but
     you must have at least one 'Color' in the list).  This might be
     changed in future.

   * Style is implemented as a list of 'StyleItem' values; note that
     empty lists are not allowed.

   * A lot of values have a possible value of @none@.  These now
     have custom constructors.  In fact, most constructors have been
     expanded upon to give an idea of what they represent rather than
     using generic terms.

   * @PointF@ and 'Point' have been combined, and feature support for pure
     'Int'-based co-ordinates as well as 'Double' ones (i.e. no floating
     point-only points for Point).  The optional '!' and third value
     for Point are not available.

   * 'Rect' uses two 'Point' values to denote the lower-left and
     top-right corners.

   * The two 'LabelLoc' attributes have been combined.

   * The defined 'LayerSep' is not used to parse 'LayerRange' or
     'LayerList'; the default (@[' ', ':', '\t']@) is instead used.

   * @SplineType@ has been replaced with @['Spline']@.

   * Only polygon-based 'Shape's are available.

   * 'PortPos' only has the 'CompassPoint' option, not
     @PortName[:CompassPoint]@ (since record shapes aren't allowed,
     and parsing HTML-like labels could be problematic).

   * Not every 'Attribute' is fully documented/described.  However,
     all those which have specific allowed values should be covered.

   * Deprecated 'Overlap' algorithms are not defined.

   * The global @Orientation@ attribute is not defined, as it is
     difficult to distinguish from the node-based 'Orientation'
     'Attribute'; also, its behaviour is duplicated by 'Rotate'.

 -}
module Data.GraphViz.Attributes
    ( -- * The actual /Dot/ attributes.
      Attribute(..)
    , Attributes
      -- ** Validity functions on @Attribute@ values.
    , usedByGraphs
    , usedBySubGraphs
    , usedByClusters
    , usedByNodes
    , usedByEdges
      -- * Value types for @Attribute@s.
    , EscString
    , URL(..)
    , ArrowType(..)
    , AspectType(..)
    , Rect(..)
    , ClusterMode(..)
    , DirType(..)
    , DEConstraints(..)
    , DPoint(..)
    , ModeType(..)
    , Model(..)
    , Label(..)
    , Point(..)
    , Overlap(..)
    , LayerRange(..)
    , LayerID(..)
    , LayerList(..)
    , OutputMode(..)
    , Pack(..)
    , PackMode(..)
    , Pos(..)
    , EdgeType(..)
    , PageDir(..)
    , Spline(..)
    , QuadType(..)
    , Root(..)
    , RankType(..)
    , RankDir(..)
    , Shape(..)
    , SmoothType(..)
    , StartType(..)
    , STStyle(..)
    , StyleItem(..)
    , StyleName(..)
    , PortPos(..)
    , CompassPoint(..)
    , ViewPort(..)
    , FocusType(..)
    , VerticalPlacement(..)
    , ScaleType(..)
    , Justification(..)
    , Ratios(..)
    , module Data.GraphViz.Attributes.Colors
      -- * Types representing the Dot grammar for @ArrowType@.
    , ArrowShape(..)
    , ArrowModifier(..)
    , ArrowFill(..)
    , ArrowSide(..)
      -- ** Default @ArrowType@ aliases.
      -- *** The 9 primitive @ArrowShape@s.
    , box
    , crow
    , diamond
    , dotArrow
    , inv
    , noArrow
    , normal
    , tee
    , vee
      -- *** 5 derived Arrows.
    , oDot
    , invDot
    , invODot
    , oBox
    , oDiamond
      -- *** 5 supported cases for backwards compatibility
    , eDiamond
    , openArr
    , halfOpen
    , emptyArr
    , invEmpty
      -- ** @ArrowModifier@ instances
    , noMods
    , openMod
      -- * Other exported functions\/values
    , defLayerSep
    , notLayerSep
    ) where

import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Util
import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import Data.Char(toLower)
import Data.Maybe(isJust)
import Control.Arrow(first)
import Control.Monad(liftM, liftM2)

-- -----------------------------------------------------------------------------

{- |

   These attributes have been implemented in a /permissive/ manner:
   that is, rather than split them up based on which type of value
   they are allowed, they have all been included in the one data type,
   with functions to determine if they are indeed valid for what
   they're being applied to.

   To interpret the /Valid for/ listings:

     [@G@] Valid for Graphs.

     [@C@] Valid for Clusters.

     [@S@] Valid for Sub-Graphs (and also Clusters).

     [@N@] Valid for Nodes.

     [@E@] Valid for Edges.

   The /Default/ listings are those that the various Graphviz commands
   use if that 'Attribute' isn't specified (in cases where this is
   /none/, this is equivalent to a 'Nothing' value; that is, no value
   is used).  The /Parsing Default/ listings represent what value is
   used (i.e. corresponds to 'True') when the 'Attribute' name is
   listed on its own in /Dot/ source code.
-}
data Attribute
    = Damping Double                   -- ^ /Valid for/: G; /Default/: @0.99@; /Minimum/: @0.0@; /Notes/: neato only
    | K Double                         -- ^ /Valid for/: GC; /Default/: @0.3@; /Minimum/: @0@; /Notes/: sfdp, fdp only
    | URL URL                          -- ^ /Valid for/: ENGC; /Default/: none; /Notes/: svg, postscript, map only
    | ArrowHead ArrowType              -- ^ /Valid for/: E; /Default/: @'normal'@
    | ArrowSize Double                 -- ^ /Valid for/: E; /Default/: @1.0@; /Minimum/: @0.0@
    | ArrowTail ArrowType              -- ^ /Valid for/: E; /Default/: @'normal'@
    | Aspect AspectType                -- ^ /Valid for/: G; /Notes/: dot only
    | Bb Rect                          -- ^ /Valid for/: G; /Notes/: write only
    | BgColor Color                    -- ^ /Valid for/: GC; /Default/: X11Color 'Transparent'
    | Center Bool                      -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'
    | Charset String                   -- ^ /Valid for/: G; /Default/: @\"UTF-8\"@
    | ClusterRank ClusterMode          -- ^ /Valid for/: G; /Default/: @'Local'@; /Notes/: dot only
    | ColorScheme ColorScheme          -- ^ /Valid for/: ENCG; /Default/: @'X11'@
    | Color [Color]                    -- ^ /Valid for/: ENC; /Default/: @X11Color 'Black'@
    | Comment String                   -- ^ /Valid for/: ENG; /Default/: @\"\"@
    | Compound Bool                    -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: dot only
    | Concentrate Bool                 -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'
    | Constraint Bool                  -- ^ /Valid for/: E; /Default/: @'True'@; /Parsing Default/: 'True'; /Notes/: dot only
    | Decorate Bool                    -- ^ /Valid for/: E; /Default/: @'False'@; /Parsing Default/: 'True'
    | DefaultDist Double               -- ^ /Valid for/: G; /Default/: @1+(avg. len)*sqrt(|V|)@; /Minimum/: @epsilon@; /Notes/: neato only
    | Dimen Int                        -- ^ /Valid for/: G; /Default/: @2@; /Minimum/: @2@; /Notes/: sfdp, fdp, neato only
    | Dim Int                          -- ^ /Valid for/: G; /Default/: @2@; /Minimum/: @2@; /Notes/: sfdp, fdp, neato only
    | Dir DirType                      -- ^ /Valid for/: E; /Default/: @'Forward'@ (directed), @'NoDir'@ (undirected)
    | DirEdgeConstraints DEConstraints -- ^ /Valid for/: G; /Default/: @'NoConstraints'@; /Parsing Default/: 'EdgeConstraints'; /Notes/: neato only
    | Distortion Double                -- ^ /Valid for/: N; /Default/: @0.0@; /Minimum/: @-100.0@
    | DPI Double                       -- ^ /Valid for/: G; /Default/: @96.0@, @0.0@; /Notes/: svg, bitmap output only; \"resolution\" is a synonym
    | EdgeURL URL                      -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, map only
    | EdgeTarget EscString             -- ^ /Valid for/: E; /Default/: none; /Notes/: svg, map only
    | EdgeTooltip EscString            -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, cmap only
    | Epsilon Double                   -- ^ /Valid for/: G; /Default/: @.0001 * # nodes@ (@mode == 'KK'@), @.0001@ (@mode == 'Major'@); /Notes/: neato only
    | ESep DPoint                      -- ^ /Valid for/: G; /Default/: @+3@; /Notes/: not dot
    | FillColor Color                  -- ^ /Valid for/: NC; /Default/: @X11Color 'LightGray'@ (nodes), @X11Color 'Black'@ (clusters)
    | FixedSize Bool                   -- ^ /Valid for/: N; /Default/: @'False'@; /Parsing Default/: 'True'
    | FontColor Color                  -- ^ /Valid for/: ENGC; /Default/: @X11Color 'Black'@
    | FontName String                  -- ^ /Valid for/: ENGC; /Default/: @\"Times-Roman\"@
    | FontNames String                 -- ^ /Valid for/: G; /Default/: @\"\"@; /Notes/: svg only
    | FontPath String                  -- ^ /Valid for/: G; /Default/: system-dependent
    | FontSize Double                  -- ^ /Valid for/: ENGC; /Default/: @14.0@; /Minimum/: @1.0@
    | Group String                     -- ^ /Valid for/: N; /Default/: @\"\"@; /Notes/: dot only
    | HeadURL URL                      -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, map only
    | HeadClip Bool                    -- ^ /Valid for/: E; /Default/: @'True'@; /Parsing Default/: 'True'
    | HeadLabel Label                  -- ^ /Valid for/: E; /Default/: @\"\"@
    | HeadPort PortPos                 -- ^ /Valid for/: E; /Default/: @'PP' 'CenterPoint'@
    | HeadTarget EscString             -- ^ /Valid for/: E; /Default/: none; /Notes/: svg, map only
    | HeadTooltip EscString            -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, cmap only
    | Height Double                    -- ^ /Valid for/: N; /Default/: @0.5@; /Minimum/: @0.02@
    | ID Label                         -- ^ /Valid for/: GNE; /Default/: @\"\"@; /Notes/: svg, postscript, map only
    | Image String                     -- ^ /Valid for/: N; /Default/: @\"\"@
    | ImageScale ScaleType             -- ^ /Valid for/: N; /Default/: @'NoScale'@; /Parsing Default/: 'UniformScale'
    | LabelURL URL                     -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, map only
    | LabelAngle Double                -- ^ /Valid for/: E; /Default/: @-25.0@; /Minimum/: @-180.0@
    | LabelDistance Double             -- ^ /Valid for/: E; /Default/: @1.0@; /Minimum/: @0.0@
    | LabelFloat Bool                  -- ^ /Valid for/: E; /Default/: @'False'@; /Parsing Default/: 'True'
    | LabelFontColor Color             -- ^ /Valid for/: E; /Default/: @X11Color 'Black'@
    | LabelFontName String             -- ^ /Valid for/: E; /Default/: @\"Times-Roman\"@
    | LabelFontSize Double             -- ^ /Valid for/: E; /Default/: @14.0@; /Minimum/: @1.0@
    | LabelJust Justification          -- ^ /Valid for/: GC; /Default/: @'JCenter'@
    | LabelLoc VerticalPlacement       -- ^ /Valid for/: GCN; /Default/: @'VTop'@ (clusters), @'VBottom'@ (root graphs), @'VCenter'@ (nodes)
    | LabelTarget EscString            -- ^ /Valid for/: E; /Default/: none; /Notes/: svg, map only
    | LabelTooltip EscString           -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, cmap only
    | Label Label                      -- ^ /Valid for/: ENGC; /Default/: @'StrLabel' \"\N\"@ (nodes), @'StrLabel' \"\"@ (otherwise)
    | Landscape Bool                   -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'
    | LayerSep String                  -- ^ /Valid for/: G; /Default/: @\" :\t\"@
    | Layers LayerList                 -- ^ /Valid for/: G; /Default/: @\"\"@
    | Layer LayerRange                 -- ^ /Valid for/: EN; /Default/: @\"\"@
    | Layout String                    -- ^ /Valid for/: G; /Default/: @\"\"@
    | Len Double                       -- ^ /Valid for/: E; /Default/: @1.0@ (neato), @0.3@ (fdp); /Notes/: fdp, neato only
    | LevelsGap Double                 -- ^ /Valid for/: G; /Default/: @0.0@; /Notes/: neato only
    | Levels Int                       -- ^ /Valid for/: G; /Default/: @MAXINT@; /Minimum/: @0@; /Notes/: sfdp only
    | LHead String                     -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: dot only
    | LPos Point                       -- ^ /Valid for/: EGC; /Notes/: write only
    | LTail String                     -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: dot only
    | Margin DPoint                    -- ^ /Valid for/: NG; /Default/: device-dependent
    | MaxIter Int                      -- ^ /Valid for/: G; /Default/: @100 * # nodes@ (@mode == 'KK'@), @200@ (@mode == 'Major'@), @600@ (fdp); /Notes/: fdp, neato only
    | MCLimit Double                   -- ^ /Valid for/: G; /Default/: @1.0@; /Notes/: dot only
    | MinDist Double                   -- ^ /Valid for/: G; /Default/: @1.0@; /Minimum/: @0.0@; /Notes/: circo only
    | MinLen Int                       -- ^ /Valid for/: E; /Default/: @1@; /Minimum/: @0@; /Notes/: dot only
    | Model Model                      -- ^ /Valid for/: G; /Default/: @'ShortPath'@; /Notes/: neato only
    | Mode ModeType                    -- ^ /Valid for/: G; /Default/: @'Major'@; /Notes/: neato only
    | Mosek Bool                       -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: neato only; requires the Mosek software
    | NodeSep Double                   -- ^ /Valid for/: G; /Default/: @0.25@; /Minimum/: @0.02@; /Notes/: dot only
    | NoJustify Bool                   -- ^ /Valid for/: GCNE; /Default/: @'False'@; /Parsing Default/: 'True'
    | Normalize Bool                   -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: not dot
    | Nslimit1 Double                  -- ^ /Valid for/: G; /Notes/: dot only
    | Nslimit Double                   -- ^ /Valid for/: G; /Notes/: dot only
    | Ordering String                  -- ^ /Valid for/: G; /Default/: @\"\"@; /Notes/: dot only
    | Orientation Double               -- ^ /Valid for/: N; /Default/: @0.0@; /Minimum/: @360.0@
    | OutputOrder OutputMode           -- ^ /Valid for/: G; /Default/: @'BreadthFirst'@
    | OverlapScaling Double            -- ^ /Valid for/: G; /Default/: @-4@; /Minimum/: @-1.0e10@; /Notes/: prism only
    | Overlap Overlap                  -- ^ /Valid for/: G; /Default/: @'KeepOverlaps'@; /Parsing Default/: 'KeepOverlaps'; /Notes/: not dot
    | PackMode PackMode                -- ^ /Valid for/: G; /Default/: @'PackNode'@; /Notes/: not dot
    | Pack Pack                        -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'DoPack'; /Notes/: not dot
    | Pad DPoint                       -- ^ /Valid for/: G; /Default/: @'DVal' 0.0555@ (4 points)
    | PageDir PageDir                  -- ^ /Valid for/: G; /Default/: @'BL'@
    | Page Point                       -- ^ /Valid for/: G
    | PenColor Color                   -- ^ /Valid for/: C; /Default/: @X11Color 'Black'@
    | PenWidth Double                  -- ^ /Valid for/: CNE; /Default/: @1.0@; /Minimum/: @0.0@
    | Peripheries Int                  -- ^ /Valid for/: NC; /Default/: shape default (nodes), @1@ (clusters); /Minimum/: 0
    | Pin Bool                         -- ^ /Valid for/: N; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: fdp, neato only
    | Pos Pos                          -- ^ /Valid for/: EN
    | QuadTree QuadType                -- ^ /Valid for/: G; /Default/: @'NormalQT'@; /Parsing Default/: 'NormalQT'; /Notes/: sfdp only
    | Quantum Double                   -- ^ /Valid for/: G; /Default/: @0.0@; /Minimum/: @0.0@
    | RankDir RankDir                  -- ^ /Valid for/: G; /Default/: @'TB'@; /Notes/: dot only
    | RankSep Double                   -- ^ /Valid for/: G; /Default/: @0.5@ (dot), @1.0@ (twopi); /Minimum/: 0.02; /Notes/: twopi, dot only
    | Rank RankType                    -- ^ /Valid for/: S; /Notes/: dot only
    | Ratio Ratios                     -- ^ /Valid for/: G
    | Rects Rect                       -- ^ /Valid for/: N; /Notes/: write only
    | Regular Bool                     -- ^ /Valid for/: N; /Default/: @'False'@; /Parsing Default/: 'True'
    | ReMinCross Bool                  -- ^ /Valid for/: G; /Default/: @'False'@; /Parsing Default/: 'True'; /Notes/: dot only
    | RepulsiveForce Double            -- ^ /Valid for/: G; /Default/: @1.0@; /Minimum/: @0.0@; /Notes/: sfdp only
    | Root Root                        -- ^ /Valid for/: GN; /Default/: @'NodeName' \"\"@ (graphs), @'NotCentral'@ (nodes); /Parsing Default/: 'IsCentral'; /Notes/: circo, twopi only
    | Rotate Int                       -- ^ /Valid for/: G; /Default/: @0@
    | SameHead String                  -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: dot only
    | SameTail String                  -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: dot only
    | SamplePoints Int                 -- ^ /Valid for/: N; /Default/: @8@ (output), @20@ (overlap and image maps)
    | SearchSize Int                   -- ^ /Valid for/: G; /Default/: @30@; /Notes/: dot only
    | Sep DPoint                       -- ^ /Valid for/: G; /Default/: @+4@; /Notes/: not dot
    | ShapeFile String                 -- ^ /Valid for/: N; /Default/: @\"\"@
    | Shape Shape                      -- ^ /Valid for/: N; /Default/: @'Ellipse'@
    | ShowBoxes Int                    -- ^ /Valid for/: ENG; /Default/: @0@; /Minimum/: @0@; /Notes/: dot only
    | Sides Int                        -- ^ /Valid for/: N; /Default/: @4@; /Minimum/: @0@
    | Size Point                       -- ^ /Valid for/: G
    | Skew Double                      -- ^ /Valid for/: N; /Default/: @0.0@; /Minimum/: @-100.0@
    | Smoothing SmoothType             -- ^ /Valid for/: G; /Default/: @'NoSmooth'@; /Notes/: sfdp only
    | SortV Int                        -- ^ /Valid for/: GCN; /Default/: @0@; /Minimum/: @0@
    | Splines EdgeType                 -- ^ /Valid for/: G; /Parsing Default/: 'SplineEdges'
    | Start StartType                  -- ^ /Valid for/: G; /Default/: @\"\"@; /Notes/: fdp, neato only
    | StyleSheet String                -- ^ /Valid for/: G; /Default/: @\"\"@; /Notes/: svg only
    | Style [StyleItem]                -- ^ /Valid for/: ENC
    | TailURL URL                      -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, map only
    | TailClip Bool                    -- ^ /Valid for/: E; /Default/: @'True'@; /Parsing Default/: 'True'
    | TailLabel Label                  -- ^ /Valid for/: E; /Default/: @\"\"@
    | TailPort PortPos                 -- ^ /Valid for/: E; /Default/: center
    | TailTarget EscString             -- ^ /Valid for/: E; /Default/: none; /Notes/: svg, map only
    | TailTooltip EscString            -- ^ /Valid for/: E; /Default/: @\"\"@; /Notes/: svg, cmap only
    | Target EscString                 -- ^ /Valid for/: ENGC; /Default/: none; /Notes/: svg, map only
    | Tooltip EscString                -- ^ /Valid for/: NEC; /Default/: @\"\"@; /Notes/: svg, cmap only
    | TrueColor Bool                   -- ^ /Valid for/: G; /Parsing Default/: 'True'; /Notes/: bitmap output only
    | Vertices [Point]                 -- ^ /Valid for/: N; /Notes/: write only
    | ViewPort ViewPort                -- ^ /Valid for/: G; /Default/: none
    | VoroMargin Double                -- ^ /Valid for/: G; /Default/: @0.05@; /Minimum/: @0.0@; /Notes/: not dot
    | Weight Double                    -- ^ /Valid for/: E; /Default/: @1.0@; /Minimum/: @0@ (dot), @1@ (neato,fdp,sfdp)
    | Width Double                     -- ^ /Valid for/: N; /Default/: @0.75@; /Minimum/: @0.01@
    | Z Double                         -- ^ /Valid for/: N; /Default/: @0.0@; /Minimum/: @-MAXFLOAT@, @-1000@
      deriving (Eq, Ord, Show, Read)

type Attributes = [Attribute]

instance PrintDot Attribute where
    unqtDot (Damping v)            = printField "Damping" v
    unqtDot (K v)                  = printField "K" v
    unqtDot (URL v)                = printField "URL" v
    unqtDot (ArrowHead v)          = printField "arrowhead" v
    unqtDot (ArrowSize v)          = printField "arrowsize" v
    unqtDot (ArrowTail v)          = printField "arrowtail" v
    unqtDot (Aspect v)             = printField "aspect" v
    unqtDot (Bb v)                 = printField "bb" v
    unqtDot (BgColor v)            = printField "bgcolor" v
    unqtDot (Center v)             = printField "center" v
    unqtDot (Charset v)            = printField "charset" v
    unqtDot (ClusterRank v)        = printField "clusterrank" v
    unqtDot (ColorScheme v)        = printField "colorscheme" v
    unqtDot (Color v)              = printField "color" v
    unqtDot (Comment v)            = printField "comment" v
    unqtDot (Compound v)           = printField "compound" v
    unqtDot (Concentrate v)        = printField "concentrate" v
    unqtDot (Constraint v)         = printField "constraint" v
    unqtDot (Decorate v)           = printField "decorate" v
    unqtDot (DefaultDist v)        = printField "defaultdist" v
    unqtDot (Dimen v)              = printField "dimen" v
    unqtDot (Dim v)                = printField "dim" v
    unqtDot (Dir v)                = printField "dir" v
    unqtDot (DirEdgeConstraints v) = printField "diredgeconstraints" v
    unqtDot (Distortion v)         = printField "distortion" v
    unqtDot (DPI v)                = printField "dpi" v
    unqtDot (EdgeURL v)            = printField "edgeURL" v
    unqtDot (EdgeTarget v)         = printField "edgetarget" v
    unqtDot (EdgeTooltip v)        = printField "edgetooltip" v
    unqtDot (Epsilon v)            = printField "epsilon" v
    unqtDot (ESep v)               = printField "esep" v
    unqtDot (FillColor v)          = printField "fillcolor" v
    unqtDot (FixedSize v)          = printField "fixedsize" v
    unqtDot (FontColor v)          = printField "fontcolor" v
    unqtDot (FontName v)           = printField "fontname" v
    unqtDot (FontNames v)          = printField "fontnames" v
    unqtDot (FontPath v)           = printField "fontpath" v
    unqtDot (FontSize v)           = printField "fontsize" v
    unqtDot (Group v)              = printField "group" v
    unqtDot (HeadURL v)            = printField "headURL" v
    unqtDot (HeadClip v)           = printField "headclip" v
    unqtDot (HeadLabel v)          = printField "headlabel" v
    unqtDot (HeadPort v)           = printField "headport" v
    unqtDot (HeadTarget v)         = printField "headtarget" v
    unqtDot (HeadTooltip v)        = printField "headtooltip" v
    unqtDot (Height v)             = printField "height" v
    unqtDot (ID v)                 = printField "id" v
    unqtDot (Image v)              = printField "image" v
    unqtDot (ImageScale v)         = printField "imagescale" v
    unqtDot (LabelURL v)           = printField "labelURL" v
    unqtDot (LabelAngle v)         = printField "labelangle" v
    unqtDot (LabelDistance v)      = printField "labeldistance" v
    unqtDot (LabelFloat v)         = printField "labelfloat" v
    unqtDot (LabelFontColor v)     = printField "labelfontcolor" v
    unqtDot (LabelFontName v)      = printField "labelfontname" v
    unqtDot (LabelFontSize v)      = printField "labelfontsize" v
    unqtDot (LabelJust v)          = printField "labeljust" v
    unqtDot (LabelLoc v)           = printField "labelloc" v
    unqtDot (LabelTarget v)        = printField "labeltarget" v
    unqtDot (LabelTooltip v)       = printField "labeltooltip" v
    unqtDot (Label v)              = printField "label" v
    unqtDot (Landscape v)          = printField "landscape" v
    unqtDot (LayerSep v)           = printField "layersep" v
    unqtDot (Layers v)             = printField "layers" v
    unqtDot (Layer v)              = printField "layer" v
    unqtDot (Layout v)             = printField "layout" v
    unqtDot (Len v)                = printField "len" v
    unqtDot (LevelsGap v)          = printField "levelsgap" v
    unqtDot (Levels v)             = printField "levels" v
    unqtDot (LHead v)              = printField "lhead" v
    unqtDot (LPos v)               = printField "lp" v
    unqtDot (LTail v)              = printField "ltail" v
    unqtDot (Margin v)             = printField "margin" v
    unqtDot (MaxIter v)            = printField "maxiter" v
    unqtDot (MCLimit v)            = printField "mclimit" v
    unqtDot (MinDist v)            = printField "mindist" v
    unqtDot (MinLen v)             = printField "minlen" v
    unqtDot (Model v)              = printField "model" v
    unqtDot (Mode v)               = printField "mode" v
    unqtDot (Mosek v)              = printField "mosek" v
    unqtDot (NodeSep v)            = printField "nodesep" v
    unqtDot (NoJustify v)          = printField "nojustify" v
    unqtDot (Normalize v)          = printField "normalize" v
    unqtDot (Nslimit1 v)           = printField "nslimit1" v
    unqtDot (Nslimit v)            = printField "nslimit" v
    unqtDot (Ordering v)           = printField "ordering" v
    unqtDot (Orientation v)        = printField "orientation" v
    unqtDot (OutputOrder v)        = printField "outputorder" v
    unqtDot (OverlapScaling v)     = printField "overlap_scaling" v
    unqtDot (Overlap v)            = printField "overlap" v
    unqtDot (PackMode v)           = printField "packmode" v
    unqtDot (Pack v)               = printField "pack" v
    unqtDot (Pad v)                = printField "pad" v
    unqtDot (PageDir v)            = printField "pagedir" v
    unqtDot (Page v)               = printField "page" v
    unqtDot (PenColor v)           = printField "pencolor" v
    unqtDot (PenWidth v)           = printField "penwidth" v
    unqtDot (Peripheries v)        = printField "peripheries" v
    unqtDot (Pin v)                = printField "pin" v
    unqtDot (Pos v)                = printField "pos" v
    unqtDot (QuadTree v)           = printField "quadtree" v
    unqtDot (Quantum v)            = printField "quantum" v
    unqtDot (RankDir v)            = printField "rankdir" v
    unqtDot (RankSep v)            = printField "ranksep" v
    unqtDot (Rank v)               = printField "rank" v
    unqtDot (Ratio v)              = printField "ratio" v
    unqtDot (Rects v)              = printField "rects" v
    unqtDot (Regular v)            = printField "regular" v
    unqtDot (ReMinCross v)         = printField "remincross" v
    unqtDot (RepulsiveForce v)     = printField "repulsiveforce" v
    unqtDot (Root v)               = printField "root" v
    unqtDot (Rotate v)             = printField "rotate" v
    unqtDot (SameHead v)           = printField "samehead" v
    unqtDot (SameTail v)           = printField "sametail" v
    unqtDot (SamplePoints v)       = printField "samplepoints" v
    unqtDot (SearchSize v)         = printField "searchsize" v
    unqtDot (Sep v)                = printField "sep" v
    unqtDot (ShapeFile v)          = printField "shapefile" v
    unqtDot (Shape v)              = printField "shape" v
    unqtDot (ShowBoxes v)          = printField "showboxes" v
    unqtDot (Sides v)              = printField "sides" v
    unqtDot (Size v)               = printField "size" v
    unqtDot (Skew v)               = printField "skew" v
    unqtDot (Smoothing v)          = printField "smoothing" v
    unqtDot (SortV v)              = printField "sortv" v
    unqtDot (Splines v)            = printField "splines" v
    unqtDot (Start v)              = printField "start" v
    unqtDot (StyleSheet v)         = printField "stylesheet" v
    unqtDot (Style v)              = printField "style" v
    unqtDot (TailURL v)            = printField "tailURL" v
    unqtDot (TailClip v)           = printField "tailclip" v
    unqtDot (TailLabel v)          = printField "taillabel" v
    unqtDot (TailPort v)           = printField "tailport" v
    unqtDot (TailTarget v)         = printField "tailtarget" v
    unqtDot (TailTooltip v)        = printField "tailtooltip" v
    unqtDot (Target v)             = printField "target" v
    unqtDot (Tooltip v)            = printField "tooltip" v
    unqtDot (TrueColor v)          = printField "truecolor" v
    unqtDot (Vertices v)           = printField "vertices" v
    unqtDot (ViewPort v)           = printField "viewport" v
    unqtDot (VoroMargin v)         = printField "voro_margin" v
    unqtDot (Weight v)             = printField "weight" v
    unqtDot (Width v)              = printField "width" v
    unqtDot (Z v)                  = printField "z" v

    listToDot = unqtListToDot

instance ParseDot Attribute where
    parseUnqt = oneOf [ liftM Damping            $ parseField "Damping"
                      , liftM K                  $ parseField "K"
                      , liftM URL                $ parseFields ["URL", "href"]
                      , liftM ArrowHead          $ parseField "arrowhead"
                      , liftM ArrowSize          $ parseField "arrowsize"
                      , liftM ArrowTail          $ parseField "arrowtail"
                      , liftM Aspect             $ parseField "aspect"
                      , liftM Bb                 $ parseField "bb"
                      , liftM BgColor            $ parseField "bgcolor"
                      , liftM Center             $ parseFieldBool "center"
                      , liftM Charset            $ parseField "charset"
                      , liftM ClusterRank        $ parseField "clusterrank"
                      , liftM ColorScheme        $ parseField "colorscheme"
                      , liftM Color              $ parseField "color"
                      , liftM Comment            $ parseField "comment"
                      , liftM Compound           $ parseFieldBool "compound"
                      , liftM Concentrate        $ parseFieldBool "concentrate"
                      , liftM Constraint         $ parseFieldBool "constraint"
                      , liftM Decorate           $ parseFieldBool "decorate"
                      , liftM DefaultDist        $ parseField "defaultdist"
                      , liftM Dimen              $ parseField "dimen"
                      , liftM Dim                $ parseField "dim"
                      , liftM Dir                $ parseField "dir"
                      , liftM DirEdgeConstraints $ parseFieldDef EdgeConstraints "diredgeconstraints"
                      , liftM Distortion         $ parseField "distortion"
                      , liftM DPI                $ parseFields ["dpi", "resolution"]
                      , liftM EdgeURL            $ parseFields ["edgeURL", "edgehref"]
                      , liftM EdgeTarget         $ parseField "edgetarget"
                      , liftM EdgeTooltip        $ parseField "edgetooltip"
                      , liftM Epsilon            $ parseField "epsilon"
                      , liftM ESep               $ parseField "esep"
                      , liftM FillColor          $ parseField "fillcolor"
                      , liftM FixedSize          $ parseFieldBool "fixedsize"
                      , liftM FontColor          $ parseField "fontcolor"
                      , liftM FontName           $ parseField "fontname"
                      , liftM FontNames          $ parseField "fontnames"
                      , liftM FontPath           $ parseField "fontpath"
                      , liftM FontSize           $ parseField "fontsize"
                      , liftM Group              $ parseField "group"
                      , liftM HeadURL            $ parseFields ["headURL", "headhref"]
                      , liftM HeadClip           $ parseFieldBool "headclip"
                      , liftM HeadLabel          $ parseField "headlabel"
                      , liftM HeadPort           $ parseField "headport"
                      , liftM HeadTarget         $ parseField "headtarget"
                      , liftM HeadTooltip        $ parseField "headtooltip"
                      , liftM Height             $ parseField "height"
                      , liftM ID                 $ parseField "id"
                      , liftM Image              $ parseField "image"
                      , liftM ImageScale         $ parseFieldDef UniformScale "imagescale"
                      , liftM LabelURL           $ parseFields ["labelURL", "labelhref"]
                      , liftM LabelAngle         $ parseField "labelangle"
                      , liftM LabelDistance      $ parseField "labeldistance"
                      , liftM LabelFloat         $ parseFieldBool "labelfloat"
                      , liftM LabelFontColor     $ parseField "labelfontcolor"
                      , liftM LabelFontName      $ parseField "labelfontname"
                      , liftM LabelFontSize      $ parseField "labelfontsize"
                      , liftM LabelJust          $ parseField "labeljust"
                      , liftM LabelLoc           $ parseField "labelloc"
                      , liftM LabelTarget        $ parseField "labeltarget"
                      , liftM LabelTooltip       $ parseField "labeltooltip"
                      , liftM Label              $ parseField "label"
                      , liftM Landscape          $ parseFieldBool "landscape"
                      , liftM LayerSep           $ parseField "layersep"
                      , liftM Layers             $ parseField "layers"
                      , liftM Layer              $ parseField "layer"
                      , liftM Layout             $ parseField "layout"
                      , liftM Len                $ parseField "len"
                      , liftM LevelsGap          $ parseField "levelsgap"
                      , liftM Levels             $ parseField "levels"
                      , liftM LHead              $ parseField "lhead"
                      , liftM LPos               $ parseField "lp"
                      , liftM LTail              $ parseField "ltail"
                      , liftM Margin             $ parseField "margin"
                      , liftM MaxIter            $ parseField "maxiter"
                      , liftM MCLimit            $ parseField "mclimit"
                      , liftM MinDist            $ parseField "mindist"
                      , liftM MinLen             $ parseField "minlen"
                      , liftM Model              $ parseField "model"
                      , liftM Mode               $ parseField "mode"
                      , liftM Mosek              $ parseFieldBool "mosek"
                      , liftM NodeSep            $ parseField "nodesep"
                      , liftM NoJustify          $ parseFieldBool "nojustify"
                      , liftM Normalize          $ parseFieldBool "normalize"
                      , liftM Nslimit1           $ parseField "nslimit1"
                      , liftM Nslimit            $ parseField "nslimit"
                      , liftM Ordering           $ parseField "ordering"
                      , liftM Orientation        $ parseField "orientation"
                      , liftM OutputOrder        $ parseField "outputorder"
                      , liftM OverlapScaling     $ parseField "overlap_scaling"
                      , liftM Overlap            $ parseFieldDef KeepOverlaps "overlap"
                      , liftM PackMode           $ parseField "packmode"
                      , liftM Pack               $ parseFieldDef DoPack "pack"
                      , liftM Pad                $ parseField "pad"
                      , liftM PageDir            $ parseField "pagedir"
                      , liftM Page               $ parseField "page"
                      , liftM PenColor           $ parseField "pencolor"
                      , liftM PenWidth           $ parseField "penwidth"
                      , liftM Peripheries        $ parseField "peripheries"
                      , liftM Pin                $ parseFieldBool "pin"
                      , liftM Pos                $ parseField "pos"
                      , liftM QuadTree           $ parseFieldDef NormalQT "quadtree"
                      , liftM Quantum            $ parseField "quantum"
                      , liftM RankDir            $ parseField "rankdir"
                      , liftM RankSep            $ parseField "ranksep"
                      , liftM Rank               $ parseField "rank"
                      , liftM Ratio              $ parseField "ratio"
                      , liftM Rects              $ parseField "rects"
                      , liftM Regular            $ parseFieldBool "regular"
                      , liftM ReMinCross         $ parseFieldBool "remincross"
                      , liftM RepulsiveForce     $ parseField "repulsiveforce"
                      , liftM Root               $ parseFieldDef IsCentral "root"
                      , liftM Rotate             $ parseField "rotate"
                      , liftM SameHead           $ parseField "samehead"
                      , liftM SameTail           $ parseField "sametail"
                      , liftM SamplePoints       $ parseField "samplepoints"
                      , liftM SearchSize         $ parseField "searchsize"
                      , liftM Sep                $ parseField "sep"
                      , liftM ShapeFile          $ parseField "shapefile"
                      , liftM Shape              $ parseField "shape"
                      , liftM ShowBoxes          $ parseField "showboxes"
                      , liftM Sides              $ parseField "sides"
                      , liftM Size               $ parseField "size"
                      , liftM Skew               $ parseField "skew"
                      , liftM Smoothing          $ parseField "smoothing"
                      , liftM SortV              $ parseField "sortv"
                      , liftM Splines            $ parseFieldDef SplineEdges "splines"
                      , liftM Start              $ parseField "start"
                      , liftM StyleSheet         $ parseField "stylesheet"
                      , liftM Style              $ parseField "style"
                      , liftM TailURL            $ parseFields ["tailURL", "tailhref"]
                      , liftM TailClip           $ parseFieldBool "tailclip"
                      , liftM TailLabel          $ parseField "taillabel"
                      , liftM TailPort           $ parseField "tailport"
                      , liftM TailTarget         $ parseField "tailtarget"
                      , liftM TailTooltip        $ parseField "tailtooltip"
                      , liftM Target             $ parseField "target"
                      , liftM Tooltip            $ parseField "tooltip"
                      , liftM TrueColor          $ parseFieldBool "truecolor"
                      , liftM Vertices           $ parseField "vertices"
                      , liftM ViewPort           $ parseField "viewport"
                      , liftM VoroMargin         $ parseField "voro_margin"
                      , liftM Weight             $ parseField "weight"
                      , liftM Width              $ parseField "width"
                      , liftM Z                  $ parseField "z"
                      ]

    parse = parseUnqt

    parseList = parseUnqtList

-- | Determine if this Attribute is valid for use with Graphs.
usedByGraphs                      :: Attribute -> Bool
usedByGraphs Damping{}            = True
usedByGraphs K{}                  = True
usedByGraphs URL{}                = True
usedByGraphs Aspect{}             = True
usedByGraphs Bb{}                 = True
usedByGraphs BgColor{}            = True
usedByGraphs Center{}             = True
usedByGraphs Charset{}            = True
usedByGraphs ClusterRank{}        = True
usedByGraphs ColorScheme{}        = True
usedByGraphs Comment{}            = True
usedByGraphs Compound{}           = True
usedByGraphs Concentrate{}        = True
usedByGraphs DefaultDist{}        = True
usedByGraphs Dimen{}              = True
usedByGraphs Dim{}                = True
usedByGraphs DirEdgeConstraints{} = True
usedByGraphs DPI{}                = True
usedByGraphs Epsilon{}            = True
usedByGraphs ESep{}               = True
usedByGraphs FontColor{}          = True
usedByGraphs FontName{}           = True
usedByGraphs FontNames{}          = True
usedByGraphs FontPath{}           = True
usedByGraphs FontSize{}           = True
usedByGraphs ID{}                 = True
usedByGraphs LabelJust{}          = True
usedByGraphs LabelLoc{}           = True
usedByGraphs Label{}              = True
usedByGraphs Landscape{}          = True
usedByGraphs LayerSep{}           = True
usedByGraphs Layers{}             = True
usedByGraphs Layout{}             = True
usedByGraphs LevelsGap{}          = True
usedByGraphs Levels{}             = True
usedByGraphs LPos{}               = True
usedByGraphs Margin{}             = True
usedByGraphs MaxIter{}            = True
usedByGraphs MCLimit{}            = True
usedByGraphs MinDist{}            = True
usedByGraphs Model{}              = True
usedByGraphs Mode{}               = True
usedByGraphs Mosek{}              = True
usedByGraphs NodeSep{}            = True
usedByGraphs NoJustify{}          = True
usedByGraphs Normalize{}          = True
usedByGraphs Nslimit1{}           = True
usedByGraphs Nslimit{}            = True
usedByGraphs Ordering{}           = True
usedByGraphs OutputOrder{}        = True
usedByGraphs OverlapScaling{}     = True
usedByGraphs Overlap{}            = True
usedByGraphs PackMode{}           = True
usedByGraphs Pack{}               = True
usedByGraphs Pad{}                = True
usedByGraphs PageDir{}            = True
usedByGraphs Page{}               = True
usedByGraphs QuadTree{}           = True
usedByGraphs Quantum{}            = True
usedByGraphs RankDir{}            = True
usedByGraphs RankSep{}            = True
usedByGraphs Ratio{}              = True
usedByGraphs ReMinCross{}         = True
usedByGraphs RepulsiveForce{}     = True
usedByGraphs Root{}               = True
usedByGraphs Rotate{}             = True
usedByGraphs SearchSize{}         = True
usedByGraphs Sep{}                = True
usedByGraphs ShowBoxes{}          = True
usedByGraphs Size{}               = True
usedByGraphs Smoothing{}          = True
usedByGraphs SortV{}              = True
usedByGraphs Splines{}            = True
usedByGraphs Start{}              = True
usedByGraphs StyleSheet{}         = True
usedByGraphs Target{}             = True
usedByGraphs TrueColor{}          = True
usedByGraphs ViewPort{}           = True
usedByGraphs VoroMargin{}         = True
usedByGraphs _                    = False

-- | Determine if this Attribute is valid for use with Clusters.
usedByClusters               :: Attribute -> Bool
usedByClusters K{}           = True
usedByClusters URL{}         = True
usedByClusters BgColor{}     = True
usedByClusters ColorScheme{} = True
usedByClusters Color{}       = True
usedByClusters FillColor{}   = True
usedByClusters FontColor{}   = True
usedByClusters FontName{}    = True
usedByClusters FontSize{}    = True
usedByClusters LabelJust{}   = True
usedByClusters LabelLoc{}    = True
usedByClusters Label{}       = True
usedByClusters LPos{}        = True
usedByClusters NoJustify{}   = True
usedByClusters PenColor{}    = True
usedByClusters PenWidth{}    = True
usedByClusters Peripheries{} = True
usedByClusters Rank{}        = True
usedByClusters SortV{}       = True
usedByClusters Style{}       = True
usedByClusters Target{}      = True
usedByClusters Tooltip{}     = True
usedByClusters _             = False

-- | Determine if this Attribute is valid for use with SubGraphs.
usedBySubGraphs        :: Attribute -> Bool
usedBySubGraphs Rank{} = True
usedBySubGraphs _      = False

-- | Determine if this Attribute is valid for use with Nodes.
usedByNodes                :: Attribute -> Bool
usedByNodes URL{}          = True
usedByNodes ColorScheme{}  = True
usedByNodes Color{}        = True
usedByNodes Comment{}      = True
usedByNodes Distortion{}   = True
usedByNodes FillColor{}    = True
usedByNodes FixedSize{}    = True
usedByNodes FontColor{}    = True
usedByNodes FontName{}     = True
usedByNodes FontSize{}     = True
usedByNodes Group{}        = True
usedByNodes Height{}       = True
usedByNodes ID{}           = True
usedByNodes Image{}        = True
usedByNodes ImageScale{}   = True
usedByNodes LabelLoc{}     = True
usedByNodes Label{}        = True
usedByNodes Layer{}        = True
usedByNodes Margin{}       = True
usedByNodes NoJustify{}    = True
usedByNodes Orientation{}  = True
usedByNodes PenWidth{}     = True
usedByNodes Peripheries{}  = True
usedByNodes Pin{}          = True
usedByNodes Pos{}          = True
usedByNodes Rects{}        = True
usedByNodes Regular{}      = True
usedByNodes Root{}         = True
usedByNodes SamplePoints{} = True
usedByNodes ShapeFile{}    = True
usedByNodes Shape{}        = True
usedByNodes ShowBoxes{}    = True
usedByNodes Sides{}        = True
usedByNodes Skew{}         = True
usedByNodes SortV{}        = True
usedByNodes Style{}        = True
usedByNodes Target{}       = True
usedByNodes Tooltip{}      = True
usedByNodes Vertices{}     = True
usedByNodes Width{}        = True
usedByNodes Z{}            = True
usedByNodes _              = False

-- | Determine if this Attribute is valid for use with Edges.
usedByEdges                  :: Attribute -> Bool
usedByEdges URL{}            = True
usedByEdges ArrowHead{}      = True
usedByEdges ArrowSize{}      = True
usedByEdges ArrowTail{}      = True
usedByEdges ColorScheme{}    = True
usedByEdges Color{}          = True
usedByEdges Comment{}        = True
usedByEdges Constraint{}     = True
usedByEdges Decorate{}       = True
usedByEdges Dir{}            = True
usedByEdges EdgeURL{}        = True
usedByEdges EdgeTarget{}     = True
usedByEdges EdgeTooltip{}    = True
usedByEdges FontColor{}      = True
usedByEdges FontName{}       = True
usedByEdges FontSize{}       = True
usedByEdges HeadURL{}        = True
usedByEdges HeadClip{}       = True
usedByEdges HeadLabel{}      = True
usedByEdges HeadPort{}       = True
usedByEdges HeadTarget{}     = True
usedByEdges HeadTooltip{}    = True
usedByEdges ID{}             = True
usedByEdges LabelURL{}       = True
usedByEdges LabelAngle{}     = True
usedByEdges LabelDistance{}  = True
usedByEdges LabelFloat{}     = True
usedByEdges LabelFontColor{} = True
usedByEdges LabelFontName{}  = True
usedByEdges LabelFontSize{}  = True
usedByEdges LabelTarget{}    = True
usedByEdges LabelTooltip{}   = True
usedByEdges Label{}          = True
usedByEdges Layer{}          = True
usedByEdges Len{}            = True
usedByEdges LHead{}          = True
usedByEdges LPos{}           = True
usedByEdges LTail{}          = True
usedByEdges MinLen{}         = True
usedByEdges NoJustify{}      = True
usedByEdges PenWidth{}       = True
usedByEdges Pos{}            = True
usedByEdges SameHead{}       = True
usedByEdges SameTail{}       = True
usedByEdges ShowBoxes{}      = True
usedByEdges Style{}          = True
usedByEdges TailURL{}        = True
usedByEdges TailClip{}       = True
usedByEdges TailLabel{}      = True
usedByEdges TailPort{}       = True
usedByEdges TailTarget{}     = True
usedByEdges TailTooltip{}    = True
usedByEdges Target{}         = True
usedByEdges Tooltip{}        = True
usedByEdges Weight{}         = True
usedByEdges _                = False

{- Delete to here -}
-- -----------------------------------------------------------------------------

{- |

   Some 'Attribute's (mainly label-like ones) take a 'String' argument
   that allows for extra escape codes.  This library doesn't do any
   extra checks or special parsing for these escape codes, but usage
   of 'EscString' rather than 'String' indicates that the Graphviz
   tools will recognise these extra escape codes for these
   'Attribute's.

   The extra escape codes include (note that these are all 'String's):

     [@\\N@] Replace with the name of the node (for Node 'Attribute's).

     [@\\G@] Replace with the name of the graph (for Node 'Attribute's)
             or the name of the graph or cluster, whichever is
             applicable (for Graph, Cluster and Edge 'Attribute's).

     [@\\E@] Replace with the name of the edge, formed by the two
             adjoining nodes and the edge type (for Edge 'Attribute's).

     [@\\T@] Replace with the name of the tail node (for Edge
             'Attribute's).

     [@\\H@] Replace with the name of the head node (for Edge
             'Attribute's).

     [@\\L@] Replace with the object's label (for all 'Attribute's).

   Also, if the 'Attribute' in question is 'Label', 'HeadLabel' or
   'TailLabel', then @\\n@, @\\l@ and @\\r@ split the label into lines
   centered, left-justified and right-justified respectively.

 -}
type EscString = String

-- -----------------------------------------------------------------------------

-- | No checks are placed on the content of a 'URL' value; however,
--   you should ensure that it does not contain any \'@>@\' or \'@<@\'
--   characters; Graphviz might care about escaping other characters
--   properly, but for the purposes of this library the presence of
--   these characters will make it harder to parse URLs.
newtype URL = UStr { urlString :: EscString }
    deriving (Eq, Ord, Show, Read)

instance PrintDot URL where
    unqtDot = wrap (char '<') (char '>')
              -- Explicitly use text here... no quotes!
              . text . urlString

instance ParseDot URL where
    parseUnqt = liftM UStr
                $ bracket (character open)
                          (character close)
                          (many1 $ satisfy ((/=) close))
        where
          open = '<'
          close = '>'

    -- No quotes
    parse = parseUnqt

-- -----------------------------------------------------------------------------

-- | /Dot/ has a basic grammar of arrow shapes which allows usage of
--   up to 1,544,761 different shapes from 9 different basic
--   'ArrowShape's.  Note that whilst an explicit list is used in the
--   definition of 'ArrowType', there must be at least one tuple and a
--   maximum of 4 (since that is what is required by Dot).  For more
--   information, see: <http://graphviz.org/doc/info/arrows.html>
--
--   The 19 basic arrows shown on the overall attributes page have
--   been defined below as a convenience.  Parsing of the 5
--   backward-compatible special cases is also supported.
newtype ArrowType = AType [(ArrowModifier, ArrowShape)]
    deriving (Eq, Ord, Show, Read)

box, crow, diamond, dotArrow, inv, noArrow, normal, tee, vee :: ArrowType
oDot, invDot, invODot, oBox, oDiamond :: ArrowType
eDiamond, openArr, halfOpen, emptyArr, invEmpty :: ArrowType

normal = AType [(noMods, Normal)]
inv = AType [(noMods, Inv)]
dotArrow = AType [(noMods, DotArrow)]
invDot = AType [ (noMods, Inv)
               , (noMods, DotArrow)]
oDot = AType [(ArrMod OpenArrow BothSides, DotArrow)]
invODot = AType [ (noMods, Inv)
                , (openMod, DotArrow)]
noArrow = AType [(noMods, NoArrow)]
tee = AType [(noMods, Tee)]
emptyArr = AType [(openMod, Normal)]
invEmpty = AType [ (noMods, Inv)
                 , (openMod, Normal)]
diamond = AType [(noMods, Diamond)]
oDiamond = AType [(openMod, Diamond)]
eDiamond = oDiamond
crow = AType [(noMods, Crow)]
box = AType [(noMods, Box)]
oBox = AType [(openMod, Box)]
openArr = vee
halfOpen = AType [(ArrMod FilledArrow LeftSide, Vee)]
vee = AType [(noMods, Vee)]

instance PrintDot ArrowType where
    unqtDot (AType mas) = hcat $ map appMod mas
        where
          appMod (m, a) = unqtDot m <> unqtDot a

instance ParseDot ArrowType where
    parseUnqt = do mas <- many1 $ do m <- parseUnqt
                                     a <- parseUnqt
                                     return (m,a)
                   return $ AType mas
                `onFail`
                specialArrowParse

specialArrowParse :: Parse ArrowType
specialArrowParse = oneOf [ stringRep eDiamond "ediamond"
                          , stringRep openArr "open"
                          , stringRep halfOpen "halfopen"
                          , stringRep emptyArr "empty"
                          , stringRep invEmpty "invempty"
                          ]

data ArrowShape = Box
                | Crow
                | Diamond
                | DotArrow
                | Inv
                | NoArrow
                | Normal
                | Tee
                | Vee
                  deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ArrowShape where
    unqtDot Box      = unqtDot "box"
    unqtDot Crow     = unqtDot "crow"
    unqtDot Diamond  = unqtDot "diamond"
    unqtDot DotArrow = unqtDot "dot"
    unqtDot Inv      = unqtDot "inv"
    unqtDot NoArrow  = unqtDot "none"
    unqtDot Normal   = unqtDot "normal"
    unqtDot Tee      = unqtDot "tee"
    unqtDot Vee      = unqtDot "vee"

instance ParseDot ArrowShape where
    parseUnqt = oneOf [ stringRep Box "box"
                      , stringRep Crow "crow"
                      , stringRep Diamond "diamond"
                      , stringRep DotArrow "dot"
                      , stringRep Inv "inv"
                      , stringRep NoArrow "none"
                      , stringRep Normal "normal"
                      , stringRep Tee "tee"
                      , stringRep Vee "vee"
                      ]

-- | What modifications to apply to an 'ArrowShape'.
data ArrowModifier = ArrMod { arrowFill :: ArrowFill
                            , arrowSide :: ArrowSide
                            }
                     deriving (Eq, Ord, Show, Read)

-- | Apply no modifications to an 'ArrowShape'.
noMods :: ArrowModifier
noMods = ArrMod FilledArrow BothSides

-- | 'OpenArrow' and 'BothSides'
openMod :: ArrowModifier
openMod = ArrMod OpenArrow BothSides

instance PrintDot ArrowModifier where
    unqtDot (ArrMod f s) = unqtDot f <> unqtDot s

instance ParseDot ArrowModifier where
    parseUnqt = do f <- parseUnqt
                   s <- parseUnqt
                   return $ ArrMod f s

data ArrowFill = OpenArrow
               | FilledArrow
                 deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ArrowFill where
    unqtDot OpenArrow   = char 'o'
    unqtDot FilledArrow = empty

instance ParseDot ArrowFill where
    parseUnqt = liftM (bool FilledArrow OpenArrow . isJust)
                $ optional (character 'o')

    -- Not used individually
    parse = parseUnqt

-- | Represents which side (when looking towards the node the arrow is
--   pointing to) is drawn.
data ArrowSide = LeftSide
               | RightSide
               | BothSides
                 deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ArrowSide where
    unqtDot LeftSide  = char 'l'
    unqtDot RightSide = char 'r'
    unqtDot BothSides = empty

instance ParseDot ArrowSide where
    parseUnqt = liftM getSideType
                $ optional (oneOf $ map character ['l', 'r'])
        where
          getSideType = maybe BothSides
                              (bool RightSide LeftSide . (==) 'l')

    -- Not used individually
    parse = parseUnqt

-- -----------------------------------------------------------------------------

data AspectType = RatioOnly Double
                | RatioPassCount Double Int
                  deriving (Eq, Ord, Show, Read)

instance PrintDot AspectType where
    unqtDot (RatioOnly r)        = unqtDot r
    unqtDot (RatioPassCount r p) = commaDel r p

    toDot at@RatioOnly{}      = unqtDot at
    toDot at@RatioPassCount{} = doubleQuotes $ unqtDot at

instance ParseDot AspectType where
    parseUnqt = liftM (uncurry RatioPassCount) commaSepUnqt
                `onFail`
                liftM RatioOnly parseUnqt


    parse = quotedParse (liftM (uncurry RatioPassCount) commaSepUnqt)
            `onFail`
            liftM RatioOnly parse

-- -----------------------------------------------------------------------------

data Rect = Rect Point Point
            deriving (Eq, Ord, Show, Read)

instance PrintDot Rect where
    unqtDot (Rect p1 p2) = commaDel p1 p2

    toDot = doubleQuotes . unqtDot

instance ParseDot Rect where
    parseUnqt = liftM (uncurry Rect) commaSepUnqt

    parse = quotedParse parseUnqt

-- -----------------------------------------------------------------------------

data ClusterMode = Local
                 | Global
                 | NoCluster
                   deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ClusterMode where
    unqtDot Local     = unqtDot "local"
    unqtDot Global    = unqtDot "global"
    unqtDot NoCluster = unqtDot "none"



instance ParseDot ClusterMode where
    parseUnqt = oneOf [ stringRep Local "local"
                      , stringRep Global "global"
                      , stringRep NoCluster "none"
                      ]

-- -----------------------------------------------------------------------------

data DirType = Forward | Back | Both | NoDir
               deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot DirType where
    unqtDot Forward = unqtDot "forward"
    unqtDot Back    = unqtDot "back"
    unqtDot Both    = unqtDot "both"
    unqtDot NoDir   = unqtDot "none"

instance ParseDot DirType where
    parseUnqt = oneOf [ stringRep Forward "forward"
                      , stringRep Back "back"
                      , stringRep Both "both"
                      , stringRep NoDir "none"
                      ]

-- -----------------------------------------------------------------------------

-- | Only when @mode == 'IpSep'@.
data DEConstraints = EdgeConstraints
                   | NoConstraints
                   | HierConstraints
                     deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot DEConstraints where
    unqtDot EdgeConstraints = unqtDot True
    unqtDot NoConstraints   = unqtDot False
    unqtDot HierConstraints = text "hier"

instance ParseDot DEConstraints where
    parseUnqt = liftM (bool NoConstraints EdgeConstraints) parse
                `onFail`
                stringRep HierConstraints "hier"

-- -----------------------------------------------------------------------------

-- | Either a 'Double' or a 'Point'.
data DPoint = DVal Double
            | PVal Point
             deriving (Eq, Ord, Show, Read)

instance PrintDot DPoint where
    unqtDot (DVal d) = unqtDot d
    unqtDot (PVal p) = unqtDot p

    toDot (DVal d) = toDot d
    toDot (PVal p) = toDot p

instance ParseDot DPoint where
    parseUnqt = liftM PVal parseUnqt
                `onFail`
                liftM DVal parseUnqt

    parse = liftM PVal parse
            `onFail`
            liftM DVal parse

-- -----------------------------------------------------------------------------

data ModeType = Major
              | KK
              | Hier
              | IpSep
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ModeType where
    unqtDot Major = text "major"
    unqtDot KK    = text "KK"
    unqtDot Hier  = text "hier"
    unqtDot IpSep = text "ipsep"

instance ParseDot ModeType where
    parseUnqt = oneOf [ stringRep Major "major"
                      , stringRep KK "KK"
                      , stringRep Hier "hier"
                      , stringRep IpSep "ipsep"
                      ]

-- -----------------------------------------------------------------------------

data Model = ShortPath
           | SubSet
           | Circuit
             deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Model where
    unqtDot ShortPath = text "shortpath"
    unqtDot SubSet    = text "subset"
    unqtDot Circuit   = text "circuit"

instance ParseDot Model where
    parseUnqt = oneOf [ stringRep ShortPath "shortpath"
                      , stringRep SubSet "subset"
                      , stringRep Circuit "circuit"
                      ]

-- -----------------------------------------------------------------------------

data Label = StrLabel EscString
           | URLLabel URL
             deriving (Eq, Ord, Show, Read)

instance PrintDot Label where
    unqtDot (StrLabel s) = unqtDot s
    unqtDot (URLLabel u) = unqtDot u

    toDot (StrLabel s) = toDot s
    toDot (URLLabel u) = toDot u

instance ParseDot Label where
    parseUnqt = liftM StrLabel parseUnqt
                `onFail`
                liftM URLLabel parseUnqt

    parse = liftM StrLabel parse
            `onFail`
            liftM URLLabel parse

-- -----------------------------------------------------------------------------

data Point = Point Int Int
           | PointD Double Double
             deriving (Eq, Ord, Show, Read)

instance PrintDot Point where
    unqtDot (Point  x y) = commaDel x y
    unqtDot (PointD x y) = commaDel x y

    toDot = doubleQuotes . unqtDot

    unqtListToDot = hsep . map unqtDot

    listToDot = doubleQuotes . unqtListToDot

instance ParseDot Point where
    -- Need to take into account the situation where first value is an
    -- integer, second a double: if Point parsing first, then it won't
    -- parse the second number properly; but if PointD first then it
    -- will treat Int/Int as Double/Double.
    parseUnqt = intDblPoint
                `onFail`
                liftM (uncurry Point)  commaSepUnqt
                `onFail`
                liftM (uncurry PointD) commaSepUnqt
        where
          intDblPoint = liftM (uncurry PointD . first fI)
                        $ commaSep' parseUnqt parseStrictFloat
          fI :: Int -> Double
          fI = fromIntegral

    parse = quotedParse parseUnqt

    parseUnqtList = sepBy1 parseUnqt whitespace

-- -----------------------------------------------------------------------------

data Overlap = KeepOverlaps
             | RemoveOverlaps
             | ScaleOverlaps
             | ScaleXYOverlaps
             | PrismOverlap (Maybe Int) -- ^ Only when sfdp is available, 'Int' is non-negative
             | CompressOverlap
             | VpscOverlap
             | IpsepOverlap -- ^ Only when @mode == 'IpSep'@
               deriving (Eq, Ord, Show, Read)

instance PrintDot Overlap where
    unqtDot KeepOverlaps     = unqtDot True
    unqtDot RemoveOverlaps   = unqtDot False
    unqtDot ScaleOverlaps    = text "scale"
    unqtDot ScaleXYOverlaps  = text "scalexy"
    unqtDot (PrismOverlap i) = maybe id (flip (<>) . unqtDot) i $ text "prism"
    unqtDot CompressOverlap  = text "compress"
    unqtDot VpscOverlap      = text "vpsc"
    unqtDot IpsepOverlap     = text "ipsep"

instance ParseDot Overlap where
    parseUnqt = oneOf [ stringRep KeepOverlaps "true"
                      , stringRep RemoveOverlaps "false"
                      , stringRep ScaleXYOverlaps "scalexy"
                      , stringRep ScaleOverlaps "scale"
                      , string "prism" >> liftM PrismOverlap (optional parse)
                      , stringRep CompressOverlap "compress"
                      , stringRep VpscOverlap "vpsc"
                      , stringRep IpsepOverlap "ipsep"
                      ]

-- -----------------------------------------------------------------------------

data LayerRange = LRID LayerID
                | LRS LayerID String LayerID
                  deriving (Eq, Ord, Show, Read)

instance PrintDot LayerRange where
    unqtDot (LRID lid)      = unqtDot lid
    unqtDot (LRS id1 s id2) = unqtDot id1 <> unqtDot s <> unqtDot id2

    toDot (LRID lid) = toDot lid
    toDot lrs        = doubleQuotes $ unqtDot lrs

instance ParseDot LayerRange where
    parseUnqt = do id1 <- parseUnqt
                   s   <- parseLayerSep
                   id2 <- parseUnqt
                   return $ LRS id1 s id2
                `onFail`
                liftM LRID parseUnqt


    parse = quotedParse ( do id1 <- parseUnqt
                             s   <- parseLayerSep
                             id2 <- parseUnqt
                             return $ LRS id1 s id2
                        )
            `onFail`
            liftM LRID parse

parseLayerSep :: Parse String
parseLayerSep = many1 . oneOf
                $ map character defLayerSep

defLayerSep :: [Char]
defLayerSep = [' ', ':', '\t']

parseLayerName :: Parse String
parseLayerName = many1 . orQuote
                 $ satisfy (liftM2 (&&) notLayerSep ((/=) quoteChar))

parseLayerName' :: Parse String
parseLayerName' = stringBlock
                  `onFail`
                  quotedParse parseLayerName

notLayerSep :: Char -> Bool
notLayerSep = flip notElem defLayerSep

-- | You should not have any quote characters for the 'LRName' option,
--   as it won't be parseable.
data LayerID = AllLayers
             | LRInt Int
             | LRName String
               deriving (Eq, Ord, Show, Read)

instance PrintDot LayerID where
    unqtDot AllLayers   = text "all"
    unqtDot (LRInt n)   = unqtDot n
    unqtDot (LRName nm) = unqtDot nm

    toDot (LRName nm) = toDot nm
    -- Other two don't need quotes
    toDot li          = unqtDot li

instance ParseDot LayerID where
    parseUnqt = liftM checkLayerName parseLayerName -- tests for Int and All

    parse = oneOf [ liftM checkLayerName parseLayerName'
                  , liftM LRInt parse -- Mainly for unquoted case.
                  ]

checkLayerName     :: String -> LayerID
checkLayerName str = maybe checkAll LRInt $ stringToInt str
  where
    checkAll = if map toLower str == "all"
               then AllLayers
               else LRName str

-- | The list represent (Separator, Name).  You should not have any
--   quote characters for any of the 'String's, since there are
--   parsing problems with them.
data LayerList = LL String [(String, String)]
                 deriving (Eq, Ord, Show, Read)

instance PrintDot LayerList where
    unqtDot (LL l1 ols) = unqtDot l1 <> hcat (map subLL ols)
        where
          subLL (s, l) = unqtDot s <> unqtDot l

    toDot (LL l1 []) = toDot l1
    -- Might not need quotes, but probably will.
    toDot ll         = doubleQuotes $ unqtDot ll

instance ParseDot LayerList where
    parseUnqt = do l1 <- parseLayerName
                   ols <- many $ do s   <- parseLayerSep
                                    lnm <- parseLayerName
                                    return (s, lnm)
                   return $ LL l1 ols

    parse = quotedParse parseUnqt
            `onFail`
            liftM (flip LL []) (parseLayerName' `onFail` numString)

-- -----------------------------------------------------------------------------

data OutputMode = BreadthFirst | NodesFirst | EdgesFirst
                  deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot OutputMode where
    unqtDot BreadthFirst = text "breadthfirst"
    unqtDot NodesFirst   = text "nodesfirst"
    unqtDot EdgesFirst   = text "edgesfirst"

instance ParseDot OutputMode where
    parseUnqt = oneOf [ stringRep BreadthFirst "breadthfirst"
                      , stringRep NodesFirst "nodesfirst"
                      , stringRep EdgesFirst "edgesfirst"
                      ]

-- -----------------------------------------------------------------------------

data Pack = DoPack
          | DontPack
          | PackMargin Int -- ^ If non-negative, then packs; otherwise doesn't.
            deriving (Eq, Ord, Show, Read)

instance PrintDot Pack where
    unqtDot DoPack         = unqtDot True
    unqtDot DontPack       = unqtDot False
    unqtDot (PackMargin m) = unqtDot m

instance ParseDot Pack where
    -- What happens if it parses 0?  It's non-negative, but parses as False
    parseUnqt = oneOf [ liftM PackMargin parseUnqt
                      , liftM (bool DontPack DoPack) onlyBool
                      ]

-- -----------------------------------------------------------------------------

data PackMode = PackNode
              | PackClust
              | PackGraph
              | PackArray Bool Bool (Maybe Int) -- ^ Sort by cols, sort
                                                -- by user, number of
                                                -- rows/cols
                deriving (Eq, Ord, Show, Read)

instance PrintDot PackMode where
    unqtDot PackNode           = text "node"
    unqtDot PackClust          = text "clust"
    unqtDot PackGraph          = text "graph"
    unqtDot (PackArray c u mi) = addNum . isU . isC . isUnder
                                 $ text "array"
        where
          addNum = maybe id (flip (<>) . unqtDot) mi
          isUnder = if c || u
                    then flip (<>) $ char '_'
                    else id
          isC = if c
                then flip (<>) $ char 'c'
                else id
          isU = if u
                then flip (<>) $ char 'u'
                else id

instance ParseDot PackMode where
    parseUnqt = oneOf [ stringRep PackNode "node"
                      , stringRep PackClust "clust"
                      , stringRep PackGraph "graph"
                      , do string "array"
                           mcu <- optional $ do character '_'
                                                many1 $ satisfy isCU
                           let c = hasCharacter mcu 'c'
                               u = hasCharacter mcu 'u'
                           mi <- optional parseUnqt
                           return $ PackArray c u mi
                      ]
        where
          hasCharacter ms c = maybe False (elem c) ms
          -- Also checks and removes quote characters
          isCU = flip elem ['c', 'u']

-- -----------------------------------------------------------------------------

data Pos = PointPos Point
         | SplinePos [Spline]
           deriving (Eq, Ord, Show, Read)

instance PrintDot Pos where
    unqtDot (PointPos p)   = unqtDot p
    unqtDot (SplinePos ss) = unqtDot ss

    toDot (PointPos p)   = toDot p
    toDot (SplinePos ss) = toDot ss

instance ParseDot Pos where
    -- Have to be careful with this: if we try to parse points first,
    -- then a spline with no start and end points will erroneously get
    -- parsed as a point and then the parser will crash as it expects
    -- a closing quote character...
    parseUnqt = do splns <- parseUnqt
                   case splns of
                     [Spline Nothing Nothing [p]] -> return $ PointPos p
                     _                            -> return $ SplinePos splns

    parse = quotedParse parseUnqt

-- -----------------------------------------------------------------------------

-- | Controls how (and if) edges are represented.
data EdgeType = SplineEdges
              | LineEdges
              | NoEdges
              | PolyLine
              | CompoundEdge -- ^ fdp only
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot EdgeType where
    unqtDot SplineEdges  = toDot True
    unqtDot LineEdges    = toDot False
    unqtDot NoEdges      = empty
    unqtDot PolyLine     = text "polyline"
    unqtDot CompoundEdge = text "compound"

    toDot NoEdges = doubleQuotes empty
    toDot et      = unqtDot et

instance ParseDot EdgeType where
    -- Can't parse NoEdges without quotes.
    parseUnqt = oneOf [ liftM (bool LineEdges SplineEdges) parse
                      , stringRep SplineEdges "spline"
                      , stringRep LineEdges "line"
                      , stringRep PolyLine "polyline"
                      , stringRep CompoundEdge "compound"
                      ]

    parse = stringRep NoEdges "\"\""
            `onFail`
            optionalQuoted parseUnqt

-- -----------------------------------------------------------------------------

-- | Upper-case first character is major order;
--   lower-case second character is minor order.
data PageDir = Bl | Br | Tl | Tr | Rb | Rt | Lb | Lt
               deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot PageDir where
    unqtDot Bl = text "BL"
    unqtDot Br = text "BR"
    unqtDot Tl = text "TL"
    unqtDot Tr = text "TR"
    unqtDot Rb = text "RB"
    unqtDot Rt = text "RT"
    unqtDot Lb = text "LB"
    unqtDot Lt = text "LT"

instance ParseDot PageDir where
    parseUnqt = oneOf [ stringRep Bl "BL"
                      , stringRep Br "BR"
                      , stringRep Tl "TL"
                      , stringRep Tr "TR"
                      , stringRep Rb "RB"
                      , stringRep Rt "RT"
                      , stringRep Lb "LB"
                      , stringRep Lt "LT"
                      ]

-- -----------------------------------------------------------------------------

-- | The number of points in the list must be equivalent to 1 mod 3;
--   note that this is not checked.
data Spline = Spline (Maybe Point) (Maybe Point) [Point]
              deriving (Eq, Ord, Show, Read)

instance PrintDot Spline where
    unqtDot (Spline ms me ps) = addS . addE
                               . hsep
                               $ map unqtDot ps
        where
          addP t = maybe id ((<+>) . commaDel t)
          addS = addP 's' ms
          addE = addP 'e' me

    toDot = doubleQuotes . unqtDot

    unqtListToDot = hcat . punctuate semi . map unqtDot

    listToDot = doubleQuotes . unqtListToDot

instance ParseDot Spline where
    parseUnqt = do ms <- parseP 's'
                   me <- parseP 'e'
                   ps <- sepBy1 parseUnqt whitespace
                   return $ Spline ms me ps
        where
          parseP t = optional $ do character t
                                   parseComma
                                   parseUnqt `discard` whitespace

    parse = quotedParse parseUnqt

    parseUnqtList = sepBy1 parseUnqt (character ';')

-- -----------------------------------------------------------------------------

data QuadType = NormalQT
              | FastQT
              | NoQT
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot QuadType where
    unqtDot NormalQT = text "normal"
    unqtDot FastQT   = text "fast"
    unqtDot NoQT     = text "none"

instance ParseDot QuadType where
    -- Have to take into account the slightly different interpretation
    -- of Bool used as an option for parsing QuadType
    parseUnqt = oneOf [ stringRep NormalQT "normal"
                      , stringRep FastQT "fast"
                      , stringRep NoQT "none"
                      , character '2'   >> return FastQT -- weird bool
                      , liftM (bool NoQT NormalQT) parse
                      ]

-- -----------------------------------------------------------------------------

-- | Specify the root node either as a Node attribute or a Graph attribute.
data Root = IsCentral       -- ^ For Nodes only
          | NotCentral      -- ^ For Nodes only
          | NodeName String -- ^ For Graphs only
            deriving (Eq, Ord, Show, Read)

instance PrintDot Root where
    unqtDot IsCentral    = unqtDot True
    unqtDot NotCentral   = unqtDot False
    unqtDot (NodeName n) = unqtDot n

    toDot (NodeName n) = toDot n
    toDot r            = unqtDot r

instance ParseDot Root where
    parseUnqt = liftM (bool NotCentral IsCentral) onlyBool
                `onFail`
                liftM NodeName parseUnqt

    parse = optionalQuoted (liftM (bool NotCentral IsCentral) onlyBool)
            `onFail`
            liftM NodeName parse

-- -----------------------------------------------------------------------------

data RankType = SameRank
              | MinRank
              | SourceRank
              | MaxRank
              | SinkRank
                deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot RankType where
    unqtDot SameRank   = text "same"
    unqtDot MinRank    = text "min"
    unqtDot SourceRank = text "source"
    unqtDot MaxRank    = text "max"
    unqtDot SinkRank   = text "sink"

instance ParseDot RankType where
    parseUnqt = oneOf [ stringRep SameRank "same"
                      , stringRep MinRank "min"
                      , stringRep SourceRank "source"
                      , stringRep MaxRank "max"
                      , stringRep SinkRank "sink"
                      ]

-- -----------------------------------------------------------------------------

data RankDir = FromTop
             | FromLeft
             | FromBottom
             | FromRight
               deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot RankDir where
    unqtDot FromTop    = text "TB"
    unqtDot FromLeft   = text "LR"
    unqtDot FromBottom = text "BT"
    unqtDot FromRight  = text "RL"

instance ParseDot RankDir where
    parseUnqt = oneOf [ stringRep FromTop "TB"
                      , stringRep FromLeft "LR"
                      , stringRep FromBottom "BT"
                      , stringRep FromRight "RL"
                      ]

-- -----------------------------------------------------------------------------

data Shape
    = BoxShape -- ^ Has synonyms of /rect/ and /rectangle/.
    | Polygon
    | Ellipse
    | Circle
    | PointShape
    | Egg
    | Triangle
    | PlainText -- ^ Has synonym of /none/.
    | DiamondShape
    | Trapezium
    | Parallelogram
    | House
    | Pentagon
    | Hexagon
    | Septagon
    | Octagon
    | DoubleCircle
    | DoubleOctagon
    | TripleOctagon
    | InvTriangle
    | InvTrapezium
    | InvHouse
    | MDiamond
    | MSquare
    | MCircle
    | Note
    | Tab
    | Folder
    | Box3D
    | Component
      deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Shape where
    unqtDot BoxShape      = text "box"
    unqtDot Polygon       = text "polygon"
    unqtDot Ellipse       = text "ellipse"
    unqtDot Circle        = text "circle"
    unqtDot PointShape    = text "point"
    unqtDot Egg           = text "egg"
    unqtDot Triangle      = text "triangle"
    unqtDot PlainText     = text "plaintext"
    unqtDot DiamondShape  = text "diamond"
    unqtDot Trapezium     = text "trapezium"
    unqtDot Parallelogram = text "parallelogram"
    unqtDot House         = text "house"
    unqtDot Pentagon      = text "pentagon"
    unqtDot Hexagon       = text "hexagon"
    unqtDot Septagon      = text "septagon"
    unqtDot Octagon       = text "octagon"
    unqtDot DoubleCircle  = text "doublecircle"
    unqtDot DoubleOctagon = text "doubleoctagon"
    unqtDot TripleOctagon = text "tripleoctagon"
    unqtDot InvTriangle   = text "invtriangle"
    unqtDot InvTrapezium  = text "invtrapezium"
    unqtDot InvHouse      = text "invhouse"
    unqtDot MDiamond      = text "Mdiamond"
    unqtDot MSquare       = text "Msquare"
    unqtDot MCircle       = text "Mcircle"
    unqtDot Note          = text "note"
    unqtDot Tab           = text "tab"
    unqtDot Folder        = text "folder"
    unqtDot Box3D         = text "box3d"
    unqtDot Component     = text "component"

instance ParseDot Shape where
    parseUnqt = oneOf [ stringRep Box3D "box3d" -- Parse this before "box"
                      , stringReps BoxShape ["box","rectangle","rect"]
                      , stringRep Polygon "polygon"
                      , stringRep Ellipse "ellipse"
                      , stringRep Circle "circle"
                      , stringRep PointShape "point"
                      , stringRep Egg "egg"
                      , stringRep Triangle "triangle"
                      , stringReps PlainText ["plaintext","none"]
                      , stringRep DiamondShape "diamond"
                      , stringRep Trapezium "trapezium"
                      , stringRep Parallelogram "parallelogram"
                      , stringRep House "house"
                      , stringRep Pentagon "pentagon"
                      , stringRep Hexagon "hexagon"
                      , stringRep Septagon "septagon"
                      , stringRep Octagon "octagon"
                      , stringRep DoubleCircle "doublecircle"
                      , stringRep DoubleOctagon "doubleoctagon"
                      , stringRep TripleOctagon "tripleoctagon"
                      , stringRep InvTriangle "invtriangle"
                      , stringRep InvTrapezium "invtrapezium"
                      , stringRep InvHouse "invhouse"
                      , stringRep MDiamond "Mdiamond"
                      , stringRep MSquare "Msquare"
                      , stringRep MCircle "Mcircle"
                      , stringRep Note "note"
                      , stringRep Tab "tab"
                      , stringRep Folder "folder"
                      , stringRep Component "component"
                      ]

-- -----------------------------------------------------------------------------

data SmoothType = NoSmooth
                | AvgDist
                | GraphDist
                | PowerDist
                | RNG
                | Spring
                | TriangleSmooth
                  deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot SmoothType where
    unqtDot NoSmooth       = text "none"
    unqtDot AvgDist        = text "avg_dist"
    unqtDot GraphDist      = text "graph_dist"
    unqtDot PowerDist      = text "power_dist"
    unqtDot RNG            = text "rng"
    unqtDot Spring         = text "spring"
    unqtDot TriangleSmooth = text "triangle"

instance ParseDot SmoothType where
    parseUnqt = oneOf [ stringRep NoSmooth "none"
                      , stringRep AvgDist "avg_dist"
                      , stringRep GraphDist "graph_dist"
                      , stringRep PowerDist "power_dist"
                      , stringRep RNG "rng"
                      , stringRep Spring "spring"
                      , stringRep TriangleSmooth "triangle"
                      ]

-- -----------------------------------------------------------------------------

data StartType = StartStyle STStyle
               | StartSeed Int
               | StartStyleSeed STStyle Int
                 deriving (Eq, Ord, Show, Read)

instance PrintDot StartType where
    unqtDot (StartStyle ss)       = unqtDot ss
    unqtDot (StartSeed s)         = unqtDot s
    unqtDot (StartStyleSeed ss s) = unqtDot ss <> unqtDot s

instance ParseDot StartType where
    parseUnqt = oneOf [ do ss <- parseUnqt
                           s  <- parseUnqt
                           return $ StartStyleSeed ss s
                      , liftM StartStyle parseUnqt
                      , liftM StartSeed parseUnqt
                      ]

data STStyle = RegularStyle
             | SelfStyle
             | RandomStyle
               deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot STStyle where
    unqtDot RegularStyle = text "regular"
    unqtDot SelfStyle    = text "self"
    unqtDot RandomStyle  = text "random"

instance ParseDot STStyle where
    parseUnqt = oneOf [ stringRep RegularStyle "regular"
                      , stringRep SelfStyle "self"
                      , stringRep RandomStyle "random"
                      ]

-- -----------------------------------------------------------------------------

data StyleItem = SItem StyleName [String]
             deriving (Eq, Ord, Show, Read)

instance PrintDot StyleItem where
    unqtDot (SItem nm args)
        | null args = dnm
        | otherwise = dnm <> parens args'
        where
          dnm = unqtDot nm
          args' = hcat . punctuate comma $ map unqtDot args

    toDot si@(SItem nm args)
        | null args = toDot nm
        | otherwise = doubleQuotes $ unqtDot si

    unqtListToDot = hcat . punctuate comma . map unqtDot

    listToDot [SItem nm []] = toDot nm
    listToDot sis           = doubleQuotes $ unqtListToDot sis

instance ParseDot StyleItem where
    parseUnqt = do nm <- parseUnqt
                   args <- tryParseList' parseArgs
                   return $ SItem nm args

    parse = quotedParse (liftM2 SItem parseUnqt parseArgs)
            `onFail`
            liftM (flip SItem []) parse

    parseUnqtList = sepBy1 parseUnqt parseComma

    parseList = quotedParse parseUnqtList
                `onFail`
                -- Might not necessarily need to be quoted if a singleton...
                liftM return parse

parseArgs :: Parse [String]
parseArgs = bracketSep (character '(')
                       parseComma
                       (character ')')
                       parseStyleName

data StyleName = Dashed    -- ^ Nodes and Edges
               | Dotted    -- ^ Nodes and Edges
               | Solid     -- ^ Nodes and Edges
               | Bold      -- ^ Nodes and Edges
               | Invisible -- ^ Nodes and Edges
               | Filled    -- ^ Nodes and Clusters
               | Diagonals -- ^ Nodes only
               | Rounded   -- ^ Nodes and Clusters
               | DD String -- ^ Device Dependent
                 deriving (Eq, Ord, Show, Read)

instance PrintDot StyleName where
    unqtDot Dashed    = text "dashed"
    unqtDot Dotted    = text "dotted"
    unqtDot Solid     = text "solid"
    unqtDot Bold      = text "bold"
    unqtDot Invisible = text "invis"
    unqtDot Filled    = text "filled"
    unqtDot Diagonals = text "diagonals"
    unqtDot Rounded   = text "rounded"
    unqtDot (DD nm)   = unqtDot nm

    toDot (DD nm) = toDot nm
    toDot sn      = unqtDot sn

instance ParseDot StyleName where
    parseUnqt = liftM checkDD parseStyleName

    parse = liftM checkDD
            $ quotedParse parseStyleName
              `onFail`
              -- In case a singleton DD is at the end of an attribute list.
              do f <- orQuote $ noneOf [quoteChar, '(', ')', ',', ' ', ']']
                 r <- many (orQuote $ noneOf [quoteChar, '(', ')', ',', ']'])
                 return $ f:r

checkDD     :: String -> StyleName
checkDD str = case map toLower str of
                "dashed"    -> Dashed
                "dotted"    -> Dotted
                "solid"     -> Solid
                "bold"      -> Bold
                "invis"     -> Invisible
                "filled"    -> Filled
                "diagonals" -> Diagonals
                "rounded"   -> Rounded
                _           -> DD str

parseStyleName :: Parse String
parseStyleName = do f <- orQuote $ noneOf [quoteChar, '(', ')', ',', ' ']
                    r <- many (orQuote $ noneOf [quoteChar, '(', ')', ','])
                    return $ f:r

-- -----------------------------------------------------------------------------

newtype PortPos = PP CompassPoint
    deriving (Eq, Ord, Show, Read)

instance PrintDot PortPos where
    unqtDot (PP cp) = unqtDot cp

    toDot (PP cp) = toDot cp

instance ParseDot PortPos where
    parseUnqt = liftM PP parseUnqt

data CompassPoint = North
                  | NorthEast
                  | East
                  | SouthEast
                  | South
                  | SouthWest
                  | West
                  | NorthWest
                  | CenterPoint
                  | NoCP
                    deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot CompassPoint where
    unqtDot NorthEast   = text "ne"
    unqtDot NorthWest   = text "nw"
    unqtDot North       = text "n"
    unqtDot East        = text "e"
    unqtDot SouthEast   = text "se"
    unqtDot SouthWest   = text "sw"
    unqtDot South       = text "s"
    unqtDot West        = text "w"
    unqtDot CenterPoint = text "c"
    unqtDot NoCP        = text "_"

instance ParseDot CompassPoint where
    -- Have to take care of longer parsing values first.
    parseUnqt = oneOf [ stringRep NorthEast "ne"
                      , stringRep NorthWest "nw"
                      , stringRep North "n"
                      , stringRep SouthEast "se"
                      , stringRep SouthWest "sw"
                      , stringRep South "s"
                      , stringRep East "e"
                      , stringRep West "w"
                      , stringRep CenterPoint "c"
                      , stringRep NoCP "_"
                      ]

-- -----------------------------------------------------------------------------

data ViewPort = VP { wVal  :: Double
                   , hVal  :: Double
                   , zVal  :: Double
                   , focus :: Maybe FocusType
                   }
                deriving (Eq, Ord, Show, Read)

instance PrintDot ViewPort where
    unqtDot vp = maybe vs ((<>) (vs <> comma) . unqtDot)
                 $ focus vp
        where
          vs = hcat . punctuate comma
               $ map (unqtDot . flip ($) vp) [wVal, hVal, zVal]

    toDot = doubleQuotes . unqtDot

instance ParseDot ViewPort where
    parseUnqt = do wv <- parseUnqt
                   parseComma
                   hv <- parseUnqt
                   parseComma
                   zv <- parseUnqt
                   mf <- optional $ parseComma >> parseUnqt
                   return $ VP wv hv zv mf

    parse = quotedParse parseUnqt

data FocusType = XY Point
               | NodeFocus String
                 deriving (Eq, Ord, Show, Read)

instance PrintDot FocusType where
    unqtDot (XY p)         = unqtDot p
    unqtDot (NodeFocus nm) = unqtDot nm

    toDot (XY p)         = toDot p
    toDot (NodeFocus nm) = toDot nm

instance ParseDot FocusType where
    parseUnqt = liftM XY parseUnqt
                `onFail`
                liftM NodeFocus parseUnqt

    parse = liftM XY parse
            `onFail`
            liftM NodeFocus parse

-- -----------------------------------------------------------------------------

data VerticalPlacement = VTop
                       | VCenter -- ^ Only valid for Nodes.
                       | VBottom
                         deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot VerticalPlacement where
    unqtDot VTop    = char 't'
    unqtDot VCenter = char 'c'
    unqtDot VBottom = char 'b'

instance ParseDot VerticalPlacement where
    parseUnqt = oneOf [ stringRep VTop "t"
                      , stringRep VCenter "c"
                      , stringRep VBottom "b"
                      ]

-- -----------------------------------------------------------------------------

data ScaleType = UniformScale
               | NoScale
               | FillWidth
               | FillHeight
               | FillBoth
                 deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot ScaleType where
    unqtDot UniformScale = unqtDot True
    unqtDot NoScale      = unqtDot False
    unqtDot FillWidth    = text "width"
    unqtDot FillHeight   = text "height"
    unqtDot FillBoth     = text "both"

instance ParseDot ScaleType where
    parseUnqt = oneOf [ stringRep UniformScale "true"
                      , stringRep NoScale "false"
                      , stringRep FillWidth "width"
                      , stringRep FillHeight "height"
                      , stringRep FillBoth "both"
                      ]

-- -----------------------------------------------------------------------------

data Justification = JLeft
                   | JRight
                   | JCenter
                     deriving (Eq, Ord, Bounded, Enum, Show, Read)

instance PrintDot Justification where
    unqtDot JLeft   = char 'l'
    unqtDot JRight  = char 'r'
    unqtDot JCenter = char 'c'

instance ParseDot Justification where
    parseUnqt = oneOf [ stringRep JLeft "l"
                      , stringRep JRight "r"
                      , stringRep JCenter "c"
                      ]

-- -----------------------------------------------------------------------------

data Ratios = AspectRatio Double
            | FillRatio
            | CompressRatio
            | ExpandRatio
            | AutoRatio
              deriving (Eq, Ord, Show, Read)

instance PrintDot Ratios where
    unqtDot (AspectRatio r) = unqtDot r
    unqtDot FillRatio       = text "fill"
    unqtDot CompressRatio   = text "compress"
    unqtDot ExpandRatio     = text "expand"
    unqtDot AutoRatio       = text "auto"

instance ParseDot Ratios where
    parseUnqt = oneOf [ liftM AspectRatio parseUnqt
                      , stringRep FillRatio "fill"
                      , stringRep CompressRatio "compress"
                      , stringRep ExpandRatio "expand"
                      , stringRep AutoRatio "auto"
                      ]
