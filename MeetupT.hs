{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}

import           Control.Monad.Trans (liftIO)
import           Lucid
import           Lucid.Base          (makeAttribute)
import           Data.Monoid
import           Data.Text         (Text, pack)
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as LT

type HtmlIO = HtmlT IO ()

path :: String -> String
path = (++) "./meetup/"

datatrim_ :: Term arg result => arg -> result
datatrim_ = term "data-trim"

datamarkdown_ :: Attribute
datamarkdown_ = makeAttribute "data-markdown" mempty

slide :: HtmlIO -> HtmlIO
slide = section_

slide' :: HtmlIO -> [HtmlIO] -> HtmlIO
slide' content notes = section_ (content <> aside_ [class_ "notes"] ns)
  where
    ns = ul_ (mapM_ li_ notes)

code :: HtmlIO -> HtmlIO
code source = pre_ $ code_ [class_"haskell", datatrim_ "contenteditable"] source

getCode :: FilePath -> Int -> Int -> IO (String)
getCode filePath a b =
  readFile filePath >>= return . unlines . take (b - a + 1) . drop (a - 1) . lines

slideImage :: HtmlIO -> Maybe HtmlIO -> String -> Int -> HtmlIO
slideImage title subtitle imageUrl size = do
  slide $ do
    h2_ title
    maybe "" h3_ subtitle
    img_ [src_ $ pack (path imageUrl) , width_ (pack . show $ size)]

slideImage' :: HtmlIO -> Maybe HtmlIO -> String -> Int -> [HtmlIO] -> HtmlIO
slideImage' title subtitle imageUrl size notes = do
  slide' (do
    h2_ title
    maybe "" h3_ subtitle
    img_ [src_ $ pack (path imageUrl) , width_ (pack . show $ size)])
    notes

slideCode :: HtmlIO -> Maybe HtmlIO -> FilePath -> Int -> Int -> HtmlIO
slideCode title subtitle codeFile a b = do
  hs <- liftIO $ getCode (path codeFile) a b
  slide $ do
    h2_ title
    maybe "" h3_ subtitle
    code $ toHtml hs

slideCode' :: HtmlIO -> Maybe HtmlIO -> FilePath -> Int -> Int -> [HtmlIO] -> HtmlIO
slideCode' title subtitle codeFile a b notes = do
  hs <- liftIO $ getCode (path codeFile) a b
  slide' (do
    h2_ title
    maybe "" h3_ subtitle
    code $ toHtml hs)
    notes

slideImageCode :: HtmlIO -> Maybe HtmlIO -> FilePath -> Int
               -> FilePath -> Int -> Int -> HtmlIO
slideImageCode title subtitle imageUrl size codeFile a b = do
  hs <- liftIO $ getCode (path codeFile) a b
  slide $ do
    h2_ title
    maybe "" h3_ subtitle
    img_ [src_ $ pack (path imageUrl) , width_ (pack. show $ size)]
    code $ toHtml hs

slideImageCode' :: HtmlIO -> Maybe HtmlIO -> FilePath -> Int
                -> FilePath -> Int -> Int -> [HtmlIO] -> HtmlIO
slideImageCode' title subtitle imageUrl size codeFile a b notes = do
  hs <- liftIO $ getCode (path codeFile) a b
  slide' (do
    h2_ title
    maybe "" h3_ subtitle
    img_ [src_ $ pack (path imageUrl) , width_ (pack. show $ size)]
    code $ toHtml hs)
    notes

slideBullets :: HtmlIO -> Maybe HtmlIO -> [HtmlIO] -> HtmlIO
slideBullets title subtitle bullets =
  slide $ do
    h2_ title
    maybe "" h3_ subtitle
    ul_ (mapM_ li_ bullets)

slideList :: HtmlIO -> Maybe HtmlIO -> [HtmlIO] -> HtmlIO
slideList title subtitle list =
  slide $ do
    h2_ title
    maybe "" h3_ subtitle
    ol_ (mapM_ li_ list)

slideBullets' :: HtmlIO -> Maybe HtmlIO -> [HtmlIO] -> [HtmlIO] -> HtmlIO
slideBullets' title subtitle bullets notes =
  slide' (do
    h2_ title
    maybe "" h3_ subtitle
    ul_ (mapM_ li_ bullets))
    notes

slideList' :: HtmlIO -> Maybe HtmlIO -> [HtmlIO] -> [HtmlIO] -> HtmlIO
slideList' title subtitle list notes =
  slide' (do
    h2_ title
    maybe "" h3_ subtitle
    ol_ (mapM_ li_ list))
    notes

slideMarkdown :: Text -> HtmlIO
slideMarkdown s = section_ [datamarkdown_] $ script_ [type_"text/template"] s

headContent :: HtmlIO
headContent = do
  meta_ [charset_ "utf-8"]
  title_ "Diagrams 1.3"
  meta "apple-mobile-web-app-capable" "yes"
  meta "apple-mobile-web-app-status-bar-style" "black-translucent"
  meta "viewport" "width=device-width, initial-scale=1.0, maximum-scale=1.0\
       \, user-scalable=no, minimal-ui"
  css "css/reveal.css"
  with (css "css/theme/white.css") [id_ "theme"]
  css "lib/css/github.css"
  script_
    "var link = document.createElement( 'link' );\
		\link.rel = 'stylesheet';\
		\link.type = 'text/css';\
		\link.href = window.location.search.match( /print-pdf/gi ) ? 'css/print/pdf.css' : 'css/print/paper.css';\
		\document.getElementsByTagName( 'head' )[0].appendChild( link );"
  where
    meta name content = meta_ [name_ name, content_ content]
    css url = link_ [rel_ "stylesheet", href_ url]

bodyHeader :: HtmlIO -> HtmlIO
bodyHeader x =
  div_ [class_ "reveal"] $ div_ [class_ "slides"] x

bodyFooter :: HtmlIO
bodyFooter = do
  script_ [src_ "lib/js/head.min.js"] ""
  script_ [src_ "js/reveal.js"] ""
  script_ "Reveal.initialize({ controls: true, progress: true, history: true\
          \, center: true, transition: 'slide', dependencies: [ { src: 'lib/js/classList.js'\
          \, condition: function() { return !document.body.classList; } }\
          \, { src: 'plugin/markdown/marked.js', condition: function() \
          \{ return !!document.querySelector( '[data-markdown]' ); } }\
          \, { src: 'plugin/markdown/markdown.js', condition: function() \
          \{ return !!document.querySelector( '[data-markdown]' ); } }\
          \, { src: 'plugin/highlight/highlight.js', async: true\
          \, condition: function() { return !!document.querySelector( 'pre code' ); }\
          \, callback: function() { hljs.initHighlightingOnLoad(); } }\
          \, { src: 'plugin/zoom-js/zoom.js', async: true }, { src: 'plugin/notes/notes.js'\
          \, async: true } ] });"

slideShow :: HtmlIO
slideShow = do
  slide $ do
    h1_ "The diagrams EDSL"
    h2_ "A declarative vector graphics library"
  slide $ do
    slide' (do
      h2_ "The Asymptote vector graphics language"
      pre_ $ code_ [class_"c", datatrim_ "contenteditable"] $ do
        "void aFunction(pair A, real s, int q, bool top=true)\n\
        \{\n\
        \  pair B=A-(1,sqrt(2))*s/2;\n\
        \  pair C=B+s;\n\
        \  if(top) draw(A--B--C--cycle);\n\
        \  draw((A+B)/2--(B+C)/2--(A+C)/2--cycle);\n\
        \  if(q > 0) {\n\
        \    aFunction(A,s/2,q-1,false);\n\
        \    aFunction((A+B)/2,s/2,q-1,false);\n\
        \    aFunction((A+C)/2,s/2,q-1,false);\n\
        \  }\n\
        \}\n\
        \\n\
        \aFunction((0,1),1,5);")
      [ "calculates coordinates of each vertex"
      , "and the coordinates of the top of each triangle"]
    slideImage'
      "Sierpinski Triangle"
      (Just "The Asymptote vector graphics language")
      "sierpinski.png" 400
      ["Not modular, inflexible"]
    slideImageCode'
      "Sierpinski Triangle"
      (Just "diagrams EDSL")
      "sierpinski.svg" 400
      "Sierpinski.hs" 8 12
      [ "===, |||, centerX"]
    slideImageCode'
      "Sierpinski Triangle"
      (Just "diagrams EDSL")
      "sierpinskicircle.svg" 400
      "SierpinskiCircle.hs" 8 12
      [ "change triangle to circle"
      , "remove centerX"]
    slideCode
      "Sierpinski Triangle"
      Nothing
      "SierpinskiGeneral.hs" 8 12
  slide $ do
    slideBullets'
      "diagrams"
      (Just "Declarative domain-specific language for creating vector graphics")
      [a_ [href_ "http://projects.haskell.org/diagrams/"]
         "http://projects.haskell.org/diagrams/"
      ," diagrams-core"
      , "diagrams-lib"
      , "diagrams-svg, diagrams-rasterific, ..."
      , "cabal update && cabal install diagrams"]
      [ "How many here have used diagrams?"
      , "website has tutorials, manual, gallery, blog, reference"]
    slide $ img_ [src_ $ pack (path "website.png")]
    slideBullets'
      "5 Active Developers"
      (Just "67 Contributors")
      ["Chris Chalmers", "Daniel Bergey", "Jeffrey Rosenbluth", "Ryan Yates", "Brent Yorgey"]
      ["The diagrams team", "see blog post contributors list"]
    slideImage'
      "Diagrams 1.3 released"
      (Just "Projections")
      "projections.svg" 500
      ["plus lots more"]
    slideImage
      "Diagrams 1.3"
      (Just "Path Intersections")
      "intersections.png" 600
    slideImage
      "Diagrams 1.3"
      (Just "Grouping for Opacity")
      "group-opacity.png" 600
    slide $ do
      h2_ "Diagrams 1.3"
      h3_ "New Backends"
      ul_ $ do
        li_ "diagrams-pgf"
        li_ "diagrams-canvas"
        li_ "diagrams-html5"
  slide $ do
    slideImageCode'
      "A Diagram"
      Nothing
      "firstDiagram.svg" 300
      "firstDiagram.hs" 1 100
      [ "NoMonoMorphismRestriction important to avoid crazy error messages"
      , "import a backend, plugable"
      , "Diagram B - a type alias for 2d diagrams"
      , "mainWith vs defaultMain"
      , "not going to show mainWith from here on in" ]
  slide $ do
    slideImageCode'
      "Composing Diagrams"
      (Just "atop")
      "atop.svg" 250
      "atop.hs" 6 13
      [ "default line color is black"
      , "defalut fill color is transparent"
      , "diagrams are monoids"
      , "atop == <>"]
    slideImageCode
      "Composing Diagrams"
      (Just "Side by Side")
      "atop2.svg" 300
      "atop2.hs" 6 15
    slideImage'
      "Composing Diagrams"
      (Just "Every diagram has a local origin")
      "atop3.svg" 400
      [ "You can think of it as the point (0,0) on global grid"]
    slideImageCode'
      "Composing Diagrams"
      (Just "Local Origin")
      "atop4.svg" 300
      "atop4.hs" 6 10
      [ "Places all local origings at (0,0)"]
    slideImage'
      "Side by Side"
      (Just "||| is moveOriginTo + atop")
      "atop5.svg" 400
      [ "moves origin of orange circle left"
      , "leaves origin of blue circle alone"
      , "combine with <>"]
    slideImageCode'
      "Composing Diagrams"
      (Just "with beside")
      "atop6.svg" 300
      "atop6.hs" 6 10
      [ "||| == beside (1,0)"
      , "explain ^&" ]
    slide $ do
       h2_ "Moral"
       h4_ "All defined in terms of atop and moveOriginTo"
       ul_ $ do
         li_ "beside, |||, ==="
         li_ "cat, hcat, vcat, hsep, vsep"
         li_ "juxtapose"
         li_ "position"
         li_ "appends"
  slide $ do
    slide $ do
      h2_ "How do we know how far to move the origin?"
      ul_ $ do
        li_ (del_ "Bounding Boxes")
        li_ "Envelopes"
    slideImage'
      "Bounding Boxes"
      (Just "beside (1, 1)")
      "boundingbox.svg" 400
      [ "notice beside doesn't really work like this"
      , "not compositional"
      , "not general"
      , "what to do with bounding box under rotation"
      , "bounding paths are complicated"]
    slideImage'
      "Envelopes"
      Nothing
      "envelope.svg" 500
      ["how far to supporting hyperplane"]
    slideImage'
      "Envelopes"
      (Just "beside (1, 1)")
      "envelope2.svg" 400
      [ "Now we see the connection between envelopes and atop"]
    slideImage'
      "Envelopes"
      (Just "beside (1, 1)")
      "envelope4b.svg" 400
      [ "What do you think happens if `beside (1.5 ^& 1)`"
      , "think of line with sope 2/3 from left origin"]
    slideImage
      "Envelopes"
      (Just "beside (1.5, 1)")
      "envelope4a.svg" 400
    slideImage'
      "Envelopes"
      (Just "beside (1.5, 1)")
      "envelope4.svg" 400
      [ "We can move these squares closer along line connection origins"
      , "What is going on?"]
    slide $ do
      h2_ "Envelopes"
      h3_ "Depend on the local origin"
      ul_ $ do
        li_ "not always intuitive"
        li_ "extensional not intensional"
        li_ "not the same as the convex hull"
      img_ [src_ $ pack (path "envelope3.svg") , width_ "500"]
      p_ $
        a_ [href_ "https://github.com/diagrams/diagrams-doc/blob/master/envelope/envelope.pdf/"]
           "https://github.com/diagrams/diagrams-doc/blob/master/envelope/envelope.pdf"
    slideImageCode'
      "Align"
      (Just "Moves the local origin to the envelope")
      "align.svg" 300
      "Align.hs" 6 13
      [ "primed versions like showOrigin' take an default record"
      , "see section 2.2 of manual, faking optional named arguments"
      , "with is a pun == def, lenses"]
    slideImage
      "Trace"
      Nothing
      "trace.svg" 600
    slideImageCode'
      "Snug"
      Nothing
      "snug.svg" 300
      "Snug.hs" 6 14
      [ "This cannot be done using align since envelopes prevent overlap"
      , "same align but uses trace instead of envelope"
      , "you can actually define your own boundry function - Alignable"]
  slide $ do
    slideImage'
      "A 2 Mirror Kaleidoscope"
      Nothing
      "kaleidoscope_by_mandelfish.jpg" 500
      [ "reflected isoceles triangles, 2n-gons"
      , "8-gon, 360 / 8 = 45 degree central anlge"
      , "random confetti in chamber"]
    slideCode'
      "Preliminaries"
      Nothing
      "Mirror.hs" 5 17
      [ "palette"
      , "MonadRandom: Rand g, evalRand"
      , "not restricted to a fixed set of confetti, like a real kaleidoscope"]
    slideCode'
      "Generating The Confetti"
      Nothing
      "Mirror.hs" 19 31
      [ "position :: [(Point v n , a)] -> a"
      , "atPoints :: [Point v n] -> [a] -> a"]
    slideImageCode'
      "Generating The Confetti"
      (Just "seed = 0, pieces = 50")
      "mirror.svg" 400
      "Mirror.hs" 33 34
      [ "evalRand $ confetti n (mkStdGen 0)"]
    slideCode'
      "Cut out a Triangle"
      Nothing
      "Mirror.hs" 36 47
      [ "A wedge might be better"
      , "TrailLike t"
      , "def and lens"
      , "clipBy - envelope of diagram"
      , "clipTo - envelope is pointwise min"
      , "clipped - envelope of clipping path"]
    slideImage
      "The Triangle"
      Nothing
      "mirror2.svg" 400
    slideImage
      "Reflect the Triangle"
      (Just "outlines just for visualization")
      "mirror4.svg" 400
    slideImage
      "Keep reflecting"
      Nothing
      "mirror5.svg" 500
    slideCode'
      "The Kaleidoscope"
      Nothing
      "Mirror.hs" 49 61
      [ "Generalize to n triangles"
      , "directions - xDir"
      , "mconcat - atop"]
    slideImage
      "The Kaleidoscope"
      (Just "60 degrees, 6 triangles")
      "mirror3.svg" 400
    slideImage'
      "The Kaleidoscope"
      (Just "36 degrees, 10 triangles")
      "mirror6.svg" 400
      [ "A real kaleidoscope has moving confetti"]
  slide $ do
    slideImage'
      "Making GIFs with diagrams"
      Nothing
      "pendulum.gif" 400
      [ "creating 3d GIFs by adding 2 lines"
      , "collaboration with juicy pixels"
      , "cairo, rasterific"
      , "beesandbombs.tumblr.com"]
    slideImageCode'
      "Pendulum"
      (Just "The Background")
      "stripes.svg" 300
      "Pendulum.hs" 27 33
      [" GIFs have no alpha"
      , "dont need the black square"
      , "make rectangles by scaling squares"]
    slideImageCode
      "Pendulum"
      (Just "The Ball")
      "ball.png" 400
      "Pendulum.hs" 9 12
    slideCode'
      "Pendulum"
      Nothing
      "Pendulum.hs" 14 25
      [ "there are ellipse functions in diagrams"]
    slideCode
      "Pendulum"
      (Just "All together now")
      "Pendulum.hs" 35 49
    slideImage
      "A 3 Mirror Kaleidoscope"
      Nothing
     "kaleidoscope.gif" 400
  -- slide $ do
  --   h2_ "TrailLike"
  --   code $
  --     "class (Metric (V t), OrderedField (N t)) => TrailLike t where\n\
  --     \  trailLike :: Located (Trail (V t) (N t)) -> t\n\
  --     \\n\
  --     \instance (Metric v, OrderedField n) => TrailLike [Point v n] where\n\
  --     \  trailLike = trailPoints\n\
  --     \\n\
  --     \instance (Metric v, OrderedField n) => TrailLike (Path v n) where\n\
  --     \  trailLike = Path . (:[])\n\
  --     \\n\
  --     \instance (TypeableFloat n, Renderable (Path V2 n) b)\n\
  --     \    => TrailLike (QDiagram b V2 n Any) where\n\
  --     \  trailLike = strokeP . trailLike"
  slide $ do
    slide $ do
      h1_ "Design Challenge"
      h2_ "What's so tricky about arrows?"
    slideList
      "The Arrow API"
      (Just "Desired Features")
      [ "Arrow heads should not scale with the diagram."
      , "Arrows should connect the same points before and after scaling."
      , "Shafts can be any curve not just straight lines."
      , "Arrow heads can be translucent." ]
    slideImage
      "The Arrow API"
      Nothing
      "arrows.svg" 600
    slideImage
      "Arrow heads and tails"
      Nothing
      "arrowheads.svg" 600
    slideImage'
      "Scale Invariance"
      (Just "1. Arrow heads should not scale with the diagram")
      "arrows1.svg" 400
      [ "Don't want head size to change"
      , "Non uniform scaling can cause a preceived rotation"]
    slideImage
      "Scale Invariance"
      (Just "2. Arrows should connect the same points before and after scaling")
      "arrows2.svg" 500
    slideImage'
      "Shafts"
      (Just "3. Shafts can be any curve not just straight lines")
      "arrowshaft.svg" 600
      [ "arc shaft"]
    slideImage'
      "Heads and Tails"
      (Just "4. Arrow heads can be translucent")
      "arrowopacity.svg" 600
      [ "Head cannot overlap shaft"]
    slideImage'
      "Heads and Tails"
      (Just "Therefore we can't overlap the head and the shaft")
      "nojoint.svg" 600
      [ "So the size of the joint depends on the line width"
      , "Line width is very flexible in diagrams and we don't know it\
      \  at the time the arrow is made."]
    slideList
      "We have two related problems"
      (Just "Related because we need to know the final size of the diagram")
      [ "How to scale arrows"
      , "Joint size depends on shaft width"]
    slideBullets'
      "Line Width and Arrow Length"
      (Just "Measurement Units")
      [ "local - just like argument x to circle x, square x, etc"
      , "normalized - proportion of final diagram size, e.g 0.1 means 10%"
      , "output - e.g.pixels"
      , "global - for backwards compatibility"]
      [ "Unless type is local, we don't konw line width until it's too late"]
    slideBullets
      "The Diagram type"
      (Just "Overview")
      [ "Diagrams are trees built from the leaves up."
      , "Every time two diagrams are composed the combined envelope is cached."
      , "Attributes like fill color and transforms like scale are stored as internal nodes."]
    slideCode
      "The Diagram type"
      Nothing
      "QDiagram.hs" 1 13
    slideCode
      "The Diagram type"
      Nothing
      "QDiagram.hs" 15 28
    slideCode
      "Simplified Diagram type"
      Nothing
      "QDiagram.hs" 30 39
    slideImageCode'
      "An Example"
      (Just "What needs to happen to render this arrow?")
      "arrowEx1.svg" 500
      "QDiagram.hs"  42 42
      [ "Why cant we just apply scale"
      , "Need to know final stroke width for joint"
      , "Final size for strok width and head length"]


main :: IO ()
main = do
  index <- renderTextT $
    html_ $ do
      head_ headContent
      body_ $ do
        bodyHeader slideShow
        bodyFooter
  LT.writeFile "index.html" index
