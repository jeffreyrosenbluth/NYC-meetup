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

slideBullets' :: HtmlIO -> Maybe HtmlIO -> [HtmlIO] -> [HtmlIO] -> HtmlIO
slideBullets' title subtitle bullets notes =
  slide' (do
    h2_ title
    maybe "" h3_ subtitle
    ul_ (mapM_ li_ bullets))
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
  slide $ do
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
        li_ "diagrams-hmlt5"
  slideImageCode'
    "A Diagram"
    Nothing
    "firstDiagram.svg" 300
    "firstDiagram.hs" 1 100
    [ "NoMonoMorphismRestriction important to avoid crazy error messages"
    , "import a backend, plugable"
    , "Diagram B - a type alias for 2d diagrams"
    , "mainWith vs defaultMain"
    , "not going to show mainWith from here in" ]
  slide $ do
    slideImageCode'
      "Composing Diagrams"
      (Just "atop")
      "atop.svg" 300
      "atop.hs" 6 13
      [ "default line color is black"
      , "defalut fill color is transparent"
      , "diagrams are monoids"
      , "atop == <>"]
    slideImageCode
      "Composing Diagrams"
      (Just "Side by Side")
      "atop2.svg" 400
      "atop2.hs" 6 15
    slideImage'
      "Composing Diagrams"
      (Just "Every diagram has a local origin")
      "atop3.svg" 400
      [ "You can think of it as the point (0,0) on global grid"]
    slideImageCode'
      "Composing Diagrams"
      (Just "with atop")
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
         li_ "besde, |||, ==="
         li_ "cat, hcat, vcat, hsep, vsep"
         li_ "juxtapose"
         li_ "position"
         li_ "appends"
  slide $ do
    slide $ do
      h4_ "How do we know where to place diagrams when composing \
         \so that they don't overlap?"
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
      "align.svg" 400
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
      , "same align but uses trace instead of envelope"]
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
    slideImage'
      "Generating The Confetti"
      (Just "seed = 0, pieces = 50")
      "mirror.svg" 400
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
    slideImage
      "The Kaleidoscope"
      (Just "36 degrees, 10 triangles")
      "mirror6.svg" 400
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
      , "dont need the black square"]
    slideImageCode
      "Pendulum"
      (Just "The Ball")
      "ball.png" 400
      "Pendulum.hs" 9 12
    slideCode
      "Pendulum"
      Nothing
      "Pendulum.hs" 14 25
    slideCode
      "Pendulum"
      (Just "All together now")
      "Pendulum.hs" 35 49
    slideImage
      "A 3 Mirror Kaleidoscope"
      Nothing
     "kaleidoscope.gif" 400

main :: IO ()
main = do
  index <- renderTextT $
    html_ $ do
      head_ headContent
      body_ $ do
        bodyHeader slideShow
        bodyFooter
  LT.writeFile "index.html" index
