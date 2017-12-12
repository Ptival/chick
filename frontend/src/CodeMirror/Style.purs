module CodeMirror.Style where

import Prelude
import CSS as CSS
import CSS.Display as CSSD
import CSS.Flexbox as CSSF
import CSS.Overflow as CSSO
import CSS.Stylesheet as CSSS
import Halogen as H
import Halogen.HTML.CSS as HCSS

type Style = ∀ r i. H.IProp (style :: String | r) i

componentStyle :: Style
componentStyle = HCSS.style $ do
  flexColumn
  --CSS.height (CSS.fromString "50%")
  CSS.width  (CSS.fromString "33%")

editorContextStyle :: Style
editorContextStyle = HCSS.style do
  flexRow
  CSSF.flex     1 1 (CSS.fromString "100%")
  CSS.minHeight CSS.nil -- needed for Firefox to render correctly

editorContainerStyle :: Style
editorContainerStyle = HCSS.style do
  CSSF.flex 1 1 (CSS.fromString "50%")

contextContainerStyle :: Style
contextContainerStyle = HCSS.style do
  CSSF.flex 1 1 (CSS.fromString "50%")

editorStyle :: Style
editorStyle = HCSS.style do
  CSS.height (CSS.fromString "100%")

contextStyle :: Style
contextStyle = HCSS.style do
  CSS.height     (CSS.fromString "100%")
  CSS.margin     CSS.nil CSS.nil CSS.nil CSS.nil -- needed for Firefox to render correctly
  CSSO.overflowY CSSO.scroll

markerBarStyle :: Style
markerBarStyle = HCSS.style do
  flexRow
  CSSF.flex 1 1 (CSS.fromString "20px")

flexColumn :: CSSS.StyleM Unit
flexColumn = do
  CSSD.display       CSS.flex
  CSSF.flexDirection CSSF.column

flexRow :: CSSS.StyleM Unit
flexRow = do
  CSSD.display       CSS.flex
  CSSF.flexDirection CSSF.row
