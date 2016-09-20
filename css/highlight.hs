{-# LANGUAGE OverloadedStrings #-}

import           Clay        hiding (aqua, black, blue, gray, green, orange,
                              pink, red, white, yellow)
import           Data.Monoid ((<>))
import           Prelude     hiding (div, rem, span)

main :: IO ()
main = putCss css

white, gray, aqua, blue, green, pink, black, yellow, ocher, orange, red :: Color
white  = rgb 248 248 248
gray   = rgb 170 170 170
aqua   = rgb 32  74  135
blue   = rgb 0   0   207
green  = rgb 78  154 6
pink   = rgb 239 41  41
black  = rgb 0   0   0
ocher  = rgb 143 89  2
orange = rgb 206 92  0
yellow = rgb 190 160 0
red    = rgb 164 0   0

css :: Css
css = do
  codeLayout
  codeColor

codeLayout :: Css
codeLayout = do
  pre # ".sourceCode" ? do
    borderLeft solid ( px 2) gray
    padding (px 10) (px 10) (px 10) (px 10)
    margin (px 10) (px 10) (px 10) (px 10)
  div # ".sourceCode" ? overflowX auto
  table # ".sourceCode" ? backgroundColor white
  table # ".sourceCode" <>
    tr  # ".sourceCode"  <>
    td  # ".lineNumbers" <>
    td  # ".sourceCode"  ? do
    margin none none none none
    padding none none none none
    -- verticalAlign baseline
    border none none none
  td # ".lineNumbers" ? do
    textAlign $ alignSide sideRight
    paddingRight $ px 4
    paddingLeft $ px 4
    color gray
    borderRight solid (px 1) gray
  td # ".sourceCode" ? paddingLeft (px 5)
  pre <> code ? backgroundColor white
  pre # ".sourceCode" <> code ? do
    fontFamily ["Inconsolata"] [monospace]
    fontSize $ rem 1.6
    lineHeight $ rem 1.6

codeColor :: Css
codeColor = do
  code |> span # ".al" ? color pink
  code |> span # ".at" ? color yellow
  code |> span # ".er" ? color red
  code |> span # ".op" ? color orange
  code |> span # ".kw"   <>
    code |> span # ".dt" <>
    code |> span # ".cf" ? color aqua
  code |> span # ".dv"   <>
    code |> span # ".bn" <>
    code |> span # ".fl" ? color blue
  code |> span # ".ch"   <>
    code |> span # ".st" <>
    code |> span # ".ss" ? color green
  code |> span # ".fu"   <>
    code |> span # ".cn" <>
    code |> span # ".sc" <>
    code |> span # ".vs" <>
    code |> span # ".va" ? color black
  code |> span # ".co"   <>
    code |> span # ".pp" <>
    code |> span # ".ot" <>
    code |> span # ".wa" <>
    code |> span # ".do" <>
   code |> span # ".an"  <>
    code |> span # ".cv" <>
    code |> span # ".in" ? color ocher
  code |> span # ".im" ? mempty
  code |> span # ".ex" ? mempty

{-
- kw: Keyword
- dt: DataType
- dv: DecVal
- bn: BaseN
- fl: Float
- ch: Char
- st: String
- co: Comment
- ot: Other
- al: Alert
- fu: Function
- er: Error
- wa: Warning
- cn: Constant
- sc: SpecialChar
- vs: VerbatimString
- ss: SpecialString
- im: Import
- va: Variable
- cf: ControlFlow
- op: Operator
- pp: Preprocessor
- ex: Extension
- at: Attribute
- do: Documentation
- an: Annotation
- cv: CommentVar
- in: Information
-}
