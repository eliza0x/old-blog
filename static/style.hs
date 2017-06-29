{-# LANGUAGE OverloadedStrings #-}

import Clay         hiding (fontColor, minWidth, aqua, black, blue, gray
                           , green, orange, pink, red, white, yellow)
import  Clay.Media         (minWidth, screen)
import  Prelude      hiding (div, rem, (**), span)
import  Data.Monoid        ((<>), mempty)

main :: IO ()
main = putCss css

fontColor, firstColor, secondColor, backGroundColor, white, gray, aqua, blue, green, pink, black, yellow, ocher, orange, red :: Color
fontColor       = "#222"
firstColor      = "#555"
secondColor     = "#aaa"
backGroundColor = "#FFF"

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
  containerCss
  baseCss
  typoGraphyCss
  teaserCss
  listCss
  headerCss
  paginationCss
  footerCss
  codeLayout
  codeColorize

baseCss :: Css
baseCss = do
  html ? fontSize (pct 62.5)
  body ? do
    backgroundColor backGroundColor
    fontSize $ em 1.7
    lineHeight auto
    fontWeight $ weight 300
    fontFamily [ "Mplus 1P"
               , "メイリオ"
               , "Hiragino Sans"
               ] [sansSerif]
    color fontColor
    let margins m = margin auto (vw m) auto (vw m) 
    margins 7.5
    query screen [minWidth $ px 600]  $ margins 15
    query screen [minWidth $ px 1000] $ margins 18
    query screen [minWidth $ px 1300] $ margins 22
    query screen [minWidth $ px 1500] $ margins 25
  blockquote ?
    margin (px 15) (px 15) (px 15) (px 15)
  ".figure" ? margin (px 10) (px 10) (px 10) (px 10) 
  ".footnotes"  |> hr ? display displayNone
  ".article_data" ? do
    listStyle none inside none
    margin (rem 3) (rem 2) (rem 3) (rem 2) 
    query screen [minWidth $ px 1000] $
      margin (rem 3) (rem 4) (rem 3) (rem 4) 
  ".post-list" ? margin (rem 5) auto (rem 5) auto
  ".archives-link" ? do
    textAlign $ alignSide sideRight
    marginTop $ rem 6

containerCss :: Css
containerCss = do
  ".container" ? display flex
  ".container--column" ? flexDirection column
  ".container--row" ? flexDirection row
  ".container__center" ? justifyContent center

headerCss :: Css
headerCss = do
  ".blogtitle" ? do
    marginLeft   $ rem (-0.5)
    letterSpacing $ rem (-0.5)
    fontWeight   $ weight 900
    textAlign $ alignSide sideCenter
    fontSize $ rem 6
    query screen [minWidth $ px 800] $ fontSize $ rem 8
    query screen [minWidth $ px 1500] $ fontSize $ rem 10
  ".navigation" ? do
    fontWeight $ weight 300
    listStyleType none
  ".navigation" |> li ? do
    let paddings p = padding (rem p) (rem p) (rem p) (rem p) 
    paddings 0.4
    query screen [minWidth $ px 800] $ paddings 0.7
    query screen [minWidth $ px 1500] $ paddings 1
    margin none none none none
  ".topnav" ? do
    fontSize (rem 1.8)
    query screen [minWidth $ px 800] $ fontSize $ rem 2.0
    query screen [minWidth $ px 1500] $ fontSize $ rem 2.3

typoGraphyCss :: Css
typoGraphyCss = do
  p ? do
    marginTop nil
    marginBottom (rem 1.6)
    textRendering optimizeSpeed
  a ? do
    color firstColor
    textDecoration none
  a |> (h1 <> h2 <> h3 <> h4 <> h5 <> h6) ?
      color fontColor
  a # hover <>
    a |> (h1 <> h2 <> h3 <> h4 <> h5 <> h6) # hover ?
      color secondColor
  h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
      marginTop    $ rem 3
      marginBottom $ rem 1.8
      fontWeight   $ weight 300
  query screen [minWidth $ px 600] $ 
    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? 
      marginTop    (rem 8)
  mapM_ headCss
    [ ( h1, rem 4.0, rem $ 4.0 + 1.2,  rem (-0.1) )
    , ( h2, rem 3.6, rem $ 3.6 + 1.25, rem (-0.1)  )
    , ( h3, rem 3.0, rem $ 3.0 + 1.3,  rem (-0.1)  )
    , ( h4, rem 2.4, rem $ 2.4 + 1.35, rem (-0.08) )
    , ( h5, rem 1.8, rem $ 1.8 + 1.5,  rem (-0.05) )
    , ( h6, rem 1.5, rem $ 1.5 + 1.6,  nil )]
  where
    headCss :: (Selector, Size a, Size b, Size c) -> Css
    headCss (selector, fsize, lheight, lspacing) = selector ? do
      fontSize         fsize
      lineHeight       lheight
      letterSpacing    lspacing

teaserCss :: Css
teaserCss = do
  ".teaser" ? listStyle none inside none
  ".teaser" # nthChild "n+2" ? do
    marginTop $ rem 1
    paddingTop $ rem 1
  ".teaser" ** ".article_data" ? display displayNone
  
listCss :: Css
listCss = do
  ul <> ol ? do
    paddingLeft $ px 0
    marginTop   $ px 0
  ul ? listStyle circleListStyle inside none
  ul ** ul ? do
    listStyle disc inside none
    marginLeft $ pct 2
  ul ** ul ** ul ? listStyle circleListStyle inside none
  ul ** ul ** ul ** ul ? listStyle disc inside none
  ol ? listStyle decimal inside none
  dl <> li ? do
    marginTop $ rem 0.5
    marginBottom $ px 0

footerCss :: Css
footerCss = footer ? do
  textAlign (alignSide sideCenter)
  marginTop    $ rem 5
  marginBottom $ rem 7

paginationCss :: Css
paginationCss = do
  ".pagination" ? do
    listStyleType none
    marginTop $ rem 4
  ".pagination" |> li ?
    padding (rem 1) (rem 1) (rem 1) (rem 1) 
  -- "pagination--button" ?
  ".pagination--button__dummy" ? do
    pointerEvents none
    color secondColor

codeLayout :: Css
codeLayout = do
  -- pre # ".sourceCode" ? do
  pre ? do
    let space s = do
        padding (rem 3) (vw s) (rem 3) (vw s)
        margin (rem 3) (vw (-s)) (rem 3) (vw (-s))
    space 7.5
    query screen [minWidth $ px 600]  $ space 15
    query screen [minWidth $ px 1000] $ space 18
    query screen [minWidth $ px 1300] $ space 22
    query screen [minWidth $ px 1500] $ space 25
    width auto
  table # ".sourceCode" ? backgroundColor white
  table # ".sourceCode" <>
    tr  # ".sourceCode"  <>
    td  # ".lineNumbers" <>
    td  # ".sourceCode"  ? do
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)
    border none none none
  td # ".sourceCode" ? paddingLeft (px 5)
  pre <> code ? backgroundColor white
  pre # ".sourceCode" <> code ? do
    fontFamily ["Roboto Mono"] [monospace]
    lineHeight auto

codeColorize :: Css
codeColorize = do
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

