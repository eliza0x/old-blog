{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module StyleSheet (styleSheet) where

import Text.Cassius
import Data.Text.Internal.Lazy (Text)
import Data.Text.Lazy (unpack)

render = undefined

styleSheet :: String
styleSheet = unpack . renderCss $ css render

fontColor           = Color 56  56  56
firstColor          = Color 255 60  0
secondColor         = Color 180 180 180 
backgroundColor     = Color 255 255 255
subBackgroundColor  = Color 114 114 114

css :: a -> Css
css = [cassius|
.container
  position: relative
  width: 100%
  max-width: 960px
  margin: 0 auto
  padding: 0 20px
  box-sizing: border-box

@media (min-width: 400px)
  .container
    width: 90%
    padding: 0

@media (min-width: 650px)
  .container
    width: 75%

html
  font-size: 62.5%

body
  font-size: 1.7em
  line-height: 1.8
  font-weight: 400
  font-family: "Open Sans", sans-serif
  color: #{fontColor}

h1, h2, h3, h4, h5, h6
  margin-top: 0
  margin-bottom: 2rem
  font-weight: 300
h1 
  font-size: 5.0rem
  line-height: 1.2
  letter-spacing: -.1rem
h2 
  font-size: 3.6rem
  line-height: 1.25
  letter-spacing: -.1rem
  font-size: 4.2rem

h3 
  font-size: 3.6rem
  line-height: 1.3
  letter-spacing: -.1rem
h4
  font-size: 3.0rem
  line-height: 1.35
  letter-spacing: -.08rem
h5 
  font-size: 2.4rem
  line-height: 1.5
  letter-spacing: -.05rem
h6 
  font-size: 1.5rem
  line-height: 1.6
  letter-spacing: 0

h1.title
  position: static
  text-align: center
  font-weight: 200

p
  margin-top: 0

a
  color: #{firstColor}
  text-decoration: none
a:hover
  color: #{subBackgroundColor}

ul
  list-style: circle inside

ol
  list-style: decimal inside

ol, ul
  padding-left: 0
  margin-top: 0
ul ul,
ul ol,
ol ol,
ol ul
  margin: 1.5rem 0 1.5rem 3rem
  font-size: 90%
li
  margin-bottom: 1rem

ul.navigation
  border-top: 1px solid #{subBackgroundColor}
  border-bottom: 1px solid #{subBackgroundColor}
  list-style-type: none
  overflow: hidden
  margin: 0
  padding: 0

ul.navigation li
  display: block
  text-align: left
  text-decoration: none
  margin: 0
  padding-top: 10px
  padding-bottom: 10px

@media (min-width: 580px)
  ul.navigation li
    float: left

  ul.navigation li a
    text-align: center
    padding-margin: 10px
    padding-top: 10px
    padding-left: 30px
    padding-right: 30px

hr
  margin-top: 3rem
  margin-bottom: 3.5rem
  border-width: 0
  border-top: 1px solid #{subBackgroundColor}

.figure img
  width: 100%

.pagination
  text-align: center
nav.navigation
  margin-top: 20px
  margin-bottom: 20px

div.clear
  clear: both

footer
  border-top: 1px solid #{subBackgroundColor}
  text-align: center
  position: static
  bottom: 0px
  padding-top: 15px
  margin-top: 15px
  margin-bottom: 15px

pre.sourceCode
  border-left: 2px solid #{secondColor}
  margin-left: 20px
  padding-left: 20px

blockquote
  border-left: 2px solid #{secondColor}
  margin-left: 20px
  padding-left: 20px

.container:after
  content: ""
  display: table
  clear: both
|]
