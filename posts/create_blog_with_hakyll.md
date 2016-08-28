----
title: Haskellでブログを作った
description: HakyllとShakespeare、そしてClayでブログを作った際の記録です。
date: 2016-08-27
tags: haskell, program, hakyll, blog
----

以前からはてなブログなどで記事は書いていたんですが、どうしてもモチベーションも保てず、なんだか嫌になってブログを削除した黒歴史があります。
しかし、やっぱり記事は書くべき時が来ますし、せっかくブログを作るならもっと自由にカスタマイズできる環境であればより楽しいんじゃないかと思ったので、HakyllとShakespeare、そしてClayで私の好きなHaskellを使いブログを作りました。

このページのソースコードは[ここ](https://github.com/eliza0x/eliza0x.github.io)に公開しています。

## Hakyllでウェブサイトの生成

HTMLやCSS, JavaScriptなど、手元で完結するようなウェブサイトの事を静的サイトと言うようです(間違っていたらごめんなさい)、そんな静的なサイトをうまいこと生成してくれるプログラムがそれはそれは沢山ある [^StaticGen] のですが、その中から私はHaskellでブログを作りたかったのでHakyllを選択しました。

[Hakyllの公式サイト](https://jaspervdj.be/hakyll/)

公式ページによると小〜中規模のサイト向けだそうです、使ってみてブログ以外でも割と使えるんじゃないかと思いました。

[^StaticGen]: [StaticGen](https://www.staticgen.com/) というサイトにまとまっています。静的サイトジェネレータのうちで有名なものといえばPelican(Python), Jekyll(Ruby), GitBook(JavaScript), Hugo(Go)などがあるでしょうか。おもしろそうなものでは、Lispで書かれたColeslawなんてものもあるそうです。

## とりあえず記事は楽して書きたい

[Pandoc](http://pandoc.org/)を知っていますか？MarkdownやreStructuredText(reSTの呼称のほうが一般的？)などで書かれたドキュメントをHTMLやTeX, 果てはWord docs形式に変換してくれる便利なソフトウェアです。

HakyllはPandocを容易に利用することが出来るよう設計されているので、記事をMarkdownやreSTで書く事が出来ます。便利ですね。

また、Pandocの機能で数式や脚注を埋め込めます。具体的に何が出来るかや、どんなフォーマットに対応しているかは、有志によるPandocのユーザーズガイドを参照すれば良いと思います。

[Pandocのユーザーズガイド(和訳版)](http://sky-y.github.io/site-pandoc-jp/users-guide/)

私の手元のPandoc(version 1.17.1)は以下のフォーマットに対応していました。

```
Input formats:
commonmark, docbook, docx, epub, haddock, html, json*, latex,
markdown, markdown_github, markdown_mmd, markdown_phpextra,
markdown_strict, mediawiki, native, odt, opml, org, rst, t2t,
textile, twiki
[ *only Pandoc's JSON version of native AST]

Output formats: 
asciidoc, beamer, commonmark, context, docbook, docbook5, docx,
dokuwiki, dzslides, epub, epub3, fb2, haddock, html, html5,
icml, json*, latex, man, markdown, markdown_github,
markdown_mmd, markdown_phpextra, markdown_strict, mediawiki,
native, odt, opendocument, opml, org, pdf**, plain, revealjs,
rst, rtf, s5, slideous, slidy, tei, texinfo, textile
[**for pdf output, use latex or beamer and -o FILENAME.pdf]
```

ちなみにCSSさえ準備してしまえば、version1.17.1の場合以下の言語のシンタックスハイライトにも対応しています。この話は後でもうすこし書きます。

```
Syntax highlighting is supported for the following languages:
abc, actionscript, ada, agda, apache, asn1, asp, awk, bash, bibtex, boo, c,
changelog, clojure, cmake, coffee, coldfusion, commonlisp, cpp, cs, css,
curry, d, diff, djangotemplate, dockerfile, dot, doxygen, doxygenlua, dtd,
eiffel, elixir, email, erlang, fasm, fortran, fsharp, gcc, glsl,
gnuassembler, go, hamlet, haskell, haxe, html, idris, ini, isocpp, java,
javadoc, javascript, json, jsp, julia, kotlin, latex, lex, lilypond,
literatecurry, literatehaskell, llvm, lua, m4, makefile, mandoc, markdown,
mathematica, matlab, maxima, mediawiki, metafont, mips, modelines, modula2,
modula3, monobasic, nasm, noweb, objectivec, objectivecpp, ocaml, octave,
opencl, pascal, perl, php, pike, postscript, prolog, pure, python, r,
relaxng, relaxngcompact, rest, rhtml, roff, ruby, rust, scala, scheme, sci,
sed, sgml, sql, sqlmysql, sqlpostgresql, tcl, tcsh, texinfo, verilog, vhdl,
xml, xorg, xslt, xul, yacc, yaml, zsh
```

聞いた事の無いようなものまで混じってしますね。

HakyllではこんなコードでPandocを呼び出し、ページを生成できます、実際このコードが動くかは知りません。

```haskell
compile $ pandocCompiler 
  >>= loadAndApplyTemplate "templates/flame.hamlet" postCtx
  >>= relativizeUrls
```

## Shakespearean Templates

いくら記事のHTMLを自動生成してくれるからといって、デザインは自分で行わないといけません。しかし、HTMLやCSSを生で書きたくない。始めはSass(CSSのめっちゃすごいやつ)とJade(Htmlのめっちゃすごいやつ)をつかおうと思っていたのですが、折角ならPure Haskellでブログを作ってみようと思い、Yesod Frameworkで使用されているShakespeareを使ってみました。

Shakespearean Templatesはテンプレート言語です、数あるテンプレート言語の中でこの言語が優れている点はHaskellとの連携が容易な点です。シームレスにHaskellの関数が呼び出せたりごにょごにょ。

> Shakespearean Templatesとは、Webコンテンツを構成するテキストをHaskell/Yesodで生成する、下記のテンプレート言語群のことです。
>
> - Hamlet(HTML)
> - Julius(JavaScript)
> - Cassius(CSS)
> - Lucius(CSS)
>
> <https://sites.google.com/site/toriaezuzakki/haskell/yesod/shakespearean-templates>

詳しく知りたければ[このチュートリアル](http://www.yesodweb.com/book/shakespearean-templates)でも読めば良いんじゃないでしょうか。

HakyllとHamletの連携が面倒だったのでライブラリを書きました、よければ使ってください。  
<https://github.com/eliza0x/hakyll-shakespeare>

こんなふうに使えます。

```haskell
match "templates/*.hamlet" $ compile hamlTemplateCompiler
```

## Clay射撃

~~CSSはCassiusを利用して[Skeleton](http://getskeleton.com/)や[Milligram](https://milligram.github.io/)を参考にしつつもがんばって一から書きました。~~

ClayというモナドベースのCSSプリプロセッサーがHakyll公式で推されていたので使ってみると予想以上に面白かったので、一度Cassiusで書いたCSSをClayで書き換えました。良いですよ、Clay。

HTMLもこういったライブラリで生成したいのは山々なんですが、Hakyllの柵 ^[テンプレートの呼び出しのあたり] で面倒くさそうです。

出来るだけシンプルにしようと心掛けて作ったのですが、[やましー](yamasy.info)の言葉を借りるとお葬式みたいな配色になってしまいました。まあまあ気にいっていますが。

こんなコードです、動くかは知りません。

```haskell
main :: String
main = putCss css

fontColor :: Color
fontColor = "#303030"

css :: Css
css = do
  html ? fontSize (pct 62.5)
  p ? do
    marginTop nil
    marginBottom (rem 3.0)
  a ? do
    textDecoration none
  a # hover ? color secondColor
```

公式サイトです、チュートリアルやサンプルコードもここにあります。

<http://fvisser.nl/clay/>

## コーディング

私は[作者の公開されているソースコード](https://github.com/jaspervdj/jaspervdj)や、[tanakhさんのブログ](http://tanakh.jp/posts/2011-11-05-haskell-infra.html),それと[Imokuri氏のブログ](https://imokuri123.com/blog/2015/12/how-to-create-blog-with-hakyll-part1.html)を参考にさせていただきました。もちろん[HakyllのHackage](https://hackage.haskell.org/package/hakyll-4.8.3.2)もです、ありがとうございます。

とりあえずImokuri氏のブログを読んでなんとなく摑んでから、作者のプログラムでも読めばいいんじゃないかと思います。

またブログを始める際、Markdownは知ってるよ、という人でも[Pandocのユーザーズガイド](http://sky-y.github.io/site-pandoc-jp/users-guide/)は読んでおいたほうが良いと思います。Pandocならではの拡張が非常に便利なので、これを使わない手は無いです。

## シンタックスハイライト

ブログのソースコードはハイライトして欲しいです。以前nanoでプログラムのデバッグをしたとき死ぬかと思いました。

始めはhighlihgt.jsでも使おうかなあなんて思っていたんですが、Pandocはシンタックスハイライトをしてくれるそうなので、染色の為に専用のCSSを準備しましょう。[Imokuriさんのこのページ](https://imokuri123.com/blog/2015/12/how-to-create-blog-with-hakyll-part4.html)に詳しく載っています。もしくは[私の書いたソースコード](https://github.com/eliza0x/eliza0x.github.io)でも読むと良いでしょう。

こんなコードです、動くかは知りません。

```haskell
main = hakyll $ do
  create ["css/highlight.css"] $ do
    route   idRoute
    compile $ makeItem (compressCss $ styleToCss tango)
```

## コメントフォーム

私の記事について質問があった時などに、メールを送ったりするのはハードルが高いかなと思ったので、コメンドフォームをDISQUSで設けました。はじめは自分で作る気でいたのですが、 ~~面倒だった~~ スパムコメントなどが怖かったのでアウトソーシングしました。デザインも可愛く気に入っています。

公式サイトの指示どおりにすると動かなかったので、[tanakh氏のブログ](tanakh.jp)のソースコードを参考にさせて頂きました。

[DISQUS](https://disqus.com/)

## 終わりに

HakyllはMonadでうまいこと面倒な部分を隠してくれていて非常に使いやすかったです。ああやって使うんですね、モナド。型クラスの恩恵は計り知れないですね。設定がMonoidのインスタンスになっていて `<>` で簡単に追加できるの、アレ良いですね。

もうすこしCSSを書かないとまだまだ粗が目立ちますね。ブログを作るのも結構大変だ…

このサイトはGihub Pagesにて公開してあります。Github PagesはGitの使い方を知らないと利用するのは難しいかもしれないです。

このブログのソースコードは[ここ](https://github.com/eliza0x/eliza0x.github.io)に公開しています。

#### 紹介できなかったサイト

- [Github Pagesの使い方](http://qiita.com/mikakane/items/87c8f676815da4e5ac04)
- [GitHub Pagesで静的なサイトを公開し、独自ドメインを設定する](http://qiita.com/tiwu_official/items/d7fb6c493ed5eb9ee4fc)
- [カスタムドメインの GitHub Pages で HTTPS を使う](http://qiita.com/superbrothers/items/95e5723e9bd320094537)
