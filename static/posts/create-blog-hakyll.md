
<div class="card-panel red darken-4 white-text">
    この記事は古くなっています。現在このブログはJavascriptで生成されています。
</div>

以前ははてなブログなどで記事は書いていたんですが、どうしてもモチベーションも保てず、なんだか嫌になってブログを削除してしまいました。
しかし、やっぱり参加したイベント等の記事は書きたい。それでせっかくブログを作るならもっと自由にカスタマイズできる環境であればより楽しく続けられると思ったので、HakyllとShakespeare、そしてClayを使い、私の好きなHaskellでブログを作りました。

記事をかいた当時のこのページのソースコードは[ここ](https://github.com/eliza0x/eliza0x.github.io/tree/bd964b8f01908b93c555d528bd020cb2060975bd)に公開しています。
また、最新版は[ここ](https://github.com/eliza0x/eliza0x.github.io)に公開しています。

<!--more-->

## Hakyllでウェブサイトの生成

HTMLやCSS, JavaScriptなど、手元で完結するようなウェブサイトの事を静的サイトと言うようです(間違っていたらごめんなさい)、そんな静的なサイトをうまいこと生成してくれるプログラムがそれはそれは沢山ある [^StaticGen] のですが、その中から私はHaskellでブログを作りたかったのでHakyllを選択しました。

[Hakyllの公式サイト](https://jaspervdj.be/hakyll/)

公式ページによると小〜中規模のサイト向けだそうです、使ってみてブログ以外でも割と使えるんじゃないかと思いました。

[^StaticGen]: [StaticGen](https://www.staticgen.com/) というサイトにまとまっています。静的サイトジェネレータのうちで有名なものといえばPelican(Python), Jekyll(Ruby), GitBook(JavaScript), Hugo(Go)などがあるでしょうか。おもしろそうなものでは、Lispで書かれたColeslawなんてものもあるそうです。

## とりあえず記事は楽して書きたい

[Pandoc](http://pandoc.org/)を知っていますか？MarkdownやreStructuredText(reSTの呼称のほうが一般的？)などで書かれたドキュメントをHTMLやTeX, 果てはWord docs形式に変換してくれる便利なソフトウェアです。

HakyllはPandocを容易に利用することが出来るよう設計されているので、記事をMarkdownやreSTで書く事が出来ます。便利ですね。

また、Pandocの機能で数式や脚注を埋め込めます。

$$\ln x = \int_{-\infty}^x \frac 1 y \, dy .$$

具体的に何が出来るかや、どんなフォーマットに対応しているかは、有志によるPandocのユーザーズガイドを参照すれば良いと思います。

[Pandocのユーザーズガイド(和訳版)](http://sky-y.github.io/site-pandoc-jp/users-guide/)

私の手元のPandoc(version 1.19.2.1)は以下のフォーマットに対応していました。



```
Input formats:

commonmark, docbook, docx, epub, haddock, html, json, latex, markdown, markdown_github, markdown_mmd, markdown_phpextra, markdown_strict, mediawiki, native, odt, opml, org, rst, t2t, textile, twiki

Output formats:

asciidoc, beamer, commonmark, context, docbook, docbook5, docx, dokuwiki, dzslides, epub, epub3, fb2, haddock, html, html5, icml, json, latex, man, markdown, markdown_github, markdown_mmd, markdown_phpextra, markdown_strict, mediawiki, native, odt, opendocument, opml, org, plain, revealjs, rst, rtf, s5, slideous, slidy, tei, texinfo, textile, zimwiki
```

ちなみにCSSさえ準備してしまえば、version1.17.1の場合以下の言語のシンタックスハイライトにも対応しています。この話は後でもうすこし書きます。

```
Syntax highlighting is supported for the following languages:

abc, asn1, asp, ats, awk, actionscript, ada, agda, alertindent, apache, bash, bibtex, boo, c, cs, cpp, cmake, css, changelog, clojure, coffee, coldfusion, commonlisp, curry, d, dtd, diff, djangotemplate, dockerfile, doxygen, doxygenlua, eiffel, elixir, email, erlang, fsharp, fortran, gcc, glsl, gnuassembler, m4, go, html, hamlet, haskell, haxe, ini, isocpp, idris, fasm, nasm, json, jsp, java, javascript, javadoc, julia, kotlin, llvm, latex, lex, lilypond, literatecurry, literatehaskell, lua, mips, makefile, markdown, mathematica, matlab, maxima, mediawiki, metafont, modelines, modula2, modula3, monobasic, ocaml, objectivec, objectivecpp, octave, opencl, php, pascal, perl, pike, postscript, prolog, pure, purebasic, python, r, relaxng, relaxngcompact, roff, ruby, rhtml, rust, sgml, sql, sqlmysql, sqlpostgresql, scala, scheme, tcl, tcsh, texinfo, mandoc, vhdl, verilog, xml, xul, yaml, yacc, zsh, dot, noweb, rest, sci, sed, xorg, xslt
```

聞いた事の無いようなものまで混じってしますね。

HakyllではこんなコードでPandocを呼び出し、ページを生成できます。

```haskell
compile $ pandocCompiler 
  >>= loadAndApplyTemplate "templates/template.hamlet" postCtx
  >>= relativizeUrls
```

## Shakespearean Templates

いくら記事のHTMLを自動生成してくれるからといって、デザインは自分で行わないといけません。しかし、HTMLやCSSを生で書きたくない。始めはSass(CSSを楽して書くためのもの)とJade(Htmlを楽して書くためのもの)をつかおうと思っていたのですが、折角ならPure Haskellでブログを作ってみようと思い、Yesod Frameworkで使用されているShakespeareを使ってみました。

Shakespearean Templatesはテンプレート言語です、数あるテンプレート言語の中でこの言語が優れている点はHaskellとの連携が容易な点です。シームレスにHaskellの関数が呼び出すことが出来ます。

> Shakespearean Templatesとは、Webコンテンツを構成するテキストをHaskell/Yesodで生成する、下記のテンプレート言語群のことです。
>
> - Hamlet(HTML)
> - Julius(JavaScript)
> - Cassius(CSS)
> - Lucius(CSS)
>
> <https://sites.google.com/site/toriaezuzakki/haskell/yesod/shakespearean-templates>

詳しく知りたければ[このチュートリアル](http://www.yesodweb.com/book/shakespearean-templates)でも読めば良いんじゃないでしょうか。

HakyllとHamletの連携が面倒だったので、薄いラッパーを書きました、よければ使ってください。  
<https://github.com/eliza0x/hakyll-shakespeare>

こんなふうに使えます。

```haskell
match "templates/*.hamlet" $ compile hamlTemplateCompiler
```

## Clay

~~CSSはCassiusを利用して[Skeleton](http://getskeleton.com/)や[Milligram](https://milligram.github.io/)を参考にしつつもがんばって一から書きました。~~

ClayというモナドベースのCSSプリプロセッサーがHakyll公式で推されていたので使ってみると予想以上に面白かったので、一度Cassiusで書いたCSSをClayで書き換えました。良いですよ、Clay。

HTMLもこういったライブラリで生成したいのは山々なんですが、Hakyllの柵 ^[テンプレートの呼び出しのあたり] で面倒くさそうです。

~~出来るだけシンプルにしようと心掛けて作ったのですが、[やましー](yamasy.info)の言葉を借りるとお葬式みたいな配色になってしまいました。まあまあ気にいっていますが。~~

追記: 2017/3/24 

無駄な線一本入れないでやろうと、もういちどデザインをしなおしてみました。いいものになったと思いますが、やっぱりデザインは難しいですね。

こんなコードでCSSを生成しています。

```haskell
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

ブログのソースコードにもシンタックスハイライトが必要です。以前nanoでプログラムのデバッグをしたとき大変でした。

始めはhighlight.jsでも使おうかなあなんて思っていたんですが、Pandocはシンタックスハイライトをしてくれるそうなので、染色の為に専用のCSSを準備しましょう。[Imokuriさんのこのページ](https://imokuri123.com/blog/2015/12/how-to-create-blog-with-hakyll-part4.html)に詳しく載っています。もしくは[私の書いたソースコード](https://github.com/eliza0x/eliza0x.github.io)でも読むと良いでしょう。

こんなコードです、動くかは知りません。

```haskell
main = hakyll $ do
  create ["css/highlight.css"] $ do
    route   idRoute
    compile $ makeItem (compressCss $ styleToCss tango)
```

追記: 2017/3/24 

現在は上記のプログラムで生成したものではなく、自分で好きなカラースキームをベースにしたものをPandoc用に移植して使用しています。

[morhetz/grubvox - https://github.com/morhetz/gruvbox](https://github.com/morhetz/gruvbox)

## コメントフォーム

私の記事について質問があった時などに、メールを送ったりするのはハードルが高いかなと思ったので、コメンドフォームをDISQUSで設けました。はじめは自分で作る気でいたのですが、 ~~面倒だった~~ スパムコメントなどが怖かったのでアウトソーシングしました。デザインも可愛く気に入っています。

公式サイトの指示どおりにすると動かなかったので、[tanakh氏のブログ](tanakh.jp)のソースコードを参考にさせて頂きました。

[DISQUS](https://disqus.com/)

## 終わりに

設定がMonoidのインスタンスになっていて `<>` で追加の設定を追加できるのが凄く便利でした。
Hakyll自体を実際に弄ってみて、型クラスについての知識が深まりました。

もうすこしCSSを書かないとまだまだ粗が目立ちますね。ブログを作るのも結構大変だ…
Clayを使ってみてなかなか良いなぁと思ったので、HTMLもそういったもので書き出すかもしれないです。

あとClay等プリプロセッサをオンデマンドでコンパイルしてHakyllから便利に扱えるようにするラッパーを書きたいですね。hintとか使えばうまくいくのかな…？

最後に、このサイトはGihub Pagesにて公開してあります。Github PagesはGitの使い方を知らないと利用するのは難しいかもしれないですが非常に便利です、いかがですか?

このブログのソースコードは[ここ](https://github.com/eliza0x/eliza0x.github.io/tree/bd964b8f01908b93c555d528bd020cb2060975bd)に公開しています。

追記: 2017/3/24 

[最新版](https://github.com/eliza0x/eliza0x.github.io/tree/7377ae11adbbaabb2dcd4713e96cfa0183663627)はこちらです。

#### Github pagesについて

Github Pagesで独自ドメインを利用するために参考にしたものです。

- [Github Pagesの使い方](http://qiita.com/mikakane/items/87c8f676815da4e5ac04)
- [GitHub Pagesで静的なサイトを公開し、独自ドメインを設定する](http://qiita.com/tiwu_official/items/d7fb6c493ed5eb9ee4fc)
- [カスタムドメインの GitHub Pages で HTTPS を使う](http://qiita.com/superbrothers/items/95e5723e9bd320094537)
