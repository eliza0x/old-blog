----
title: TaPLに必要な知識の学習記録
description: TaPLの予備知識(主に数学)の学習メモです。
date: 2016-09-15
tags: haskell, book, memo, programming, math
----

TaPLを読むのために少し勉強したのでメモを残しておきます。
内容の真偽の保証はできません。必要に応じて追記していきます。

[数学初心者のための「型システム入門」入門](http://zoetrope.hatenablog.jp/entry/2013/07/24/204613)によると前提として以下の知識が必要になるようです。

> - 数学(離散数学、アルゴリズム、論理学)
> - 高階関数型プログラミング言語(Scheme、ML、Haskellなど)
> - コンパイラの基礎的な概念(抽象構文、BNF文法、評価、抽象機械など)

TaPLの読書記録は[このページ](/posts/TaPL.html)に載っています。

<!--more-->

## 論理学

"「全ての人間は２種類に分けられる。スウィングする者としない者だ"は真

- [「論理学入門」講義ノート](http://abelard.flet.keio.ac.jp/person/mitsu/pdf/nyumon_logic.pdf)が論理学で検索してヒットしたものの中で一番容易に読み進めることができそうだったので読み流した(証明は追いかけるの面倒だったので飛ばした)。
- 記号がダメな人はアレルギーをおこしそうな分野でした、印刷したテキストをうっかり直視した友人は帰らぬ人となりました。(まあプログラマは大丈夫だと思う)

## 集合論

論理学とあわせて、何かを表現する際の述語を勉強している気分です。

何故型システムに集合論が必要かというと、型付けされた値を集合(集合族？)として捉える事が出来るからだと思っています。

- TaPLの冒頭に集合がなんだかんだと書かれていたので勉強が必要だと思い、[東京女子大学の代数学IA(集合と論理)のテキスト](http://www.math.twcu.ac.jp/~yamauchi/text/2012/alg2012/set_logic.pdf)を読んでみました。
    + 私の環境では途中で組版がすこし崩れています。
    + これを読むのは論理学のテキストを読んだ後が良いです。
- 「集合・位相入門」もすこし読みました。 <https://www.iwanami.co.jp/.BOOKS/00/4/0054240.html>
- 関係？
    + プログラマがわかるように言うと、trueがfalseで分類できる二項演算子^[本当はn項らしい]の事。
- 反射性、推移性、対称性ってそもそもどういう意味？
    + 推移関係
        - $\forall a,b,c \in X,\; aRb \land bRc \Rightarrow aRc$
        - 関数合成をイメージした
    + 対象関係
        - $\forall a,b \in X,\; aRb \Rightarrow bRa$
        - 左右を入れ替えても問題がない関係と理解した。
        - たとえば、$A=B$なら$B=A$
    + 反射関係
        - $\forall a \in X,\; aRa.$
        - <blockquote class="twitter-tweet" data-conversation="none" data-lang="ja"><p lang="ja" dir="ltr"><a href="https://twitter.com/Eliza_0x">@Eliza_0x</a> あれは，一言で言うと「同じものは同じ」って言いたいの</p>&mdash; しんぷっと(読み方：唯一無二の光) (@a0symptote) <a href="https://twitter.com/a0symptote/status/777573971227095040">2016年9月18日</a></blockquote>
- 以上三つを満たした関係を同値といいます。

## 離散数学

いつかやりたいなぁ…

## ラムダ計算

- [「ラムダ計算」を独学で学習するための，講義ノートやPDFのリンク集 （復習用の問題付き）](http://language-and-engineering.hatenablog.jp/entry/20130313/LambdaCalculusBasicNoteLinks)に詳しくまとまっています。

## アルゴリズム

## 高階関数型プログラミング言語

- まあまあHaskellが書けるので問題はなさそうと慢心。TaPLのサンプルコードはOCaml(MLの方言)で書かれていました。
- Haskellで書かれたサンプルコードは <https://github.com/zerokarmaleft/tapl-haskell> ここにありました。多言語でもある筈です。
- サンプルコードの実装にパーサを書かなければいけないみたいなので、ekmett氏の[trifecta](http://hackage.haskell.org/package/trifecta)でも使ってみようと思います。
