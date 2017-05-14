---
title: プログラミングコンテストに初参加した
date: 2017-05-14
tags: haskell, kosen14s, programming
---
昨日はじめてプログラミングコンテストに参加しました[^false]。Atcoder Beginner Contest 061です。一問も解けませんでした。
<!--more-->

## どうして始めたか
もともと私はHaskellやSchemeが好きで、プログラミングコンテストは興味が無いというふうにしておいたほうが良い、という状況でした。なんとなく興味はずっと持っていたので、機会[^oit]もあり、競技プログラミングのチームに加入しました。

## なにがあったか
```haskell
main :: IO () 
main = do
  [a, b, c] <- words <$> getLine
  putStrLn $ if (c >= a) && (c <= b) then "Yes" else "No"
```

一見正しそうにみえますが、`a, b, c`の型に注目すると、String型であることに気がつくでしょう。じゃあ何故比較できているか、StringはOrdのインスタンスなんですね、型は或る種の間違いは防いでくれるがStringは比較できてしまった。

**型注釈は書こう！**

```haskell
main :: IO () 
main = do
  [a, b, c] <- map toInt . words <$> getLine
  putStrLn $ if (c >= a) && (c <= b) then "Yes" else "No"
  where
  toInt = read :: String -> Int
```
これが正しいコードです。

ふだん使わない関数も沢山使えて面白かったですね、普段は`&&&`や`replicate`なんて使わないのですが。

純粋関数型のHaskellは入出力に苦労する、と思われているかもしれませんが今回問題に出た「一行目で渡された回数だけ入力を受け取る」なんてのも案外素直に書けてしまいます。もしかすると命令型言語よりも綺麗に書けているのではないでしょうか。

```haskell
main :: IO ()
main = do
  [n, m] <- map toInt . words <$> getLine
  xs <- mapM (\_ -> getLine) [1..n] :: IO [String]
  mapM_ putStrLn xs
  where
  toInt = read :: String -> Int
```

## おもしろかった
例題は通るのですが、ACできない。他の問題もきっと問題文をきちんと理解できていないような気がします。はじめは問題をC++で解こうとしていたのですが、まったくアルゴリズムが思いつかない。しかたなくHaskellに切り換えると思いつく、といったふうでしたから、プログラミング言語に思考は縛られているんだという実感をえることができました。もっとC++をかけるようにならなくちゃならない[^ggl]。くやしいので来週のコンテストにも参加します。 

[^false]: これは半分ぐらい正しくなくて、一度だけ飛び入り参加で爆死したことがある
[^oit]: 大学の学生プロジェクトが存在する
[^ggl]: 実行速度云々以前に、Haskellが使えないコンテストにもチームで参加するため
