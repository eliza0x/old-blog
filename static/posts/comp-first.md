昨日はじめてプログラミングコンテストに参加しました。Atcoder Beginner Contest 061です。一問も解けませんでした。

## どうして競技プログラミングを始めたか
以前から興味があったので、機会もあり、競技プログラミングのチームに加入しました。

~~あと先輩から競技プログラミングをはじめると企業の金で寿司が食べられると唆された~~

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

ふだん使わない関数も沢山使えて面白かったですね。

純粋関数型のHaskellは入出力に苦労する、と思われているかもしれませんが今回問題に出た「一行目で渡された回数だけ入力を受け取る」なんてのも案外素直に書けてしまったので案外アリなんじゃないでしょうか。

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
例題は通るのですが、ACできない。他の問題もきっと問題文をきちんと理解できていないような気がします。はじめは問題をC++で解こうとしていたのですが、まったくアルゴリズムが思いつかない。しかたなくHaskellに切り換えると思いつく、といったかんじだったのでもっとC++をかけるようにならなくちゃならないですね…くやしいので来週のコンテストにも参加します。 
