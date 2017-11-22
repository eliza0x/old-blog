部長からの挑戦状に応える形で、バブルソートを書いてみました。これはそのときの記録です。

普通のバブルソートを書くだけではあまり面白くないですから、入出力を含めて一行ですべてすませるプログラムを書いてみることにします。

<!--more-->

## バブルソートとは？
Wikipediaの[バブルソート](https://ja.wikipedia.org/wiki/%E3%83%90%E3%83%96%E3%83%AB%E3%82%BD%E3%83%BC%E3%83%88)を読めばいいと思います。ざっくりと説明すると、リストの隣り合った要素同士を次々と比較していくとソート出来る。というアルゴリズムだそうです。

## ふつうに書いてみる

```haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- getArgs :: IO [String]
  print . bubble . map toInt $ input
  where
    toInt = read :: String -> Int

bubble :: Ord a => [a] -> [a]
bubble l = let s = bubble1 l
           in if s == l then l 
                        else bubble s

bubble1 :: Ord a => [a] -> [a]
bubble1 (l:m:n) = min l m : bubble1 (max l m : n)
bubble1 x = x
```

```
$ ghc -O2 bubble_normal.hs
$ ./bubble_normal 1 2 4 2 1 9 3 1 7 9 10 3 2 4 2
[1,1,1,2,2,2,2,3,3,4,4,7,9,9,10]
```

うまくうごいていますね。

## 簡単な解説

やっていることは単純です、上から順に解説していきます。

```haskell
main :: IO ()
main = do
  input <- getArgs :: IO [String]
  print . bubble . map toInt $ input
  where
    toInt = read :: String -> Int
```

`main`関数では、`getArgs`でコマンドライン引数を受け取ったあと、`map toInt`で文字型のリストを整数型のリストに変換したものを、後に定義する予定である`bubble`関数でソートしたのち、`print`関数で出力しています。

```haskell
bubble :: Ord a => [a] -> [a]
bubble l = let s = bubble1 l
           in if s == l then l 
                        else bubble s
```

`bubble`関数では、受け取ったリストが変化しなくなるまで`bubble1`を繰り返す、という関数になっています。もしリストを`bubble1`に適用しても変化しないと言う事は何度`bubble1`に繰り返し適用したとしてももう変化しないと言うことで、`bubble1`が望ましい動作をするならば、そのリストは整列済みであるということです。

```haskell
bubble1 :: Ord a => [a] -> [a]
bubble1 (l:m:n) = min l m : bubble1 (max l m : n)
bubble1 x = x
```

このコードの肝である、`bubble1`関数です。といっても単純な定義で、`max`は与えられた二つの引数のうち大きな値を返す関数、`min`は小さな値を返す関数です。この操作で、リストを端からふたつづつ、ひとつズレで舐めていきます。

これらの単純な関数を組み合わせることで、このコードではバブルソートを実現しています。

## もっと短いコードで書くための下処理

```haskell
import System.Environment

main :: IO ()
main = print . bubble . map (read :: String -> Int) =<< getArgs

bubble :: Ord a => [a] -> [a]
bubble = \l -> let s = bubble1 l
                in if s == l 
                     then l 
                     else bubble s

bubble1 :: Ord a => [a] -> [a]
bubble1 = \list -> if length list >= 2 
                     then (\(l:m:n) -> min l m : bubble1 (max l m : n)) list
                     else list
```

取り敢えず、`main`関数を一行で、後の作業のための前処理のために、関数をlambda式でかきなおします。このコードでは、先程存在した`toInt`関数を、そのまま`main`関数の中にうめこんでしまいました。これから`bubble`と`bubble1`にたいしてこの作業を行っていきます。

埋め込む際に問題になるのは、再起表現です。これを解決するために不動点コンビネータを導入します。不動点コンビネータについての解説は、Wikibooksの[Haskell/不動点と再帰](https://ja.wikibooks.org/wiki/Haskell/%E4%B8%8D%E5%8B%95%E7%82%B9%E3%81%A8%E5%86%8D%E5%B8%B0)かWikipediaの[不動点コンビネータ](https://ja.wikipedia.org/wiki/%E4%B8%8D%E5%8B%95%E7%82%B9%E3%82%B3%E3%83%B3%E3%83%93%E3%83%8D%E3%83%BC%E3%82%BF)を読めばいいとおもいます。

実際に不動点コンビネータで書き直したものが以下のコードです。

```haskell
import System.Environment (getArgs)
import Control.Monad.Fix (fix)

main :: IO ()
main = print . bubble . map (read :: String -> Int) =<< getArgs

bubble :: Ord a => [a] -> [a]
bubble = fix $ 
  \rec l -> let s = bubble1 l
            in if s == l then l else rec s

bubble1 :: Ord a => [a] -> [a]
bubble1 = fix $ 
  \rec list -> if length list >= 2 
                 then (\(l:m:n) -> min l m : rec (max l m : n)) list
                 else list
```

ほとんど書き換えることなく`fix`で再帰を表現することができました。これで`bubble`や`bubble1`内に自身の名前が登場することはありません、早速`main`内に埋め込んでみましょう。

## 一行でバブルソートを書く

```haskell
import System.Environment (getArgs)
import Control.Monad.Fix (fix)

main = print . bubble . map (read :: String -> Int) =<< getArgs

b = (fix $ \rec l -> let s = bubble1 l in if s == l 
        then l else rec s)

b1 = (fix $ \rec' ls' -> if length ls' >= 2 
        then (\(l':m':n') -> min l' m' : rec (max l' m' : n')) ls' 
        else ls')
```

最後にこれらを埋め込みます。

```haskell
import System.Environment (getArgs)
import Control.Monad.Fix (fix)

main = print . (fix $ \rec l -> let s = (fix $ \rec' ls' -> if length ls' >= 2 then (\(l':m':n') -> min l' m' : rec (max l' m' : n')) ls' else ls') l in if s == l then l else rec s) . map (read :: String -> Int) =<< getArgs
```

できました、`import foobar`部を除くと確かに一行です。絶対に読みたくないコードですね。

```
./bubble_oneline 1 9 2 9 1 9 3 7 1 9 3 2
[1,1,1,2,2,3,3,7,9,9,9,9]
```

確かに動いていますね。これで部長に大きな顔ができそうです。


