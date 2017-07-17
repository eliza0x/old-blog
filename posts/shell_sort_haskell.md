---
title: Haskellでシェルソートを書く
date: 2017-07-17
tags: haskell, programming
---

以前書いたバブルソートは遅いので、シェルソートを書いてみました。

まずはじめに普通の挿入ソートを書きます。

<!--more-->

```haskell
main :: IO ()
main = (print . insertSort . map read . words) =<< getLine

insertSort :: [Int] -> [Int]
insertSort = foldr insert [] 

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs) = if n < x then n:x:xs
                           else x:insert n xs
```

手元で実験したところ、これだけでバブルソートの100倍ぐらい性能が出て驚きました。


```haskell
import Data.List (transpose)

main :: IO ()
main = (print . shellSort . map read . words) =<< getLine

shellSort :: [Int] -> [Int]
shellSort l = foldr (\n l -> merge . map insertSort $ splitBy n l) l (reverse . genH $ length l)

genH :: Int -> [Int]
genH n = takeWhile (<n) $ map (\i -> div (3^i-1) 2) [1..]

splitBy :: Int -> [Int] -> [[Int]]
splitBy n l = transpose $ splitBy' n l
  where splitBy' _ [] = []
        splitBy' n l = take n l : splitBy' n (drop n l)

merge :: [[Int]] -> [Int]
merge = concat . transpose 

insertSort :: [Int] -> [Int]
insertSort = foldr insert [] 

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs) = if n < x then n:x:xs
                           else x:insert n xs

```

つぎにこれをN分割してはソート、N分割してはソート、を繰り返すシェルソートのに書き直します。`splitBy`でN個区切りのリストに変換したあと、`transpose`で転置して、シェルソートしています。

## 結果

元の1000倍ぐらい速くなりました

