[もは氏/mc475_46](https://twitter.com/mc475_46)の勧めもあってついにAVRマイコンに手を出してみました。後悔するのは後になってからでも構わないでしょう。
自分の書いたプログラムがPCの外で動いているというのは新鮮ですし、マイコンはプログラミングと日常をつないでくれるような気がしてなんだかとても楽しいです。

PlatformIOでFT232RLを使ってAtTiny85(AVR)に書き込んでいる記事なネットでみつけられなくて苦労したのでこの記事を書きました。

## はじめに

回路図も作ってがんばって記事を書くつもりでしたが、Fritzing(可愛い回路図をつくれるソフト)のビルドが目の前でSegmentation faultを投げてから心が終わってしまったのでそれほど長い記事にはならない筈です。

さて、今回目指すのはattiny85aでLチカをすることです。

ArduinoIDEなどGUIからでも出来るようですが、わたしはVimが使いたかったので、コマンドラインからよしなにしてくれるPlatformIOというソフトウェアをつかってコンパイルから書き込みまで行ってみようと思います。

## attiny85a?

[データシート](http://www.atmel.com/static/images/Atmel-2586-AVR-8-bit-Microcontroller-ATtiny25-ATtiny45-ATtiny85_Datasheet.pdf)

AVRはAtmel社が作っているRISCのマイコンです。AtTiny85はそのAVRのうちのTinyシリーズに属するもので、Tiny(ちっぽけ)の名にふさわしくピンは8本しかありません、さらにそのうちの2PINは電源に使用するので我々は6PINしか使う事が出来ません。しかしDIP版でもわずか1cm * 1cmのマイコンにはROMが8Kbyte,RAMが512Byteも搭載されています、必要十分ではないでしょうか。

![画像は秋月電子様より](/static/images/AtTiny85.jpg)

## PlatformIO？

<blockquote>
platformioがすごいのは、IDEが必要ないということだ。inoは別途IDEが必要だったがplatformioは必要ない。パッケージとして自動でダウンロードしてくれるのだ。さらにすごいのは様々な組み込みボードをサポートしている。
有名どころはばっちり抑えているようで、STM32 Nucleoなど最近のボードもサポートされている。  
[Qiita - コマンドラインでArduino開発 : vim + platformio](http://qiita.com/caad1229/items/7b5fb47f034ae6e0baf2)
</blockquote>

コマンド一つでそのマイコンに必要なソフトウェアをインストールしてくれたり、ライブラリのインストーラになったりする凄いソフトウェアです、上記のリンクの記事を読めばだいたいの使い方は分かると思います。

## FT232RL

秋月電子さんが出しているUSB - Serial変換基盤です、FTDI社のFT232Rというチップを利用しているようです。特殊なモードでマイコンにプログラムを書き込みます。
専用の書き込み機を買うと結構値が張るのでありがたいですね。

## Avrdude

[AVRDUDE is a utility to download/upload/manipulate the ROM and EEPROM contents of AVR microcontrollers using the in-system programming technique (ISP).](http://www.nongnu.org/avrdude/)

AVRマイコンにプログラムを書き込むのに、PlatformIOはバックエンドでこれを使用しているみたいです。

しかしAvrdudeもPlatformIOもインターネットに記事が少ない…苦労しました。

## プログラムを書き込む

はじめにFT232Rのドライバをインストールしましょう、お好みのパッケージマネージャや[FTDIのWebサイト](http://www.ftdichip.com/Drivers/VCP.htm)からダウンロードしてください。
普通にFT232Rを使用するだけならドライバは必要無いようなのですが、BitBangモードでFT232Rを使用するには専用のドライバが必要になるそうです。

BitBangモードについては、[この記事](http://ore-kb.net/hard/BitBang/)にくわしく書かれていました。

つぎにPlatformIOをインストールしてください、これもパッケージマネージャや[公式サイト](http://platformio.org/)からインストールできます。

```bash
$ mkdir AtTiny85  
$ cd AtTiny85/  
$ platformio init -b attiny85  
```

さあ、AtTiny85に書き込む準備は整いました

```c
void setup(){
  pinMode(1, OUTPUT);
}

void loop(){
  digitalWrite(1 , HIGH);
  delay(1000);
  digitalWrite(1 , LOW); 
  delay(1000);
}
```
`main.ino`

```ini
[env:attiny85]
platform = atmelavr
framework = arduino
board = attiny85
upload_protocol = diecimila
upload_flags = -v -F -e -p t85 -B9600
board_f_cpu = 1000000L
# upload_port = /dev/ttyUSB0
```
`platformio.ini`

あとは`main.ino`を自動生成されたsrcディレクトリ下、`platformio.ini`を置き換えて下さい。

工夫しているのはPlatformIOの`upload_protocol`と`upload_flags`ぐらいで、Avrdude用にすこしオプションを書いてあるだけです。
マイコンをAtTiny85以外を使いたいなら、`board`の中身を変更してください。

具体的にどう変更すれば良いかわからなければ、`platformio boards` で一覧が表示されます。

さあ、あとは結線をするだけです。

[Atmelのデータシート](http://www.atmel.com/images/Atmel-2586-AVR-8-bit-Microcontroller-ATtiny25-ATtiny45-ATtiny85_Datasheet.pdf)

[結線はこのリンクを参考にしてください](http://qiita.com/erukiti/items/0a51d959082e242e2e2a#%E5%9B%9E%E8%B7%AF)

さあ、出来ましたか？

![](/static/images/Ft232rlToAttiny85.jpg)

それでは `$ platformio run -t upload` して下さい。

書き込めましたか？それではAtTiny85を差し替えてBlinkしてみましょう、AtTiny85のIOはこの通りになっています。

![画像はSparkFun様より](/static/images/AttinyBlink.png)

ArduinoでLEDを光らせていた時の1/10程度の大きさで同じことが実現できてしまいましたね、最高です。

![Blink](/static/images/Blink.jpg)

## Thanks

紹介できなかった私が参考にしたリンク達

- [FT232RLでAVRライターを自作してATtiny85をDigispark互換にするまで](http://qiita.com/erukiti/items/0a51d959082e242e2e2a)
- [Arduino IDEでATtiny他の開発（Arduino-ISP編）](http://make.kosakalab.com/make/electronic-work/arduino-ide-arduinoisp/)
- [Tiny AVR Programmer Hookup Guide](https://learn.sparkfun.com/tutorials/tiny-avr-programmer-hookup-guide/attiny85-use-hints)
- [電子工作に必要な道具達](http://techlife.cookpad.com/entry/2015/10/28/080000)
- [ステップアップ電子工作/レベル1前編 ブレッドボードで電子工作を始めよう (まずは準備編)](http://qiita.com/erukiti/items/1a517c3e424835ca14f1)
- [AVRのタイマー](http://startelc.com/AVR/Avr_100timrMemo.html)
- [ArduinoユーザーのためのATTiny入門](http://qiita.com/tadfmac/items/3a42a641531f2c3679a1)
- [Using timers to generate 38KHZ - for IR](http://www.ernstc.dk/arduino/38khz_timer.htm)
- [C言語　演算子](https://www40.atwiki.jp/system-ed/pages/16.html)
- [Arduinoから取り外したATmega328を外付け水晶振動子(発振子)で動作させる。](http://tyk-systems.com/ATmega328/ATmega328.html)
- [arduino(AVR)でキャリア波38KHzを発生させる](http://www.wsnak.com/wsnakblog/?p=4110)

