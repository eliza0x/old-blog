<template lang="pug">
    footer.page-footer.grey.darken-4
        .container
            .row
                .col.s9
                    h5.white-text About me
                    p.grey-text.text-lighten-2 大学一年生のプログラマです。Haskellが好きです、ここ最近はコンパイラやCPUを作っています。楽しいものを見つけていきたいと思っています。Kosen14s, Hxs, CSL, oit-compに所属しています。
                    p.grey-text.text-lighten-2 このブログはVue.jsで生成して、Github Pagesで運用しています。
                .col.s3
                    h5.white-text Links
                    ul
                        li 
                            a(href="https://github.com/eliza0x").grey-text.text-lighten-2
                                | Github
                        li 
                            a(href="https://twitter.com/Eliza_0x").grey-text.text-lighten-2
                                | Twitter
                        li
                            a(href="https://www.facebook.com/profile.php?id=100023006521490").grey-text.text-lighten-2
                                | Facebook
                        li
                            a(href="mailto:me@eliza.link").grey-text.text-lighten-1
                                | Mail

        .footer-copyright
            .container © 2017 Copyright Text
</template>

<script>
export default {
  name: 'FooterComponents',
  data () {
    return {
    }
  }
}
</script>
