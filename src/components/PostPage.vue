<template lang="pug">
div 
  .container
    .row
      .col.s12
        main#article
            header
                h1 {{ currentPage.title }}
                div.chip 
                    img(:src="\"/static/images/\"+currentPage.auther+\".png\"" alt="Auther ")
                    | auther: {{ currentPage.auther }}
                div(v-for="tag in currentPage.tags").chip 
                    a(:href="\"/#/tags/\"+tag").tag {{ tag }}
                p date: {{ currentPage.date }}

            section(v-html="html")
</template>

<script>
import $ from 'jquery'
import axios from 'axios'
import materialize from 'materialize-css'
import articleInfo from '../../static/articles.json'

export default {
  name: 'PostPage',
  data () {
    return {
      html: '',
      currentURI: '/#/posts/' + this.$route.params.id,
      articleURI: '/static/posts/' + this.$route.params.id,
      currentPage: {}
    }
  },
  computed: {
    markdownURI () {
      return this.currentPage.file
    }
  },
  created () {
    var marked = require('marked')
    ;(function () {
      var renderer = new marked.Renderer()
      renderer.code = function (code, language) {
        // eslint-disable-next-line
        return '<pre class="card"><code class="card-content hljs">' + hljs.highlightAuto(code).value + '</code></pre>'
      }
      renderer.image = function (href, title, text) {
        return '<div class="card"><div class="card-image"><img src="' + href + '"></div></div>'
      }
      marked.setOptions({
        renderer: renderer
      })
    })()

    var vm = this
    this.currentPage = articleInfo.find((element, index, array) => {
      if (element.uri === vm.currentURI) {
        return true
      } else {
        return false
      }
    })

    axios.get(this.markdownURI)
      .then(function (response) {
        vm.html = marked(response.data)
      })
      .catch(function (_) {
        materialize.toast('記事の取得に失敗しました', 4000)
      })
    $(document).ready(function () {
      $('.parallax').parallax()
    })
  }
}
</script>

<style>
#article a {
    color: #777;
    font-weight: 900;
}
</style>
