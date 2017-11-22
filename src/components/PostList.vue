<template lang="pug">
  div
    ul
      li(v-for="article in articleList")
        a(:href="article.uri").clear-color
          .card.grey.lighten-4
            .card-content
              span.card-title.article-title {{ article.title }}
              div.chip 
                img(:src="\"/static/images/\"+article.auther+\".png\"" alt="Auther ")
                | auther: {{ article.auther }}
              div(v-for="tag in article.tags").chip 
                a(:href="\"/#/tags/\"+tag").tag \#{{ tag }}
              p {{ article.description }}
</template>

<script>
import articleInfo from '../../static/articles.json'

export default {
  props: ['count', 'tagFilter', 'tag'],
  name: 'PostList',
  data () {
    return {
      articleList: []
    }
  },
  created () {
    var vm = this
    this.articleList = articleInfo
      .slice(0, Math.min(vm.count, articleInfo.length))
      .filter((element, index, array) => { return !(vm.tagFilter) || element.tags.indexOf(vm.tag) >= 0 })
  }
}
</script>

<style lang="stylus">
p
  margin 1rem 0rem
  font-size 1.1rem
.clear-color
  color #222

.tag
  color: #444

</style>

