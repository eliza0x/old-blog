<template lang="pug">
main 
  .parallax-container.parallax-height-top
    .parallax
      img(src="/static/images/parallax1.jpg")
  .container
    .row
      .col.s12
        main
          h3 最近の投稿 
          postlist-components(:count=5, :tagFilter=false)
          h5 
            a(href="/#/posts").grey-text.text-darken-2 And more ...
  .parallax-container.parallax-height-bottom
    .parallax
      img(src="/static/images/parallax2.jpg")
</template>

<script>
import $ from 'jquery'
import Vue from 'vue'
import PostList from './PostList.vue'
Vue.component('postlist-components', PostList)

export default {
  name: 'RootPage',
  created () {
    $(document).ready(function () {
      $('.parallax').parallax()
    })
  }
}
</script>

<style lang="stylus">
p
  margin 1rem 0rem
  font-size 1.1rem

.parallax-height-top
  height 400px

.parallax-height-bottom
  height 200px
</style>

