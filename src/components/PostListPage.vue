<template lang="pug">
main 
  .container
    .row
      .col.s12.m9
        main
          h3 投稿一覧
          postlist-components(:count=100000, :tagFilter=false)
</template>

<script>
import Vue from 'vue'
import PostList from './PostList.vue'
Vue.component('postlist-components', PostList)

export default {
  name: 'RootPage'
}
</script>

<style lang="stylus">
p
  margin 1rem 0rem
  font-size 1.1rem
</style>

