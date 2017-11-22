<template lang="pug">
main 
  .container
    .row
      .col.s12.m9
        main
          h3 Tag: {{ tag }} のついているページ
          postlist-components(:count="100000", :tagFilter="true", :tag="tag")
</template>

<script>
import Vue from 'vue'
import PostList from './PostList.vue'
Vue.component('postlist-components', PostList)

export default {
  name: 'TagPageListPage',
  data () {
    return {
      tag: this.$route.params.id
    }
  }
}
</script>

