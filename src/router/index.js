import Vue from 'vue'
import Router from 'vue-router'
import RootPage from '@/components/RootPage'
import PostPage from '@/components/PostPage'
import PostListPage from '@/components/PostListPage'
import TagPageListPage from '@/components/TagPageListPage'

Vue.use(Router)

export default new Router({
  routes: [
    {
      path: '/',
      name: 'RootPage',
      component: RootPage
    },
    {
      path: '/posts',
      name: 'PostListPage',
      component: PostListPage
    },
    {
      path: '/posts/:id',
      name: 'PostPage',
      component: PostPage
    },
    {
      path: '/tags/:id',
      name: 'TagPageListPage',
      component: TagPageListPage
    }
  ]
})
