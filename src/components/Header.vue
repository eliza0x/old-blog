<template lang="pug">
nav.grey.darken-4
	.nav-wrapper
		.container
			a(href="/#/").blog-title のばなしうさぎ
			ul#nav-mobile.right.hide-on-med-and-down
				li
					a(href="/#/").orange.darken-4 Home
				li
					a(href="/#/posts") Posts
</template>

<script>
export default {
  name: 'Header',
  data () {
    return {
    }
  }
}
</script>

<style scoped lang="stylus">
.blog-title
  font-size 1.7rem
  margin 0rem 1rem
</style>
