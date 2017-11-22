<template lang="pug">
#app
    header-components
    router-view
    footer-components
</template>

<script>
import Vue from 'vue'
import Header from './components/Header.vue'
import Footer from './components/Footer.vue'

export default {
  name: 'app'
}

Vue.component('header-components', Header)
Vue.component('footer-components', Footer)
</script>

<style>
h1:before, h2:before, h3:before {
    content: "# ";
    color: #e65100;
}

</style>
