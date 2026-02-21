<script setup lang="ts">
import { withBase } from 'ufo'

/**
 * AppHeaderLogo — Hyalo vault override
 *
 * The Hyalo logo SVG uses `currentColor` fills, so it adapts to light/dark
 * mode automatically without any CSS filter inversion. Includes the idle
 * blink animation from the lithos default.
 */

const config = useRuntimeConfig()
const baseURL = config.app.baseURL || '/'

const siteTitle = computed(() => {
  const name = (config.public.siteName as string) || (config.public.vaultName as string) || 'Hyalo'
  return name.charAt(0).toUpperCase() + name.slice(1)
})

const rawLogoPath = computed(() => withBase('/_raw/logo.svg', baseURL))
const rawBlinkPath = computed(() => withBase('/_raw/logo-blink.svg', baseURL))

const isBlinking = ref(false)
let blinkTimer: ReturnType<typeof setTimeout> | null = null
let hoverActive = false

function singleBlink() {
  isBlinking.value = true
  setTimeout(() => { isBlinking.value = false }, 150)
}

function doubleBlink() {
  isBlinking.value = true
  setTimeout(() => {
    isBlinking.value = false
    setTimeout(() => {
      isBlinking.value = true
      setTimeout(() => { isBlinking.value = false }, 150)
    }, 200)
  }, 150)
}

function scheduleBlink() {
  blinkTimer = setTimeout(() => {
    if (!hoverActive) {
      Math.random() < 0.3 ? doubleBlink() : singleBlink()
    }
    scheduleBlink()
  }, Math.random() * 20000)
}

function onMouseEnter() { hoverActive = true; isBlinking.value = true }
function onMouseLeave() { hoverActive = false; isBlinking.value = false }

onMounted(() => scheduleBlink())
onUnmounted(() => { if (blinkTimer) clearTimeout(blinkTimer) })

const currentSrc = computed(() => isBlinking.value ? rawBlinkPath.value : rawLogoPath.value)
</script>

<template>
  <div
    class="header-logo"
    @mouseenter="onMouseEnter"
    @mouseleave="onMouseLeave"
  >
    <ClientOnly>
      <img
        :src="currentSrc"
        alt="Hyalo"
        class="logo-img"
      />
      <template #fallback>
        <img :src="rawLogoPath" alt="Hyalo" class="logo-img" />
      </template>
    </ClientOnly>
    <span class="logo-text">{{ siteTitle }}</span>
  </div>
</template>

<style scoped>
.header-logo {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  color: inherit;
}

.logo-img {
  height: 28px;
  width: auto;
  /* SVG loaded as <img> can't inherit currentColor — use filter for dark mode */
  filter: none;
  transition: filter 0.2s ease;
}

/* Dark mode: SVG paths are dark-on-transparent, invert to white */
:global(.dark .logo-img) {
  filter: invert(1);
}

.logo-text {
  font-size: 1.125rem;
  font-weight: 700;
  letter-spacing: -0.02em;
}
</style>
