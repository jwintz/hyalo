<script setup lang="ts">
/**
 * RellaxHero — Apple-style cinematic wordmark hero.
 *
 * Full-viewport opening: oversized product name, short punchy tagline,
 * a large background image with parallax, and CTA buttons floating
 * over a gradient fade. Inspired by apple.com/fr/os/macos hero.
 */

interface Cta {
  label: string
  to: string
  icon?: string
  variant?: string
}

const props = defineProps<{
  label?: string
  title?: string
  subtitle?: string
  tagline?: string
  image?: string
  cta?: Cta[]
}>()

const heroRef = ref<HTMLElement | null>(null)
const parallaxY = ref(0)
const opacity = ref(1)
let rafId: number | null = null

function tick() {
  if (heroRef.value) {
    const top = heroRef.value.getBoundingClientRect().top
    const scrolled = Math.max(0, -top)
    parallaxY.value = scrolled * 0.3
    opacity.value = Math.max(0, 1 - scrolled / 600)
  }
  rafId = null
}
function onScroll() {
  if (!rafId) rafId = requestAnimationFrame(tick)
}
onMounted(() => window.addEventListener('scroll', onScroll, { passive: true }))
onUnmounted(() => {
  window.removeEventListener('scroll', onScroll)
  if (rafId) cancelAnimationFrame(rafId)
})
</script>

<template>
  <ClientOnly>
    <section ref="heroRef" class="rh-root">
      <!-- Background image with parallax -->
      <div
        v-if="image"
        class="rh-bg"
        :style="{ transform: `translate3d(0, ${parallaxY}px, 0)` }"
      >
        <img :src="image" :alt="title ?? 'Hyalo'" class="rh-bg-img" />
      </div>

      <!-- Top gradient overlay -->
      <div class="rh-overlay-top" />

      <!-- Bottom gradient overlay -->
      <div class="rh-overlay-bottom" />

      <!-- Content -->
      <div class="rh-content" :style="{ opacity }">
        <div v-if="label" class="rh-label">{{ label }}</div>
        <h1 class="rh-wordmark">{{ title ?? 'Hyalo' }}</h1>
        <p v-if="subtitle" class="rh-subtitle">{{ subtitle }}</p>
        <p v-if="tagline" class="rh-tagline">{{ tagline }}</p>
        <div v-if="cta?.length" class="rh-cta">
          <UButton
            v-for="(btn, i) in cta"
            :key="i"
            :to="btn.to"
            :variant="(btn.variant as any) ?? 'solid'"
            :leading-icon="btn.icon"
            size="xl"
            color="neutral"
          >{{ btn.label }}</UButton>
        </div>
      </div>
    </section>
  </ClientOnly>
</template>

<style scoped>
.rh-root {
  position: relative;
  width: 100vw;
  left: 50%;
  right: 50%;
  margin-left: -50vw;
  margin-right: -50vw;
  height: 100vh;
  min-height: 640px;
  max-height: 1200px;
  overflow: hidden;
  display: flex;
  align-items: center;
  justify-content: center;
}

.rh-bg {
  position: absolute;
  inset: -10% 0;
  will-change: transform;
}
.rh-bg-img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  object-position: center 40%;
  display: block;
}

.rh-overlay-top {
  position: absolute;
  inset: 0;
  background: linear-gradient(
    to bottom,
    color-mix(in srgb, var(--ui-bg) 80%, transparent) 0%,
    color-mix(in srgb, var(--ui-bg) 30%, transparent) 40%,
    transparent 60%
  );
  pointer-events: none;
}

.rh-overlay-bottom {
  position: absolute;
  bottom: 0;
  left: 0;
  right: 0;
  height: 40%;
  background: linear-gradient(
    to top,
    var(--ui-bg) 0%,
    color-mix(in srgb, var(--ui-bg) 60%, transparent) 50%,
    transparent 100%
  );
  pointer-events: none;
}

.rh-content {
  position: relative;
  z-index: 1;
  display: flex;
  flex-direction: column;
  align-items: center;
  text-align: center;
  padding: 0 2rem;
  will-change: opacity;
}

.rh-label {
  font-size: 0.8rem;
  font-weight: 600;
  letter-spacing: 0.15em;
  text-transform: uppercase;
  color: var(--ui-text-muted);
  margin-bottom: 1rem;
}

.rh-wordmark {
  font-size: clamp(4rem, 14vw, 12rem);
  font-weight: 800;
  letter-spacing: -0.05em;
  line-height: 0.9;
  color: var(--ui-text);
  margin: 0;
  padding: 0;
}

.rh-subtitle {
  font-size: clamp(1.1rem, 2.5vw, 1.75rem);
  font-weight: 600;
  color: var(--ui-text);
  margin: 1.5rem 0 0;
  max-width: 36ch;
  line-height: 1.3;
}

.rh-tagline {
  font-size: clamp(0.95rem, 1.8vw, 1.2rem);
  font-weight: 500;
  color: var(--ui-text-muted);
  margin: 0.75rem 0 0;
  max-width: 48ch;
  line-height: 1.5;
}

.rh-cta {
  display: flex;
  gap: 1rem;
  flex-wrap: wrap;
  justify-content: center;
  margin-top: 2rem;
}

@media (max-width: 640px) {
  .rh-root {
    min-height: 520px;
  }
}
</style>
