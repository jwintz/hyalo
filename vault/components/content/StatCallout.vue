<script setup lang="ts">
/**
 * StatCallout — Full-bleed dark band with large stats over a parallax image.
 *
 * Cinematic dark overlay with oversized numbers and labels.
 * Used as a visual break between content sections.
 */

interface Stat {
  value: string
  label: string
}

const props = defineProps<{
  image?: string
  stats?: Stat[]
  caption?: string
}>()

const rootRef = ref<HTMLElement | null>(null)
const parallaxY = ref(0)
let rafId: number | null = null

function tick() {
  if (rootRef.value) {
    const rect = rootRef.value.getBoundingClientRect()
    const progress = (window.innerHeight / 2 - rect.top - rect.height / 2) / window.innerHeight
    parallaxY.value = progress * 60
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
    <section ref="rootRef" class="sc-root">
      <div
        class="sc-bg"
        :style="{ transform: `translate3d(0, ${parallaxY}px, 0)` }"
      >
        <img v-if="image" :src="image" alt="" role="presentation" class="sc-bg-img" />
        <div class="sc-overlay" />
      </div>
      <div class="sc-content">
        <div v-if="stats?.length" class="sc-stats">
          <div v-for="(s, i) in stats" :key="i" class="sc-stat">
            <span class="sc-value">{{ s.value }}</span>
            <span class="sc-unit">{{ s.label }}</span>
          </div>
        </div>
        <p v-if="caption" class="sc-caption">{{ caption }}</p>
        <slot />
      </div>
    </section>
  </ClientOnly>
</template>

<style scoped>
.sc-root {
  position: relative;
  width: 100vw;
  left: 50%;
  right: 50%;
  margin-left: -50vw;
  margin-right: -50vw;
  min-height: 480px;
  display: flex;
  align-items: center;
  justify-content: center;
  overflow: hidden;
}

.sc-bg {
  position: absolute;
  inset: -12% 0;
  will-change: transform;
}
.sc-bg-img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  object-position: center;
  display: block;
}
.sc-overlay {
  position: absolute;
  inset: 0;
  background: linear-gradient(
    160deg,
    rgba(0, 0, 0, 0.78) 0%,
    rgba(0, 0, 0, 0.55) 50%,
    rgba(0, 0, 0, 0.72) 100%
  );
}

.sc-content {
  position: relative;
  z-index: 1;
  text-align: center;
  padding: 5rem 2rem;
  max-width: 960px;
}

.sc-stats {
  display: flex;
  flex-wrap: wrap;
  gap: 1.5rem 5rem;
  justify-content: center;
  margin-bottom: 2.5rem;
}

.sc-stat {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.5rem;
}

.sc-value {
  font-size: clamp(3rem, 9vw, 7rem);
  font-weight: 800;
  letter-spacing: -0.04em;
  line-height: 0.85;
  color: #fff;
}

.sc-unit {
  font-size: clamp(0.8rem, 1.2vw, 0.95rem);
  font-weight: 500;
  color: rgba(255, 255, 255, 0.65);
  max-width: 18ch;
  text-align: center;
  line-height: 1.4;
}

.sc-caption {
  font-size: clamp(0.95rem, 1.6vw, 1.15rem);
  color: rgba(255, 255, 255, 0.55);
  line-height: 1.6;
  max-width: 50ch;
  margin: 0 auto;
}

@media (max-width: 640px) {
  .sc-stats { gap: 1rem 2.5rem; }
  .sc-root { min-height: 380px; }
}
</style>
