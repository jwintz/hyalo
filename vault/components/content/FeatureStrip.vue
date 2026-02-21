<script setup lang="ts">
/**
 * FeatureStrip — Alternating full-bleed image + text panel.
 *
 * Apple-style side-by-side layout: one half is a large image with
 * subtle parallax, the other half is text with a label, heading,
 * accent line, body, and a learn-more link. Flip prop reverses order.
 */

const props = defineProps<{
  label?: string
  heading?: string
  accent?: string
  body?: string
  image?: string
  flip?: boolean
  to?: string
  toLinkLabel?: string
}>()

const panelRef = ref<HTMLElement | null>(null)
const parallaxY = ref(0)
let rafId: number | null = null

function tick() {
  if (panelRef.value) {
    const rect = panelRef.value.getBoundingClientRect()
    const progress = (window.innerHeight / 2 - rect.top - rect.height / 2) / window.innerHeight
    parallaxY.value = progress * 50
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
    <section
      ref="panelRef"
      class="fs-root"
      :class="{ 'fs-flip': flip }"
    >
      <div class="fs-img-col">
        <div
          class="fs-img-inner"
          :style="{ transform: `translate3d(0, ${parallaxY}px, 0)` }"
        >
          <img v-if="image" :src="image" :alt="heading ?? ''" class="fs-img" />
        </div>
      </div>
      <div class="fs-text-col">
        <div class="fs-text-inner">
          <span v-if="label" class="fs-label">{{ label }}</span>
          <h2 class="fs-heading">
            <span class="fs-heading-main">{{ heading }}</span>
            <br v-if="accent" />
            <span v-if="accent" class="fs-heading-accent">{{ accent }}</span>
          </h2>
          <p v-if="body" class="fs-body">{{ body }}</p>
          <slot />
          <NuxtLink v-if="to" :to="to" class="fs-link">
            {{ toLinkLabel ?? 'Learn more' }}
            <UIcon name="i-lucide-arrow-right" class="fs-link-arrow" />
          </NuxtLink>
        </div>
      </div>
    </section>
  </ClientOnly>
</template>

<style scoped>
.fs-root {
  position: relative;
  width: 100vw;
  left: 50%;
  right: 50%;
  margin-left: -50vw;
  margin-right: -50vw;
  display: grid;
  grid-template-columns: 1fr 1fr;
  min-height: 560px;
  overflow: hidden;
  background: var(--ui-bg);
}

.fs-flip .fs-img-col  { order: 2; }
.fs-flip .fs-text-col { order: 1; }

.fs-img-col {
  position: relative;
  overflow: hidden;
}
.fs-img-inner {
  position: absolute;
  inset: -10% 0;
  will-change: transform;
}
.fs-img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  object-position: center;
  display: block;
}

.fs-text-col {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: clamp(2.5rem, 6vw, 5rem);
}

.fs-text-inner {
  max-width: 480px;
}

.fs-label {
  display: inline-block;
  font-size: 0.75rem;
  font-weight: 600;
  letter-spacing: 0.12em;
  text-transform: uppercase;
  color: var(--ui-primary);
  margin-bottom: 1.25rem;
}

.fs-heading {
  font-size: clamp(1.75rem, 3.5vw, 2.75rem);
  font-weight: 800;
  letter-spacing: -0.03em;
  line-height: 1.1;
  margin: 0 0 1.5rem;
}
.fs-heading-main { color: var(--ui-text); }
.fs-heading-accent { color: var(--ui-primary); }

.fs-body {
  font-size: 1.05rem;
  line-height: 1.7;
  color: var(--ui-text-muted);
  margin: 0 0 2rem;
}

.fs-link {
  display: inline-flex;
  align-items: center;
  gap: 0.4rem;
  font-size: 0.95rem;
  font-weight: 600;
  color: var(--ui-primary);
  text-decoration: none;
  transition: gap 0.2s ease;
}
.fs-link:hover { gap: 0.7rem; }
.fs-link-arrow { width: 1em; height: 1em; }

@media (max-width: 768px) {
  .fs-root {
    grid-template-columns: 1fr;
    min-height: auto;
  }
  .fs-img-col {
    order: 0 !important;
    height: 280px;
  }
  .fs-img-inner {
    position: relative;
    inset: auto;
    height: 100%;
  }
  .fs-text-col {
    order: 1 !important;
    padding: 2rem 1.5rem 3rem;
  }
}
</style>
